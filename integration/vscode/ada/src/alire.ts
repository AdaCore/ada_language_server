import { accessSync, constants, existsSync, statSync } from 'fs';
import { mkdir, readdir } from 'fs/promises';
import { homedir } from 'os';
import * as path from 'path';
import * as vscode from 'vscode';
import { downloadAlire, getAlireRelease, installAlireArchive } from './alireInstallation';
import { CMD_RESTART_LANG_SERVERS } from './constants';
import { exe, getToolEnvironment, which } from './helpers';
import { createInteractiveToolTask, runTaskAndGetResult } from './taskProviders';
import { setManagedToolDirectories } from './toolEnvironment';

export const CMD_INSTALL_ALIRE = 'ada.installAlire';
const EXECUTABLE_PATH_SETTING = 'alire.executablePath';
let installation: Promise<void> | undefined;

function expandHomeDirectory(value: string): string {
    if (value === '~') return homedir();
    return /^~[/\\]/.test(value) ? path.join(homedir(), value.slice(2)) : value;
}

/** Validate the executable that will be found by name through PATH. */
function validateAlireExecutablePath(value: string): string {
    const executable = expandHomeDirectory(value);
    if (!path.isAbsolute(executable)) {
        throw new Error('Use an absolute path, optionally starting with ~.');
    }
    const name = `alr${exe}`;
    const basename = path.basename(executable);
    if ((process.platform === 'win32' ? basename.toLowerCase() : basename) !== name) {
        throw new Error(`The executable must be named ${name} so tools can find it on PATH.`);
    }
    try {
        if (!statSync(executable).isFile()) throw new Error('Not a file');
        accessSync(executable, constants.X_OK);
    } catch {
        throw new Error(`The file ${executable} does not exist or is not executable.`);
    }
    return executable;
}

function configuredAlireExecutable(): string | undefined {
    const configured = vscode.workspace
        .getConfiguration('ada')
        .get<string>(EXECUTABLE_PATH_SETTING);
    return configured ? validateAlireExecutablePath(configured) : undefined;
}

function alireExecutable(directory: string) {
    return path.join(directory, 'bin', `alr${exe}`);
}

async function chooseInstallationDirectory(): Promise<string | undefined> {
    const selected = await vscode.window.showInputBox({
        title: 'Install Alire',
        prompt: 'Installation directory (new or empty folder, or an existing Alire installation)',
        value: path.join(homedir(), 'alire'),
        ignoreFocusOut: true,
        validateInput: async (value) => {
            const directory = expandHomeDirectory(value);
            if (!path.isAbsolute(directory))
                return 'Enter an absolute path, or a path starting with ~.';
            if (existsSync(alireExecutable(directory))) return undefined;
            try {
                if ((await readdir(directory)).length) {
                    return 'This folder is not empty. Choose another folder to install Alire.';
                }
            } catch (error) {
                if ((error as NodeJS.ErrnoException).code !== 'ENOENT') {
                    return 'This path is not an accessible folder.';
                }
            }
            return undefined;
        },
    });
    return selected === undefined ? undefined : expandHomeDirectory(selected);
}

function useManagedAlire(context: vscode.ExtensionContext, executable: string) {
    const directory = path.dirname(executable);
    setManagedToolDirectories([directory]);
    const collection = context.environmentVariableCollection;
    collection.persistent = true;
    collection.description = 'Makes Alire available in VS Code';
    collection.prepend('PATH', directory + path.delimiter);
}

/** Apply the explicit Alire executable before creating either language server. */
export function initializeAlireEnvironment(context: vscode.ExtensionContext) {
    setManagedToolDirectories([]);
    context.environmentVariableCollection.delete('PATH');
    try {
        const executable = configuredAlireExecutable();
        // With no override, the usual helpers search PATH including terminal settings.
        if (executable) useManagedAlire(context, executable);
    } catch (error) {
        void vscode.window
            .showErrorMessage(
                `Invalid ada.${EXECUTABLE_PATH_SETTING}: ${String(error)} ` +
                    'Alire discovery will use PATH until you correct or clear this setting ' +
                    'and reload VS Code.',
                'Open Alire Settings',
            )
            .then((action) => {
                if (action) {
                    return vscode.commands.executeCommand(
                        'workbench.action.openSettings',
                        `ada.${EXECUTABLE_PATH_SETTING}`,
                    );
                }
                return undefined;
            });
    }
}

/** Reuse or install Alire, then interactively select a compiler and GPRbuild. */
export function installAndSetupAlire(context: vscode.ExtensionContext): Promise<void> {
    // Repeated button clicks share the same installation and setup task.
    installation ??= setupAlire(context).finally(() => {
        installation = undefined;
    });
    return installation;
}

async function setupAlire(context: vscode.ExtensionContext): Promise<void> {
    try {
        const configured = configuredAlireExecutable();
        if (configured) useManagedAlire(context, configured);
        let executable = configured ?? which('alr');
        if (!executable) {
            const destination = await chooseInstallationDirectory();
            if (destination === undefined) return;
            executable = alireExecutable(destination);
            if (!existsSync(executable)) {
                executable = await downloadAndInstallAlire(destination);
            }
            await vscode.workspace
                .getConfiguration('ada')
                .update(EXECUTABLE_PATH_SETTING, executable, vscode.ConfigurationTarget.Global);
            useManagedAlire(context, executable);
        }

        // The selection assistant may ask about MSYS2 and toolchain downloads.
        // Keep it interactive and avoid running in an existing Alire crate.
        await mkdir(context.globalStorageUri.fsPath, { recursive: true });
        const task = createInteractiveToolTask(
            'Select Alire compiler and build tools',
            executable,
            ['toolchain', '--select'],
            context.globalStorageUri.fsPath,
        );
        let result: number | undefined;
        try {
            result = await runTaskAndGetResult(task);
        } finally {
            // Already running servers must also see the newly installed alr.
            await vscode.commands.executeCommand(CMD_RESTART_LANG_SERVERS);
        }
        if (result !== 0) {
            throw new Error(
                'Alire is available, but toolchain setup did not finish. ' +
                    'See the task terminal and run Install Alire again to retry.',
            );
        }
        await vscode.commands.executeCommand('setContext', 'ada.alireReady', true);
        const pathHelp = 'Use Alire outside VS Code';
        void vscode.window
            .showInformationMessage(
                `Alire is ready at ${executable}. Open a new VS Code terminal to use alr.`,
                pathHelp,
            )
            .then(async (action) => {
                if (action !== pathHelp) return;
                const document = await vscode.workspace.openTextDocument({
                    language: 'markdown',
                    content: alirePathInstructions(executable),
                });
                await vscode.window.showTextDocument(document);
            });
    } catch (error) {
        // Cancelling the installation raises AbortError; exit without an error notification.
        if (error instanceof Error && error.name === 'AbortError') return;
        const action = await vscode.window.showErrorMessage(
            `Could not set up Alire: ${String(error)}`,
            'Download Alire Manually',
            'Open Alire Settings',
        );
        if (action === 'Open Alire Settings') {
            await vscode.commands.executeCommand(
                'workbench.action.openSettings',
                `ada.${EXECUTABLE_PATH_SETTING}`,
            );
        } else if (action) {
            await vscode.env.openExternal(vscode.Uri.parse('https://alire.ada.dev/'));
        }
    }
}

async function downloadAndInstallAlire(destination: string): Promise<string> {
    return vscode.window.withProgress(
        {
            location: vscode.ProgressLocation.Notification,
            title: 'Install Alire',
            cancellable: true,
        },
        async (progress, token) => {
            // The VS Code UI uses CancellationToken; download and extraction use AbortSignal.
            const controller = new AbortController();
            const cancellation = token.onCancellationRequested(() => controller.abort());
            try {
                if (token.isCancellationRequested) controller.abort();
                progress.report({ message: 'Finding the latest Alire release...' });
                const release = await getAlireRelease(controller.signal);
                progress.report({ message: 'Downloading Alire...' });
                const archive = await downloadAlire(release, controller.signal);
                progress.report({ message: 'Installing Alire...' });
                return await installAlireArchive(
                    archive,
                    release,
                    destination,
                    getToolEnvironment(),
                    controller.signal,
                );
            } finally {
                cancellation.dispose();
            }
        },
    );
}

/** Explain external shell setup without modifying the user's shell configuration. */
function alirePathInstructions(executable: string): string {
    const directory = path.dirname(executable);
    const introduction =
        '# Use Alire outside VS Code\n\n' + `Alire is installed at:\n\n    ${executable}\n\n`;
    if (process.platform === 'win32') {
        return (
            introduction +
            `Add this directory to your user PATH:\n\n    ${directory}\n\n` +
            '1. Open **Edit environment variables for your account** ' +
            'from the Windows Start menu.\n' +
            '2. Edit **Path**, choose **New**, and add the directory above.\n' +
            '3. Open a new terminal and run `alr --version`.\n'
        );
    }
    const quoted = "'" + directory.replace(/'/g, "'\\''") + "'";
    return (
        introduction +
        'For Bash or Zsh, run:\n\n```sh\n' +
        `export PATH=${quoted}:"$PATH"\n` +
        '```\n\nTo keep this change, add the line to your shell startup file ' +
        '(`~/.bashrc` for Bash, `~/.zshrc` for Zsh, or `~/.profile` for login shells).\n\n' +
        'For Fish, run:\n\n```fish\n' +
        `fish_add_path ${quoted}\n` +
        '```\n\nThen run `alr --version` to check the installation.\n\n' +
        'For SSH, WSL, or containers, run these commands ' +
        'on the machine where Alire was installed.\n'
    );
}
