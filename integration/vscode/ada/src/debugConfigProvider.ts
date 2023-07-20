import * as vscode from 'vscode';
import * as path from 'path';
import { ContextClients } from './clients';
import { getExecutables, getMains } from './helpers';

/**
 * Ada Configuration for a debug session
 */
interface AdaConfig extends vscode.DebugConfiguration {
    MIMode: string;
    program: string;
    cwd: string;
    targetArchitecture: string;
    stopAtEntry: boolean;
    args: string[];
    setupCommands: {
        description: string;
        text: string;
        ignoreFailures: boolean;
    }[];
}

/**
 * Initialize debugging on an ada project by creating a default configuration in launch.json
 * @param ctx - the ada extension context
 * @param clients - the language clients
 * @returns the debug configuration provider
 */
export async function initializeDebugging(ctx: vscode.ExtensionContext, clients: ContextClients) {
    const provider = new AdaDebugConfigProvider(clients);
    ctx.subscriptions.push(
        vscode.debug.registerDebugConfigurationProvider(
            AdaDebugConfigProvider.adaConfigType,
            provider
        )
    );

    const workspaceConfig = vscode.workspace.getConfiguration();
    const configurations: vscode.DebugConfiguration[] =
        (workspaceConfig.get('launch.configurations') as vscode.DebugConfiguration[]) || [];

    let status = false;
    for (const config of configurations) {
        if (config.name == 'Debug Ada') {
            status = true;
            break;
        }
    }

    if (!status) {
        const initialDebugConfiguration = initializeConfig(undefined, '${command:AskForProgram}');

        configurations.push(initialDebugConfiguration);

        await workspaceConfig.update(
            'launch.configurations',
            configurations,
            vscode.ConfigurationTarget.Workspace
        );
    }
    return provider;
}
/**
 * Initialize the GDB debug configuration for an executable,
 * either program or command must be specified
 * @param program - the executable to debug (optional)
 * @param command - the command to collect the program to debug (optional)
 * @returns an AdaConfig
 */
function initializeConfig(program?: string, command?: string): AdaConfig {
    // Get the executable name from the relative path
    const config: AdaConfig = {
        type: 'cppdbg',
        name: 'Ada Config',
        request: 'launch',
        targetArchitecture: process.arch,
        cwd: '${workspaceFolder}',
        program: 'Ada executable to debug',
        stopAtEntry: false,
        externalConsole: false,
        args: [],
        MIMode: 'gdb',
        // preLaunchTask: 'gpr: Build Executable for File ' + main_name,
        preLaunchTask: 'ada: Build current project',
        setupCommands: setupCmd,
    };
    if (command) {
        config.name = 'Debug Ada';
        config.program = command;
    } else if (program) {
        const name = path.basename(program);
        config.name = 'Debug executable ' + name;
        config.program = program;
    }
    return config;
}

export class AdaDebugConfigProvider implements vscode.DebugConfigurationProvider {
    private readonly clients: ContextClients;
    public static adaConfigType = 'cppdbg';

    constructor(clients: ContextClients) {
        this.clients = clients;
    }
    // Provider
    async provideDebugConfigurations(
        folder: vscode.WorkspaceFolder | undefined,
        _token?: vscode.CancellationToken | undefined
    ): Promise<vscode.DebugConfiguration[]> {
        // provide a non-existent debug/launch configuration
        const config: vscode.DebugConfiguration[] = [];
        if (_token?.isCancellationRequested) {
            return [];
        }
        if (folder != undefined) {
            const execs = await getExecutables(this.clients.adaClient);
            // Show the option for the user to choose the executable
            const quickpick = execs.map((e) => ({
                label: vscode.workspace.asRelativePath(e),
                description: 'Generate the associated configuration',
            }));
            const selectedProgram = await vscode.window.showQuickPick(quickpick, {
                placeHolder: 'Select a program to debug',
            });
            if (selectedProgram) {
                const configuration = initializeConfig(selectedProgram.label);
                config.push(configuration);
            }
            return config;
        } else {
            return config;
        }
    }

    async resolveDebugConfiguration(
        _folder: vscode.WorkspaceFolder | undefined,
        debugConfiguration: vscode.DebugConfiguration,
        _token?: vscode.CancellationToken | undefined
    ): Promise<vscode.DebugConfiguration | undefined> {
        // resolve a incompleted debug/launch configuration
        if (_token?.isCancellationRequested) {
            return undefined;
        }
        if (debugConfiguration.request == 'launch') {
            return debugConfiguration;
        }
        const file = vscode.window.activeTextEditor?.document.uri.path;
        if (file != undefined) {
            const mains = await getMains(this.clients.adaClient);
            const execs = await getExecutables(this.clients.adaClient);
            for (let i = 0; i < mains.length; i++) {
                if (file == mains[i]) {
                    const config = initializeConfig(execs[i]);
                    return config;
                }
            }
            const quickpick = mains.map((e) => ({
                label: vscode.workspace.asRelativePath(e),
                description: 'Run & Debug',
                main: e,
            }));
            const selectedProgram = await vscode.window.showQuickPick(quickpick, {
                placeHolder: 'Select a main file',
            });
            if (selectedProgram) {
                const index = mains.indexOf(selectedProgram.main);
                const configuration = initializeConfig(execs[index]);
                return configuration;
            }
        }
        return undefined;
    }

    /**
     * Resolves the program path fron the 'Debug Ada' default configuration
     * @returns the executable path to debug
     */
    async initDebugCmd(): Promise<string | undefined> {
        const file = vscode.window.activeTextEditor?.document.uri.path;
        const mains = await getMains(this.clients.adaClient);
        const execs = await getExecutables(this.clients.adaClient);

        if (mains.length == 1) return execs[0];
        if (file != undefined) {
            for (let i = 0; i < mains.length; i++) {
                if (file == mains[i]) {
                    return execs[i];
                }
            }
        }
        const quickpick = mains.map((e) => ({
            label: vscode.workspace.asRelativePath(e),
            description: 'Run & Debug',
            main: e,
        }));
        const selectedProgram = await vscode.window.showQuickPick(quickpick, {
            placeHolder: 'Select a main file',
        });
        if (selectedProgram) {
            const index = mains.indexOf(selectedProgram.main);
            return execs[index];
        }
        return undefined;
    }
}

/**
 * GDB default setup options
 */
const setupCmd = [
    {
        description: 'Catch all Ada exceptions',
        text: 'catch exception',
        ignoreFailures: true,
    },
    {
        description: 'Enable pretty-printing for gdb',
        text: '-enable-pretty-printing',
        ignoreFailures: true,
    },
    {
        description: 'Set Disassembly Flavor to Intel',
        text: '-gdb-set disassembly-flavor intel',
        ignoreFailures: true,
    },
];
