import * as path from 'path';
import * as vscode from 'vscode';
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
export function initializeDebugging(ctx: vscode.ExtensionContext, clients: ContextClients) {
    // Instantiate a DebugConfigProvider for Ada and register it.
    const provider = new AdaDebugConfigProvider(clients);

    // This provider is registered for the 'ada' debugger type. It means that
    // it is triggered either when a configuration with type 'ada' is launched,
    // or when the applicable context of the 'ada' debugger type is enabled
    // (see package.json/debuggers).
    //
    // However concretely we never define debug configurations of the type
    // 'ada'. All the provided configurations use the type 'cppdbg'. This means
    // that once a 'cppdbg' is declared in the launch.json file, this provider
    // is no longer called since it is not registered for the type 'cppdbg'.
    // Moreover, it is somewhat discouraged to register it for the type
    // 'cppdbg' since that type is provided by another extension.
    ctx.subscriptions.push(vscode.debug.registerDebugConfigurationProvider('ada', provider));

    // TODO it is also possible to register another provider with trigger kind
    // 'Dynamic', however the role of such a provider is unclear. In practical
    // experiments it ends up never being called. The above provider is enough
    // to make it possible to launch debug sessions without a launch.json.

    return provider;
}
/**
 * Initialize a debug configuration based on 'cppdbg' for the given executable
 * if specified. Otherwise the program field includes
 * ${command:ada.askForProgram} to prompt the User for an executable to debug.
 *
 * @param program - the executable to debug (optional)
 * @returns an AdaConfig
 */
function initializeConfig(program?: string): AdaConfig {
    // TODO it would be nice if this and the package.json configuration snippet
    // were the same.
    const config: AdaConfig = {
        type: 'cppdbg',
        name: 'Ada: Debugger Launch',
        request: 'launch',
        targetArchitecture: process.arch,
        cwd: '${workspaceFolder}',
        program: '${workspaceFolder}/${command:ada.askForProgram}',
        stopAtEntry: false,
        externalConsole: false,
        args: [],
        MIMode: 'gdb',
        preLaunchTask: 'ada: Build current project',
        setupCommands: setupCmd,
    };

    if (program) {
        const name = path.basename(program);
        config.name = 'Ada: Debug executable - ' + name;
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

    async provideDebugConfigurations(
        folder: vscode.WorkspaceFolder | undefined,
        _token?: vscode.CancellationToken | undefined
    ): Promise<vscode.DebugConfiguration[]> {
        // This method is called when no launch.json exists. The provider
        // should return a set of configurations to initialize the launch.json
        // file with.
        const configs: vscode.DebugConfiguration[] = [];

        if (_token?.isCancellationRequested) {
            return Promise.reject('Cancelled');
        }

        if (folder != undefined) {
            // Offer a list of known Mains from the project
            const execs = await getExecutables(this.clients.adaClient);
            const quickpick = execs.map((e) => ({
                label: vscode.workspace.asRelativePath(e),
                description: 'Generate the associated configuration',
            }));
            const selectedProgram = await vscode.window.showQuickPick(quickpick, {
                placeHolder: 'Select a program to debug',
            });
            if (selectedProgram) {
                // The cppdbg debug configuration exepects the executable to be
                // a full path rather than a path relative to the specified
                // cwd. That is why we include ${workspaceFolder}.
                const configuration = initializeConfig(
                    `\${workspaceFolder}/${selectedProgram.label}`
                );
                configs.push(configuration);
            } else {
                return Promise.reject('Cancelled');
            }
        }

        return configs;
    }

    async resolveDebugConfiguration(
        _folder: vscode.WorkspaceFolder | undefined,
        debugConfiguration: vscode.DebugConfiguration,
        _token?: vscode.CancellationToken | undefined
    ): Promise<vscode.DebugConfiguration | undefined> {
        // This method is called when a debug session is being started. The
        // debug configuration either comes from the launch.json file, or is
        // empty when no launch.json exists.

        if (_token?.isCancellationRequested) {
            return undefined;
        }

        if (debugConfiguration.request == 'launch') {
            // When the given debug configuration has its fields set, it means
            // that the debug configuration is coming from a launch.json file
            // and we don't want to alter it. Concretely this never occurs
            // because we register this provider for the debugger type 'ada'
            // which we never create in launch.json files. Instead we always
            // create 'cppdbg' configurations which never go through this
            // provider.
            return debugConfiguration;
        }

        // We are operating without a launch.json. So we try to determine the
        // program to debug dynamically. If the current editor matches one of
        // the Mains of the project, then debug the corresponding executable.
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
        }

        // There is no current file or it matches no known Main of the project,
        // so we offer all Main in a QuickPicker for the user to choose from.
        const quickpick = execs.map((e) => ({
            label: vscode.workspace.asRelativePath(e),
            description: 'Run & Debug',
            fullPath: e,
        }));
        const selectedProgram = await vscode.window.showQuickPick(quickpick, {
            placeHolder: 'Select an executable to debug',
        });
        if (selectedProgram) {
            // This is an in-memory configuration that will not be stored. It's
            // okay to use the full path directly instead of using
            // ${workspaceFolder}.
            const configuration = initializeConfig(selectedProgram.fullPath);
            return configuration;
        }

        return undefined;
    }

    /**
     * Consults the project for a list of Mains. If only one is defined, it is
     * returned immediately. If multiple ones are defines, a QuickPicker is
     * given to the User to choose and executable to debug or to specify in a
     * debug configuration.
     *
     * @returns the path of the executable to debug relative to the workspace
     */
    async askForProgram(): Promise<string | undefined> {
        const file = vscode.window.activeTextEditor?.document.uri.path;
        const mains = await getMains(this.clients.adaClient);
        const execs = await getExecutables(this.clients.adaClient);

        if (execs.length == 1) return vscode.workspace.asRelativePath(execs[0]);

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
            return vscode.workspace.asRelativePath(execs[index]);
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
