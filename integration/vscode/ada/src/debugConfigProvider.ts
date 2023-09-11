import assert from 'assert';
import * as path from 'path';
import * as vscode from 'vscode';
import { contextClients } from './extension';
import { getExecutables, getMains, getProjectFile } from './helpers';

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
 * Initialize debugging support for Ada projects.
 *
 * @param ctx - the Ada extension context
 * @returns the debug configuration provider
 */
export function initializeDebugging(ctx: vscode.ExtensionContext) {
    // Instantiate a DebugConfigProvider for Ada and register it.
    const provider = new AdaDebugConfigProvider();

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
 * $\{command:ada.getOrAskForProgram\} to prompt the User for an executable to
 * debug.
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
        program: '${workspaceFolder}/${command:ada.getOrAskForProgram}',
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
    public static adaConfigType = 'cppdbg';

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
            const mains = await getMains(contextClients.adaClient);
            const execs = await getExecutables(contextClients.adaClient);
            assert(
                execs.length == mains.length,
                `The ALS returned mains.length = ${mains.length} and ` +
                    `execs.length = ${execs.length}` +
                    `when they should be equal`
            );

            if (mains.length > 0) {
                const quickpick = [];
                for (let i = 0; i < mains.length; i++) {
                    const exec = execs[i];
                    const main = mains[i];
                    quickpick.push({
                        label: vscode.workspace.asRelativePath(main),
                        description: 'Generate the associated launch configuration',
                        execPath: vscode.workspace.asRelativePath(exec),
                    });
                }
                const selectedProgram = await vscode.window.showQuickPick(quickpick, {
                    placeHolder: 'Select a main to create a launch configuration',
                });
                if (selectedProgram) {
                    // The cppdbg debug configuration exepects the executable to be
                    // a full path rather than a path relative to the specified
                    // cwd. That is why we include ${workspaceFolder}.
                    const configuration = initializeConfig(
                        `\${workspaceFolder}/${selectedProgram.execPath}`
                    );
                    configs.push(configuration);
                } else {
                    return Promise.reject('Cancelled');
                }
            } else {
                void warnAboutNoMains();
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

        const exec = await getOrAskForProgram();

        if (exec) {
            return initializeConfig(`\${workspaceFolder}/${exec}`);
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

/**
 * Get an executable based on the project and the current open file.
 *
 * If the project only defines one main, it is returned immediately.
 *
 * If the project defines multiple mains, and if the current open file
 * matches one of the mains, the corresponding executable is returned.
 *
 * Otherwise, the list of mains is offered to the user as a QuickPicker to
 * choose a main file. The executable corresponding to the selected main
 * file is returned.
 *
 * Note that paths are returned relative to the workspace.
 *
 * @returns the path of the executable to debug *relative to the workspace*,
 * or *undefined* if no selection was made.
 */
async function getOrAskForProgram(): Promise<string | undefined> {
    const mains = await getMains(contextClients.adaClient);
    const execs = await getExecutables(contextClients.adaClient);

    assert(
        execs.length == mains.length,
        `The ALS returned mains.length = ${mains.length} and ` +
            `execs.length = ${execs.length}` +
            `when they should be equal`
    );

    if (execs.length == 1) return vscode.workspace.asRelativePath(execs[0]);

    // Check if the current file matches one of the mains of the project. If
    // so, use it.
    const file = vscode.window.activeTextEditor?.document.uri.path;
    if (file != undefined) {
        for (let i = 0; i < mains.length; i++) {
            if (file == mains[i]) {
                return vscode.workspace.asRelativePath(execs[i]);
            }
        }
    }

    if (mains.length > 0) {
        // There is no current file or it matches no known Main of the project,
        // so we offer all Mains in a QuickPicker for the user to choose from.
        const quickpick = [];
        for (let i = 0; i < mains.length; i++) {
            const exec = execs[i];
            const main = mains[i];
            quickpick.push({
                label: vscode.workspace.asRelativePath(main),
                description: 'Select for debugging',
                execRelPath: vscode.workspace.asRelativePath(exec),
            });
        }
        const selectedProgram = await vscode.window.showQuickPick(quickpick, {
            placeHolder: 'Select a main file to debug',
        });
        if (selectedProgram) {
            return selectedProgram.execRelPath;
        }
    } else {
        void warnAboutNoMains();
    }

    return undefined;
}
async function warnAboutNoMains() {
    void vscode.window.showWarningMessage(
        `Your Ada project file '${await getProjectFile(
            contextClients.adaClient
        )}' does not define a 'Main' attribute. ` + 'Debugging is not possible without it.'
    );
}
