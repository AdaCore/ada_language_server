import assert from 'assert';
import * as vscode from 'vscode';
import { contextClients } from './extension';
import { AdaMain, getAdaMains, getProjectFile } from './helpers';

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

    ctx.subscriptions.push(
        vscode.debug.registerDebugConfigurationProvider(
            'ada',
            {
                async provideDebugConfigurations(
                    // eslint-disable-next-line @typescript-eslint/no-unused-vars
                    _folder: vscode.WorkspaceFolder | undefined
                ): Promise<vscode.DebugConfiguration[]> {
                    const quickpick = await createQuickPickItems('Build & Debug');

                    const configs: vscode.DebugConfiguration[] = quickpick.map((i) => {
                        assert(i.adaMain);
                        return initializeConfig(i.adaMain.execFullPath, i.adaMain.mainRelPath());
                    });

                    return configs;
                },
            },
            // The 'Dynamic' trigger type only works if the package.json lists
            // "onDebugDynamicConfigurations:ada" as part of the
            // activationEvents.
            vscode.DebugConfigurationProviderTriggerKind.Dynamic
        )
    );

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
 * debug. If `main` is specified, it is simply used in the name of the launch
 * configuration.
 *
 * @param program - the executable to debug (optional)
 * @param main - the main source file to be displayed in the configuration label (optional)
 * @param name - the full name of the configuration (optional). If provided,
 * this name shortcircuits the automatic naming based on the 'main' or the
 * 'program' parameter.
 * @returns an AdaConfig
 */
function initializeConfig(program?: string, main?: string, name?: string): AdaConfig {
    // TODO it would be nice if this and the package.json configuration snippet
    // were the same.
    const config: AdaConfig = {
        type: 'cppdbg',
        name:
            name ??
            (main
                ? `Ada: Debug main - ${main}`
                : program
                ? `Ada: Debug executable - ${program.replace('${workspaceFolder}/', '')}`
                : 'Ada: Debugger Launch'),
        request: 'launch',
        targetArchitecture: process.arch,
        cwd: '${workspaceFolder}',
        program: program ?? '${workspaceFolder}/${command:ada.getOrAskForProgram}',
        stopAtEntry: false,
        externalConsole: false,
        args: [],
        MIMode: 'gdb',
        preLaunchTask: 'ada: Build current project',
        setupCommands: setupCmd,
    };

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
            const itemDescription = 'Generate the associated launch configuration';
            const quickpick = await createQuickPickItems(itemDescription);

            const generateAll: QuickPickAdaMain = {
                label: 'All of the above',
                description: 'Generate launch configurations for each Main file of the project',
                adaMain: undefined,
            };
            if (quickpick.length > 1) {
                quickpick.push(generateAll);
            }

            const selectedProgram = await vscode.window.showQuickPick(quickpick, {
                placeHolder: 'Select a main file to create a launch configuration',
            });

            if (selectedProgram == generateAll) {
                for (let i = 0; i < quickpick.length; i++) {
                    const item = quickpick[i];
                    if (item != generateAll) {
                        assert(item.adaMain);
                        configs.push(
                            initializeConfig(
                                `\${workspaceFolder}/${item.adaMain.execRelPath()}`,
                                item.adaMain.mainRelPath()
                            )
                        );
                    }
                }
            } else if (selectedProgram) {
                assert(selectedProgram.adaMain);

                // The cppdbg debug configuration exepects the executable to be
                // a full path rather than a path relative to the specified
                // cwd. That is why we include ${workspaceFolder}.
                const configuration = initializeConfig(
                    `\${workspaceFolder}/${selectedProgram.adaMain.execRelPath()}`,
                    selectedProgram.label
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
        // debug configuration either comes from the launch.json file, or from
        // the dynamic configuration provider or is empty when the "Debug: Start
        // Debugging" command is used with no launch.json exists.

        if (_token?.isCancellationRequested) {
            return undefined;
        }

        if (debugConfiguration.request == 'launch') {
            // When the given debug configuration has its fields set, it means
            // that the debug configuration is coming from a launch.json file
            // or from the dynamic configuration provider.
            //
            // Concretely this never occurs because we register this provider
            // for the debugger type 'ada' which we never create neither in
            // launch.json files nor in the dynamic configuration provider.
            // Instead we always create 'cppdbg' configurations which never go
            // through this provider.
            //
            // In the future we may want to use the 'ada' configuration type to
            // intercept it here and create a corresponding 'cppdbg'
            // configuration on the fly.
            return debugConfiguration;
        } else {
            const exec = await getOrAskForProgram();

            if (exec) {
                return initializeConfig(`\${workspaceFolder}/${exec}`);
            }
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

type QuickPickAdaMain = {
    label: string;
    description: string;
    adaMain?: AdaMain;
};

/**
 *
 * @param itemDescription - description to use for each item
 * @param mains - optional list of AdaMains if known on the caller site,
 * otherwise it will be computed by the call
 * @returns a list of objects to use with a QuickPicker, one per Main declared in the project.
 */
async function createQuickPickItems(
    itemDescription: string,
    mains?: AdaMain[]
): Promise<QuickPickAdaMain[]> {
    mains = mains ?? (await getAdaMains());

    await assertProjectHasMains();

    return mains.map((main) => ({
        label: vscode.workspace.asRelativePath(main.mainFullPath),
        description: itemDescription,
        adaMain: main,
    }));
}

/**
 *
 * @param mains - list of AdaMains if available at the caller site, otherwise it
 * will be computed by the call
 * @returns a rejected Promise if the project does not define Mains.
 */
async function assertProjectHasMains(mains?: AdaMain[]) {
    mains = mains ?? (await getAdaMains());

    if (mains.length == 0) {
        const msg =
            `The Ada project '${await getProjectFile(
                contextClients.adaClient
            )}' does not define a 'Main' attribute. ` + 'Debugging is not possible without it.';

        // Display a warning message
        void vscode.window.showWarningMessage(msg);

        // When this function is called through the command
        // ada.getOrAskForProgram, the following message is also displayed in a
        // dialog message.
        return Promise.reject(msg);
    }

    return undefined;
}

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
 * @param mains - a list of AdaMains if available at the caller site, otherwise
 * it is computed by the call.
 * @returns the path of the executable to debug *relative to the workspace*,
 * or *undefined* if no selection was made.
 */
export async function getOrAskForProgram(mains?: AdaMain[]): Promise<string | undefined> {
    // Compute list of mains if not provided by the caller
    mains = mains ?? (await getAdaMains());

    await assertProjectHasMains(mains);

    if (mains.length == 1) return mains[0].execRelPath();

    // Check if the current file matches one of the mains of the project. If
    // so, use it.
    const currentFile = vscode.window.activeTextEditor?.document.uri.path;
    if (currentFile != undefined) {
        const adaMain = await getAdaMainForSourceFile(currentFile, mains);
        if (adaMain) {
            return adaMain.execRelPath();
        }
    }

    // There is no current file or it matches no known Main of the project, so
    // we offer all Mains in a QuickPicker for the user to choose from.
    const quickpick = await createQuickPickItems('Select for debugging', mains);
    const selectedProgram = await vscode.window.showQuickPick(quickpick, {
        placeHolder: 'Select a main file to debug',
    });
    if (selectedProgram) {
        return selectedProgram.adaMain?.execRelPath();
    }

    return undefined;
}

/**
 *
 * @param srcPath - a source file to lookup the corresponding Main declaration
 * @param mains - a list of AdaMains if available at the call site, otherwise it
 * will be computed by the call
 * @returns the AdaMain corresponding to the given source file path, or
 * *undefined* if the source file does not match one of the Mains declared in
 * the project.
 */
async function getAdaMainForSourceFile(
    srcPath: string,
    mains?: AdaMain[]
): Promise<AdaMain | undefined> {
    mains = mains ?? (await getAdaMains());

    return mains.find((val) => srcPath == val.mainFullPath);
}
