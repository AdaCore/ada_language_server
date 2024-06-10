import assert from 'assert';
import {
    AdaConfig,
    adaDynamicDebugConfigProvider,
    createQuickPicksInitialLaunch,
    getOrFindGdb,
} from '../../../src/debugConfigProvider';
import { exe } from '../../../src/helpers';
import { activate } from '../utils';
import { adaExtState } from '../../../src/extension';

suite('Debug Configurations', function () {
    let expectedConfigs: AdaConfig[];

    this.beforeAll(async () => {
        await activate();
        expectedConfigs = [
            {
                type: 'cppdbg',
                name: 'Ada: Debug main - src/main1.adb',
                request: 'launch',
                targetArchitecture: 'x64',
                cwd: '${workspaceFolder}',
                program: '${workspaceFolder}/obj/main1exec' + exe,
                stopAtEntry: false,
                externalConsole: false,
                args: [],
                MIMode: 'gdb',
                preLaunchTask: 'ada: Build main - src/main1.adb',
                setupCommands: [
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
                ],
                miDebuggerPath: getOrFindGdb() ?? '<undefined>',
            },
            {
                name: 'Ada: Attach debugger to running process - src/main1.adb',
                type: 'cppdbg',
                request: 'attach',
                program: '${workspaceFolder}/obj/main1exec' + exe,
                processId: '${command:pickProcess}',
                MIMode: 'gdb',
                miDebuggerPath: getOrFindGdb() ?? '<undefined>',
            },
            {
                type: 'cppdbg',
                name: 'Ada: Debug main - src/test.adb',
                request: 'launch',
                targetArchitecture: 'x64',
                cwd: '${workspaceFolder}',
                program: '${workspaceFolder}/obj/test' + exe,
                stopAtEntry: false,
                externalConsole: false,
                args: [],
                MIMode: 'gdb',
                preLaunchTask: 'ada: Build main - src/test.adb',
                setupCommands: [
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
                ],
                miDebuggerPath: getOrFindGdb() ?? '<undefined>',
            },
            {
                name: 'Ada: Attach debugger to running process - src/test.adb',
                type: 'cppdbg',
                request: 'attach',
                program: '${workspaceFolder}/obj/test' + exe,
                processId: '${command:pickProcess}',
                MIMode: 'gdb',
                miDebuggerPath: getOrFindGdb() ?? '<undefined>',
            },
        ];
    });

    test('GDB path is explicitely set in offered debug config', async () => {
        const firstConfig = (await adaDynamicDebugConfigProvider.provideDebugConfigurations()).at(
            0
        ) as AdaConfig;

        assert.notEqual(firstConfig.miDebuggerPath, undefined);
    });

    test('GDB path is the same for all configs', async () => {
        const configs = await adaDynamicDebugConfigProvider.provideDebugConfigurations();

        assert(configs.length > 1);
        const miDebuggerPath = configs.at(0)?.miDebuggerPath;
        assert.notEqual(miDebuggerPath, '');
        for (let i = 0; i < configs.length; i++) {
            const c = configs[i];
            assert.equal(c.miDebuggerPath, miDebuggerPath);
        }
    });

    test('Two debug configs per main are proposed', async () => {
        const configs = await adaDynamicDebugConfigProvider.provideDebugConfigurations();
        const expected = `
Ada: Debug main - src/main1.adb
Ada: Attach debugger to running process - src/main1.adb
Ada: Debug main - src/test.adb
Ada: Attach debugger to running process - src/test.adb`.trim();
        assert.equal(configs.map((v) => `${v.name}`).join('\n'), expected);
    });

    test('Initial', async () => {
        const { quickpick } = await createQuickPicksInitialLaunch();

        const expected = `
Ada: Debug main - src/main1.adb
Ada: Attach debugger to running process - src/main1.adb
Ada: Debug main - src/test.adb
Ada: Attach debugger to running process - src/test.adb

All of the above - Create all of the above configurations in the launch.json file`;
        assert.equal(
            quickpick
                .map((e) => `${e.label}${e.description ? ' - ' + e.description : ''}`)
                .join('\n'),
            expected.trim()
        );

        const actualConfigs = quickpick
            .filter((e) => 'conf' in e)
            .map((e) => {
                assert('conf' in e);
                return e.conf;
            });

        assert.deepEqual(actualConfigs, expectedConfigs);
    });

    test('Dynamic', async () => {
        const configs = await adaExtState.dynamicDebugConfigProvider.provideDebugConfigurations();
        assert.deepEqual(configs, expectedConfigs);
    });
});
