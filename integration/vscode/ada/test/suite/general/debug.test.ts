import assert from 'assert';
import { suite, test } from 'mocha';
import { AdaConfig, adaDynamicDebugConfigProvider } from '../../../src/debugConfigProvider';
import { activate } from '../utils';

suite('Debug Configurations', function () {
    this.beforeAll(async () => {
        await activate();
    });

    test('GDB path is explicitely set in offered debug config', async () => {
        const firstConfig = (await adaDynamicDebugConfigProvider.provideDebugConfigurations()).at(
            0
        ) as AdaConfig;

        assert.notEqual(firstConfig.miDebuggerPath, undefined);
    });

    test('GDB path is the same for all configs', async () => {
        const configs =
            (await adaDynamicDebugConfigProvider.provideDebugConfigurations()) as AdaConfig[];

        assert(configs.length > 1);
        const miDebuggerPath = configs.at(0)?.miDebuggerPath;
        assert.notEqual(miDebuggerPath, '');
        for (let i = 0; i < configs.length; i++) {
            const c = configs[i];
            assert.equal(c.miDebuggerPath, miDebuggerPath);
        }
    });

    test('Two debug configs per main are proposed', async () => {
        const configs =
            (await adaDynamicDebugConfigProvider.provideDebugConfigurations()) as AdaConfig[];
        const expected = `
Ada: Debug main - src/main1.adb
Ada: Attach debugger to running process - src/main1.adb
Ada: Debug main - src/test.adb
Ada: Attach debugger to running process - src/test.adb`.trim();
        assert.equal(configs.map((v) => `${v.name}`).join('\n'), expected);
    });
});
