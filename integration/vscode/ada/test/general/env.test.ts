import assert from 'assert';
import { setTerminalEnvironment } from '../../src/helpers';
import { matchTaskPathCasing } from '../../src/toolEnvironment';

suite('Environment init', () => {
    for (const inheritedKey of ['PATH', 'Path']) {
        for (const overrideKey of ['PATH', 'Path', 'path']) {
            test(`Windows task override ${overrideKey} replaces inherited ${inheritedKey}`, () => {
                const env = { [inheritedKey]: 'inherited tools', OTHER: 'preserved' };
                const overrides = { [overrideKey]: 'task tools' };
                matchTaskPathCasing(env, overrides, 'win32');
                assert.deepStrictEqual(
                    { ...env, ...overrides },
                    { [overrideKey]: 'task tools', OTHER: 'preserved' },
                );
            });
        }
    }

    test('Unix task environment keys remain case-sensitive', () => {
        const env = { PATH: 'inherited tools', Path: 'unrelated value' };
        matchTaskPathCasing(env, { path: 'another variable' }, 'linux');
        assert.deepStrictEqual(env, { PATH: 'inherited tools', Path: 'unrelated value' });
    });

    test('Env init', () => {
        /**
         * We want to test the cases [existing variable, non-existing variable]
         * x [null value, empty string, non-empty string] exhaustively. So we
         * should end up with 6 test cases.
         */

        const targetEnv = {
            VAR1: '',
            VAR2: '',
            VAR3: '',
            VAR7: 'Should be preserved',
        };

        const userEnv: { [name: string]: string | null } = {
            // Existing variables
            VAR1: null,
            VAR2: '',
            VAR3: 'Some new value',
            // Non-existing variables
            VAR4: null,
            VAR5: '',
            VAR6: 'Some other value',
        };

        setTerminalEnvironment(targetEnv, userEnv);

        const expected = {
            VAR2: '',
            VAR3: 'Some new value',
            VAR5: '',
            VAR6: 'Some other value',
            VAR7: 'Should be preserved',
        };

        assert.deepEqual(targetEnv, expected);
    });
});
