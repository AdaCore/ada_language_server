import assert from 'assert';
import { suite, test } from 'mocha';
import { setTerminalEnvironment } from '../../../src/helpers';

suite('Environment init', () => {
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
