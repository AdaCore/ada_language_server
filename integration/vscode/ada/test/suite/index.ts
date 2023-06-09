import { resolve } from 'path';
import { Glob, GlobOptionsWithFileTypesUnset } from 'glob';
import Mocha from 'mocha';
import { env } from 'process';

export function run(): Promise<void> {
    // Create the mocha test
    const mocha = new Mocha({
        ui: 'tdd',
        color: true,
    });

    const testsRoot = resolve(__dirname, '..');

    return new Promise((c, e) => {
        const globOptions: GlobOptionsWithFileTypesUnset = { cwd: testsRoot };
        const g = new Glob('**/**.test.js', globOptions);
        for (const file of g) {
            mocha.addFile(resolve(testsRoot, file));
        }
        try {
            if (env['MOCHA_TIMEOUT']) {
                mocha.timeout(env['MOCHA_TIMEOUT']);
            }

            // Run the mocha test
            mocha.run((failures) => {
                if (failures > 0) {
                    e(new Error(`${failures} tests failed.`));
                } else {
                    c();
                }
            });
        } catch (err) {
            e(err);
        }
    });
}
