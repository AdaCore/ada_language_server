import { Glob, GlobOptionsWithFileTypesUnset } from 'glob';
import Mocha, { MochaOptions } from 'mocha';
import { resolve } from 'path';
import { env } from 'process';

export function run(): Promise<void> {
    const mochaOptions: MochaOptions = {
        ui: 'tdd',
        color: true,
    };

    if (process.env.MOCHA_REPORTER) {
        // If a reporter was specified externally, use it. For example, the CI
        // environment could set this to 'mocha-junit-reporter' to produce JUnit
        // results.
        mochaOptions.reporter = process.env.MOCHA_REPORTER;
    }

    if (!mochaOptions.reporterOptions) {
        mochaOptions.reporterOptions = {
            maxDiffSize: 0,
        };
    }

    // Create the mocha test
    const mocha = new Mocha(mochaOptions);

    const testsRoot = resolve(__dirname, '..');

    return new Promise((c, e) => {
        const globOptions: GlobOptionsWithFileTypesUnset = { cwd: testsRoot };
        const g = new Glob('**/**.test.js', globOptions);
        for (const file of g) {
            mocha.addFile(resolve(testsRoot, file));
        }
        try {
            // This variable is set in the launch configuration (launch.json) of
            // the VS Code workspace to allow debugging without triggering test
            // timeouts.
            if (env['MOCHA_TIMEOUT']) {
                mocha.timeout(env['MOCHA_TIMEOUT']);
            }

            if (env['MOCHA_GREP']) {
                mocha.grep(env['MOCHA_GREP']);
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
