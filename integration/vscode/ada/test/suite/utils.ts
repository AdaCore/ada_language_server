import * as vscode from 'vscode';
import assert from 'assert';
import { env } from 'process';
import { resolve } from 'path';
import Mocha, { MochaOptions } from 'mocha';
import { Glob, GlobOptionsWithFileTypesUnset } from 'glob';
import { existsSync, readFileSync, writeFileSync } from 'fs';

/**
 * This function compares some actual output to an expected referenced stored in
 * a file.
 *
 * If the environment variable MOCHA_ALS_UPDATE is set, the function overwrites
 * the expected reference with the current actual output.
 *
 * @param actual - actual output to be compared with the reference
 * @param expectedUri - path to the file containing the expected output
 */
export function assertEqualToFileContent(actual: string, expectedUri: vscode.Uri) {
    if (update()) {
        writeFileSync(expectedUri.fsPath, actual);
    } else {
        if (!existsSync(expectedUri.fsPath)) {
            throw Error(`Expected output file does not exist: ${expectedUri.fsPath}`);
        }

        const expected: string = readFileSync(expectedUri.fsPath, 'utf-8');
        assert.strictEqual(actual, expected);
    }
}

/**
 *
 * @returns true if the testsuite is running in update mode, i.e. the
 * environment variable MOCHA_ALS_UPDATE is set. For example, the VS Code
 * workspace of this repository provides a launch configuration with that
 * environment variable set to allow quickly updating test references.
 */
export function update(): boolean {
    return process.env.MOCHA_ALS_UPDATE ? true : false;
}

/**
 * This function queries the VS Code API for the Ada extension and waits until
 * it is activated.
 */
export async function activate(): Promise<void> {
    const ext = vscode.extensions.getExtension('AdaCore.ada');
    if (ext !== undefined) {
        if (!ext.isActive) {
            await ext.activate();
        }
    }
}

export function runMochaTests(dirPath: string) {
    const mochaOptions: MochaOptions = {
        ui: 'bdd',
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

    const testsRoot = dirPath;

    return new Promise<void>((c, e) => {
        const globOptions: GlobOptionsWithFileTypesUnset = { cwd: testsRoot };
        const g = new Glob('**/*.test.js', globOptions);
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
