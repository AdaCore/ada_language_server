import assert from 'assert';
import { existsSync, readFileSync, writeFileSync } from 'fs';
import * as vscode from 'vscode';

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
 * environment variable MOCHA_ALS_UPDATE is set
 */
export function update(): boolean {
    return process.env.MOCHA_ALS_UPDATE ? true : false;
}
