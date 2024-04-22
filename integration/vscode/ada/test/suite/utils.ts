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
    // Normalize the actual string
    actual = normalizeLineEndings(actual);

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

export function normalizeLineEndings(str: string, lineEnding = '\n'): string {
    return str.replace(/\r?\n/g, lineEnding);
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
    assert(ext);
    /**
     * Previously this code returned when ext.isActive was true. This is not
     * enough because it doesn't indicate if any errors occured during
     * activation. Instead, always awaiting the result of the activate() method
     * does report activation errors as a promise rejection.
     */
    await ext.activate();
}
