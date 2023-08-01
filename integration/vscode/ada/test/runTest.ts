import * as path from 'path';

import { runTests } from '@vscode/test-electron';
import { TestOptions } from '@vscode/test-electron/out/runTest';

async function main() {
    try {
        // The folder containing the Extension Manifest package.json
        // Passed to `--extensionDevelopmentPath`
        const extensionDevelopmentPath = path.resolve(__dirname, '../../');

        // The path to the extension test runner script
        // Passed to --extensionTestsPath
        const extensionTestsPath = path.resolve(__dirname, '../test/suite/general');
        const testWorkspace = path.resolve(extensionDevelopmentPath, './test/TestWorkspace');

        const testOptions: TestOptions = {
            extensionDevelopmentPath,
            extensionTestsPath,
            launchArgs: [testWorkspace],
        };

        if (process.env.VSCODE) {
            // If specified, use the VSCode executable provided externally. This
            // can be use to test with an externally installed VS Code version
            // such as in a CI environment for example.
            //
            // The expected value is the path to <install-dir>/code and not
            // <install-dir>/bin/code.
            testOptions.vscodeExecutablePath = process.env.VSCODE;
        } else {
            // Otherwise download the latest stable version and test using that.
            testOptions.version = 'stable';
        }

        // Download VS Code, unzip it and run the integration test
        await runTests(testOptions);

        testOptions.extensionTestsPath = path.resolve(__dirname, '../test/suite/gnattest');
        testOptions.launchArgs = [
            path.resolve(extensionDevelopmentPath, './test/GnattestWorkspace'),
        ];
        await runTests(testOptions);
    } catch (err) {
        console.error(err);
        console.error('Failed to run tests');
        process.exit(1);
    }
}

// eslint-disable-next-line @typescript-eslint/no-floating-promises
main();
