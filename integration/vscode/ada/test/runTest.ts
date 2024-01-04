import * as path from 'path';
import os from 'os';

import { runTests } from '@vscode/test-electron';
import { TestOptions } from '@vscode/test-electron/out/runTest';

async function main() {
    // The folder containing the Extension Manifest package.json
    // Passed to `--extensionDevelopmentPath`
    const extensionDevelopmentPath = path.resolve(__dirname, '../../');

    const testsuites = ['general', 'gnattest', 'workspace_missing_dirs'];

    let someTestsuiteFailed = false;

    for (const testsuite of testsuites) {
        // The path to the extension test runner script
        //  Passed to --extensionTestsPath
        const extensionTestsPath = path.resolve(
            extensionDevelopmentPath,
            `out/test/suite/${testsuite}`
        );

        // The workspace that will be opened in VSCode
        // Passed as an argument
        const testWorkspace = path.resolve(
            extensionDevelopmentPath,
            `test/workspaces/${testsuite}`
        );

        const testOptions: TestOptions = {
            extensionDevelopmentPath,
            extensionTestsPath,
            // --user-data-dir is set to the OS default directory for temporary files to avoid
            // warnings related to longs paths in IPC sockets created by VSCode.
            launchArgs: ['--user-data-dir', `${os.tmpdir()}`, testWorkspace],
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

        // Catch any errors running this testsuite, but continue running the remaining ones.
        try {
            // Download and unzip VS Code (if it has not been done before), and run this testsuite.
            await runTests(testOptions);
        } catch (err) {
            console.error(err);
            console.error(`Failed to run ${testsuite} testsuite`);
            // If this testsuite failed, flag it so that we can exit with a non zero error code
            // later.
            someTestsuiteFailed = true;
        }
    }

    if (someTestsuiteFailed) {
        process.exit(1);
    }
}

// eslint-disable-next-line @typescript-eslint/no-floating-promises
main();
