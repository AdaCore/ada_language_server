import * as path from 'path';

import { runTests } from '@vscode/test-electron';

async function main() {
    try {
        // The folder containing the Extension Manifest package.json
        // Passed to `--extensionDevelopmentPath`
        const extensionDevelopmentPath = path.resolve(__dirname, '../../');

        // The path to the extension test runner script
        // Passed to --extensionTestsPath
        const extensionTestsPath = path.resolve(__dirname, '../test/suite');
        const testWorkspace = path.resolve(extensionDevelopmentPath, './test/TestWorkspace');

        // Download VS Code, unzip it and run the integration test
        await runTests({
            version: 'stable',
            extensionDevelopmentPath,
            extensionTestsPath,
            launchArgs: [testWorkspace],
        });
    } catch (err) {
        console.error(err);
        console.error('Failed to run tests');
        process.exit(1);
    }
}

// eslint-disable-next-line @typescript-eslint/no-floating-promises
main();
