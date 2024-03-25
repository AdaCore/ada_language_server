import { defineConfig } from '@vscode/test-cli';
import { mkdtempSync } from 'fs';
import * as os from 'os';
import { join } from 'path';

let baseMochaOptions = {
    ui: 'tdd',
    color: true,
};

if (process.env.MOCHA_REPORTER) {
    // If a reporter was specified externally, use it. For example, the CI
    // environment could set this to 'mocha-junit-reporter' to produce JUnit
    // results.
    baseMochaOptions.reporter = process.env.MOCHA_REPORTER;
}

if (!baseMochaOptions.reporterOptions) {
    baseMochaOptions.reporterOptions = {
        maxDiffSize: 0,
    };
}

if (process.env['MOCHA_TIMEOUT']) {
    baseMochaOptions.timeout = process.env['MOCHA_TIMEOUT'];
} else {
    /**
     * Some tests involve calling gprbuild which takes time. So we disable test
     * timeouts altogether.
     */
    baseMochaOptions.timeout = '0';
}

if (process.env['MOCHA_GREP']) {
    baseMochaOptions.grep = process.env['MOCHA_GREP'];
}

const testsuites = ['general', 'gnattest', 'workspace_missing_dirs'];

export default defineConfig(
    testsuites.map((suiteName) => {
        // --user-data-dir is set to a unique directory under the OS
        // default tmp directory for temporary files to avoid
        // warnings related to longs paths in IPC sockets created by
        // VSCode. The directory is made unique to avoid
        // interference between successive runs.
        //
        // It also allows multiple testsuites to run concurrently with each VS
        // Code instance using a different User data directory. This can happen
        // when tests are launched from the VS Code UI.
        const tmpdir = mkdtempSync(`${os.tmpdir()}/vsc-ada-test-`);

        // Create a mocha options objects by copying the base one
        let mochaOptions = { ...baseMochaOptions };

        if (process.env.MOCHA_REPORTER) {
            /**
             * Produce results for each testsuite separately
             */
            const mochaFile = process.env.MOCHA_RESULTS_DIR
                ? join(process.env.MOCHA_RESULTS_DIR, `${suiteName}.xml`)
                : `${suiteName}.xml`;
            mochaOptions.reporterOptions = { mochaFile: mochaFile };
        }

        return {
            label: `Ada extension testsuite: ${suiteName}`,
            files: `out/test/suite/${suiteName}/**/*.test.js`,
            workspaceFolder: `./test/workspaces/${suiteName}`,
            mocha: mochaOptions,
            env: {
                // When working remotely on Linux, it is necessary to have "Xvfb
                // :99" running in the background, and this env variable set for
                // the VS Code instances spawned for testing.
                //
                // This may prevent running locally on Linux and having the test
                // windows visible, but we consider this a minor use case for
                // now. A workaround is to remove this line.
                DISPLAY: ':99',
            },
            launchArgs: [
                // It's important to use the --user-data-dir=<path> form. The
                // --user-data-dir <path> form sometimes gets <path> considered
                // as another workspace root directory.
                `--user-data-dir=${tmpdir}`,
            ],
            // Use external installation if provided in the VSCODE env variable
            useInstallation: process.env.VSCODE ? { fromPath: process.env.VSCODE } : undefined,
        };
    })
);
