/* eslint-disable max-len */
import assert from 'assert';
import path from 'path';
import { exe, getProjectFile } from '../../src/helpers';
import { createAdaTaskProvider } from '../../src/taskProviders';
import { activate, getCommandLines, isCoreTask } from '../utils';

suite('Aggregate Projects Support', function () {
    let projectPath: string;

    // Set the timeout to 15 seconds unless already configured to more, or
    // disabled altogether (timeout() === 0), which is the default when
    // running locally. This has to happen at suite scope: mocha fixes a
    // test's timeout when the test is added, so raising it from inside the
    // test body has no effect.
    const inheritedTimeout = this.timeout();
    if (inheritedTimeout !== 0) {
        this.timeout(Math.max(inheritedTimeout, 15000));
    }

    this.beforeAll(async () => {
        await activate();
        projectPath = await getProjectFile();
    });

    /**
     * Check that the list of offered Ada tasks is expected for
     * aggregate projects, in particular we should offer tasks to
     * build and run the mains of all aggregated projects.
     */
    test('Ada tasks for aggregate projects', async () => {
        const expectedCmdLines = `
ada: Clean current project - gprclean -P ${projectPath}
ada: Build current project - gprbuild -P ${projectPath} '-cargs:ada' -gnatef
ada: Check current file - gprbuild -q -f -c -u -gnatc -P ${projectPath} \${fileBasename} '-cargs:ada' -gnatef
ada: Compile current file - gprbuild -q -f -c -u -P ${projectPath} \${fileBasename} '-cargs:ada' -gnatef
ada: Generate documentation from the project - gnatdoc -P ${projectPath}
ada: Build main - src/main_1.adb - gprbuild -P ${projectPath} src/main_1.adb '-cargs:ada' -gnatef
ada: Run main - src/main_1.adb - .${path.sep}main1exec${exe}
ada: Build main - src/main_2.adb - gprbuild -P ${projectPath} src/main_2.adb '-cargs:ada' -gnatef
ada: Run main - src/main_2.adb - .${path.sep}main2exec${exe}
ada: Build main - src/main_3.adb - gprbuild -P ${projectPath} src/main_3.adb '-cargs:ada' -gnatef
ada: Run main - src/main_3.adb - .${path.sep}main3exec${exe}
`.trim();

        const prov = createAdaTaskProvider();
        /**
         * Exclude GNAT SAS tasks because they are tested in integration-testsuite.
         */
        const actualCommandLines = await getCommandLines(prov, isCoreTask);
        assert.equal(actualCommandLines, expectedCmdLines);
    });
});
