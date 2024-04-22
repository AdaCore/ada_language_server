/* eslint-disable @typescript-eslint/restrict-template-expressions */
/* eslint-disable max-len */
/* eslint-disable @typescript-eslint/no-unsafe-argument */
import assert from 'assert';
import { spawnSync } from 'child_process';
import {
    anything,
    capture,
    instance,
    mock,
    reset,
    resetCalls,
    spy,
    verify,
    when,
} from 'ts-mockito';
import { TestController, TestItem, TestItemCollection, TestMessage, TestRun } from 'vscode';
import { adaExtState } from '../../../src/extension';
import {
    collectLeafItems,
    collectLeafsFromCollection,
    controller,
    determineTestOutcome,
    getGnatTestDriverExecPath,
    getGnatTestDriverProjectPath,
    refreshTestItemTree,
    resolveHandler,
    runHandler,
} from '../../../src/gnattest';
import { activate } from '../utils';
import { escapeRegExp } from '../../../src/helpers';

suite('GNATtest Integration Tests', function () {
    this.beforeAll(async () => {
        await activate();
        const cmd = ['gnattest', '-P', await adaExtState.getProjectFile()];
        const cp = spawnSync(cmd[0], cmd.slice(1));
        if (cp.status != 0) {
            assert.fail(
                `'${cmd.join(' ')}' had status code ${cp.status} and output:\n
                ${cp.stdout.toLocaleString()}\n${cp.stderr.toLocaleString()}`
            );
        }
    });

    test('Resolving tests', async () => {
        /**
         * First initialization of the test tree
         */
        await refreshTestItemTree();

        /**
         * Check initial test list
         */
        assert.equal(
            getTestTree(controller),
            `
[speed1.ads]
[speed2.ads]
`.trim()
        );

        /**
         * Resolve one item and check the outcome
         */
        await resolveHandler(controller.items.get('speed2.ads'));
        assert.equal(
            getTestTree(controller),
            `
[speed1.ads]
[speed2.ads]
  [Desired_Speed 10:13]
  [Set_Desired_Speed 12:14]
  [Adjust_Speed 16:14]
`.trim()
        );

        /**
         * Now perform recursive resolution of the same node.
         */
        await resolveHandler(controller.items.get('speed2.ads'), true);
        assert.equal(
            getTestTree(controller),
            `
[speed1.ads]
[speed2.ads]
  [Desired_Speed 10:13]
    [speed2.ads:10]
  [Set_Desired_Speed 12:14]
    [speed2.ads:12]
  [Adjust_Speed 16:14]
    [speed2.ads:16]
`.trim()
        );

        /**
         * Now resolve all nodes recursively
         */
        await resolveAllTestTree();
        assert.equal(
            getTestTree(controller),
            `
[speed1.ads]
  [Speed 12:13]
    [speed1.ads:12]
  [Adjust_Speed 13:14]
    [speed1.ads:13]
[speed2.ads]
  [Desired_Speed 10:13]
    [speed2.ads:10]
  [Set_Desired_Speed 12:14]
    [speed2.ads:12]
  [Adjust_Speed 16:14]
    [speed2.ads:16]
`.trim()
        );
    });

    /**
     * This tests the choice of IDs and labels for tests in vscode
     */
    test('IDs and labels', async () => {
        /**
         * Resolve all nodes recursively
         *
         * Even though column information is available in the GNATtest XML, only
         * line information is include in test IDs. So a test ID recognized by
         * GNATtest is <tested-source-name>:<tested-subprogram-line>.
         */
        await resolveAllTestTree();
        assert.equal(
            getTestTree(controller, (item) => `[${item.id}] ${item.label}`),
            `
[speed1.ads] Tests for speed1.ads
  [Speed 12:13] Tests for subprogram Speed
    [speed1.ads:12] speed1.ads:12
  [Adjust_Speed 13:14] Tests for subprogram Adjust_Speed
    [speed1.ads:13] speed1.ads:13
[speed2.ads] Tests for speed2.ads
  [Desired_Speed 10:13] Tests for subprogram Desired_Speed
    [speed2.ads:10] speed2.ads:10
  [Set_Desired_Speed 12:14] Tests for subprogram Set_Desired_Speed
    [speed2.ads:12] speed2.ads:12
  [Adjust_Speed 16:14] Tests for subprogram Adjust_Speed
    [speed2.ads:16] speed2.ads:16
`.trim()
        );
    });

    test('Getting leaf tests to run', async () => {
        /**
         * Resolve the entire tree
         */
        await resolveAllTestTree();

        /**
         * Test on second item
         */
        const speed2Item = controller.items.get('speed2.ads') as TestItem;
        assert.equal(
            testItemArrayToStr(collectLeafItems(speed2Item)),
            `
[speed2.ads:10]
[speed2.ads:12]
[speed2.ads:16]
`.trim()
        );

        /**
         * Test on all items
         */
        assert.equal(
            testItemArrayToStr(collectLeafsFromCollection(controller.items)),
            `
[speed1.ads:12]
[speed1.ads:13]
[speed2.ads:10]
[speed2.ads:12]
[speed2.ads:16]
`.trim()
        );
    });

    test('Parsing test results', async () => {
        /**
         * Resolve the entire test tree
         */
        await resolveAllTestTree();

        /**
         * Consider one of the existing tests
         */
        const testItem = collectLeafsFromCollection(controller.items)[0];

        /**
         * Create dummy pass and fail test driver outputs
         */
        const driverOutputSuccess = `
foo
bar
${testItem.id}:4: info: corresponding test PASSED
foo
bar
`.trim();
        const driverOutputFail = `
foo
bar
${testItem.id}:4: error: some test failure message
foo
bar
`.trim();

        /**
         * Create a mock of the TestRun object to test for specific methods
         * being called.
         */
        // The mock object keeps track of method calls.
        const mockRun = mock<TestRun>();
        // The mock instance should be passed to the implementation.
        const run: TestRun = instance(mockRun);

        /**
         * Test that parsing a success message causes the passed() method to be
         * called
         */
        determineTestOutcome(testItem, driverOutputSuccess, run);
        // eslint-disable-next-line @typescript-eslint/no-unsafe-argument
        verify(mockRun.passed(testItem, anything())).once();

        /**
         * Test that parsing a failure message causes the failed() method to be
         * called
         */
        resetCalls(mockRun);
        determineTestOutcome(testItem, driverOutputFail, run);
        // eslint-disable-next-line @typescript-eslint/no-unsafe-argument
        verify(mockRun.failed(testItem, anything(), anything())).once();
        /**
         * Now capture the arguments of the call to failed and check that the
         * test failure message was stored correctly.
         */
        // eslint-disable-next-line max-len
        // eslint-disable-next-line @typescript-eslint/unbound-method, @typescript-eslint/no-unused-vars
        const msg = capture(mockRun.failed).last()[1];
        if (msg instanceof TestMessage) {
            assert.equal(msg.message, `${testItem.id}:4: error: some test failure message`);
        } else {
            throw Error('Could not verify expected message');
        }

        /**
         * Test that when no messages concerning the test are found, the test is
         * marked as errored.
         */
        resetCalls(mockRun);
        determineTestOutcome(testItem, 'No output about the test', run);
        // eslint-disable-next-line @typescript-eslint/no-unsafe-argument
        verify(mockRun.errored(testItem, anything(), anything())).once();

        /**
         * Multiple pass messages should result in an error
         */
        resetCalls(mockRun);
        determineTestOutcome(testItem, driverOutputSuccess + driverOutputSuccess, run);
        // eslint-disable-next-line @typescript-eslint/no-unsafe-argument
        verify(mockRun.errored(testItem, anything(), anything())).once();

        /**
         * Multiple fail messages should result in an error
         */
        resetCalls(mockRun);
        determineTestOutcome(testItem, driverOutputFail + driverOutputFail + driverOutputFail, run);
        // eslint-disable-next-line @typescript-eslint/no-unsafe-argument
        verify(mockRun.errored(testItem, anything(), anything())).once();

        /**
         * Both pass and fail messages should result in an error
         */
        resetCalls(mockRun);
        determineTestOutcome(
            testItem,
            driverOutputFail + driverOutputSuccess + driverOutputFail,
            run
        );
        // eslint-disable-next-line @typescript-eslint/no-unsafe-argument
        verify(mockRun.errored(testItem, anything(), anything())).once();
    });

    test('Running tests', async () => {
        /**
         * Set up a spy on the test controller to stub some methods
         */
        const spyCtrl = spy(controller);
        try {
            /**
             * Create a mock TestRun
             */
            const mockRun = mock<TestRun>();
            /**
             * Capture the output by stubbing the appendOutput() method
             */
            let outputs = '';
            when(mockRun.appendOutput(anything())).thenCall((output: string) => {
                /**
                 * Remove '\\r's from the console outputs for easier comparison
                 */
                outputs += output.replace(/\r\n/g, '\n');
            });
            const run = instance(mockRun);

            /**
             * Install a stub in the test controller spy to return the TestRun mock
             */
            when(spyCtrl.createTestRun(anything(), anything(), anything())).thenReturn(run);

            /**
             * Call the run handler with a request for running all tests.
             */
            await runHandler(
                { include: undefined, exclude: undefined, profile: undefined },
                undefined
            );

            /**
             * Since the gprbuild output can change depending on whether a
             * previous test run has already executed gprbuild, we don't check
             * for it verbatim. Instead we check that the gprbuild command line
             * was called, and that the test driver was called and gave the
             * expected output.
             */
            const buildOutput = `
Building the test harness project
$ "gprbuild" "-P" "${await getGnatTestDriverProjectPath()}"`.trimStart();
            const runOutput = `
Running the test driver
$ "${await getGnatTestDriverExecPath()}" "--passed-tests=show"
speed1.ads:12:4: info: corresponding test PASSED
speed1.ads:13:4: info: corresponding test PASSED
speed2.ads:12:4: info: corresponding test PASSED
speed2.ads:16:4: info: corresponding test PASSED
speed1.ads:12:4: inherited at speed2.ads:20:4: info: corresponding test PASSED
speed2.ads:10:4: error: corresponding test FAILED: Test not implemented. (speed2-auto_controller_test_data-auto_controller_tests.adb:46)
6 tests run: 5 passed; 1 failed; 0 crashed.`.trimStart();
            assert.match(outputs, RegExp(escapeRegExp(buildOutput)));
            assert.match(outputs, RegExp(escapeRegExp(runOutput)));

            /**
             * Check that calling the run handler with an empty include array
             * also yields an execution of all tests.
             */
            outputs = '';
            await runHandler({ include: [], exclude: undefined, profile: undefined }, undefined);
            assert.match(outputs, RegExp(escapeRegExp(buildOutput)));
            assert.match(outputs, RegExp(escapeRegExp(runOutput)));
        } finally {
            /**
             * Reset the controller object on which we set a spy
             */
            reset(spyCtrl);
        }
    });
});

async function resolveAllTestTree() {
    const promises: Promise<void>[] = [];
    controller.items.forEach((item) => promises.push(resolveHandler(item, true)));
    await Promise.all(promises);
}

function getTestTree(
    ctrl: TestController,
    testItemToStr: (item: TestItem) => string = defaultTestItemToStr
): string {
    return testItemCollToStr(ctrl.items, testItemToStr);
}

function testItemArrayToStr(
    items: TestItem[],
    testItemToStr: (items: TestItem) => string = defaultTestItemToStr
): string {
    return items.map((item) => itemWithChildrenToStr(item, testItemToStr)).join('\n');
}

function testItemCollToStr(
    col: TestItemCollection,
    testItemToStr: (item: TestItem) => string = defaultTestItemToStr
) {
    let res = '';
    col.forEach((item) => {
        res += '\n' + itemWithChildrenToStr(item, testItemToStr);
    });
    return res.trim();
}

function itemWithChildrenToStr(item: TestItem, testItemToStr: (item: TestItem) => string) {
    let res = testItemToStr(item);

    if (item.children.size > 0) {
        res += '\n' + indentString(testItemCollToStr(item.children, testItemToStr), 2);
    }

    return res;
}

function defaultTestItemToStr(item: TestItem) {
    return `[${item.id}]`;
}

const indentString = (str: string, count: number, indent = ' ') =>
    str.replace(/^/gm, indent.repeat(count));
