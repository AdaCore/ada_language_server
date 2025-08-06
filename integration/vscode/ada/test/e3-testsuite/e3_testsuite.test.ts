/* eslint-disable @typescript-eslint/no-unsafe-argument */
import assert from 'assert';
import { EOL } from 'os';
import path from 'path';
import { anything, instance, mock, reset, spy, when } from 'ts-mockito';
import { TestItem, TestItemCollection, TestMessage, TestRun, Uri, workspace } from 'vscode';
import { CancellationTokenSource } from 'vscode-languageclient';
import * as e3 from '../../src/e3Testsuite';
import { activate } from '../utils';

suite('e3-testsuite', function () {
    this.beforeAll(async function () {
        await activate();
    });

    test('Test list', async function () {
        await e3.controller.refreshHandler!(new CancellationTokenSource().token);

        assert.deepStrictEqual(toTestResult(e3.controller.items), [
            {
                id: Uri.joinPath(workspace.workspaceFolders![0].uri, 'testsuite.py').toString(),
                label: 'testsuite.py',
                children: [
                    {
                        id: '01-test-one-result',
                        label: '01-test-one-result',
                        children: [],
                    },
                    {
                        id: '02-test-multiple-results',
                        label: '02-test-multiple-results',
                        children: [],
                    },
                    {
                        id: '03-test-multiple-passing-results',
                        label: '03-test-multiple-passing-results',
                        children: [],
                    },
                    {
                        id: '04-test-only-passing-sub-results',
                        label: '04-test-only-passing-sub-results',
                        children: [],
                    },
                    {
                        id: '05-test-only-sub-results-one-failing',
                        label: '05-test-only-sub-results-one-failing',
                        children: [],
                    },
                    {
                        id: '06-test-with-diff',
                        label: '06-test-with-diff',
                        children: [],
                    },
                    {
                        id: '07-test-with-no-results',
                        label: '07-test-with-no-results',
                        children: [],
                    },
                ],
            },
        ]);
    });

    test('Test list after run', async function () {
        await e3.controller.refreshHandler!(new CancellationTokenSource().token);

        const spyCtrl = spy(e3.controller);
        try {
            const mockRun = mock<TestRun>();
            let consoleOutput = '';
            when(mockRun.appendOutput(anything())).thenCall((output: string) => {
                consoleOutput += output.replace(/(\r+\n|\n\r+)/g, '\n');
            });

            const run = instance(mockRun);

            when(spyCtrl.createTestRun(anything())).thenReturn(run);
            when(spyCtrl.createTestRun(anything(), anything())).thenReturn(run);
            when(spyCtrl.createTestRun(anything(), anything(), anything())).thenReturn(run);

            await e3.runHandler(
                { include: undefined, exclude: undefined, profile: undefined },
                new CancellationTokenSource().token,
            );

            const expectedResult = [
                {
                    id: Uri.joinPath(workspace.workspaceFolders![0].uri, 'testsuite.py').toString(),
                    label: 'testsuite.py',
                    children: [
                        {
                            id: '01-test-one-result',
                            label: '01-test-one-result',
                            children: [],
                        },
                        {
                            id: '02-test-multiple-results',
                            label: '02-test-multiple-results',
                            /**
                             * We expect these sub-results to appear after the test run
                             */
                            children: [
                                {
                                    id: '02-test-multiple-results.sub3',
                                    label: '02-test-multiple-results.sub3',
                                    children: [],
                                },
                                {
                                    id: '02-test-multiple-results.sub2',
                                    label: '02-test-multiple-results.sub2',
                                    children: [],
                                },
                                {
                                    id: '02-test-multiple-results.sub1',
                                    label: '02-test-multiple-results.sub1',
                                    children: [],
                                },
                            ],
                        },
                        {
                            id: '03-test-multiple-passing-results',
                            label: '03-test-multiple-passing-results',
                            /**
                             * We expect these sub-results to appear after the test run
                             */
                            children: [
                                {
                                    id: '03-test-multiple-passing-results.sub3',
                                    label: '03-test-multiple-passing-results.sub3',
                                    children: [],
                                },
                                {
                                    id: '03-test-multiple-passing-results.sub2',
                                    label: '03-test-multiple-passing-results.sub2',
                                    children: [],
                                },
                                {
                                    id: '03-test-multiple-passing-results.sub1',
                                    label: '03-test-multiple-passing-results.sub1',
                                    children: [],
                                },
                            ],
                        },
                        {
                            id: '04-test-only-passing-sub-results',
                            label: '04-test-only-passing-sub-results',
                            /**
                             * We expect these sub-results to appear after the test run
                             */
                            children: [
                                {
                                    id: '04-test-only-passing-sub-results.sub3',
                                    label: '04-test-only-passing-sub-results.sub3',
                                    children: [],
                                },
                                {
                                    id: '04-test-only-passing-sub-results.sub2',
                                    label: '04-test-only-passing-sub-results.sub2',
                                    children: [],
                                },
                                {
                                    id: '04-test-only-passing-sub-results.sub1',
                                    label: '04-test-only-passing-sub-results.sub1',
                                    children: [],
                                },
                            ],
                        },
                        {
                            id: '05-test-only-sub-results-one-failing',
                            label: '05-test-only-sub-results-one-failing',
                            /**
                             * We expect these sub-results to appear after the test run
                             */
                            children: [
                                {
                                    id: '05-test-only-sub-results-one-failing.sub3',
                                    label: '05-test-only-sub-results-one-failing.sub3',
                                    children: [],
                                },
                                {
                                    id: '05-test-only-sub-results-one-failing.sub2',
                                    label: '05-test-only-sub-results-one-failing.sub2',
                                    children: [],
                                },
                                {
                                    id: '05-test-only-sub-results-one-failing.sub1',
                                    label: '05-test-only-sub-results-one-failing.sub1',
                                    children: [],
                                },
                            ],
                        },
                        {
                            id: '06-test-with-diff',
                            label: '06-test-with-diff',
                            children: [],
                        },
                        {
                            id: '07-test-with-no-results',
                            label: '07-test-with-no-results',
                            children: [],
                        },
                    ],
                },
            ];

            const actualResult = toTestResult(e3.controller.items);

            try {
                assert.deepStrictEqual(actualResult, expectedResult);
            } catch (error) {
                // Include console output in the error message for debugging
                const errorMessage =
                    `Test structure assertion failed.\n\n` +
                    `Console output:\n${consoleOutput}\n\n` +
                    `Original error:\n${String(error)}`;
                throw new Error(errorMessage);
            }
        } finally {
            reset(spyCtrl);
        }
    });

    test('Capturing test results', async function () {
        await e3.controller.refreshHandler!(new CancellationTokenSource().token);

        const spyCtrl = spy(e3.controller);
        try {
            const mockRun = mock<TestRun>();
            let consoleOutput = '';
            when(mockRun.appendOutput(anything())).thenCall((output: string) => {
                consoleOutput += output.replace(/(\r+\n|\n\r+)/g, '\n');
            });

            const enqueued: string[] = [];
            const started: string[] = [];
            const passed: string[] = [];
            const failed: { id: string; message: TestMessage[] }[] = [];
            const errors: { id: string; message: TestMessage[] }[] = [];

            when(mockRun.enqueued(anything())).thenCall((item: TestItem) => {
                enqueued.push(item.id);
            });
            when(mockRun.started(anything())).thenCall((item: TestItem) => {
                started.push(item.id);
            });
            when(mockRun.passed(anything())).thenCall((item: TestItem) => {
                passed.push(item.id);
            });
            when(mockRun.failed(anything(), anything())).thenCall(
                (item: TestItem, message?: TestMessage | TestMessage[]) => {
                    let messages: TestMessage[] = [];
                    if (Array.isArray(message)) {
                        messages = message;
                    } else if (message) {
                        messages = [message];
                    }
                    failed.push({ id: item.id, message: messages });
                },
            );
            when(mockRun.errored(anything(), anything())).thenCall(
                (item: TestItem, message?: TestMessage | TestMessage[]) => {
                    let messages: TestMessage[] = [];
                    if (Array.isArray(message)) {
                        messages = message;
                    } else if (message) {
                        messages = [message];
                    }
                    errors.push({ id: item.id, message: messages });
                },
            );

            const run = instance(mockRun);

            when(spyCtrl.createTestRun(anything())).thenReturn(run);
            when(spyCtrl.createTestRun(anything(), anything())).thenReturn(run);
            when(spyCtrl.createTestRun(anything(), anything(), anything())).thenReturn(run);

            await e3.runHandler(
                { include: undefined, exclude: undefined, profile: undefined },
                new CancellationTokenSource().token,
            );

            const workspaceRoot = workspace.workspaceFolders![0].uri.fsPath;
            const testsuitePath = `${workspaceRoot}${path.sep}testsuite.py`;
            const pythonPathRegex =
                /Running: "(.*python)" ".+testsuite\.py" "--failure-exit-code=0"/;

            // Extract the python path from the output
            const pythonMatch = consoleOutput.match(pythonPathRegex);
            assert.ok(pythonMatch, `Python path not found in console output: ${consoleOutput}`);
            const pythonPath = pythonMatch[1];

            // Check that the console output contains the expected command and results
            const expectedCommand =
                `Running: "${pythonPath}" "${testsuitePath}" ` + '"--failure-exit-code=0"';
            assert.ok(
                consoleOutput.includes(expectedCommand),
                `Expected command '${expectedCommand}' not` +
                    ` found in console output: ${consoleOutput}`,
            );

            // Check for expected test results (order-independent)
            const expectedResults = [
                'INFO     FAIL            06-test-with-diff: Failure short message',
                'INFO     PASS            01-test-one-result',
                'INFO     PASS            04-test-only-passing-sub-results.sub3',
                'INFO     PASS            04-test-only-passing-sub-results.sub2',
                'INFO     PASS            04-test-only-passing-sub-results.sub1',
                'INFO     PASS            05-test-only-sub-results-one-failing.sub3',
                'INFO     FAIL            05-test-only-sub-results-one-failing.sub2',
                'INFO     PASS            05-test-only-sub-results-one-failing.sub1',
                'INFO     PASS            03-test-multiple-passing-results',
                'INFO     PASS            03-test-multiple-passing-results.sub3',
                'INFO     PASS            03-test-multiple-passing-results.sub2',
                'INFO     PASS            03-test-multiple-passing-results.sub1',
                'INFO     PASS            02-test-multiple-results',
                'INFO     FAIL            02-test-multiple-results.sub3: Failure message',
                'INFO     FAIL            02-test-multiple-results.sub2: Failure message',
                'INFO     PASS            02-test-multiple-results.sub1',
            ];

            for (const result of expectedResults) {
                assert.ok(
                    consoleOutput.includes(result),
                    `Expected result not found: ${result}\nActual console output: ${consoleOutput}`,
                );
            }

            // Check for the summary section
            const expectedSummary = 'INFO     Summary:\n  PASS         12\n  FAIL         4';
            assert.ok(
                consoleOutput.includes(expectedSummary),
                `Expected summary not found in console output: ${consoleOutput}`,
            );

            assert.deepStrictEqual(enqueued.sort(), [
                '01-test-one-result',
                '02-test-multiple-results',
                '03-test-multiple-passing-results',
                '04-test-only-passing-sub-results',
                '05-test-only-sub-results-one-failing',
                '06-test-with-diff',
                '07-test-with-no-results',
            ]);
            assert.deepStrictEqual(started.sort(), [
                '01-test-one-result',
                '02-test-multiple-results',
                '03-test-multiple-passing-results',
                '04-test-only-passing-sub-results',
                '05-test-only-sub-results-one-failing',
                '06-test-with-diff',
                '07-test-with-no-results',
            ]);
            assert.deepStrictEqual(passed.sort(), [
                '01-test-one-result',
                '02-test-multiple-results',
                '02-test-multiple-results.sub1',
                '03-test-multiple-passing-results',
                '03-test-multiple-passing-results.sub1',
                '03-test-multiple-passing-results.sub2',
                '03-test-multiple-passing-results.sub3',
                '04-test-only-passing-sub-results.sub1',
                '04-test-only-passing-sub-results.sub2',
                '04-test-only-passing-sub-results.sub3',
                '05-test-only-sub-results-one-failing.sub1',
                '05-test-only-sub-results-one-failing.sub3',
            ]);
            // Sort failed tests by ID to make assertion order-independent
            const sortedFailed = failed.sort((a, b) => a.id.localeCompare(b.id));

            // Extract only the relevant properties from TestMessage objects
            const extractedFailed = sortedFailed.map((f) => ({
                id: f.id,
                message: f.message.map((msg) => ({
                    message: msg.message,
                    ...(msg.actualOutput ? { actualOutput: msg.actualOutput } : {}),
                    ...(msg.expectedOutput ? { expectedOutput: msg.expectedOutput } : {}),
                })),
            }));

            assert.deepStrictEqual(extractedFailed, [
                {
                    id: '02-test-multiple-results.sub2',
                    message: [{ message: 'Failure message' }],
                },
                {
                    id: '02-test-multiple-results.sub3',
                    message: [{ message: `Failure message${EOL + EOL}Long\nExecution\nLog` }],
                },
                {
                    id: '05-test-only-sub-results-one-failing.sub2',
                    message: [{ message: 'No message in test result' }],
                },
                {
                    id: '06-test-with-diff',
                    message: [
                        {
                            message: 'Failure short message',
                            actualOutput: 'Actual\nOutput\nText',
                            expectedOutput: 'Expected\nOutput\nText content',
                        },
                        { message: `Test Log${EOL}Long\nExecution\nLog` },
                    ],
                },
            ]);
            assert.deepStrictEqual(errors, []);
        } finally {
            reset(spyCtrl);
        }
    });

    test('Settings e3-testsuite.args', async function () {
        await e3.controller.refreshHandler!(new CancellationTokenSource().token);

        // Get original configuration to restore later
        const config = workspace.getConfiguration('e3-testsuite');
        const originalArgs = config.get<string[]>('args');

        try {
            // Set custom args in workspace configuration
            const testArgs = ['--verbose', '--show-time-info'];
            await config.update('args', testArgs, false);

            const spyCtrl = spy(e3.controller);
            try {
                const mockRun = mock<TestRun>();
                let consoleOutput = '';
                when(mockRun.appendOutput(anything())).thenCall((output: string) => {
                    consoleOutput += output.replace(/(\r+\n|\n\r+)/g, '\n');
                });

                const run = instance(mockRun);

                when(spyCtrl.createTestRun(anything())).thenReturn(run);
                when(spyCtrl.createTestRun(anything(), anything())).thenReturn(run);
                when(spyCtrl.createTestRun(anything(), anything(), anything())).thenReturn(run);

                // Run the handler
                await e3.runHandler(
                    { include: undefined, exclude: undefined, profile: undefined },
                    new CancellationTokenSource().token,
                );

                // Check that the custom args appear in the console output
                assert.ok(
                    consoleOutput.includes('--verbose'),
                    `Expected '--verbose' argument not found in console output: ${consoleOutput}`,
                );
                assert.ok(
                    consoleOutput.includes('--show-time-info'),
                    `Expected '--show-time-info' argument not found in console output: ` +
                        `${consoleOutput}`,
                );

                // Verify the command line includes both the failure-exit-code and our custom args
                const commandLineRegex = /Running: ".*python.*" ".*testsuite\.py" ".*"/;
                const match = consoleOutput.match(commandLineRegex);
                assert.ok(match, `Command line not found in console output: ${consoleOutput}`);

                const commandLine = match[0];
                assert.ok(
                    commandLine.includes('--failure-exit-code=0'),
                    `Expected '--failure-exit-code=0' not found in command: ${commandLine}`,
                );
                assert.ok(
                    commandLine.includes('--verbose'),
                    `Expected '--verbose' not found in command: ${commandLine}`,
                );
                assert.ok(
                    commandLine.includes('--show-time-info'),
                    `Expected '--show-time-info' not found in command: ${commandLine}`,
                );
            } finally {
                reset(spyCtrl);
            }
        } finally {
            // Always restore original configuration
            await config.update('args', originalArgs, false);
        }
    });
});

interface TestResultItem {
    id: string;
    label: string;
    children: TestResultItem[];
}

function toTestResult(items: TestItemCollection): TestResultItem[] {
    const res: TestResultItem[] = [];
    function process(item: TestItem): void {
        res.push({
            id: item.id,
            label: item.label,
            children: toTestResult(item.children),
        });
    }
    items.forEach(process);
    return res;
}
