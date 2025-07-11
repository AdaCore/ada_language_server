import assert from 'assert';
import { commands, TestItem, TestItemCollection, Uri, workspace } from 'vscode';
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
        await commands.executeCommand('testing.runAll');

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
        ]);
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
