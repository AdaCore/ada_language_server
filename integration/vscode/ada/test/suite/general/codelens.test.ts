import assert from 'assert';
import { Uri, window, workspace } from 'vscode';
import { adaExtState } from '../../../src/extension';
import { activate, closeAllEditors } from '../utils';

suite('CodeLens', function () {
    this.beforeAll(async () => {
        await activate();
    });

    this.beforeEach(async function () {
        await closeAllEditors();
    });

    test('in main file offer run & debug', async () => {
        assert(workspace.workspaceFolders !== undefined);
        const rootUri = workspace.workspaceFolders[0].uri;
        const mainUri = Uri.joinPath(rootUri, 'src', 'main1.adb');
        const textEditor = await window.showTextDocument(mainUri);
        const codelenses = await adaExtState.codelensProvider.provideCodeLenses(
            textEditor.document
        );
        assert.deepEqual(
            /**
             * Check a subset of the fields in CodeLenses
             */
            codelenses?.map((v) => ({
                ...v.command,
                // The argument is expected to be a Uri, in which case use the
                // normalized fsPath for comparison with the expected result
                // eslint-disable-next-line @typescript-eslint/no-unsafe-return
                arguments: v.command?.arguments?.map((a) => (a instanceof Uri ? a.fsPath : a)),
            })),
            [
                {
                    command: 'ada.buildAndRunMain',
                    title: '$(run) Run',
                    arguments: [mainUri.fsPath],
                },
                {
                    command: 'ada.buildAndDebugMain',
                    title: '$(debug-alt-small) Debug',
                    arguments: [mainUri.fsPath],
                },
            ]
        );
    });

    test("in non-main file don't offer run & debug", async () => {
        assert(workspace.workspaceFolders !== undefined);
        const rootUri = workspace.workspaceFolders[0].uri;
        const mainUri = Uri.joinPath(rootUri, 'src', 'foo.ads');
        const textEditor = await window.showTextDocument(mainUri);
        const codelenses = await adaExtState.codelensProvider.provideCodeLenses(
            textEditor.document
        );

        if (codelenses) {
            /**
             * Assert that run and debug codelenses have not been provided
             */
            assert(
                codelenses.every((cl) => {
                    assert(cl.command?.command);
                    return (
                        !cl.command.command.toLowerCase().includes('run') &&
                        !cl.command.command.toLowerCase().includes('debug')
                    );
                })
            );
        }
    });
});

// eslint-disable-next-line @typescript-eslint/no-unused-vars
function toString(codelenses: import('vscode').CodeLens[] | null | undefined): unknown {
    return JSON.stringify(
        codelenses?.map((cl) => ({
            command: cl.command?.command ?? '',
            title: cl.command?.title ?? '',
            arguments: cl.command?.arguments?.map((o) =>
                /**
                 * If the argument is a URI, render it as a relative
                 * path.  Otherwise, keep the object as is.
                 */
                // eslint-disable-next-line @typescript-eslint/no-unsafe-return
                o instanceof Uri ? workspace.asRelativePath(o) : o
            ),
        })),
        null,
        2
    );
}
