import assert from 'assert';
import { Uri, window, workspace } from 'vscode';
import { adaExtState } from '../../../src/extension';
import { activate } from '../utils';

suite('CodeLens', function () {
    this.beforeAll(async () => {
        await activate();
    });

    test('in main file offer run & debug', async () => {
        assert(workspace.workspaceFolders !== undefined);
        const rootUri = workspace.workspaceFolders[0].uri;
        const mainUri = Uri.joinPath(rootUri, 'src', 'main1.adb');
        const textEditor = await window.showTextDocument(mainUri);
        const codelenses = await adaExtState.codelensProvider.provideCodeLenses(
            textEditor.document
        );
        assert.equal(
            /**
             * Check a string representation of the CodeLenses.
             */
            toString(codelenses),
            `
[
  {
    "command": "ada.buildAndRunMain",
    "title": "$(run) Run",
    "arguments": [
      "src/main1.adb"
    ]
  },
  {
    "command": "ada.buildAndDebugMain",
    "title": "$(debug-alt-small) Debug",
    "arguments": [
      "src/main1.adb"
    ]
  }
]`.trim()
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
