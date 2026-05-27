/*----------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2024, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
----------------------------------------------------------------------------*/

import assert from 'assert';
import * as vscode from 'vscode';
import { adaExtState } from '../../src/extension';
import { activate } from '../utils';

suite('Custom Naming Convention – Language Detection', function () {
    this.beforeAll(async () => {
        await activate();
        // Fetch project information eagerly so the language map is populated
        // before the tests run.  refreshProjectView() now contacts the server
        // directly without requiring the Project View panel to be open.
        await adaExtState.refreshProjectView();
    });

    test('getLanguageForUri maps custom-extension Ada files to "ada"', () => {
        // Verify that the language map was built correctly from the GPR
        // Naming package declarations.  No document needs to be opened for
        // this: it exercises the map-lookup layer in isolation.
        assert.ok(adaExtState, 'Extension state should be initialized after activation');

        const folder = vscode.workspace.workspaceFolders![0].uri;

        for (const fileName of ['my_package.ada_spec', 'my_package.ada_body']) {
            const uri = vscode.Uri.joinPath(folder, 'src', fileName);
            assert.strictEqual(
                adaExtState.getLanguageForUri(uri),
                'ada',
                `Expected getLanguageForUri to return 'ada' for '${fileName}' ` +
                    `(custom Ada extension defined in the GPR Naming package)`,
            );
        }
    });

    test('getLanguageForUri maps custom-extension C++ files to "cpp"', () => {
        // Same as the Ada map-lookup test but for C++ sources.
        assert.ok(adaExtState, 'Extension state should be initialized after activation');

        const folder = vscode.workspace.workspaceFolders![0].uri;

        for (const fileName of ['my_module.cpp_hdr', 'my_module.cpp_src']) {
            const uri = vscode.Uri.joinPath(folder, 'src', fileName);
            assert.strictEqual(
                adaExtState.getLanguageForUri(uri),
                'cpp',
                `Expected getLanguageForUri to return 'cpp' for '${fileName}' ` +
                    `(custom C++ extension defined in the GPR Naming package)`,
            );
        }
    });

    test('Opening a custom-extension Ada file gives it the "ada" language ID', async () => {
        assert.ok(adaExtState, 'Extension state should be initialized after activation');

        const folder = vscode.workspace.workspaceFolders![0].uri;
        const fileUri = vscode.Uri.joinPath(folder, 'src', 'my_package.ada_spec');

        const doc = await vscode.workspace.openTextDocument(fileUri);

        // Wait a bit before checking the language, since onDidOpenDocument is async
        await new Promise((resolve) => setTimeout(resolve, 500));

        // Check that the language ID was correctly set to 'ada'
        assert.strictEqual(
            doc.languageId,
            'ada',
            `Expected language 'ada' for '${fileUri.fsPath}', ` + `but got '${doc.languageId}'`,
        );
    });

    test('Opening a custom-extension C++ file gives it the "cpp" language ID', async () => {
        assert.ok(adaExtState, 'Extension state should be initialized after activation');

        const folder = vscode.workspace.workspaceFolders![0].uri;
        const fileUri = vscode.Uri.joinPath(folder, 'src', 'my_module.cpp_src');

        const doc = await vscode.workspace.openTextDocument(fileUri);

        // The onDidOpenTextDocument handler is async: VS Code fires the event
        // and returns without awaiting the handler's completion. Poll until
        // setTextDocumentLanguage has settled or the timeout expires.
        const deadline = Date.now() + 5000;
        while (doc.languageId !== 'cpp' && Date.now() < deadline) {
            await new Promise((resolve) => setTimeout(resolve, 50));
        }

        assert.strictEqual(
            doc.languageId,
            'cpp',
            `Expected language 'cpp' for '${fileUri.fsPath}', ` + `but got '${doc.languageId}'`,
        );
    });

    test('Files already open when project (re-)loads get their language corrected', async () => {
        assert.ok(adaExtState, 'Extension state should be initialized after activation');

        const folder = vscode.workspace.workspaceFolders![0].uri;
        const fileUri = vscode.Uri.joinPath(folder, 'src', 'my_package.ada_body');

        // Open the document now, then force a fresh project-info cycle so that
        // it is "already open" when the map is rebuilt.
        const doc = await vscode.workspace.openTextDocument(fileUri);

        // Simulating a project reload: refreshProjectView() fetches fresh info
        // from the server, rebuilds the language map, and applies language
        // overrides to all currently open documents.
        await adaExtState.refreshProjectView();

        assert.strictEqual(
            doc.languageId,
            'ada',
            `Language should be corrected to 'ada' for an already-open file ` +
                `after the project info is (re-)loaded`,
        );
    });
});
