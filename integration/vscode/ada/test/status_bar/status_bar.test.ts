import * as assert from 'assert';
import { activate } from '../utils';

import * as vscode from 'vscode';
import { adaExtState } from '../../src/extension';
import { CMD_RELOAD_PROJECT } from '@/src/constants';
import { readFileSync, writeFileSync } from 'fs';
import { integer } from 'vscode-languageclient';

suite('Status Bar Test Suite', function () {
    // Make sure the extension is activated
    this.beforeAll(async () => {
        await activate();
    });

    /**
     * This function checks diagnostics for the given URI, making sure we have
     * the expected count for the given severity.
     */
    async function checkDiagnosticsAndStatusBar(
        prjUri: vscode.Uri,
        nbDiags: integer,
        severity: vscode.DiagnosticSeverity,
    ) {
        // Get the diagnostics with the expected severity
        const diagnostics = new Promise<vscode.Diagnostic[]>((resolve) => {
            const disposable = vscode.languages.onDidChangeDiagnostics(
                (e: vscode.DiagnosticChangeEvent) => {
                    if (e.uris.some((uri) => uri.path == prjUri.path)) {
                        const diags = vscode.languages.getDiagnostics(prjUri);
                        if (diags.some((diag) => diag.severity == severity)) {
                            disposable.dispose();
                            resolve(diags);
                        }
                    }
                },
            );
        });
        const alsDiagnostics = await diagnostics;

        // Check that we have the diagnostics we expect
        assert.strictEqual(
            alsDiagnostics.length,
            nbDiags,
            `Wrong number of project-related diagnostics with ${severity} severity.
Actual diagnostics are:
${JSON.stringify(alsDiagnostics)}`,
        );

        // Check that the status bar colors have been updated accordingly
        const expectedBgColor =
            severity == vscode.DiagnosticSeverity.Error
                ? new vscode.ThemeColor('statusBarItem.errorBackground')
                : severity == vscode.DiagnosticSeverity.Warning
                  ? new vscode.ThemeColor('statusBarItem.warningBackground')
                  : undefined;
        const expectedFgColor =
            severity == vscode.DiagnosticSeverity.Error
                ? new vscode.ThemeColor('statusBarItem.errorForeground')
                : severity == vscode.DiagnosticSeverity.Warning
                  ? new vscode.ThemeColor('statusBarItem.warningForeground')
                  : undefined;

        assert.deepEqual(
            adaExtState.statusBar.backgroundColor,
            expectedBgColor,
            `Status bar foreground has wrong color for ${severity}`,
        );
        assert.deepEqual(
            adaExtState.statusBar.color,
            expectedFgColor,
            `Status bar foreground has wrong color for ${severity}`,
        );
    }

    test('Status Bar - Project loaded successfully', () => {
        if (vscode.workspace.workspaceFolders !== undefined) {
            // Gather all the diagnostics from the interesting ALS diagnostics' sources.
            // For the status bar we are interested only in project-related diagnostics.
            const alsDiagnostics: vscode.Diagnostic[] = vscode.languages
                .getDiagnostics()
                .flatMap(([, diagnostics]) => diagnostics)
                .filter((diag) => ['ada.project', 'ada.alire'].includes(diag.source ?? ''));

            // Check that we don't have any project-related
            assert.equal(
                alsDiagnostics.length,
                0,
                'We should not have project-related diagnostics',
            );

            // Check the status bar colors: they should be transparent
            assert.strictEqual(
                adaExtState.statusBar.backgroundColor,
                undefined,
                'Status bar background color should be transparent',
            );
            assert.strictEqual(
                adaExtState.statusBar.color,
                undefined,
                'Status bar foreground color should be transparent',
            );
        }
    });

    test('Status Bar - Project loaded with warnings', async () => {
        if (vscode.workspace.workspaceFolders !== undefined) {
            // Get the workspace root folder and project URI
            const folder = vscode.workspace.workspaceFolders[0].uri;
            const prjUri = vscode.Uri.joinPath(folder, 'workspace.gpr');
            const contentBefore = readFileSync(prjUri.fsPath, 'utf-8');

            try {
                // Modify the .gpr file so that we get warnings after reloading it
                const newContent = contentBefore.replace(
                    'for Languages use ("ada");',
                    'for Languages use ("ada", "c");',
                );
                writeFileSync(prjUri.fsPath, newContent, 'utf-8');

                // Reload the project
                await vscode.commands.executeCommand(CMD_RELOAD_PROJECT);

                await checkDiagnosticsAndStatusBar(prjUri, 1, vscode.DiagnosticSeverity.Warning);
            } finally {
                // Restore the old .gpr file content
                writeFileSync(prjUri.fsPath, contentBefore);
            }
        }
    });

    test('Status Bar - Project loaded with errors', async () => {
        if (vscode.workspace.workspaceFolders !== undefined) {
            // Get the workspace root folder and project URI
            const folder = vscode.workspace.workspaceFolders[0].uri;
            const prjUri = vscode.Uri.joinPath(folder, 'workspace.gpr');
            const contentBefore = readFileSync(prjUri.fsPath, 'utf-8');

            try {
                // Modify the .gpr file so that we get warnings after reloading it
                const newContent = contentBefore.replace(
                    'for Languages use ("ada");',
                    'for Unknown_Attriute use "unknown";',
                );
                writeFileSync(prjUri.fsPath, newContent, 'utf-8');

                // Reload the project
                await vscode.commands.executeCommand(CMD_RELOAD_PROJECT);

                await checkDiagnosticsAndStatusBar(prjUri, 1, vscode.DiagnosticSeverity.Error);
            } finally {
                // Restore the old .gpr file content
                writeFileSync(prjUri.fsPath, contentBefore);
            }
        }
    });
});
