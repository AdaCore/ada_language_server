import { browser, expect } from '@wdio/globals';

before('finish loading extension', async () => {});

describe('VS Code Extension Testing', () => {
    it('should be able to load VSCode', async () => {
        await browser.executeWorkbench(async (vscode) => {
            const workspacePath = vscode.workspace.workspaceFolders[0].uri.fsPath;
            const doc = await vscode.workspace.openTextDocument(
                vscode.Uri.file(workspacePath + '/cycle.adb'),
            );
            const editor = await vscode.window.showTextDocument(doc);
            const position = new vscode.Position(13, 13);
            const selection = new vscode.Selection(position, position);
            editor.selection = selection;
            editor.revealRange(new vscode.Range(position, position));
        });
        const workbench = await browser.getWorkbench();

        const editor = await $('.monaco-editor');
        await editor.waitForExist({ timeout: 5000 });
        // await editor.click({ button: 'right' });
        const line = await $('div.view-lines > div.view-line:nth-child(14)');
        await line.waitForExist();
        const word = await line.$('span=Foo');
        await word.waitForExist();
        word.click({ button: 'right' });

        const shadow = await $('.shadow-root-host');
        await shadow.waitForExist();
        const item = await shadow.shadow$(
            '.action-menu-item .action-label[aria-label*="Show Call Hierarchy (graph)"',
        );
        item.waitForExist();
        item.click();

        const webview = await workbench.getAllWebviews();
        await webview[0].open();
        const nodes = await $$('.visualizer__basic_node');
        nodes.forEach((node) => node.waitForExist());
        expect(nodes.length).toBe(3);
    });
});
