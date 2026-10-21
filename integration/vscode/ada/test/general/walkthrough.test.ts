import assert from 'assert';
import { mkdtemp, rm } from 'fs/promises';
import { tmpdir } from 'os';
import path from 'path';
import * as vscode from 'vscode';
import { downloadHelloWorldProject } from '../../src/walkthrough';
import { runTaskAndGetResult } from '../../src/taskProviders';

suite('Alire walkthrough', () => {
    let parent: vscode.Uri;

    setup(async () => {
        parent = vscode.Uri.file(await mkdtemp(path.join(tmpdir(), 'als-walkthrough-')));
    });

    teardown(async () => {
        await rm(parent.fsPath, { recursive: true, force: true });
    });

    async function createCrate(name: string): Promise<vscode.Uri> {
        const uri = vscode.Uri.joinPath(parent, name);
        await vscode.workspace.fs.createDirectory(vscode.Uri.joinPath(uri, 'src'));
        for (const file of ['alire.toml', 'hello.gpr', 'src/hello.adb']) {
            await vscode.workspace.fs.writeFile(vscode.Uri.joinPath(uri, file), Buffer.from(''));
        }
        return uri;
    }

    test('downloads and builds hello without changing existing folders', async () => {
        const existing = await createCrate('hello_1.0.1_existing');
        const marker = vscode.Uri.joinPath(existing, 'keep.txt');
        await vscode.workspace.fs.writeFile(marker, Buffer.from('existing project'));
        let downloaded: vscode.Uri | undefined;

        const result = await downloadHelloWorldProject(parent, async (task) => {
            // Check the Alire command and terminal interaction offered to the user.
            assert(task.execution instanceof vscode.ProcessExecution);
            assert.deepStrictEqual(task.execution.args, ['get', '--build', 'hello']);
            assert.strictEqual(task.execution.options?.cwd, parent.fsPath);
            assert.strictEqual(task.presentationOptions.focus, true);
            downloaded = await createCrate('hello_1.0.2_new');
            return 0;
        });

        assert.strictEqual(result.toString(), downloaded?.toString());
        assert.strictEqual(
            Buffer.from(await vscode.workspace.fs.readFile(marker)).toString(),
            'existing project',
        );
    });

    test('does not offer a partially downloaded crate after a failed build', async () => {
        await assert.rejects(
            downloadHelloWorldProject(parent, async () => {
                await createCrate('hello_1.0.2_partial');
                return 1;
            }),
            /Alire did not complete successfully/,
        );
    });

    test('waits for the task process to finish before opening the crate', async () => {
        const result = await downloadHelloWorldProject(parent, async (task) => {
            // Exercise the real VS Code task lifecycle without network access.
            const script = `
                const fs = require('fs');
                fs.mkdirSync('hello_1.0.2_task/src', { recursive: true });
                for (const file of ['alire.toml', 'hello.gpr', 'src/hello.adb']) {
                    fs.writeFileSync('hello_1.0.2_task/' + file, '');
                }
            `;
            task.execution = new vscode.ProcessExecution(process.execPath, ['-e', script], {
                cwd: parent.fsPath,
                env: { ELECTRON_RUN_AS_NODE: '1' },
            });
            return runTaskAndGetResult(task);
        });
        assert.strictEqual(
            result.toString(),
            vscode.Uri.joinPath(parent, 'hello_1.0.2_task').toString(),
        );
    });

    test('does not continue after the task is cancelled', async () => {
        await assert.rejects(
            downloadHelloWorldProject(parent, () => Promise.resolve(undefined)),
            /Alire did not complete successfully/,
        );
    });

    test('does not reuse an existing crate when no new crate was downloaded', async () => {
        await createCrate('hello_1.0.2_existing');
        await assert.rejects(
            downloadHelloWorldProject(parent, () => Promise.resolve(0)),
            /Could not locate the downloaded hello crate/,
        );
    });

    test('checks that the downloaded folder contains the example files', async () => {
        await assert.rejects(
            downloadHelloWorldProject(parent, async () => {
                await vscode.workspace.fs.createDirectory(
                    vscode.Uri.joinPath(parent, 'hello_1.0.2_incomplete'),
                );
                return 0;
            }),
        );
    });
});
