/*----------------------------------------------------------------------------
--                    GNATcoverage external annotations                     --
--                                                                          --
-- Support for GNATcoverage "external annotations": annotations that live   --
-- in a separate TOML file instead of being written as pragmas or comments  --
-- in the source code.                                                      --
--                                                                          --
-- This module runs gnatcov. Parsing lives in extAnnotationsParser.ts and   --
-- rendering in extAnnotationsDecorations.ts.                               --
----------------------------------------------------------------------------*/

import { ChildProcess, spawn } from 'child_process';
import * as path from 'path';
import * as vscode from 'vscode';
import { adaExtState, logger } from './extension';
import {
    AnnotationLocation,
    CreateAnnotationParams,
    ShowAnnotationsOutput,
    buildAddAnnotationArgs,
    parseShowAnnotationsJson,
} from './extAnnotationsParser';
import { getToolEnvironment, gprScenarioArgs, which } from './helpers';

export * from './extAnnotationsParser';

/**
 * Convert a gnatcov location into a vscode.Range.
 *
 * gnatcov uses 1-based lines and columns, and the end position designates the
 * last character *inside* the range. vscode.Range is 0-based with an exclusive
 * end, hence the +1 on the end column.
 */
export function toVscodeRange(loc: AnnotationLocation): vscode.Range {
    return new vscode.Range(
        Math.max(loc.startLine - 1, 0),
        Math.max(loc.startColumn - 1, 0),
        Math.max(loc.endLine - 1, 0),
        Math.max(loc.endColumn, 0),
    );
}

/**
 * @returns the annotation files in effect, as {@link ShowAnnotationsOutput}
 * describes them, or an empty list when the project designates none.
 */
export async function getAnnotationFiles(): Promise<string[]> {
    return (await showAnnotations()).annotationFiles;
}

/**
 * Result of a `show-annotations` invocation: what gnatcov reported, plus how
 * the invocation went. Both lists are empty when it did not go well.
 */
export type ShowAnnotationsResult = ShowAnnotationsOutput & {
    /** Set when gnatcov could not be run at all, or exited with an error. */
    error?: string;
    /**
     * Set when the project designates no annotation file, which gnatcov reports
     * by refusing to run. This is the feature being off rather than a failure.
     */
    notConfigured?: boolean;
};

/** A result carrying nothing, for an invocation that failed. */
function noResult(rest: { error?: string; notConfigured?: boolean }): ShowAnnotationsResult {
    return { annotations: [], annotationFiles: [], ...rest };
}

/**
 * How gnatcov reports that nothing designates an annotation file.
 *
 * Matched on the stable part of the message rather than the whole of it, which
 * goes on to spell out both ways of designating one.
 */
const NO_ANNOTATION_FILE = 'no external annotation file';

/**
 * Run `gnatcov show-annotations`.
 *
 * @param sourcePath - restrict the query to one source file. Callers that
 * decorate a single editor rely on this rather than filtering afterwards, so
 * the result describes that file and no other. Omit it to report the
 * annotations of every unit in the project tree, which is what the tree view
 * and the file watchers need.
 */
export async function showAnnotations(sourcePath?: string): Promise<ShowAnnotationsResult> {
    const context = await resolveContext();
    if ('error' in context) {
        return noResult({ error: context.error });
    }

    const args = [
        'show-annotations',
        '--format=json',
        `-P${context.projectFile}`,
        ...gprScenarioArgs(),
    ];

    /*
     * Without a source file, gnatcov reports the annotations of every unit in
     * the project, which is what the tree view needs.
     */
    if (sourcePath !== undefined) {
        args.push(sourcePath);
    }

    logSafely(args);

    const output = await invoke(args);

    if (output.code !== 0) {
        const message = output.stderr.trim() || output.stdout.trim();

        /*
         * gnatcov refuses to run when it has no annotation file, which is how a
         * project without a Coverage'External_Annotations attribute presents
         * itself. Report that as the feature being off, not as an error.
         */
        if (message.includes(NO_ANNOTATION_FILE)) {
            return noResult({ notConfigured: true });
        }

        return noResult({
            error: `gnatcov show-annotations exited with status ${String(output.code)}: ${message}`,
        });
    }

    try {
        return parseShowAnnotationsJson(output.stdout);
    } catch (err) {
        return noResult({
            error: `could not read the output of gnatcov show-annotations: ${String(err)}`,
        });
    }
}

/**
 * Watch the project's annotation files and call `onChange` whenever one of them
 * is created, modified or deleted.
 *
 * This covers edits made outside the IDE as well as the extension's own calls
 * to `add-annotation` and `delete-annotation`, so those do not need to refresh
 * anything explicitly.
 *
 * Establishing the watchers costs one `show-annotations` run, since gnatcov is
 * what resolves the paths. It happens once at activation and again whenever the
 * project changes, off the critical path.
 *
 * @returns a disposable that stops watching. It is bound to the paths in effect
 * when it was created, so it must be replaced when the project changes.
 */
export async function watchAnnotationFiles(onChange: () => void): Promise<vscode.Disposable> {
    const watchers = (await getAnnotationFiles()).map((file) => {
        /*
         * Watch the directory, and match the base name so that only this file
         * is reported. Sibling files are ignored.
         *
         * The directory is what must be watched: creation is reported only to
         * a watcher on the parent, and the file does not exist until the first
         * annotation. A plain string pattern would also cover only the
         * workspace folders, and the file may lie outside them.
         */
        const watcher = vscode.workspace.createFileSystemWatcher(
            new vscode.RelativePattern(path.dirname(file), path.basename(file)),
        );

        watcher.onDidChange(onChange);
        watcher.onDidCreate(onChange);
        watcher.onDidDelete(onChange);
        return watcher;
    });

    return new vscode.Disposable(() => {
        for (const watcher of watchers) {
            watcher.dispose();
        }
    });
}

type ResolvedContext = { projectFile: string };

/**
 * Test seam.
 *
 * What is worth testing here is the command line handed to gnatcov and what is
 * made of its output; neither needs a gnatcov on the PATH nor a loaded project.
 * These hooks stand in for both. They are undefined in normal use.
 */
export const gnatcovTestHooks: {
    projectFile?: () => Promise<string>;
    run?: (cmd: string, args: string[]) => Promise<ProcessOutput>;
} = {};

function invoke(args: string[]): Promise<ProcessOutput> {
    return (gnatcovTestHooks.run ?? run)('gnatcov', args);
}

/**
 * Resolve everything a gnatcov invocation needs from the workspace.
 */
async function resolveContext(): Promise<ResolvedContext | { error: string }> {
    if (gnatcovTestHooks.run === undefined && which('gnatcov') === undefined) {
        return { error: 'gnatcov not found on the PATH' };
    }

    try {
        return {
            projectFile: await (gnatcovTestHooks.projectFile === undefined
                ? adaExtState.getProjectFile()
                : gnatcovTestHooks.projectFile()),
        };
    } catch (err) {
        return { error: `could not determine the project file: ${String(err)}` };
    }
}

/**
 * Create an annotation, by running `gnatcov add-annotation`.
 *
 * Neither the annotation files nor the output file are named: gnatcov takes
 * both from the project's `Coverage'External_Annotations` attribute, updating
 * the first file it designates.
 *
 * @returns undefined on success, or a message describing the failure.
 */
export async function addAnnotation(
    params: CreateAnnotationParams,
    sourcePath: string,
): Promise<string | undefined> {
    const context = await resolveContext();
    if ('error' in context) {
        return context.error;
    }

    const args = [
        'add-annotation',
        `-P${context.projectFile}`,
        ...gprScenarioArgs(),
        ...buildAddAnnotationArgs(params),
        sourcePath,
    ];

    logSafely(args);

    return failureOf(await invoke(args));
}

/**
 * Delete the annotation with the given identifier.
 *
 * As for creation, gnatcov takes the files to read and the file to update from
 * the project.
 *
 * @returns undefined on success, or a message describing the failure.
 */
export async function deleteAnnotation(id: string): Promise<string | undefined> {
    const context = await resolveContext();
    if ('error' in context) {
        return context.error;
    }

    const args = [
        'delete-annotation',
        `-P${context.projectFile}`,
        `--annotation-id=${id}`,
        ...gprScenarioArgs(),
    ];

    logSafely(args);

    return failureOf(await invoke(args));
}

/**
 * @returns undefined if the process succeeded, else the most informative
 * message it produced.
 */
function failureOf(output: ProcessOutput): string | undefined {
    if (output.code === 0) {
        return undefined;
    }

    return output.stderr.trim() || output.stdout.trim() || `exit status ${String(output.code)}`;
}

/**
 * Log a gnatcov invocation without copying user data into the output channel.
 *
 * Scenario variable values and justifications come from the user's settings and
 * prompts, so the value of anything that carries them is elided rather than
 * written to a log that gets attached to bug reports.
 */
function logSafely(args: string[]): void {
    const redacted = args.map((arg) => {
        for (const prefix of ['-X', '--justification=', '--dump-filename-prefix=']) {
            if (arg.startsWith(prefix)) {
                const name = prefix === '-X' ? arg.slice(0, arg.indexOf('=') + 1) : prefix;
                return `${name}<elided>`;
            }
        }
        return arg;
    });

    logger.debug(`Running: gnatcov ${redacted.join(' ')}`);
}

export type ProcessOutput = { code: number | null; stdout: string; stderr: string };

function run(cmd: string, args: string[]): Promise<ProcessOutput> {
    return new Promise<ProcessOutput>((resolve, reject) => {
        const stdout: Buffer[] = [];
        const stderr: Buffer[] = [];

        const p: ChildProcess = spawn(cmd, args, {
            cwd: vscode.workspace.workspaceFolders?.[0]?.uri.fsPath,
            env: getToolEnvironment(),
        });

        p.stdout?.on('data', (chunk: Buffer) => stdout.push(chunk));
        p.stderr?.on('data', (chunk: Buffer) => stderr.push(chunk));
        p.on('error', reject);
        p.on('close', (code: number | null) =>
            resolve({
                code: code,
                stdout: Buffer.concat(stdout).toString(),
                stderr: Buffer.concat(stderr).toString(),
            }),
        );
    });
}
