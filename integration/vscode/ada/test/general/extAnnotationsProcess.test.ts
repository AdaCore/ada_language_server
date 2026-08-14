/*
 * Exercises the gnatcov invocations and what the extension makes of their
 * output, with gnatcov itself replaced by a stub. The command lines asserted
 * here are the contract with gnatcov: if one of them changes, an annotation
 * silently fails to be created, displayed or deleted.
 */

import assert from 'assert';
import * as vscode from 'vscode';
import {
    ProcessOutput,
    addAnnotation,
    deleteAnnotation,
    getAnnotationFiles,
    gnatcovTestHooks,
    showAnnotations,
} from '../../src/extAnnotations';
import { AnnotationNode, ExternalAnnotationTreeProvider } from '../../src/extAnnotationsTree';

const PROJECT = '/w/p.gpr';

/** The envelope gnatcov prints, with the given annotation objects. */
function jsonOutput(annotations: unknown[], files: string[] = ['/w/annotations.toml']): string {
    return JSON.stringify({ annotation_files: files, annotations: annotations });
}

function ok(stdout: string): ProcessOutput {
    return { code: 0, stdout: stdout, stderr: '' };
}

/**
 * One annotation as gnatcov reports it. Omitting the line makes it stale, which
 * is how gnatcov reports an annotation it could not resolve.
 */
function annotationJson(id: string, file: string, line?: number) {
    return {
        file: file,
        id: id,
        kind: 'Exempt_On',
        stale: line === undefined,
        ...(line === undefined
            ? { diagnostic: 'file has been modified' }
            : {
                  location: {
                      start_line: line,
                      start_column: 1,
                      end_line: line,
                      end_column: 1,
                  },
              }),
    };
}

/** Let configuration-change listeners finish reacting. */
function settle(): Promise<void> {
    return new Promise((resolve) => setTimeout(resolve, 500));
}

function setScenarioVariables(value: Record<string, string> | undefined): Thenable<void> {
    return vscode.workspace
        .getConfiguration()
        .update('ada.scenarioVariables', value, vscode.ConfigurationTarget.Workspace);
}

/** Give the event loop a chance to reach `condition`. */
async function waitFor(condition: () => boolean, what: string): Promise<void> {
    for (let i = 0; i < 200 && !condition(); i += 1) {
        await new Promise((resolve) => setTimeout(resolve, 10));
    }
    assert.ok(condition(), what);
}

suite('GNATcoverage external annotations: gnatcov invocations', function () {
    /** Argument lists handed to gnatcov, in order. */
    let calls: string[][];

    /** What the stub answers. Replaced per test. */
    let answer: (args: string[]) => Promise<ProcessOutput>;

    /** Providers created by a test, disposed however it ends. */
    let providers: ExternalAnnotationTreeProvider[];

    setup(async () => {
        calls = [];
        providers = [];
        answer = () => Promise.resolve(ok(jsonOutput([])));

        gnatcovTestHooks.projectFile = () => Promise.resolve(PROJECT);
        gnatcovTestHooks.run = (_cmd, args) => {
            calls.push(args);
            return answer(args);
        };

        /*
         * Scenario variables are spliced into every command line, so the
         * assertions below depend on there being none. Another suite sharing
         * this VS Code host may have left some behind.
         *
         * Resetting them fires a configuration event, which the extension's own
         * listeners answer by refreshing -- spawning gnatcov of their own
         * accord. The stub is installed first so those land in the recording
         * rather than on the real gnatcov, and the recording is cleared once
         * they have settled.
         */
        await setScenarioVariables(undefined);
        await settle();
        calls = [];
    });

    teardown(async () => {
        /*
         * Reset the setting while the stub is still installed: a listener
         * reacting to the reset would otherwise spawn the real gnatcov.
         */
        await setScenarioVariables(undefined);
        await settle();

        gnatcovTestHooks.projectFile = undefined;
        gnatcovTestHooks.run = undefined;

        for (const provider of providers) {
            provider.dispose();
        }
    });

    /** A provider disposed at teardown, whatever the test does. */
    function newProvider(): ExternalAnnotationTreeProvider {
        const provider = new ExternalAnnotationTreeProvider();
        providers.push(provider);
        return provider;
    }

    /** The recorded invocations of one gnatcov command. */
    function callsTo(command: string): string[][] {
        return calls.filter((args) => args[0] === command);
    }

    test('asks for JSON, naming the project and optionally one source', async () => {
        await showAnnotations();
        assert.deepStrictEqual(calls[0], ['show-annotations', '--format=json', `-P${PROJECT}`]);

        await showAnnotations('/w/src/pkg.adb');
        assert.deepStrictEqual(calls[1], [
            'show-annotations',
            '--format=json',
            `-P${PROJECT}`,
            '/w/src/pkg.adb',
        ]);
    });

    test('returns the annotations and the files they live in', async () => {
        answer = () =>
            Promise.resolve(
                ok(
                    jsonOutput(
                        [
                            {
                                file: '/w/src/pkg.adb',
                                id: 'x',
                                kind: 'Exempt_On',
                                stale: false,
                                location: {
                                    start_line: 6,
                                    start_column: 7,
                                    end_line: 6,
                                    end_column: 7,
                                },
                                justification: 'j',
                            },
                        ],
                        ['/w/annotations.toml'],
                    ),
                ),
            );

        const result = await showAnnotations();

        assert.strictEqual(result.error, undefined);
        assert.strictEqual(result.annotations.length, 1);
        assert.deepStrictEqual(result.annotationFiles, ['/w/annotations.toml']);

        //  The watchers are bound to exactly what gnatcov resolved.
        assert.deepStrictEqual(await getAnnotationFiles(), ['/w/annotations.toml']);
    });

    test('reads a project designating no annotation file as the feature being off', async () => {
        /*
         * Verbatim from gnatcov. Reporting this as an error would put a failure
         * message in the tree of every project that uses no annotations.
         */
        answer = () =>
            Promise.resolve({
                code: 1,
                stdout: JSON.stringify({
                    code: 'not_configured',
                    message:
                        'no external annotation file: pass --external-annotations,' +
                        " or designate one through the Coverage'External_Annotations" +
                        ' project attribute',
                    annotation_files: [],
                    annotations: [],
                }),
                stderr:
                    'gnatcov: no external annotation file: pass --external-annotations,' +
                    " or designate one through the Coverage'External_Annotations project attribute",
            });

        const result = await showAnnotations();

        assert.strictEqual(result.notConfigured, true);
        assert.strictEqual(result.error, undefined);
        assert.deepStrictEqual(result.annotations, []);
    });

    test('reports any other failure as an error', async () => {
        answer = () =>
            Promise.resolve({ code: 1, stdout: '', stderr: 'gnatcov: no such project file' });

        const result = await showAnnotations();

        assert.strictEqual(result.notConfigured, undefined);
        assert.ok(result.error?.includes('no such project file'));
    });

    test('reports output it cannot read as an error', async () => {
        /*
         * A gnatcov too old to know --format=json prints the text form and
         * exits 0. Treating that as "no annotations" would quietly hide every
         * annotation in the project.
         */
        answer = () => Promise.resolve(ok('pkg.adb:\n- 6:7 - 6:7; id: x; kind: Exempt_On\n'));

        const result = await showAnnotations();

        assert.ok(result.error !== undefined);
        assert.deepStrictEqual(result.annotations, []);
    });

    test('builds the documented add-annotation command line', async () => {
        const failure = await addAnnotation(
            {
                kind: 'Exempt_Region',
                location: { startLine: 5, startColumn: 4, endLine: 6, endColumn: 10 },
                justification: 'why',
            },
            '/w/src/pkg.adb',
        );

        assert.strictEqual(failure, undefined);
        assert.deepStrictEqual(calls[0], [
            'add-annotation',
            `-P${PROJECT}`,
            '--kind=Exempt_Region',
            '--start-location=5:4',
            '--end-location=6:10',
            '--justification=why',
            '/w/src/pkg.adb',
        ]);

        //  A point annotation, with the direction gnatcov calls --annotate-after.
        const pointFailure = await addAnnotation(
            {
                kind: 'Dump_Buffers',
                location: { startLine: 27, startColumn: 7, endLine: 27, endColumn: 7 },
                insertAfter: true,
                tracePrefix: 'showcase',
            },
            '/w/src/pkg.adb',
        );

        assert.strictEqual(pointFailure, undefined);
        assert.deepStrictEqual(calls[1], [
            'add-annotation',
            `-P${PROJECT}`,
            '--kind=Dump_Buffers',
            '--location=27:7',
            '--annotate-after',
            '--dump-filename-prefix=showcase',
            '/w/src/pkg.adb',
        ]);
    });

    test('names neither the annotation file nor the output file', async () => {
        /*
         * gnatcov chooses both from Coverage'External_Annotations: it reads
         * every file the project tree designates, and writes to the one
         * belonging to the project that owns the annotated unit. Naming either
         * switch here would override that and put every annotation in one
         * file, whichever project it belongs to.
         */
        await addAnnotation(
            {
                kind: 'Exempt_On',
                location: { startLine: 1, startColumn: 1, endLine: 1, endColumn: 1 },
                justification: 'j',
            },
            '/w/src/pkg.adb',
        );
        await deleteAnnotation('some-id');

        assert.strictEqual(callsTo('add-annotation').length, 1);
        assert.strictEqual(callsTo('delete-annotation').length, 1);

        for (const args of calls) {
            assert.ok(!args.some((a) => a.startsWith('--external-annotations')));
            assert.ok(!args.some((a) => a.startsWith('--output')));
        }
    });

    test('deletes by identifier and reports the failure text', async () => {
        answer = () =>
            Promise.resolve({
                code: 1,
                stdout: '',
                stderr: 'gnatcov: No annotation associated with identifier "gone"',
            });

        const failure = await deleteAnnotation('gone');

        assert.deepStrictEqual(calls[0], [
            'delete-annotation',
            `-P${PROJECT}`,
            '--annotation-id=gone',
        ]);
        assert.ok(failure?.includes('No annotation associated'));
    });

    test('discards a tree query that a refresh superseded', async () => {
        /*
         * A query started before a refresh completes after it. Caching its
         * result would restore the annotations of a previous project or
         * scenario, and offer delete actions for identifiers that may no longer
         * exist.
         */
        let release: (value: ProcessOutput) => void = () => undefined;
        const pending = new Promise<ProcessOutput>((resolve) => (release = resolve));
        answer = () => pending;

        const provider = newProvider();

        const inFlight = provider.getChildren();

        //  The refresh must land while the query is genuinely in flight.
        await waitFor(() => callsTo('show-annotations').length >= 1, 'the query did not start');
        provider.refresh();

        //  Only now does the superseded run report the state before the refresh.
        release(ok(jsonOutput([annotationJson('superseded', '/w/src/pkg.adb', 1)])));
        await inFlight;

        //  What the tree shows must come from the query made after the refresh.
        answer = () =>
            Promise.resolve(ok(jsonOutput([annotationJson('current', '/w/src/pkg.adb', 2)])));

        const roots = await provider.getChildren();
        assert.strictEqual(
            callsTo('show-annotations').length,
            2,
            'the superseded result must not have been cached',
        );

        const children = await provider.getChildren(roots[0]);
        assert.deepStrictEqual(
            children.map((child) => (child as AnnotationNode).annotation.id),
            ['current'],
            'the tree must not fall back to the superseded annotations',
        );
    });

    test('caches a query that no refresh superseded', async () => {
        const provider = newProvider();

        await provider.getChildren();
        await provider.getChildren();
        assert.strictEqual(
            callsTo('show-annotations').length,
            1,
            'the whole-project query runs gnatcov, so it is cached',
        );

        provider.refresh();
        await provider.getChildren();
        assert.strictEqual(
            callsTo('show-annotations').length,
            2,
            'a refresh must invalidate the cache',
        );
    });

    test('shares one gnatcov run between concurrent queries', async () => {
        /*
         * Callers that arrive while a query is in flight must join it. This
         * holds because the cache entry is the pending query itself, which is
         * also what keeps a superseded query from installing its result.
         */
        let release: (value: ProcessOutput) => void = () => undefined;
        const pending = new Promise<ProcessOutput>((resolve) => (release = resolve));
        answer = () => pending;

        const provider = newProvider();

        const first = provider.getChildren();
        await waitFor(() => callsTo('show-annotations').length >= 1, 'the query did not start');
        const second = provider.getChildren();

        release(ok(jsonOutput([])));
        await Promise.all([first, second]);

        assert.strictEqual(
            callsTo('show-annotations').length,
            1,
            'the second caller should have joined the first',
        );
    });

    test('groups tree entries by full path', async () => {
        /*
         * Two sources sharing a base name must not be merged, and every entry
         * must be navigable.
         */
        answer = () =>
            Promise.resolve(
                ok(
                    jsonOutput([
                        {
                            file: '/w/src/a.adb',
                            id: 'one',
                            kind: 'Exempt_On',
                            stale: false,
                            location: {
                                start_line: 1,
                                start_column: 1,
                                end_line: 1,
                                end_column: 1,
                            },
                        },
                        {
                            file: '/w/other/a.adb',
                            id: 'two',
                            kind: 'Exempt_On',
                            stale: false,
                            location: {
                                start_line: 2,
                                start_column: 1,
                                end_line: 2,
                                end_column: 1,
                            },
                        },
                    ]),
                ),
            );

        const provider = newProvider();
        const roots = await provider.getChildren();

        assert.strictEqual(roots.length, 2, 'same base name, different directories');

        for (const root of roots) {
            const children = await provider.getChildren(root);
            assert.strictEqual(children.length, 1);
            assert.ok(children[0].command !== undefined, 'entries must be navigable');
        }
    });

    test('orders files by name and annotations by position, stale last', async () => {
        /*
         * gnatcov emits stale entries first within a file. They have no line
         * number, so showing them among numbered ones reads as an ordering
         * glitch; they go last.
         */
        answer = () =>
            Promise.resolve(
                ok(
                    jsonOutput([
                        annotationJson('gone', '/w/src/b.adb'),
                        annotationJson('late', '/w/src/b.adb', 10),
                        annotationJson('early', '/w/src/b.adb', 2),
                        annotationJson('other', '/w/src/a.adb', 1),
                    ]),
                ),
            );

        const provider = newProvider();
        const roots = await provider.getChildren();

        assert.deepStrictEqual(
            roots.map((root) => root.label),
            ['a.adb', 'b.adb'],
        );

        const children = await provider.getChildren(roots[1]);
        assert.deepStrictEqual(
            children.map((child) => (child as AnnotationNode).annotation.id),
            ['early', 'late', 'gone'],
        );

        //  A stale entry cannot be revealed, so it offers no navigation.
        const stale = children[2] as AnnotationNode;
        assert.strictEqual(stale.command, undefined);
        assert.strictEqual(stale.description, 'stale');
    });

    test('says why the tree is empty', async () => {
        const messageOf = async () => {
            const provider = newProvider();
            const roots = await provider.getChildren();
            assert.strictEqual(roots.length, 1);
            return String(roots[0].label);
        };

        assert.strictEqual(await messageOf(), 'No annotations');

        answer = () =>
            Promise.resolve({
                code: 1,
                stdout: JSON.stringify({
                    code: 'not_configured',
                    message: 'no external annotation file',
                    annotation_files: [],
                    annotations: [],
                }),
                stderr: 'gnatcov: no external annotation file: pass --external-annotations',
            });
        assert.ok((await messageOf()).includes("Coverage'External_Annotations"));

        answer = () =>
            Promise.resolve({ code: 1, stdout: '', stderr: 'gnatcov: no such project file' });
        assert.ok((await messageOf()).includes('no such project file'));
    });

    test('passes the configured scenario variables to every command', async () => {
        await setScenarioVariables({ MODE: 'debug' });

        /*
         * Changing the setting makes the extension refresh, which spawns
         * gnatcov of its own accord -- how much depends on what other suites
         * left open in this VS Code. Let that settle, then discard it: the
         * assertions below are about the command lines this test builds, and
         * are written to stay true whatever else runs alongside them.
         */
        await settle();
        calls = [];

        await showAnnotations('/w/src/pkg.adb');
        await addAnnotation(
            {
                kind: 'Exempt_On',
                location: { startLine: 1, startColumn: 1, endLine: 1, endColumn: 1 },
                justification: 'j',
            },
            '/w/src/pkg.adb',
        );
        await deleteAnnotation('some-id');

        const commands = calls.map((args) => args[0]);
        for (const command of ['show-annotations', 'add-annotation', 'delete-annotation']) {
            assert.ok(commands.includes(command), `${command} did not run`);
        }

        for (const args of calls) {
            assert.ok(
                args.includes('-XMODE=debug'),
                `scenario variable missing from: ${args.join(' ')}`,
            );
        }

        /*
         * gnatcov takes the source file positionally, so the switches have to
         * come before it.
         */
        const show = calls.find(
            (args) => args[0] === 'show-annotations' && args.includes('/w/src/pkg.adb'),
        );
        assert.ok(show !== undefined, 'the per-file query did not run');
        assert.ok(show.indexOf('-XMODE=debug') < show.indexOf('/w/src/pkg.adb'));
    });
});
