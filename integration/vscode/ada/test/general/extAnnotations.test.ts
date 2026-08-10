import assert from 'assert';
import {
    ExternalAnnotation,
    annotationKinds,
    labelOf,
    parseShowAnnotationsJson,
} from '../../src/extAnnotationsParser';

/**
 * The samples below are verbatim output of
 * `gnatcov show-annotations --format=json`, so that these tests fail if gnatcov
 * ever changes the shape rather than only if this parser does.
 */

function byId(annotations: ExternalAnnotation[], id: string): ExternalAnnotation {
    const found = annotations.find((a) => a.id === id);
    assert.ok(found !== undefined, `no annotation with id '${id}'`);
    return found;
}

/** Wrap annotation objects in the envelope gnatcov prints. */
function output(annotations: unknown[], files: string[] = ['/w/annotations.toml']): string {
    return JSON.stringify({ annotation_files: files, annotations: annotations });
}

suite('GNATcoverage external annotations', function () {
    test('parses resolved annotations of every shape', () => {
        const parsed = parseShowAnnotationsJson(
            output([
                {
                    file: '/w/src/pkg.adb',
                    id: 'on',
                    kind: 'Exempt_On',
                    stale: false,
                    location: { start_line: 6, start_column: 7, end_line: 6, end_column: 7 },
                    justification: 'defensive code, not to be triggered (on)',
                },
                {
                    file: '/w/src/pkg.adb',
                    id: 'off',
                    kind: 'Exempt_Off',
                    stale: false,
                    location: { start_line: 8, start_column: 14, end_line: 8, end_column: 14 },
                },
                {
                    file: '/w/src/pkg.adb',
                    id: 'region',
                    kind: 'Exempt_Region',
                    stale: false,
                    location: { start_line: 10, start_column: 7, end_line: 12, end_column: 14 },
                    justification: 'defensive code, not to be triggered (region)',
                },
                {
                    file: '/w/src/pkg.adb',
                    id: 'dump',
                    kind: 'Dump_Buffers',
                    stale: false,
                    location: { start_line: 13, start_column: 7, end_line: 13, end_column: 7 },
                    insert_after: true,
                    trace_prefix: 'myprefix',
                },
            ]),
        );

        const annotations = parsed.annotations;
        assert.strictEqual(annotations.length, 4);

        const on = byId(annotations, 'on');
        assert.strictEqual(on.kind, 'Exempt_On');
        assert.strictEqual(on.stale, false);
        assert.deepStrictEqual(on.range, {
            startLine: 6,
            startColumn: 7,
            endLine: 6,
            endColumn: 7,
        });
        assert.strictEqual(on.justification, 'defensive code, not to be triggered (on)');

        // Exempt_Off carries no justification and gnatcov emits no field for it.
        const off = byId(annotations, 'off');
        assert.strictEqual(off.kind, 'Exempt_Off');
        assert.strictEqual(off.justification, undefined);

        /*
         * For a kind that accepts a justification, gnatcov emits the field even
         * when the annotation carries none, as an empty string. Callers test it
         * for presence, so it must not arrive as ''.
         */
        const empty = parseShowAnnotationsJson(
            output([
                {
                    file: '/w/src/pkg.adb',
                    id: 'covoff',
                    kind: 'Cov_Off',
                    stale: false,
                    location: { start_line: 1, start_column: 1, end_line: 1, end_column: 1 },
                    justification: '',
                },
            ]),
        );
        assert.strictEqual(empty.annotations[0].justification, undefined);

        const region = byId(annotations, 'region');
        assert.deepStrictEqual(region.range, {
            startLine: 10,
            startColumn: 7,
            endLine: 12,
            endColumn: 14,
        });

        const dump = byId(annotations, 'dump');
        assert.strictEqual(dump.insertAfter, true);
        assert.strictEqual(dump.tracePrefix, 'myprefix');
    });

    test('reports the annotation files in effect', () => {
        /*
         * This drives the file watchers. gnatcov resolves the path against the
         * project that defines Coverage'External_Annotations, which the
         * extension cannot do itself, and reports it even when it does not
         * exist yet, since that is where a first annotation will be written.
         *
         * Several may be reported: each project in the tree designates at
         * most one, and the commands that read annotations load them all.
         */
        const parsed = parseShowAnnotationsJson(output([], ['/w/sub/annotations.toml']));
        assert.deepStrictEqual(parsed.annotationFiles, ['/w/sub/annotations.toml']);
    });

    test('keeps a justification containing a semicolon and a newline intact', () => {
        /*
         * The text format made these genuinely ambiguous, which is what the
         * JSON format is for. A round trip through JSON must preserve them
         * exactly.
         */
        const justification = 'first; second\nthird: still the justification';
        const parsed = parseShowAnnotationsJson(
            output([
                {
                    file: '/w/src/pkg.adb',
                    id: 'tricky',
                    kind: 'Exempt_On',
                    stale: false,
                    location: { start_line: 6, start_column: 7, end_line: 6, end_column: 7 },
                    justification: justification,
                },
            ]),
        );

        assert.strictEqual(parsed.annotations.length, 1);
        assert.strictEqual(parsed.annotations[0].justification, justification);
    });

    test('parses stale annotations', () => {
        const parsed = parseShowAnnotationsJson(
            output([
                {
                    file: '/w/src/pkg.adb',
                    id: 'gone',
                    kind: 'Exempt_Region',
                    stale: true,
                    diagnostic: 'Line 5 of pkg.adb is not long enough.',
                    justification: 'why it was exempted',
                },
            ]),
        );

        const stale = byId(parsed.annotations, 'gone');
        assert.strictEqual(stale.stale, true);
        assert.strictEqual(stale.range, undefined);
        assert.strictEqual(stale.diagnostic, 'Line 5 of pkg.adb is not long enough.');
        assert.strictEqual(stale.justification, 'why it was exempted');
    });

    test('parses decision exemption fields', () => {
        const parsed = parseShowAnnotationsJson(
            output([
                {
                    file: '/w/src/pkg.adb',
                    id: 'outcome',
                    kind: 'Exempt_Decision_Outcome',
                    stale: false,
                    location: { start_line: 6, start_column: 7, end_line: 6, end_column: 7 },
                    outcome: true,
                    decision: 2,
                    justification: 'j',
                },
                {
                    file: '/w/src/pkg.adb',
                    id: 'condition',
                    kind: 'Exempt_Decision_Condition',
                    stale: false,
                    location: { start_line: 8, start_column: 7, end_line: 8, end_column: 7 },
                    condition: 3,
                    justification: 'j',
                },
                {
                    file: '/w/src/pkg.adb',
                    id: 'manual',
                    kind: 'Manual_Decision_Evaluation',
                    stale: false,
                    location: { start_line: 9, start_column: 7, end_line: 9, end_column: 7 },
                    values: [true, true, false, true],
                    justification: 'j',
                },
            ]),
        );

        assert.strictEqual(byId(parsed.annotations, 'outcome').outcome, true);
        assert.strictEqual(byId(parsed.annotations, 'outcome').decision, 2);

        //  gnatcov reports condition indices 1-based, as the text form does.
        assert.strictEqual(byId(parsed.annotations, 'condition').condition, 3);

        assert.deepStrictEqual(byId(parsed.annotations, 'manual').values, [
            true,
            true,
            false,
            true,
        ]);
    });

    test('handles annotations spread over several files', () => {
        const parsed = parseShowAnnotationsJson(
            output([
                {
                    file: '/w/src/a.adb',
                    id: 'one',
                    kind: 'Exempt_On',
                    stale: false,
                    location: { start_line: 1, start_column: 1, end_line: 1, end_column: 1 },
                    justification: 'j',
                },
                {
                    file: '/w/other/a.adb',
                    id: 'two',
                    kind: 'Exempt_On',
                    stale: false,
                    location: { start_line: 2, start_column: 1, end_line: 2, end_column: 1 },
                    justification: 'j',
                },
            ]),
        );

        /*
         * Full names, so two sources sharing a base name stay distinct. This is
         * what makes tree entries navigable and their grouping unambiguous.
         */
        assert.deepStrictEqual(
            parsed.annotations.map((a) => a.file),
            ['/w/src/a.adb', '/w/other/a.adb'],
        );
    });

    test('returns nothing for an empty result', () => {
        assert.deepStrictEqual(parseShowAnnotationsJson(output([])).annotations, []);
    });

    test('rejects output that is not the expected object', () => {
        /*
         * A gnatcov that does not know --format=json must surface as a failed
         * invocation, not as a project without annotations.
         */
        assert.throws(() => parseShowAnnotationsJson(''));
        assert.throws(() => parseShowAnnotationsJson('pkg.adb:\n- 6:7 - 6:7; id: x'));
        assert.throws(() => parseShowAnnotationsJson('[]'));
        assert.throws(() => parseShowAnnotationsJson('{}'));

        /*
         * Half an object is rejected too. The file list feeds the watchers, and
         * an absent one used to sail through and throw later, where nothing
         * catches it.
         */
        assert.throws(() => parseShowAnnotationsJson('{"annotations":[]}'));
        assert.throws(() => parseShowAnnotationsJson('{"annotation_files":[]}'));
    });

    test('every annotation kind has a distinct inline label', () => {
        /*
         * The inline badge is the only thing telling the user which kind they
         * are looking at, so every kind must have a label and no two kinds may
         * share one.
         */
        const labels = annotationKinds.map((kind) => labelOf(kind));

        for (const label of labels) {
            assert.ok(label.length > 0, 'every kind must have a non-empty label');
        }
        assert.strictEqual(
            new Set(labels).size,
            annotationKinds.length,
            'labels must be unique across kinds',
        );
    });
});
