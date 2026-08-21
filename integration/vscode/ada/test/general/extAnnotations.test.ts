import assert from 'assert';
import {
    ExternalAnnotation,
    annotationKinds,
    buildAddAnnotationArgs,
    creatableKinds,
    describeAnnotation,
    hasInsertionSide,
    labelOf,
    parseShowAnnotationsJson,
    requiresJustification,
    shapeOf,
    toGnatcovLocation,
} from '../../src/extAnnotationsParser';
import { toVscodeRange } from '../../src/extAnnotations';

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

    test('only the buffer kinds have a side of the statement', () => {
        /*
         * gnatcov writes insert_after for Dump_Buffers and Reset_Buffers only
         * (ss_annotations.adb, the add-annotation case statement). The other
         * kinds insert no code: an exempted region is applied line by line and
         * a fine-grained exemption resolves through a decision offset, so a
         * side would be stored, displayed, and read by nothing.
         */
        assert.deepStrictEqual(annotationKinds.filter(hasInsertionSide), [
            'Dump_Buffers',
            'Reset_Buffers',
        ]);
    });

    test('states the insertion direction of buffer annotations', () => {
        /*
         * gnatcov reports the same location for a Dump_Buffers whether or not
         * --annotate-after was passed, so `insertAfter` is the only thing that
         * says which side of the statement the annotation applies to. The badge
         * is placed accordingly, and the hover names the direction in both
         * cases so that 'before' is never left implicit.
         */
        const dumpBefore: ExternalAnnotation = {
            file: 'x.adb',
            id: 'a',
            kind: 'Dump_Buffers',
            stale: false,
            insertAfter: false,
        };
        const dumpAfter: ExternalAnnotation = { ...dumpBefore, id: 'b', insertAfter: true };

        assert.ok(describeAnnotation(dumpBefore).includes('before the statement'));
        assert.ok(describeAnnotation(dumpAfter).includes('after the statement'));

        // An absent flag means 'before', which is gnatcov's default.
        const dumpUnset: ExternalAnnotation = { ...dumpBefore, id: 'c', insertAfter: undefined };
        assert.ok(describeAnnotation(dumpUnset).includes('before the statement'));

        assert.ok(
            describeAnnotation({
                ...dumpBefore,
                kind: 'Reset_Buffers',
                insertAfter: true,
            }).includes('after the statement'),
        );

        /*
         * Other kinds must not gain a direction: it is meaningless for them, and
         * they are rendered before the annotated code like everything else.
         */
        assert.ok(
            !describeAnnotation({ ...dumpAfter, kind: 'Exempt_Region' }).includes('statement'),
        );

        // The badge text stays the plain kind label; position carries direction.
        assert.strictEqual(labelOf('Dump_Buffers'), 'dump buffers');
    });

    test('does not clamp the end column of a cross-line selection', () => {
        /*
         * vscode's end is exclusive, so selecting to the start of a later line
         * leaves the end at column 0 there. Clamping it against the start column
         * used to record an end far to the right of what was selected.
         */
        assert.deepStrictEqual(
            toGnatcovLocation({
                start: { line: 0, character: 19 },
                end: { line: 1, character: 0 },
            }),
            { startLine: 1, startColumn: 20, endLine: 2, endColumn: 1 },
        );

        // A genuine cross-line end keeps its own column.
        assert.deepStrictEqual(
            toGnatcovLocation({
                start: { line: 0, character: 19 },
                end: { line: 2, character: 4 },
            }),
            { startLine: 1, startColumn: 20, endLine: 3, endColumn: 4 },
        );

        // Ordering is still enforced within one line.
        assert.deepStrictEqual(
            toGnatcovLocation({
                start: { line: 5, character: 6 },
                end: { line: 5, character: 6 },
            }),
            { startLine: 6, startColumn: 7, endLine: 6, endColumn: 7 },
        );
    });

    test('converts an editor selection to a gnatcov location', () => {
        /*
         * vscode is 0-based with an exclusive end; gnatcov is 1-based with an
         * inclusive end. Selecting all of line 14 columns 7..20 as a user sees
         * them is, in editor coordinates, (13, 6) to (13, 20).
         */
        assert.deepStrictEqual(
            toGnatcovLocation({
                start: { line: 13, character: 6 },
                end: { line: 13, character: 20 },
            }),
            { startLine: 14, startColumn: 7, endLine: 14, endColumn: 20 },
        );

        /*
         * toVscodeRange is the exact inverse, so anything gnatcov reports must
         * survive the round trip through editor coordinates.
         */
        for (const location of [
            { startLine: 6, startColumn: 7, endLine: 6, endColumn: 7 },
            { startLine: 10, startColumn: 7, endLine: 12, endColumn: 14 },
            { startLine: 1, startColumn: 1, endLine: 1, endColumn: 1 },
        ]) {
            assert.deepStrictEqual(
                toGnatcovLocation(toVscodeRange(location)),
                location,
                `round trip of ${JSON.stringify(location)}`,
            );
        }

        /*
         * An empty selection is just a cursor. The end column must not collapse
         * below the start, otherwise gnatcov gets an inverted range.
         */
        assert.deepStrictEqual(
            toGnatcovLocation({ start: { line: 0, character: 0 }, end: { line: 0, character: 0 } }),
            { startLine: 1, startColumn: 1, endLine: 1, endColumn: 1 },
        );
    });

    test('builds add-annotation arguments per kind shape', () => {
        const loc = { startLine: 14, startColumn: 7, endLine: 15, endColumn: 20 };

        // A region kind gets --start-location and --end-location.
        assert.deepStrictEqual(
            buildAddAnnotationArgs({
                kind: 'Exempt_Region',
                location: loc,
                justification: 'why',
            }),
            [
                '--kind=Exempt_Region',
                '--start-location=14:7',
                '--end-location=15:20',
                '--justification=why',
            ],
        );

        // A point kind gets --location, built from the start only.
        assert.deepStrictEqual(buildAddAnnotationArgs({ kind: 'Exempt_Off', location: loc }), [
            '--kind=Exempt_Off',
            '--location=14:7',
        ]);

        // Dump_Buffers extras.
        assert.deepStrictEqual(
            buildAddAnnotationArgs({
                kind: 'Dump_Buffers',
                location: loc,
                insertAfter: true,
                tracePrefix: 'showcase',
            }),
            [
                '--kind=Dump_Buffers',
                '--location=14:7',
                '--annotate-after',
                '--dump-filename-prefix=showcase',
            ],
        );

        // An empty justification is omitted rather than passed as an empty
        // switch value, which gnatcov would reject.
        assert.deepStrictEqual(
            buildAddAnnotationArgs({ kind: 'Cov_On', location: loc, justification: '' }),
            ['--kind=Cov_On', '--location=14:7'],
        );

        //  insertAfter false is the default: no switch, rather than a negated one.
        assert.deepStrictEqual(
            buildAddAnnotationArgs({ kind: 'Reset_Buffers', location: loc, insertAfter: false }),
            ['--kind=Reset_Buffers', '--location=14:7'],
        );

        //  A chosen identifier is passed through.
        assert.deepStrictEqual(
            buildAddAnnotationArgs({ kind: 'Cov_On', location: loc, annotationId: 'mine' }),
            ['--kind=Cov_On', '--location=14:7', '--annotation-id=mine'],
        );

        /*
         * A decision kind takes a location like any other. gnatcov stops with
         * "Missing --start-location on the command line" without one, whatever
         * --outcome/--condition/--decision say.
         */
        assert.deepStrictEqual(
            buildAddAnnotationArgs({
                kind: 'Exempt_Full_Decision',
                location: loc,
                justification: 'j',
            }),
            ['--kind=Exempt_Full_Decision', '--location=14:7', '--justification=j'],
        );
    });

    test('every creatable kind takes a location', () => {
        /*
         * gnatcov requires a location for every kind, so the command line built
         * for each creatable kind must carry one. Asserting on the switches
         * rather than on shapeOf keeps this honest: it is the command line that
         * gnatcov rejects.
         */
        const location = { startLine: 14, startColumn: 7, endLine: 14, endColumn: 20 };
        for (const kind of creatableKinds) {
            const args = buildAddAnnotationArgs({ kind: kind, location: location });
            assert.ok(
                args.some((arg) => arg.startsWith('--location=')) ||
                    args.some((arg) => arg.startsWith('--start-location=')),
                `${kind} is created without a location: ${args.join(' ')}`,
            );
        }

        // Only Exempt_Region needs a selection; the rest work off the cursor.
        const regions = creatableKinds.filter((kind) => shapeOf(kind) === 'region');
        assert.deepStrictEqual(regions, ['Exempt_Region']);

        // Kinds gnatcov refuses or warns about without a justification.
        const needJustification = creatableKinds.filter(requiresJustification);
        assert.deepStrictEqual(
            new Set(needJustification),
            new Set(['Exempt_Region', 'Exempt_On', 'Exempt_Branch', 'Cov_Off']),
        );

        //  The decision kinds need one too; they are just not creatable here.
        for (const kind of [
            'Exempt_Decision_Outcome',
            'Exempt_Decision_Condition',
            'Exempt_Full_Decision',
            'Manual_Decision_Evaluation',
        ] as const) {
            assert.ok(requiresJustification(kind), kind);
        }
    });

    test('reports an unknown kind rather than dropping the annotation', () => {
        const parsed = parseShowAnnotationsJson(
            output([
                {
                    file: '/w/src/pkg.adb',
                    id: 'x',
                    kind: 'Some_New_Kind',
                    stale: false,
                    location: { start_line: 6, start_column: 7, end_line: 6, end_column: 7 },
                },
            ]),
        );

        assert.strictEqual(parsed.annotations.length, 1);
        assert.strictEqual(parsed.annotations[0].kind, 'Unknown');
    });
});
