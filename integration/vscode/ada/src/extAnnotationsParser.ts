/*----------------------------------------------------------------------------
--           GNATcoverage external annotations: types and parsing           --
--                                                                          --
-- This module deliberately does not import 'vscode', so that the parsing   --
-- logic can be unit tested in isolation. Everything that needs the editor  --
-- API lives in extAnnotations.ts.                                          --
----------------------------------------------------------------------------*/

/**
 * The annotation kinds supported by gnatcov, spelled exactly as accepted by
 * `gnatcov add-annotation --kind` and as printed by `show-annotations`.
 */
export const annotationKinds = [
    'Exempt_Region',
    'Exempt_On',
    'Exempt_Off',
    'Exempt_Decision_Outcome',
    'Exempt_Decision_Condition',
    'Exempt_Full_Decision',
    'Manual_Decision_Evaluation',
    'Exempt_Branch',
    'Dump_Buffers',
    'Reset_Buffers',
    'Cov_On',
    'Cov_Off',
    'Unknown',
] as const;

export type AnnotationKind = (typeof annotationKinds)[number];

/**
 * Broad categories used to decide how an annotation is rendered. Exemptions,
 * coverage switching and buffer manipulation are visually distinct because
 * they mean very different things.
 */
export const enum AnnotationCategory {
    Exemption,
    CoverageSwitch,
    Buffers,
}

/**
 * Which location switches `gnatcov add-annotation` takes for a kind.
 *
 *   - `region`: `--start-location` and `--end-location`
 *   - `point`: `--location`
 *
 * Every kind needs one or the other. Without a location gnatcov stops with
 * "Missing --start-location on the command line", the decision kinds included:
 * their `--decision`, `--condition` and `--outcome` say *which* decision at
 * that location is meant, not where it is.
 *
 * Only Exempt_Region takes a range. Exempt_Branch accepts either form but
 * stores a point in both cases, discarding any end location.
 */
export type AnnotationShape = 'region' | 'point';

export function shapeOf(kind: AnnotationKind): AnnotationShape {
    return kind === 'Exempt_Region' ? 'region' : 'point';
}

/**
 * Whether gnatcov expects a justification for this kind.
 *
 * For Cov_Off the justification is optional, but gnatcov emits
 * `warning: --justification missing for a --kind=Cov_Off annotation`, so it is
 * treated as expected and the user is prompted for one.
 */
export function requiresJustification(kind: AnnotationKind): boolean {
    switch (kind) {
        case 'Exempt_On':
        case 'Exempt_Region':
        case 'Exempt_Branch':
        case 'Cov_Off':
        case 'Exempt_Decision_Outcome':
        case 'Exempt_Decision_Condition':
        case 'Exempt_Full_Decision':
        case 'Manual_Decision_Evaluation':
            return true;
        default:
            return false;
    }
}

/**
 * The kinds the IDE offers when creating an annotation.
 *
 * The four decision-related kinds are deliberately excluded. They take a
 * location like the rest, but also need `--outcome`, `--condition`,
 * `--decision` or `--values` to say which decision at that location is meant.
 * That input deserves a dedicated UI, not a string prompt.
 *
 * They can still be created with `gnatcov add-annotation`. Once created they
 * display and delete like any other.
 */
export const creatableKinds: readonly AnnotationKind[] = [
    'Exempt_Region',
    'Exempt_On',
    'Exempt_Off',
    'Exempt_Branch',
    'Cov_Off',
    'Cov_On',
    'Dump_Buffers',
    'Reset_Buffers',
];

/**
 * Short label displayed inline next to an annotation.
 *
 * Annotations are rendered as a text badge rather than a symbol: a marker
 * glyph alone tells the user that *something* is annotated, but not which of
 * the twelve kinds it is, which is the first question one asks when looking at
 * an exemption.
 */
export function labelOf(kind: AnnotationKind): string {
    switch (kind) {
        case 'Exempt_On':
            return 'exempt on';
        case 'Exempt_Off':
            return 'exempt off';
        case 'Exempt_Region':
            return 'exempt region';
        case 'Exempt_Decision_Outcome':
            return 'exempt outcome';
        case 'Exempt_Decision_Condition':
            return 'exempt condition';
        case 'Exempt_Full_Decision':
            return 'exempt decision';
        case 'Manual_Decision_Evaluation':
            return 'manual decision';
        case 'Exempt_Branch':
            return 'exempt branch';
        case 'Dump_Buffers':
            return 'dump buffers';
        case 'Reset_Buffers':
            return 'reset buffers';
        case 'Cov_On':
            return 'coverage on';
        case 'Cov_Off':
            return 'coverage off';
        case 'Unknown':
            return 'unknown annotation';
    }
}

/**
 * Whether the annotation applies to one side of the statement it designates,
 * i.e. whether gnatcov records `--annotate-after` for this kind.
 *
 * Only the buffer kinds do, because only they insert code. Where the call sits
 * in the statement list decides when it runs.
 *
 * Every other kind is consumed by comparing positions. An exempted region is
 * applied line by line, and a fine-grained exemption resolves through a
 * decision offset. A side would be a preference nothing reads.
 */
export function hasInsertionSide(kind: AnnotationKind): boolean {
    return kind === 'Dump_Buffers' || kind === 'Reset_Buffers';
}

export function categoryOf(kind: AnnotationKind): AnnotationCategory {
    switch (kind) {
        case 'Cov_On':
        case 'Cov_Off':
            return AnnotationCategory.CoverageSwitch;
        case 'Dump_Buffers':
        case 'Reset_Buffers':
            return AnnotationCategory.Buffers;
        default:
            return AnnotationCategory.Exemption;
    }
}

export type AnnotationLocation = {
    startLine: number;
    startColumn: number;
    endLine: number;
    endColumn: number;
};

/**
 * A single external annotation, as resolved by gnatcov against the current
 * state of the source file.
 */
export type ExternalAnnotation = {
    /** File the annotation applies to, as reported by gnatcov. */
    file: string;

    /** Annotation identifier; the key used by `delete-annotation`. */
    id: string;

    kind: AnnotationKind;

    /**
     * True when gnatcov could not resolve the annotation's stable sloc against
     * the current source. Such an annotation has no `range`.
     */
    stale: boolean;

    /**
     * Resolved location. Always set when {@link stale} is false, never set
     * otherwise.
     *
     * gnatcov reports 1-based, end-inclusive line/column numbers. The
     * conversion to vscode.Range is done by `toVscodeRange` in
     * extAnnotations.ts, so that this type stays a faithful representation of
     * what gnatcov reported.
     */
    range?: AnnotationLocation;

    /** Reason why the annotation could not be resolved. Set iff stale. */
    diagnostic?: string;

    justification?: string;

    /* Kind-specific fields. */
    outcome?: boolean;
    /** 1-based condition index, as printed by gnatcov. */
    condition?: number;
    decision?: number;
    values?: boolean[];
    insertAfter?: boolean;
    tracePrefix?: string;
};

/*----------------------------------------------------------------------------
--                                 Parsing                                  --
--                                                                          --
-- `gnatcov show-annotations --format=json` prints a single object,         --
-- described by ShowAnnotationsJson below.                                  --
--                                                                          --
-- gnatcov ships with the same toolchain as this extension, so its output   --
-- is taken at face value. What follows converts it, it does not validate   --
-- it. Output that is not that object throws, and the caller reports a      --
-- failed invocation. An older gnatcov printing the text form lands there.  --
----------------------------------------------------------------------------*/

/** The shape of what gnatcov prints. Field names are gnatcov's. */
type ShowAnnotationsJson = {
    annotation_files: string[];
    annotations: {
        file: string;
        id: string;
        kind: string;
        stale: boolean;
        location?: {
            start_line: number;
            start_column: number;
            end_line: number;
            end_column: number;
        };
        diagnostic?: string;
        justification?: string;
        outcome?: boolean;
        condition?: number;
        decision?: number;
        values?: boolean[];
        insert_after?: boolean;
        trace_prefix?: string;
    }[];
};

/**
 * Everything `show-annotations` reports.
 */
export type ShowAnnotationsOutput = {
    /**
     * The external annotation files in effect, as absolute paths.
     *
     * gnatcov resolves these itself, against the project that defines
     * `Coverage'External_Annotations` rather than the root project, so this is
     * the only reliable way to know which files to watch. A file the project
     * designates but that does not exist yet is listed too, since that is where
     * a first annotation will be written.
     */
    annotationFiles: string[];

    annotations: ExternalAnnotation[];
};

/**
 * Parse the output of `gnatcov show-annotations --format=json`.
 *
 * @throws if the output is not the expected JSON object. That means a gnatcov
 * which does not implement this format, which the caller reports as a failed
 * invocation rather than as an absence of annotations.
 */
export function parseShowAnnotationsJson(stdout: string): ShowAnnotationsOutput {
    const root = JSON.parse(stdout) as ShowAnnotationsJson;

    if (!Array.isArray(root.annotations)) {
        throw new Error('expected an "annotations" array');
    }

    /*
     * Checked as well: this list is what the file watchers are built from, and
     * mapping over an absent one would throw where nothing catches it, leaving
     * the session with no watchers and no message.
     */
    if (!Array.isArray(root.annotation_files)) {
        throw new Error('expected an "annotation_files" array');
    }

    return {
        annotationFiles: root.annotation_files,
        annotations: root.annotations.map((annotation) => ({
            file: annotation.file,
            id: annotation.id,

            //  A kind this extension does not know must still display and
            //  delete, rather than render as a blank badge.
            kind: isAnnotationKind(annotation.kind) ? annotation.kind : 'Unknown',

            stale: annotation.stale,
            range:
                annotation.location === undefined
                    ? undefined
                    : {
                          startLine: annotation.location.start_line,
                          startColumn: annotation.location.start_column,
                          endLine: annotation.location.end_line,
                          endColumn: annotation.location.end_column,
                      },
            diagnostic: annotation.diagnostic,

            //  gnatcov emits the field for every kind that accepts one, empty
            //  when the annotation carries no justification.
            justification: annotation.justification || undefined,

            outcome: annotation.outcome,
            condition: annotation.condition,
            decision: annotation.decision,
            values: annotation.values,
            insertAfter: annotation.insert_after,
            tracePrefix: annotation.trace_prefix,
        })),
    };
}

function isAnnotationKind(value: string): value is AnnotationKind {
    return (annotationKinds as readonly string[]).includes(value);
}

/*----------------------------------------------------------------------------
--                           Creating annotations                           --
----------------------------------------------------------------------------*/

/**
 * A zero-based, end-exclusive text span, i.e. the shape of a
 * `vscode.Selection`. Duck-typed rather than typed as vscode.Selection so that
 * the conversion below stays testable outside the editor.
 */
export type EditorSpan = {
    start: { line: number; character: number };
    end: { line: number; character: number };
};

/**
 * Convert an editor selection into the 1-based, end-inclusive location that
 * gnatcov expects.
 *
 * vscode.Selection is 0-based with an *exclusive* end; gnatcov is 1-based with
 * an *inclusive* end. So lines and the start column gain 1, while the end
 * column does not: the exclusive end column of a 0-based span is already the
 * inclusive end column of the corresponding 1-based span. This is the exact
 * inverse of `toVscodeRange`.
 */
export function toGnatcovLocation(span: EditorSpan): AnnotationLocation {
    const sameLine = span.start.line === span.end.line;

    return {
        startLine: span.start.line + 1,
        startColumn: span.start.character + 1,
        endLine: span.end.line + 1,

        /*
         * Ordering is only enforced within a single line. On a later line the
         * end column stands on its own, and clamping it against the start
         * column would push the end beyond what the user selected: selecting
         * from line 1 column 20 to the start of line 2 would be recorded as
         * ending at line 2 column 20.
         *
         * The floor of 1 keeps the column a valid 1-based one; callers holding
         * the document are expected to have moved an end sitting at column 0
         * back to the end of the previous line.
         */
        endColumn: sameLine
            ? Math.max(span.end.character, span.start.character + 1)
            : Math.max(span.end.character, 1),
    };
}

export type CreateAnnotationParams = {
    kind: AnnotationKind;
    /** 1-based, end-inclusive, as returned by {@link toGnatcovLocation}. */
    location: AnnotationLocation;
    justification?: string;
    annotationId?: string;
    /** Dump_Buffers and Reset_Buffers only. */
    insertAfter?: boolean;
    /** Dump_Buffers only. */
    tracePrefix?: string;
};

/**
 * Build the kind-specific part of a `gnatcov add-annotation` command line.
 *
 * The project, annotation file, output file and source file are added by the
 * caller, which is the part that knows about the workspace.
 *
 * The selection is passed through as the user made it: gnatcov accepts an
 * arbitrary range, including one that starts mid-expression, and silently
 * stores it as given. Snapping it to statement boundaries here would hide what
 * actually ends up in the annotation file.
 */
export function buildAddAnnotationArgs(params: CreateAnnotationParams): string[] {
    const args: string[] = [`--kind=${params.kind}`];
    const loc = params.location;

    switch (shapeOf(params.kind)) {
        case 'region':
            args.push(
                `--start-location=${String(loc.startLine)}:${String(loc.startColumn)}`,
                `--end-location=${String(loc.endLine)}:${String(loc.endColumn)}`,
            );
            break;
        case 'point':
            args.push(`--location=${String(loc.startLine)}:${String(loc.startColumn)}`);
            break;
    }

    if (params.justification !== undefined && params.justification.length > 0) {
        args.push(`--justification=${params.justification}`);
    }

    if (params.annotationId !== undefined && params.annotationId.length > 0) {
        args.push(`--annotation-id=${params.annotationId}`);
    }

    if (params.insertAfter === true) {
        args.push('--annotate-after');
    }

    if (params.tracePrefix !== undefined && params.tracePrefix.length > 0) {
        args.push(`--dump-filename-prefix=${params.tracePrefix}`);
    }

    return args;
}

/**
 * @returns a human-readable one-line description of an annotation's extra
 * fields, used in hovers and in error messages.
 *
 * The kind is left out: every caller displays it separately, more prominently
 * than these details.
 */
export function describeAnnotation(annotation: ExternalAnnotation): string {
    const parts: string[] = [];

    if (annotation.condition !== undefined) {
        parts.push(`condition ${String(annotation.condition)}`);
    }
    if (annotation.outcome !== undefined) {
        parts.push(`outcome ${String(annotation.outcome)}`);
    }
    if (annotation.decision !== undefined) {
        parts.push(`decision offset ${String(annotation.decision)}`);
    }
    if (annotation.values !== undefined) {
        parts.push(`values ${annotation.values.map((v) => (v ? 'T' : 'F')).join('')}`);
    }
    /*
     * Stated in both directions: 'before' is the default, and leaving it
     * implicit makes it impossible to tell a before-annotation from one whose
     * position simply was not reported.
     */
    if (annotation.kind === 'Dump_Buffers' || annotation.kind === 'Reset_Buffers') {
        parts.push(
            annotation.insertAfter === true
                ? 'inserted after the statement'
                : 'inserted before the statement',
        );
    }
    if (annotation.tracePrefix !== undefined) {
        parts.push(`trace prefix ${annotation.tracePrefix}`);
    }

    return parts.join(', ');
}
