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
