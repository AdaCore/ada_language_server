import { X2jOptions, XMLParser } from 'fast-xml-parser';
import * as fs from 'fs';

/**
 * TypeScript types to represent data from GNATcoverage XML reports
 */
export type CovIndex = {
    coverage_report: coverage_report_type;
};
type coverage_report_type = {
    coverage_info?: coverage_info_type;
    coverage_summary?: coverage_summary_type;
    sources?: sources_type;
    '@_coverage_level': coverage_level_type;
};
type coverage_info_type = {
    traces: traces_type;
};
type traces_type = {
    trace: trace_type[];
};
type trace_type = {
    '@_filename': string;
    '@_kind': trace_kind_type;
    '@_program': string;
    '@_date': string;
    '@_tag'?: string;
};
type trace_kind_type = 'binary' | 'source';
type coverage_summary_type = {
    metric: metric_type;
    obligation_stats: obligation_stats_type;
    file: file_type[];
};
type file_type = {
    metric: metric_type[];
    obligation_stats: obligation_stats_type[];
    '@_name'?: string;
};
type coverage_level_type =
    | 'branch'
    | 'insn'
    | 'stmt'
    | 'stmt+decision'
    | 'stmt+mcdc'
    | 'stmt+uc_mcdc';
type sources_type = {
    source: source_type[];
};
type source_type = {
    '@_file': string;
    '@_coverage_level': coverage_level_type;
    scope_metric: scope_metric_type[];
    src_mapping: src_mapping_type[];
};
type scope_metric_type = {
    metric: metric_type[];
    obligation_stats: obligation_stats_type[];
    scope_metric: scope_metric_type[];
    '@_scope_name': string;
    '@_scope_line': number;
};
type metric_type = {
    '@_kind': metric_kind_type;
    '@_count': number;
    '@_ratio': number;
};
type metric_kind_type =
    | 'total_obligations_of_relevance'
    | 'total_lines_of_relevance'
    | 'fully_covered'
    | 'partially_covered'
    | 'not_covered'
    | 'undetermined_coverage'
    | 'disabled_coverage'
    | 'exempted_no_violation'
    | 'exempted_undetermined_coverage'
    | 'exempted';
type obligation_stats_type = {
    metric: metric_type[];
    '@_kind': string;
};
type src_mapping_type = {
    src: src_type;
    statement: statement_type[];
    decision: decision_type[];
    message: message_type[];
    '@_coverage': coverage_type;
};
type src_type = {
    line: line_type[];
};
type line_type = {
    '@_num': number;
    '@_src': string;
    '@_column_begin': number;
    '@_column_end': number;
};
type statement_type = {
    src?: src_type;
    '@_coverage': coverage_type;
    '@_id': number;
    '@_text': string;
};
type coverage_type = '.' | '+' | '-' | '!' | '?' | '#' | '@' | '*' | '0' | 'v' | '>';
type decision_type = {
    src?: src_type;
    condition: condition_type[];
    '@_coverage': coverage_type;
    '@_id': number;
    '@_text': string;
};
type condition_type = {
    src?: src_type;
    '@_coverage': coverage_type;
    '@_id': number;
    '@_text': string;
};
type message_type = {
    '@_kind': message_kind_type;
    '@_address': string;
    '@_SCO': string;
    '@_message': string;
};
type message_kind_type =
    | 'notice'
    | 'warning'
    | 'error'
    | 'violation'
    | 'undetermined_cov'
    | 'exclusion';

/**
 *
 * @param path - path to GNATcoverage index.xml report
 * @returns parsed report
 */
export function parseGnatcovIndexXml(path: string): CovIndex {
    const fileContentAsBuffer = fs.readFileSync(path);

    const options: Partial<X2jOptions> = {
        // By default the parser ignores attributes, so we set this option
        // to obtain attributes.
        ignoreAttributes: false,
        // This prefix is used in the JS objects resulting from the parsing
        // to differentiate attributes from child nodes.
        attributeNamePrefix: '@_',
        isArray: (_, jPath) => {
            return ([] as string[]).indexOf(jPath) !== -1;
        },
    };
    const parser = new XMLParser(options);
    // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
    const parseResult = parser.parse(fileContentAsBuffer);
    if ('document' in parseResult) {
        // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access
        return parseResult.document as CovIndex;
    } else {
        throw Error(`Could not parse GNATcoverage report: ${path}`);
    }
}
