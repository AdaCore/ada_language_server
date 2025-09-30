import assert from 'assert';
import { X2jOptions, XMLParser } from 'fast-xml-parser';
import * as fs from 'fs';
import { cpus } from 'os';
import * as path from 'path';
import * as vscode from 'vscode';
import { CancellationToken } from 'vscode-languageclient';
import { logger } from './extension';
import {
    getMatchingPrefixes,
    parallelize,
    showErrorMessageWithOpenLogButton,
    staggerProgress,
    toPosix,
} from './helpers';

/**
 * Parsing GNATcoverage XML reports.
 *
 * GNATcoverage generates an XSD schema along with its XML reports, however the
 * library 'fast-xml-parser' that we are using doesn't support a schema based
 * import. It parses an XML file into JS object with no assumptions on the
 * types of XML attributes and child objects.
 *
 * For example, when the parser encounters a tag `<a>`, it creates a field `a`
 * in the parent object whose value is a JS object. Later if another `<a>` is
 * encountered, the value is replaced with a list. Also, all attributes are
 * parsed as strings by default since the parser doesn't have any knowledge of
 * the schema.
 *
 * To address that, the parser is configured with a function `isArray`
 * where we can give the XML paths that should always be parsed as lists, even
 * when one tag is encountered. However if no tags are encountered, the parser
 * simply doesn't create the corresponding property in the result object, and
 * accessing the property yields `undefined`. For this reason all array
 * properties are typed as optional.
 *
 * Similarly, the parser is configured with a function
 * `attributeValueProcessor` allowing to convert attributes to more specific
 * types based on the attribute name (e.g. numbers).
 *
 * Below we define a set of types that represent a parsed GNATcoverage report,
 * but bare in mind that the parsed XML object is not checked for validity
 * against the declared types, it is simply cast without a check. So there can
 * be divergence between the declared types and the underlying objects, e.g. if
 * GNATcoverage generates an XML that violates the schema, or if a developer
 * makes a mistake in the type declaration.
 */

/**
 * TypeScript types to represent data from GNATcoverage XML reports
 */
export type document_type = {
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
    trace?: trace_type[];
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
    metric?: metric_type[];
    obligation_stats?: obligation_stats_type[];
    file?: file_type[];
};
type file_type = {
    metric?: metric_type[];
    obligation_stats?: obligation_stats_type[];
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
    source?: source_type[];
    'xi:include'?: xi_include_type[];
};

type xi_include_type = {
    '@_parse': string;
    '@_href': string;
};

export type source_type = {
    '@_file': string;
    '@_coverage_level': coverage_level_type;
    scope_metric?: scope_metric_type[];
    src_mapping?: src_mapping_type[];
};
type scope_metric_type = {
    metric?: metric_type[];
    obligation_stats?: obligation_stats_type[];
    scope_metric?: scope_metric_type[];
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
    metric?: metric_type[];
    '@_kind': string;
};
export type src_mapping_type = {
    src: src_type;
    statement?: statement_type[];
    decision?: decision_type[];
    message?: message_type[];
    '@_coverage': coverage_type;
};
type src_type = {
    line?: line_type[];
};
export type line_type = {
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

/**
 *
 *   .   |  No coverage obligation is attached to the line
 *
 *   -   |  Coverage obligations attached to the line, none satisfied
 *   !   |  Coverage obligations attached to the line, some satisfied
 *   ?   |  Coverage obligations attached to the line, undetermined coverage state
 *       |  (in the absence of other violations)
 *   +   |  Coverage obligations attached to the line, all satisfied
 *
 *   #   |  Zero violations in exempted region
 *   *   |  One violation in exempted region
 *   \@  |  Also related to exemptions, unclear semantics
 *
 *   0   |  not coverable code, only relevant in binary traces mode (no
 *       |  instrumentation) which is unsupported in VS Code
 *
 *   v, \>  |  symbols for object level coverage which is not supported in VS
 *          |  Code. We only support source level coverage.
 * See
 * https://docs.adacore.com/gnatcoverage-docs/html/gnatcov/cov_source.html#annotated-sources-text-xcov
 * and
 * https://docs.adacore.com/gnatcoverage-docs/html/gnatcov/exemptions.html#reporting-about-coverage-exemptions
 * for more information.
 */
export const coverage_type_values = [
    '.',
    '+',
    '-',
    '!',
    '?',
    '#',
    '@',
    '*',
    '0',
    'v',
    '>',
] as const;
export type coverage_type = (typeof coverage_type_values)[number];

type decision_type = {
    src?: src_type;
    condition?: condition_type[];
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
 * Process attribute values to convert certain attributes to number values.
 *
 * @param name - attribute name
 * @param value - unprocessed value
 * @returns processed value
 */
function attributeValueProcessor(name: string, value: string): unknown {
    return ['scope_line', 'count', 'ratio', 'num', 'column_begin', 'column_end', 'id'].includes(
        name,
    )
        ? Number.parseInt(value)
        : value;
}

/**
 * This constant lists the XML paths that should always be parsed as a list.
 *
 * (otherwise the XML parser parses a simple object if there is only one in the
 * XML file, and then updates to a list if another object is encountered. We
 * want to avoid this ambivalence so we ask the parser to always create lists
 * for these XML paths.)
 */
const INDEX_XML_ARRAY_PATHS = [
    'document.coverage_report.sources.source',
    'document.coverage_report.coverage_summary.metric',
    'document.coverage_report.coverage_summary.obligation_stats',
    'document.coverage_report.coverage_summary.obligation_stats.metric',
    'document.coverage_report.coverage_summary.file',
    'document.coverage_report.coverage_summary.file.metric',
    'document.coverage_report.coverage_summary.file.obligation_stats',
    'document.coverage_report.coverage_summary.file.obligation_stats.metric',
];
/**
 *
 * @param path - path to GNATcoverage index.xml report
 * @returns parsed report
 */
export function parseGnatcovIndexXml(path: string): document_type {
    const fileContentAsBuffer = fs.readFileSync(path);

    const options: Partial<X2jOptions> = {
        // By default the parser ignores attributes, so we set this option
        // to obtain attributes.
        ignoreAttributes: false,
        // This prefix is used in the JS objects resulting from the parsing
        // to differentiate attributes from child nodes.
        attributeNamePrefix: '@_',
        isArray: (_, jPath) => {
            return INDEX_XML_ARRAY_PATHS.indexOf(jPath) !== -1;
        },
        trimValues: false,
        attributeValueProcessor: attributeValueProcessor,
    };
    const parser = new XMLParser(options);
    // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
    const parseResult = parser.parse(fileContentAsBuffer);
    if ('document' in parseResult) {
        // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access
        return parseResult.document as document_type;
    } else {
        throw Error(`Could not parse GNATcoverage report: ${path}`);
    }
}

/**
 * This constant lists the XML paths that should always be parsed as a list.
 *
 * (otherwise the XML parser parses a simple object if there is only one in the
 * XML file, and then updates to a list if another object is encountered. We
 * want to avoid this ambivalence so we ask the parser to always create lists
 * for these XML paths.)
 */
const SOURCE_XML_ARRAY_PATHS = [
    'source.src_mapping',
    'source.src_mapping.statement',
    'source.src_mapping.decision',
    'source.src_mapping.message',
    'source.src_mapping.src.line',
    'source.src_mapping.statement.src.line',
    'source.src_mapping.decision.src.line',
    'source.src_mapping.decision.condition',
    'source.src_mapping.decision.condition.src.line',
];
/**
 *
 * @param path - path to GNATcoverage <source-file>.xml report
 * @returns parsed report
 */
export function parseGnatcovFileXml(path: string): source_type {
    const fileContentAsBuffer = fs.readFileSync(path);

    const options: Partial<X2jOptions> = {
        // By default the parser ignores attributes, so we set this option
        // to obtain attributes.
        ignoreAttributes: false,
        // This prefix is used in the JS objects resulting from the parsing
        // to differentiate attributes from child nodes.
        attributeNamePrefix: '@_',
        isArray: (_, jPath) => {
            return SOURCE_XML_ARRAY_PATHS.indexOf(jPath) !== -1;
        },
        /**
         * By default, attribute values are trimmed from leading and trailing
         * whitespace. GNATcov stores the content of source code lines in XML
         * attributes so we don't want to trim in order to preserve the
         * original content.
         */
        trimValues: false,
        attributeValueProcessor: attributeValueProcessor,
    };

    const parser = new XMLParser(options);
    // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
    const parseResult = parser.parse(fileContentAsBuffer);
    if ('source' in parseResult) {
        // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access
        return parseResult.source as source_type;
    } else {
        throw Error(`Could not parse GNATcoverage report: ${path}`);
    }
}

/**
 *
 * @param run - the test run to add coverage data to
 * @param covDir - The path to the directory containing GNATcoverage XML
 * reports. The directory must contain a single 'index.xml' file.
 */
export async function addCoverageData(run: vscode.TestRun, covDir: string) {
    const indexPath = path.join(covDir, 'index.xml');
    const data = parseGnatcovIndexXml(indexPath);

    await vscode.window.withProgress(
        {
            cancellable: true,
            location: vscode.ProgressLocation.Notification,
            title: 'Loading GNATcoverage report',
        },
        async (progress, token) => {
            const array = data.coverage_report.coverage_summary?.file ?? [];
            let done: number = 0;
            let lastProgress = 0;
            const totalFiles = array.length;
            progress.report({
                message: `${done} / ${totalFiles} source files`,
            });

            let posixForeignPrefix: string | undefined;
            let posixLocalPrefix: string | undefined;

            /**
             * Given a `file_type` object from a GNATcoverage XML report, this
             * function searches for the corresponding source file in the
             * workspace given the following considerations:
             *
             *  - The paths in the XML report may have a different convention
             *  that the environment of this workspace (i.e. Windows/POSIX)
             *
             *  - The path in the XML report may be just the basename of a
             *  source file, or a full path to the source file on the machine
             *  where the report was created.
             *
             *  - Once a file is found, the path mapping between the report
             *  machine and the current machine can help find other files
             *  quickly. This is achieved with the state variables
             *  `posixForeignPrefix` and `posixLocalPrefix`.
             *
             * @param file - a `file_type` object from a GNATcoverage XML report
             * @returns the URI of the corresponding source file in the workspace
             */
            async function computeUri(file: file_type): Promise<vscode.Uri | undefined> {
                let srcUri: vscode.Uri | undefined = undefined;

                assert(file['@_name']);
                const foreignPath = file['@_name'];
                /**
                 * The foreign machine may have a different path
                 * format, so we normalize to POSIX which is valid on
                 * both Windows and POSIX OS-es.
                 */
                const posixForeignPath = toPosix(foreignPath);

                /**
                 * The goal here is to find the file in the workspace
                 * corresponding to the name attribute in the GNATcov
                 * report.
                 *
                 * The name could be a basename (older GNATcov
                 * versions) or an absolute path (newer GNATcov
                 * versions).
                 *
                 * In the case of a basename, the only course of action
                 * is to do a file lookup in the workspace.
                 *
                 * In the case of an absolute path, the path
                 * corresponds to the machine where the report was
                 * created which might be a foreign machine or the
                 * local host. We can't know in which situation we are
                 * so it's best to assume that it's a foreign machine.
                 *
                 * Then the logic consists of searching for the first
                 * file by basename, which gives a local absolute path.
                 * Then by comparing the local absolute path and the
                 * foreign absolute path, we can find a foreign prefix
                 * path that should be mapped to the local prefix path
                 * to compute local absolute paths. Subsequent files
                 * can use the computed prefixes directly without a
                 * workspace lookup.
                 */

                if (path.posix.isAbsolute(posixForeignPath)) {
                    let localFullPath;
                    if (posixLocalPrefix && posixForeignPrefix) {
                        /**
                         * The prefixes have already been determined, so
                         * use them directly.
                         */

                        if (posixForeignPath.startsWith(posixForeignPrefix)) {
                            // Extract the relative path based on the foreign prefix
                            const posixForeignRelPath = path.relative(
                                posixForeignPrefix,
                                posixForeignPath,
                            );

                            // Resolve the relative path with the local prefix
                            localFullPath = path.join(posixLocalPrefix, posixForeignRelPath);
                        }
                    }

                    // Fallback to using the input path as is
                    localFullPath = localFullPath ?? foreignPath;

                    if (fs.existsSync(localFullPath)) {
                        srcUri = vscode.Uri.file(localFullPath);
                    }
                }

                if (srcUri === undefined) {
                    /**
                     * GNATcov generates instrumented versions of the
                     * sources with the same basenames. We want to
                     * avoid associating coverage data with the
                     * instrumented sources, so we exclude any paths
                     * containing the special directory
                     * `*gnatcov-instr`. Ideally it would have been
                     * nice to exclude precisely
                     * `<obj-dir>/<prj-name>-gnatcov-instr` but that
                     * would need to be computed for each project in
                     * the closure. As we don't have access to that
                     * information, we ignore all paths containing a
                     * `*gnatcov-instr` component.
                     *
                     * Note that a previous version excluded the entire
                     * object dir which did not work well on projects
                     * that use '.' as the object dir. In that case
                     * excluding the object dir would exclude the
                     * entire workspace and prevent finding any files.
                     */
                    const exclude = `**/*gnatcov-instr/**/*`;

                    /**
                     * If the prefixes haven't been found yet, or
                     * the last prefixes used were not successful,
                     * try a workspace lookup of the basename.
                     */
                    const found = await vscode.workspace.findFiles(
                        `**/${path.posix.basename(posixForeignPath)}`,
                        exclude,
                        1,
                        token,
                    );
                    if (found.length == 0) {
                        return undefined;
                    }

                    srcUri = found[0];
                }

                if (
                    posixForeignPrefix === undefined &&
                    posixLocalPrefix === undefined &&
                    path.posix.isAbsolute(posixForeignPath)
                ) {
                    /**
                     * If the prefixes haven't been calculated, and the
                     * foreign path is absolute, let's try to compute
                     * the prefixes based on the workspace URI that was
                     * found.
                     */

                    const localAbsPath = srcUri.fsPath;
                    const posixLocalAbsPath = toPosix(localAbsPath);

                    [posixForeignPrefix, posixLocalPrefix] = getMatchingPrefixes(
                        posixForeignPath,
                        posixLocalAbsPath,
                    );
                }

                return srcUri;
            }
            const procs = process.env.PROCESSORS ? Number(process.env.PROCESSORS) : 0;
            const fileCovs = (
                await parallelize(
                    array,
                    Math.min(procs == 0 ? cpus().length : procs, 8),
                    async (file) => {
                        if (token.isCancellationRequested) {
                            throw new vscode.CancellationError();
                        }

                        const srcUri: vscode.Uri | undefined = await computeUri(file);

                        assert(
                            srcUri,
                            `Could not find the file in the workspace: ${file['@_name']}`,
                        );

                        const fileReportBasename = data.coverage_report.sources?.[
                            'xi:include'
                        ]?.find(
                            (inc) => inc['@_href'] == `${path.posix.basename(srcUri.path)}.xml`,
                        )?.['@_href'];

                        if (!fileReportBasename) {
                            const msg = `Malformed GNATcoverage report ${indexPath}`;
                            void showErrorMessageWithOpenLogButton(msg);
                            logger.warn(
                                `${msg}: cannot find <xi:include> element for source file ` +
                                    `${path.posix.basename(srcUri.path)}`,
                            );
                            return undefined;
                        }

                        const fileReportPath = path.join(covDir, fileReportBasename);

                        const stmtStats = getStats(file, 'Stmt') ?? { covered: 0, total: 0 };
                        const decisionStats = getStats(file, 'Decision');
                        const mcdcStats = getStats(file, 'MCDC');

                        let branchCoverage: vscode.TestCoverageCount | undefined = undefined;
                        if (decisionStats || mcdcStats) {
                            /**
                             * The VS Code API doesn't have a kind of coverage
                             * dedicated to MC/DC, so we report the stats along
                             * with the decision level coverage.
                             */
                            branchCoverage = {
                                covered: (decisionStats?.covered ?? 0) + (mcdcStats?.covered ?? 0),
                                total: (decisionStats?.total ?? 0) + (mcdcStats?.total ?? 0),
                            };
                        }

                        const fileCov = new GnatcovFileCoverage(
                            fileReportPath,
                            srcUri,
                            stmtStats,
                            branchCoverage,
                        );

                        /**
                         * Do we need a lock to increment this counter given
                         * that we are processing with threads?
                         *
                         * The answer is no.
                         *
                         * In JavaScript semantics each function runs to
                         * completion uninterrupted. 'async' doesn't mean that
                         * functions run concurrently. It means that something
                         * will be executed later.
                         *
                         * When a function flow encounters 'await', it hands
                         * off processing to another async operation.
                         *
                         * So at any one time, only one function is executing
                         * and accessing the counter. It is safe to increment
                         * without locking.
                         */
                        ++done;

                        /**
                         * Reporting progress too often can cause the UI to freeze.
                         * Instead we use staggerProgress to report progress only
                         * when the percentage has increased by 1%.
                         */
                        lastProgress = staggerProgress(
                            done,
                            totalFiles,
                            lastProgress,
                            (increment) => {
                                progress.report({
                                    message: `${done} / ${totalFiles} source files`,
                                    increment: increment,
                                });
                            },
                        );

                        return fileCov;
                    },
                    token,
                )
            ).filter((v) => !!v);
            fileCovs.map((fileCov) => {
                run.addCoverage(fileCov);
            });
            await Promise.all(fileCovs);
        },
    );
}
/**
 * A class that holds the summary coverage data of a source file, and provides
 * a method to load detailed coverage data about that source file.
 */
export class GnatcovFileCoverage extends vscode.FileCoverage {
    sourceFileXmlReport: string;

    /**
     *
     * @param detailedXmlReportPath - The path to the GNATcov XML report for the given source file.
     * @param uri - URI of the source file
     * @param statementCoverage - statement coverage information
     * @param branchCoverage - branch coverage information, if available
     * @param declarationCoverage - declaration coverage information, if available
     */
    constructor(
        detailedXmlReportPath: string,
        uri: vscode.Uri,
        statementCoverage: vscode.TestCoverageCount,
        branchCoverage?: vscode.TestCoverageCount,
        declarationCoverage?: vscode.TestCoverageCount,
    ) {
        super(uri, statementCoverage, branchCoverage, declarationCoverage);
        this.sourceFileXmlReport = detailedXmlReportPath;
    }

    /**
     * Report detailed coverage information by loading the file's GNATcov XML report.
     */
    public async load(token?: CancellationToken): Promise<vscode.FileCoverageDetail[]> {
        const data = parseGnatcovFileXml(this.sourceFileXmlReport);
        return Promise.resolve(convertSourceReport(data, token));
    }
}

/**
 *
 * @param file - a `file_type` object from a GNATcoverage XML report
 * @param level - the coverage level as it appears in the XML report
 * @returns the coverage summary as a {@link vscode.TestCoverageCount} object
 * if the requested coverage level exists in the report, otherwise `undefined`.
 */
function getStats(
    file: file_type,
    level: 'Stmt' | 'Decision' | 'MCDC',
): vscode.TestCoverageCount | undefined {
    const stats = file.obligation_stats?.find((s) => s['@_kind'] == level);
    if (stats) {
        const total =
            stats?.metric?.find((m) => m['@_kind'] == 'total_obligations_of_relevance')?.[
                '@_count'
            ] ?? 0;
        const covered =
            stats?.metric?.find((m) => m['@_kind'] == 'fully_covered')?.['@_count'] ?? 0;
        return { covered, total };
    } else {
        return undefined;
    }
}

export function convertSourceReport(
    data: source_type,
    token?: CancellationToken,
): vscode.StatementCoverage[] {
    return (
        data.src_mapping
            ?.flatMap((src_mapping) => {
                if (token?.isCancellationRequested) {
                    throw new vscode.CancellationError();
                }

                return convertSrcMapping(src_mapping);
            })
            .flat()
            .filter((v) => !!v) ?? []
    );
}

export function convertSrcMapping(src_mapping: src_mapping_type): vscode.StatementCoverage[] {
    /**
     * Create one StatementCoverage per statement in the report
     */
    const statementsCov =
        src_mapping.statement
            // Skip statements with no coverage obligation
            ?.filter((s) => s['@_coverage'] != '.')
            ?.map((stmt) => {
                const src = stmt.src;
                assert(src);
                const range = toRange(src);
                return new vscode.StatementCoverage(stmt['@_coverage'] == '+', range);
            }) ?? [];

    /**
     * Create BranchCoverage objects for reported decisions
     */
    for (const decision of src_mapping.decision
        // Skip decisions with no coverage obligation
        ?.filter((d) => d['@_coverage'] != '.') ?? []) {
        assert(decision.src);
        const range = toRange(decision.src);
        /**
         * Find the statement coverage object that encompasses this decision
         */
        const stmt =
            statementsCov.find((s) => (s.location as vscode.Range).contains(range)) ??
            // Fallback to the first statement of the <src_mapping>
            statementsCov[0];

        if (stmt) {
            type Branches = {
                true: vscode.BranchCoverage;
                false: vscode.BranchCoverage;
            };
            const branches: Branches = {
                true: new vscode.BranchCoverage(false, range, 'True'),
                false: new vscode.BranchCoverage(false, range, 'False'),
            };

            switch (decision['@_coverage']) {
                case '+':
                    // All coverage obligations are satisfied, mark both branches as covered
                    branches.true.executed = branches.false.executed = true;
                    break;
                case '!':
                    // Some of the coverage obligations are satisfied,
                    // mark one of the branches as covered
                    branches.true.executed = true;
                    break;

                default:
                    // Leave both branches uncovered by default
                    break;
            }

            /**
             * Determine which branch got covered based on reported messages
             */
            // Find message with the decision ID
            const relevantMsgs = getMessages(decision, src_mapping);
            for (const br in branches) {
                const outcomeNeverExercised = !!relevantMsgs.find((m) =>
                    m['@_message'].includes(`outcome ${br.toLocaleUpperCase()} never exercised`),
                );
                branches[br as keyof Branches].executed = !outcomeNeverExercised;
            }

            stmt.branches = [
                /**
                 * The order is relevant to the reporting in the UI.
                 * The last object of the list determines the color of
                 * the decision highlight.
                 *
                 * - If the last item is covered, the decision is
                 * highlighted as green and a small orange square
                 * appears in case of other uncovered branches.
                 *
                 * - If the last item is uncovered, the decision is
                 * highlighted in red.
                 *
                 * We choose to report the True branch last so that the
                 * highlighting of the decision matches the status of
                 * the True branch which is the closest syntactically.
                 * Missing coverage of the farther False branch will
                 * appear as the orange square.
                 */
                branches.false,
                branches.true,
            ];

            /**
             * Add <condition> reports as more branches
             */
            const conditions: vscode.BranchCoverage[] = (decision.condition ?? [])
                .filter((c) => c['@_coverage'] != '.')
                .flatMap((condition) => {
                    assert(condition.src);
                    const mergedText = mergeText(condition.src);
                    const messages = getMessages(condition, src_mapping);
                    if (messages.length > 0) {
                        /**
                         * Return one BranchCoverage object per message.
                         */
                        return messages.map(
                            (m) =>
                                new vscode.BranchCoverage(
                                    m['@_kind'] == 'notice',
                                    condition.src ? toRange(condition.src) : undefined,

                                    `condition ${m['@_kind']}: '${mergedText}' ${m['@_message']}`,
                                ),
                        );
                    } else {
                        /**
                         * If there are no messages, create one
                         * BranchCoverage object to report if
                         * everything is covered or not.
                         */

                        const label = `condition '${mergedText}' independent influence pair`;
                        return new vscode.BranchCoverage(
                            condition['@_coverage'] == '+',
                            toRange(condition.src),
                            label,
                        );
                    }
                });

            stmt.branches.push(...conditions);
        }
    }

    return statementsCov;
}

/**
 *
 * @param item - an XML object with an 'id' attribute
 * @param src_mapping - a `<src_mapping>` potentially holding messages
 * @returns the messages of the `<src_mapping>` pertaining to the given id.
 */
function getMessages(item: { '@_id': number }, src_mapping: src_mapping_type) {
    const scoPrefix = `SCO #${item['@_id']}`;
    const messages = src_mapping.message?.filter((m) => m['@_SCO'].startsWith(scoPrefix)) ?? [];
    return messages;
}

/**
 *
 * @param src - a `<src>` object from a GNATcoverage XML report
 * @returns the joined lines spanned by the src object
 */
function mergeText(src: src_type | undefined): string {
    return src?.line?.map((l) => l['@_src'].trim()).join(' ') ?? '';
}

/**
 *
 * @param src - an `src_type` from a GNATcoverage XML report
 * @returns a {@link vscode.Range} that spans the corresponding region
 */
function toRange(src: src_type): vscode.Range {
    /**
     * The <src> object may contain multiple <line>s, so we need to compute the
     * region start and end based on the first and last lines.
     */
    const firstLine = src.line?.at(0);
    assert(firstLine);
    const lastLine = src.line?.at(-1);
    assert(lastLine);
    const range = new vscode.Range(
        firstLine['@_num'] - 1,
        firstLine['@_column_begin'] - 1,
        lastLine['@_num'] - 1,
        lastLine['@_column_end'],
    );
    return range;
}
