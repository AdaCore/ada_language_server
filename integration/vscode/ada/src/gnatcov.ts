import assert from 'assert';
import { X2jOptions, XMLParser } from 'fast-xml-parser';
import * as fs from 'fs';
import { cpus } from 'os';
import * as path from 'path';
import * as vscode from 'vscode';
import { CancellationToken } from 'vscode-languageclient';
import { getMatchingPrefixes, parallelize, staggerProgress, toPosix } from './helpers';

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
    source?: source_type[];
    'xi:include': xi_include_type[];
};

type xi_include_type = {
    '@_parse': string;
    '@_href': string;
};

export type source_type = {
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

    /**
     *  should be 'number' but we don't want to ask the XML parser to parse
     *  numbers
     */
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
export type src_mapping_type = {
    src: src_type;
    statement: statement_type[];
    decision: decision_type[];
    message: message_type[];
    '@_coverage': coverage_type;
};
type src_type = {
    line: line_type[];
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
        trimValues: false,
        attributeValueProcessor: attributeValueProcessor,
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
            return (['source.src_mapping.src.line'] as string[]).indexOf(jPath) !== -1;
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
            const array = data.coverage_report.coverage_summary!.file;
            let done: number = 0;
            let lastProgress = 0;
            const totalFiles = array.length;
            progress.report({
                message: `${done} / ${totalFiles} source files`,
            });

            let posixForeignPrefix: string | undefined;
            let posixLocalPrefix: string | undefined;

            const procs = process.env.PROCESSORS ? Number(process.env.PROCESSORS) : 0;
            const fileCovs = (
                await parallelize(
                    array,
                    Math.min(procs == 0 ? cpus().length : procs, 8),
                    async (file) => {
                        if (token.isCancellationRequested) {
                            throw new vscode.CancellationError();
                        }

                        assert(file['@_name']);
                        const foreignPath = file['@_name'];
                        /**
                         * The foreign machine may have a different path
                         * format, so we normalize to POSIX which is valid on
                         * both Windows and POSIX OS-es.
                         */
                        const posixForeignPath = toPosix(foreignPath);

                        let srcUri: vscode.Uri | undefined = undefined;

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
                                    localFullPath = path.join(
                                        posixLocalPrefix,
                                        posixForeignRelPath,
                                    );
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
                             * `gnatcov-instr`. Ideally it would have been nice
                             * to exclude precisely `<obj-dir>/gnatcov-instr`
                             * but that would need to be repeated for each obj
                             * dir of each project in the closure. As we don't
                             * have access to that information, we ignore all
                             * paths containing a `gnatcov-instr` component.
                             *
                             * Note that a previous version excluded the entire
                             * object dir which did not work well on projects
                             * that use '.' as the object dir. In that case
                             * excluding the object dir would exclude the
                             * entire workspace and prevent finding any files.
                             */
                            const exclude = `**/gnatcov-instr/**/*`;

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

                        const total = file.metric.find(
                            (m) => m['@_kind'] == 'total_lines_of_relevance',
                        )!['@_count'];
                        const covered = file.metric.find((m) => m['@_kind'] == 'fully_covered')![
                            '@_count'
                        ];

                        const fileReportBasename = data.coverage_report.sources!['xi:include'].find(
                            (inc) =>
                                inc['@_href'] == `${path.posix.basename(posixForeignPath)}.xml`,
                        )!['@_href'];
                        const fileReportPath = path.join(covDir, fileReportBasename);

                        if (covered > total) {
                            throw Error(
                                `Got ${covered} covered lines for a` +
                                    ` total of ${total} in ${file['@_name']}`,
                            );
                        }

                        const fileCov = new GnatcovFileCoverage(fileReportPath, srcUri, {
                            covered,
                            total,
                        });

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

export function convertSourceReport(
    data: source_type,
    token?: CancellationToken,
): vscode.StatementCoverage[] {
    return data.src_mapping.flatMap((src_mapping) => convertSrcMapping(src_mapping, token));
}

export function convertSrcMapping(
    src_mapping: src_mapping_type,
    token?: CancellationToken,
): vscode.StatementCoverage[] {
    return src_mapping.src.line
        .map((line) => {
            if (token?.isCancellationRequested) {
                throw new vscode.CancellationError();
            }

            const zeroBasedLine = line['@_num'] - 1;
            const lineLength = line['@_src'].length;
            const res = new vscode.StatementCoverage(
                false,
                new vscode.Range(zeroBasedLine, 0, zeroBasedLine, lineLength),
            );

            switch (src_mapping['@_coverage']) {
                case '-':
                case '!':
                case '?':
                case '+':
                    /**
                     * Lines with coverage obligations are highlighted
                     * as covered iff they are marked as '+'
                     */
                    res.executed = src_mapping['@_coverage'] == '+';
                    return res;

                case '.':
                    /**
                     * Lines with no coverage obligations are not highlighted
                     */
                    return undefined;

                case '#':
                case '@':
                case '*':
                    /**
                     * Exempted lines are not highlighted
                     */
                    return undefined;

                case 'v':
                case '>':
                    /**
                     * Symbols are for object level coverage which
                     * is not supported in VS Code.
                     */
                    return undefined;

                case '0':
                    /**
                     * Symbol specific to binary traces which are
                     * not used in the VS Code integration.
                     */
                    return undefined;
            }
        })
        .filter((v) => !!v);
}
