import * as fs from 'fs';
import * as path from 'path';
import * as vscode from 'vscode';
import { XMLParser } from 'fast-xml-parser';
import { logger } from './extension';

/**
 * Dictionary of metrics for a unit, with optional name/sloc and a metrics object.
 */
export interface UnitMetricsDict {
    name?: string;
    sloc?: vscode.Position;
    metrics: { [key: string]: number };
}

/**
 * Dictionary of metric display names, as found in the config node.
 * Example: \{ code_lines: "code lines", ... \}
 */
export type MetricDisplayNames = { [key: string]: string };

/**
 * Get the thresholds for metrics from VS Code settings.
 * If no user-defined thresholds are set, it returns an empty object,
 * meaning no thresholds will be applied.
 */
export function getMetricsThresholds(): Record<string, { warn?: number; error?: number }> {
    const config = vscode.workspace.getConfiguration('ada');
    const userThresholds =
        config.get<Record<string, { warn?: number; error?: number }>>('metricThresholds');
    // Merge user settings with defaults (user overrides default)
    return userThresholds || {};
}

/**
 * Formats a metric value with an appropriate label and color based on predefined thresholds.
 * The function checks if the given metric key has defined thresholds for warning and error levels.
 * If the value exceeds the error threshold, it is prefixed with a red circle emoji; if it exceeds
 * the warning threshold, it is prefixed with an orange circle emoji. If no thresholds are defined
 * for the metric key, it simply returns the label and value without any emoji.
 * @param key - The key of the metric to be formatted (e.g., "cyclomatic_complexity").
 * @param label - The display label for the metric (e.g., "Cyclomatic Complexity").
 * @param value - The numeric value of the metric to be formatted.
 * @returns A formatted string representing the metric, potentially with an emoji
 * indicating its severity.
 */
export function formatMetric(key: string, label: string, value: number): string {
    const thresholds = getMetricsThresholds();
    const t = thresholds[key];
    if (!t) return `${label}: ${value}`;

    if (t.error !== undefined && value >= t.error) {
        return `🔴 ${label}: ${value}`;
    }
    if (t.warn !== undefined && value >= t.warn) {
        return `🟠 ${label}: ${value}`;
    }
    return `${label}: ${value}`;
}

/**
 * Creates a VS Code Diagnostic for a metric threshold violation.
 * Returns undefined if no threshold is violated.
 *
 * @param metricName - The name of the metric (e.g., "cyclomatic_complexity").
 * @param value - The numeric value of the metric to be checked against thresholds.
 * @param sloc - The source location (line and column) where the metric is defined.
 * @param displayNames - A dictionary mapping metric names to their display labels.
 * @returns A vscode.Diagnostic object if the metric value exceeds the
 * configured threshold, otherwise undefined.
 */
export function createMetricDiagnostic(
    metricName: string,
    value: number,
    sloc: vscode.Position,
    displayNames: Record<string, string>,
): vscode.Diagnostic | undefined {
    const metricsThresholds = getMetricsThresholds();

    function makeDiagnostic(
        threshold: number | undefined,
        thresholdType: 'error' | 'warning',
        severity: vscode.DiagnosticSeverity,
    ): vscode.Diagnostic | undefined {
        // Check if the metric exceeds the specified threshold and
        // create a diagnostic if it does
        if (metricName in metricsThresholds && threshold !== undefined && value > threshold) {
            const diagnostic = new vscode.Diagnostic(
                new vscode.Range(sloc, sloc),
                `${displayNames[metricName] ?? metricName} metric value (${value}) exceeds` +
                    ` the configured ${thresholdType} threshold (${threshold})`,
                severity,
            );
            diagnostic.source = metricName;
            return diagnostic;
        }

        // If the metric does not exceed the threshold, return undefined
        return undefined;
    }

    // First check for error threshold violation, then for warning violation: we
    // don't want to create a warning diagnostic if the value already
    // exceeds the error threshold
    return (
        makeDiagnostic(
            metricsThresholds[metricName]?.error,
            'error',
            vscode.DiagnosticSeverity.Error,
        ) ||
        makeDiagnostic(
            metricsThresholds[metricName]?.warn,
            'warning',
            vscode.DiagnosticSeverity.Warning,
        )
    );
}

/**
 * Finds the metrics XML file corresponding to a given source file. The
 * function assumes that the metrics XML file is named after the source file (without
 * extension) and is located in the specified object directory.
 * @param sourcePath - The path to the source file.
 * @param objectDir -  The directory where the metrics XML files are located.
 * @returns The path to the metrics XML file if it exists, otherwise undefined.
 */
export function findMetricsXmlForSource(sourcePath: string, objectDir: string): string | undefined {
    const baseName = path.basename(sourcePath, path.extname(sourcePath));
    const metricsFile = path.join(objectDir, `${baseName}.metrics.xml`);
    if (fs.existsSync(metricsFile)) {
        return metricsFile;
    }
    return undefined;
}

function isObject(val: unknown): val is Record<string, unknown> {
    return typeof val === 'object' && val !== null;
}

/**
 * Extracts the relevant metrics from a given node in the parsed XML structure.
 * The function handles both cases where metrics are nested within a "metric" property
 * or directly available as properties of the node.
 * @param node - The node from which to extract metrics.
 * @returns An object containing the extracted metrics.
 */
function extractMetricsFromNode(node: unknown): UnitMetricsDict {
    const result: UnitMetricsDict = { metrics: {} };
    if (!isObject(node)) return result;

    // Fetch the metric name
    if (typeof node['@_name'] === 'string') {
        result.name = node['@_name'];
    }

    // Fetch the source location (line and column)
    // VS Code uses 0-based indexing for lines and columns,
    // while the metrics XML uses 1-based indexing.
    if (typeof node['@_line'] === 'string' && typeof node['@_col'] === 'string') {
        result.sloc = new vscode.Position(
            parseInt(node['@_line']) - 1,
            parseInt(node['@_col']) - 1,
        );
    }

    // Collect all metric nodes (array or object)
    let metricsList: unknown[] = [];
    if ('metric' in node) {
        const metricProp = node['metric'];
        if (Array.isArray(metricProp)) {
            metricsList = metricsList.concat(metricProp);
        } else if (isObject(metricProp)) {
            metricsList.push(metricProp);
        }
    }

    // Parse all metrics
    for (const metric of metricsList) {
        if (isObject(metric)) {
            const name = metric['@_name'];
            let value = metric['#text'];
            if (typeof name === 'string') {
                if (typeof value === 'string') value = parseFloat(value);
                if (typeof value === 'number' && !isNaN(value)) {
                    result.metrics[name] = value;
                }
            }
        }
    }

    // Fallback: sometimes metrics are direct properties (e.g., metric_name: value)
    for (const key of Object.keys(node)) {
        if (key.startsWith('@_')) continue;
        if (typeof node[key] === 'string' || typeof node[key] === 'number') {
            let value: number | undefined;
            if (typeof node[key] === 'string') {
                value = parseFloat(node[key]);
            } else {
                value = node[key];
            }
            if (!isNaN(Number(value))) {
                result.metrics[key] = value;
            }
        }
    }

    return result;
}

/**
 * Recursively collects metrics for all units within the given node. The function
 * traverses the XML structure, looking for nodes that contain metrics and extracting
 * them using the `extractMetricsFromNode` function.
 * It handles both cases where units are nested within a "unit" property or
 * directly available as properties of the node.
 * @param node - The node from which to start collecting metrics.
 * @param acc - An accumulator array to store the collected metrics.
 * @returns An array of collected metrics for all units found in the XML structure.
 */
function collectUnitMetrics(node: unknown, acc: UnitMetricsDict[] = []): UnitMetricsDict[] {
    if (!isObject(node)) return acc;

    // Check if the current node has metrics and extract them if present
    if (
        'metric' in node &&
        typeof node['@_line'] === 'string' &&
        typeof node['@_col'] === 'string'
    ) {
        acc.push(extractMetricsFromNode(node));
    }

    // Recursively collect metrics from child units (if any)
    if ('unit' in node) {
        const units = Array.isArray(node['unit']) ? node['unit'] : [node['unit']];
        for (const unit of units) {
            collectUnitMetrics(unit, acc);
        }
    }

    return acc;
}

/**
 * Parses the metrics XML file at the given path and extracts the relevant metrics for all units.
 * The function reads the XML file, parses it using the `fast-xml-parser` library, and then
 * traverses the resulting object structure to collect metrics for all units (files, procedures,
 * functions) defined in the XML. It returns an array of `UnitMetrics` objects containing the
 * extracted metrics, or undefined if the XML structure is not as expected or if any error occurs
 * during parsing.
 * @param metricsXmlPath - The path to the metrics XML file to be parsed.
 * @returns A promise that resolves to an array of `UnitMetrics` objects or undefined.
 */
/**
 * Parses the metrics XML file and returns:
 *   - an array of UnitMetricsDict for all units
 *   - a dictionary of metric display names from the config node
 */
export async function parseMetricsXml(
    metricsXmlPath: string,
): Promise<{ units: UnitMetricsDict[]; displayNames: MetricDisplayNames } | undefined> {
    try {
        const xmlUri = vscode.Uri.file(metricsXmlPath);
        const fileContent = await vscode.workspace.fs.readFile(xmlUri);
        const fileContentAsBuffer = Buffer.from(fileContent);
        const parser = new XMLParser({
            ignoreAttributes: false,
            attributeNamePrefix: '@_',
        });
        const result = parser.parse(fileContentAsBuffer.toString()) as unknown;

        if (isObject(result) && 'global' in result && isObject(result.global)) {
            const globalObj = result.global;
            // Parse display names from config
            const displayNames: MetricDisplayNames = {};
            if (
                'config' in globalObj &&
                isObject(globalObj.config) &&
                'metric' in globalObj.config
            ) {
                const configMetrics = globalObj.config.metric;
                const configList = Array.isArray(configMetrics) ? configMetrics : [configMetrics];
                for (const m of configList) {
                    if (
                        isObject(m) &&
                        typeof m['@_name'] === 'string' &&
                        typeof m['@_display_name'] === 'string'
                    ) {
                        displayNames[m['@_name']] = m['@_display_name'];
                    }
                }
            }

            // Parse units
            if ('file' in globalObj) {
                const fileNode = globalObj.file;
                const files = Array.isArray(fileNode) ? fileNode : [fileNode];
                const units: UnitMetricsDict[] = [];
                for (const file of files) {
                    if (
                        isObject(file) &&
                        'metric' in file &&
                        typeof file['@_line'] === 'string' &&
                        typeof file['@_col'] === 'string'
                    ) {
                        units.push(extractMetricsFromNode(file));
                    }
                    if (isObject(file) && 'unit' in file) {
                        collectUnitMetrics(file.unit, units);
                    }
                }
                return { units, displayNames };
            }
            return { units: [], displayNames };
        }

        logger.error(`Unexpected metrics XML structure in file ${metricsXmlPath}`);
        return undefined;
    } catch (error) {
        logger.error(`Failed to parse metrics XML at ${metricsXmlPath}: ${String(error)}`);
        return undefined;
    }
}
