import { VisualizerHandler } from '../alsVisualizerProvider';
import * as vscode from 'vscode';

/**
 * Specialization of the Generic Visualizer Handler for JavaScript and TypeScript.
 */
export class JsTsVisualizerHandler extends VisualizerHandler {
    isWithinWorkspace(uri: vscode.Uri) {
        return !uri.fsPath.includes('node_modules') && super.isWithinWorkspace(uri);
    }
}
