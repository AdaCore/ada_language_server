/*----------------------------------------------------------------------------
--         Creating and deleting GNATcoverage external annotations          --
----------------------------------------------------------------------------*/

import * as vscode from 'vscode';
import { CMD_EDIT_PROJECT_FILE } from './constants';
import {
    AnnotationKind,
    AnnotationLocation,
    CreateAnnotationParams,
    ExternalAnnotation,
    addAnnotation,
    creatableKinds,
    deleteAnnotation,
    hasInsertionSide,
    showAnnotations,
    labelOf,
    requiresJustification,
    shapeOf,
    toGnatcovLocation,
} from './extAnnotations';

/**
 * One-line explanation of each kind, shown next to it in the picker. Users are
 * not expected to remember what twelve annotation kinds do.
 */
function descriptionOf(kind: AnnotationKind): string {
    switch (kind) {
        case 'Exempt_Region':
            return 'Exempt the selected region from coverage violations';
        case 'Exempt_On':
            return 'Start an exempted region at the cursor';
        case 'Exempt_Off':
            return 'End the exempted region started by an Exempt_On';
        case 'Exempt_Branch':
            return 'Exempt the branch at the cursor';
        case 'Cov_Off':
            return 'Stop analysing coverage from the cursor onwards';
        case 'Cov_On':
            return 'Resume analysing coverage from the cursor onwards';
        case 'Dump_Buffers':
            return 'Dump the coverage buffers at the cursor';
        case 'Reset_Buffers':
            return 'Reset the coverage buffers at the cursor';
        default:
            return '';
    }
}

/**
 * Ask the user for a kind, a justification if the kind needs one, and any
 * kind-specific extras, then create the annotation.
 *
 * Returns silently if the user dismisses any of the prompts.
 */
export async function createAnnotationCommand(): Promise<void> {
    const editor = vscode.window.activeTextEditor;

    if (editor === undefined) {
        void vscode.window.showErrorMessage('No active editor.');
        return;
    }

    if ((await showAnnotations(editor.document.uri.fsPath)).notConfigured === true) {
        const edit = 'Edit Project File';
        const answer = await vscode.window.showErrorMessage(
            'No project designates an external annotation file. Add an ' +
                'External_Annotations attribute to the Coverage package of ' +
                'the project owning the units you want to annotate.',
            edit,
        );
        if (answer === edit) {
            await vscode.commands.executeCommand(CMD_EDIT_PROJECT_FILE);
        }
        return;
    }

    const kind = await pickKind();
    if (kind === undefined) {
        return;
    }

    /*
     * A region kind needs a selection; a point kind only needs the cursor, and
     * an empty selection is exactly that.
     */
    if (shapeOf(kind) === 'region' && editor.selection.isEmpty) {
        void vscode.window.showErrorMessage(
            `${labelOf(kind)} applies to a region: select the code to annotate first.`,
        );
        return;
    }

    const params = await collectParams(kind, locationFor(kind, editor));
    if (params === undefined) {
        return;
    }

    const failure = await vscode.window.withProgress(
        { location: vscode.ProgressLocation.Window, title: 'Adding annotation...' },
        () => addAnnotation(params, editor.document.uri.fsPath),
    );

    if (failure !== undefined) {
        void vscode.window.showErrorMessage(`Could not add the annotation: ${failure}`);
        return;
    }

    /*
     * The annotation file has changed on disk, which the file watcher picks up
     * to refresh both the decorations and the tree view. Nothing to do here.
     */
    void vscode.window.setStatusBarMessage(`Added ${labelOf(kind)} annotation`, 3000);
}

async function pickKind(): Promise<AnnotationKind | undefined> {
    /*
     * The extra property is named annotationKind rather than kind, since
     * vscode.QuickPickItem already declares an unrelated `kind` of its own.
     */
    type KindItem = vscode.QuickPickItem & { annotationKind: AnnotationKind };

    const items: KindItem[] = creatableKinds.map((kind) => ({
        annotationKind: kind,
        label: labelOf(kind),
        description: kind,
        detail: descriptionOf(kind),
    }));

    const picked = await vscode.window.showQuickPick(items, {
        title: 'Create a GNATcoverage external annotation',
        placeHolder: 'Annotation kind',
        matchOnDescription: true,
        matchOnDetail: true,
    });

    return picked?.annotationKind;
}

/**
 * Narrow a selection that ends at the start of a later line back to the end of
 * the previous one.
 *
 * Selecting whole lines leaves the exclusive end at column 0 of the following
 * line, which as an inclusive end would cover text the user did not select.
 */
function regionSpan(document: vscode.TextDocument, selection: vscode.Selection): vscode.Range {
    const end = selection.end;

    if (end.character !== 0 || end.line <= selection.start.line) {
        return selection;
    }

    const previous = document.lineAt(end.line - 1);
    return new vscode.Range(selection.start, previous.range.end);
}

/**
 * Determine the location to record for a new annotation.
 *
 * The selection is recorded as the user made it.
 *
 * gnatcov takes it from there. It looks for the statement list enclosing the
 * location, and warns and ignores the annotation if it finds none. Correcting
 * the cursor here would only let the IDE and gnatcov disagree. It would also
 * undo the documented way to annotate a statement: target the whitespace
 * before it.
 */
function locationFor(kind: AnnotationKind, editor: vscode.TextEditor): AnnotationLocation {
    if (shapeOf(kind) === 'region') {
        return toGnatcovLocation(regionSpan(editor.document, editor.selection));
    }

    const position = editor.selection.start;
    return toGnatcovLocation({ start: position, end: position });
}

/**
 * Prompt for the fields the chosen kind needs.
 */
async function collectParams(
    kind: AnnotationKind,
    location: AnnotationLocation,
): Promise<CreateAnnotationParams | undefined> {
    const params: CreateAnnotationParams = {
        kind: kind,
        location: location,
    };

    if (requiresJustification(kind)) {
        const justification = await vscode.window.showInputBox({
            title: `Justification for the ${labelOf(kind)} annotation`,
            prompt: 'Why is this code exempted? This ends up in the coverage report.',
            /*
             * gnatcov rejects an empty justification for the kinds that require
             * one, so validate before spawning it rather than surfacing its
             * error afterwards.
             */
            validateInput: (value) =>
                value.trim().length === 0 ? 'A justification is required.' : undefined,
        });

        if (justification === undefined) {
            return undefined;
        }
        params.justification = justification;
    }

    if (hasInsertionSide(kind)) {
        type SideItem = vscode.QuickPickItem & { insertAfter: boolean };

        const side = await vscode.window.showQuickPick<SideItem>(
            [
                {
                    label: 'Before the statement',
                    detail: "gnatcov's default",
                    insertAfter: false,
                },
                {
                    label: 'After the statement',
                    detail: 'The only way to reach the end of a statement list',
                    insertAfter: true,
                },
            ],
            {
                title: `Where should the ${labelOf(kind)} call go?`,
                placeHolder: 'Side of the designated statement',
            },
        );

        if (side === undefined) {
            return undefined;
        }
        params.insertAfter = side.insertAfter;
    }

    if (kind === 'Dump_Buffers') {
        /*
         * Optional: gnatcov only honours a trace prefix when dumping to a file,
         * so an empty answer simply omits the switch.
         */
        const prefix = await vscode.window.showInputBox({
            title: 'Trace file name prefix (optional)',
            prompt: 'Leave empty to use the default prefix.',
        });

        if (prefix === undefined) {
            return undefined;
        }
        if (prefix.trim().length > 0) {
            params.tracePrefix = prefix.trim();
        }
    }

    return params;
}

/**
 * Delete an annotation, after asking for confirmation.
 *
 * Deleting rewrites the shared annotation file, and the identifier means
 * nothing to the user on its own, so the prompt spells out what is being
 * removed.
 */
export async function deleteAnnotationCommand(annotation: ExternalAnnotation): Promise<void> {
    const remove = 'Delete';
    const answer = await vscode.window.showWarningMessage(
        `Delete the ${labelOf(annotation.kind)} annotation '${annotation.id}'?`,
        { modal: true, detail: annotation.justification },
        remove,
    );

    if (answer !== remove) {
        return;
    }

    const failure = await vscode.window.withProgress(
        { location: vscode.ProgressLocation.Window, title: 'Deleting annotation...' },
        () => deleteAnnotation(annotation.id),
    );

    if (failure !== undefined) {
        void vscode.window.showErrorMessage(`Could not delete the annotation: ${failure}`);
        return;
    }

    void vscode.window.setStatusBarMessage(`Deleted annotation '${annotation.id}'`, 3000);
}
