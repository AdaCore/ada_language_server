/*----------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                        Copyright (C) 2026, AdaCore                       --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
----------------------------------------------------------------------------*/

import * as vscode from 'vscode';
import { CMD_SCENARIO_VIEW_SET_VARIABLE } from './constants';

// ---------------------------------------------------------------------------
// Raw wire types – match the JSON field names returned by the server
// ---------------------------------------------------------------------------

interface Raw_ScenarioVariable {
    name: string;
    typed: boolean;
    conflicting: boolean;
    'possible-values'?: string[];
    value?: string;
}

export interface Raw_ScenarioVariablesResponse {
    variables: Raw_ScenarioVariable[];
}

// ---------------------------------------------------------------------------
// Parsed types – clean field names, arrays always present
// ---------------------------------------------------------------------------

/**
 * Metadata for a single scenario (external) variable, as reported by the
 * als-scenario-variables-information command.
 */
export interface ScenarioVariableInfo {
    name: string;
    typed: boolean;
    /** A conflict means that the same external variable is declared with
     * different types across the project tree; GPR2 cannot give us a
     * single authoritative type
     */
    conflicting: boolean;
    /** Legal values for a typed variable; empty for an untyped one. */
    possibleValues: string[];
    /**
     * The value GPR2 actually resolved this variable to, whichever of
     * ada.scenarioVariables, the .als.json file, or the OS environment it
     * came from, or `undefined` if none of them currently set it.
     */
    value?: string;
}

export function parseScenarioVariablesResponse(
    raw: Raw_ScenarioVariablesResponse,
): ScenarioVariableInfo[] {
    return (raw.variables ?? [])
        .map((v) => ({
            name: v.name,
            typed: v.typed,
            conflicting: v.conflicting,
            possibleValues: v['possible-values'] ?? [],
            value: v.value,
        }))
        .sort((a, b) => a.name.localeCompare(b.name));
}

/**
 * Label shown as a variable's value when it has no currently resolved
 * value. GPR2 does not currently expose the literal default value text of
 * a scenario (external) variable, so it cannot be displayed here.
 */
export const SCENARIO_VARIABLE_DEFAULT_LABEL = '(default)';

/**
 * A tree item for the Scenario View, representing a single scenario
 * (external) variable.
 */
export class ScenarioViewItem extends vscode.TreeItem {
    constructor(public readonly info: ScenarioVariableInfo) {
        super(info.name, vscode.TreeItemCollapsibleState.None);

        const currentValue = info.value;
        const isSet = currentValue !== undefined;

        this.description = isSet ? currentValue : SCENARIO_VARIABLE_DEFAULT_LABEL;
        this.id = `scenario-var-${info.name}`;

        // A conflict means that the same external variable is declared with
        // different types across the project tree; GPR2 cannot give us a
        // single authoritative type, so we surface it both as a warning icon
        // and as an explanatory tooltip message.
        this.iconPath = info.conflicting
            ? new vscode.ThemeIcon(
                  'warning',
                  new vscode.ThemeColor('problemsWarningIcon.foreground'),
              )
            : new vscode.ThemeIcon(info.typed ? 'symbol-enum' : 'symbol-string');

        this.tooltip = this.buildTooltip(isSet, currentValue);

        // Encodes typed/untyped and set/unset in the context value so that
        // view/item/context and view/item/inline menu `when` clauses can
        // target the right state (e.g. only show "Reset to Default" when a
        // value is currently set).
        this.contextValue = `scenarioVariable${info.typed ? 'Typed' : 'Untyped'}${
            isSet ? 'Set' : 'Unset'
        }`;

        this.command = {
            command: CMD_SCENARIO_VIEW_SET_VARIABLE,
            title: 'Edit Value…',
            arguments: [this],
        };
    }

    private buildTooltip(isSet: boolean, currentValue: string | undefined): vscode.MarkdownString {
        const md = new vscode.MarkdownString();
        md.appendMarkdown(`**Name:** \`${this.info.name}\`\n\n`);

        if (this.info.typed) {
            md.appendMarkdown(
                `**Possible values:** \`${this.info.possibleValues.join(', ')}\`\n\n`,
            );
        } else {
            md.appendMarkdown('**Type:** untyped variable; any value is accepted\n\n');
        }

        md.appendMarkdown(
            isSet
                ? `**Current value:** \`${currentValue ?? ''}\`\n\n`
                : `**Current value:** \`${SCENARIO_VARIABLE_DEFAULT_LABEL}\`` +
                      ` (the project's default is used)\n\n`,
        );

        if (this.info.conflicting) {
            md.appendMarkdown(
                '**Warning:** this variable is declared with conflicting types ' +
                    'across the project tree; different project declarations ' +
                    'disagree on its type.',
            );
        }

        return md;
    }
}

/**
 * Tree data provider for the Scenario View.
 */
export class ScenarioViewProvider implements vscode.TreeDataProvider<ScenarioViewItem> {
    private _onDidChangeTreeData: vscode.EventEmitter<ScenarioViewItem | undefined | null | void> =
        new vscode.EventEmitter<ScenarioViewItem | undefined | null | void>();
    readonly onDidChangeTreeData: vscode.Event<ScenarioViewItem | undefined | null | void> =
        this._onDidChangeTreeData.event;

    private variables: ScenarioVariableInfo[] = [];

    /**
     * Sets the scenario variables information and refreshes the tree.
     * Called by ExtensionState whenever the project is (re-)loaded or the
     * `ada.scenarioVariables` setting changes.
     *
     * @param variables - The newly fetched scenario variables information
     */
    setScenarioVariables(variables: ScenarioVariableInfo[]): void {
        this.variables = variables;
        this._onDidChangeTreeData.fire();
    }

    getTreeItem(element: ScenarioViewItem): vscode.TreeItem {
        return element;
    }

    getChildren(element?: ScenarioViewItem): ScenarioViewItem[] {
        // Flat list: only the root level has children.
        if (element) {
            return [];
        }
        return this.variables.map((v) => new ScenarioViewItem(v));
    }
}
