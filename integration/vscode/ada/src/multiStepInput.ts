/* eslint-disable @typescript-eslint/no-unsafe-return */
/* eslint-disable @typescript-eslint/no-explicit-any */
/* eslint-disable @typescript-eslint/no-unsafe-argument */
/* eslint-disable @typescript-eslint/prefer-promise-reject-errors */
/* eslint-disable @typescript-eslint/only-throw-error */
/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *  Licensed under the MIT License. See License.txt in the project root for license information.
 *--------------------------------------------------------------------------------------------*/

/**
 * This file is largely based on the Quick Picker Sample project by Microsoft,
 * with some additions and fixes by AdaCore.
 */

import {
    Disposable,
    QuickInput,
    QuickInputButton,
    QuickInputButtons,
    QuickPickItem,
    window,
} from 'vscode';
import { inTesting } from './helpers';

// -------------------------------------------------------
// Helper code that wraps the API for the multi-step case.
// -------------------------------------------------------

export class InputFlowAction {
    static back = new InputFlowAction();
    static cancel = new InputFlowAction();
    static resume = new InputFlowAction();
}

type InputStep = (input: MultiStepInput) => Thenable<InputStep | void>;

interface QuickPickParameters<T extends QuickPickItem> {
    title: string;
    step: number;
    totalSteps: number;
    items: T[];
    canSelectMany?: boolean;
    activeItems?: T | T[];
    selectedItems?: T | T[];
    ignoreFocusOut?: boolean;
    placeholder: string;
    buttons?: QuickInputButton[];
    shouldResume?: () => Thenable<boolean>;
}

interface InputBoxParameters {
    title: string;
    step: number;
    totalSteps: number;
    value: string;
    prompt: string;
    validate: (value: string) => string | undefined | Promise<string | undefined>;
    buttons?: QuickInputButton[];
    ignoreFocusOut?: boolean;
    placeholder?: string;
    shouldResume?: () => Thenable<boolean>;
}

export class MultiStepInput {
    static async run(start: InputStep) {
        const input = new MultiStepInput();
        return input.stepThrough(start);
    }

    private current?: QuickInput;
    private steps: InputStep[] = [];

    private async stepThrough(start: InputStep) {
        try {
            let step: InputStep | void = start;
            while (step) {
                this.steps.push(step);
                if (this.current) {
                    this.current.enabled = false;
                    this.current.busy = true;
                }
                try {
                    step = await step(this);
                } catch (err) {
                    if (err === InputFlowAction.back) {
                        this.steps.pop();
                        step = this.steps.pop();
                    } else if (err === InputFlowAction.resume) {
                        step = this.steps.pop();
                    } else if (err === InputFlowAction.cancel) {
                        step = undefined;
                        throw err;
                    } else {
                        throw err;
                    }
                }
            }
        } finally {
            if (this.current) {
                this.current.dispose();
            }
        }
    }

    async showQuickPick<T extends QuickPickItem, P extends QuickPickParameters<T>>({
        title,
        step,
        totalSteps,
        items,
        canSelectMany,
        activeItems,
        selectedItems,
        ignoreFocusOut,
        placeholder,
        buttons,
        shouldResume,
    }: P): Promise<
        | (P extends { canSelectMany: boolean } ? T[] : T)
        | (P extends { buttons: (infer I)[] } ? I : never)
    > {
        const disposables: Disposable[] = [];
        try {
            return await new Promise<
                | (P extends { canSelectMany: boolean } ? T[] : T)
                | (P extends { buttons: (infer I)[] } ? I : never)
            >((resolve, reject) => {
                const input = window.createQuickPick<T>();
                input.title = title;
                input.step = step;
                input.totalSteps = totalSteps;
                input.ignoreFocusOut = ignoreFocusOut ?? false;
                input.placeholder = placeholder;
                input.items = items;
                input.canSelectMany = canSelectMany ?? false;
                if (activeItems) {
                    if (activeItems instanceof Array) {
                        input.activeItems = activeItems;
                    } else {
                        input.activeItems = [activeItems];
                    }
                }
                if (selectedItems) {
                    if (selectedItems instanceof Array) {
                        input.selectedItems = selectedItems;
                    } else {
                        input.selectedItems = [selectedItems];
                    }
                }
                input.buttons = [
                    ...(this.steps.length > 1 ? [QuickInputButtons.Back] : []),
                    ...(buttons || []),
                ];
                disposables.push(
                    input.onDidTriggerButton((item) => {
                        if (item === QuickInputButtons.Back) {
                            reject(InputFlowAction.back);
                        } else {
                            resolve(<any>item);
                        }
                    }),
                    input.onDidChangeSelection((items) => {
                        if (!canSelectMany) {
                            resolve(<any>items[0]);
                        }
                    }),
                    input.onDidAccept(() => {
                        if (canSelectMany) {
                            resolve(<any>input.selectedItems.concat());
                        }
                    }),
                    input.onDidHide(() => {
                        (async () => {
                            reject(
                                shouldResume && (await shouldResume())
                                    ? InputFlowAction.resume
                                    : InputFlowAction.cancel,
                            );
                        })().catch(reject);
                    }),
                );
                if (this.current) {
                    this.current.dispose();
                }
                this.current = input;
                this.current.show();

                if (inTesting) {
                    /**
                     * In testing, do not wait for User selection and return
                     * the default selections immediately.
                     */
                    this.current.hide();
                    if (canSelectMany) {
                        resolve(<any>input.selectedItems.concat());
                    } else {
                        resolve(<any>input.activeItems[0]);
                    }
                }
            });
        } finally {
            disposables.forEach((d) => d.dispose());
        }
    }

    async showInputBox<P extends InputBoxParameters>({
        title,
        step,
        totalSteps,
        value,
        prompt,
        validate,
        buttons,
        ignoreFocusOut,
        placeholder,
        shouldResume,
    }: P) {
        const disposables: Disposable[] = [];
        try {
            return await new Promise<string | (P extends { buttons: (infer I)[] } ? I : never)>(
                (resolve, reject) => {
                    const input = window.createInputBox();
                    input.title = title;
                    input.step = step;
                    input.totalSteps = totalSteps;
                    input.value = value || '';
                    input.prompt = prompt;
                    input.ignoreFocusOut = ignoreFocusOut ?? false;
                    input.placeholder = placeholder;
                    input.buttons = [
                        ...(this.steps.length > 1 ? [QuickInputButtons.Back] : []),
                        ...(buttons || []),
                    ];
                    let validating = validate('');
                    disposables.push(
                        input.onDidTriggerButton((item) => {
                            if (item === QuickInputButtons.Back) {
                                reject(InputFlowAction.back);
                            } else {
                                resolve(<any>item);
                            }
                        }),
                        input.onDidAccept(async () => {
                            const value = input.value;
                            input.enabled = false;
                            input.busy = true;
                            if (!(await validate(value))) {
                                resolve(value);
                            }
                            input.enabled = true;
                            input.busy = false;
                        }),
                        input.onDidChangeValue(async (text) => {
                            const current = validate(text);
                            validating = current;
                            const validationMessage = await current;
                            if (current === validating) {
                                input.validationMessage = validationMessage;
                            }
                        }),
                        input.onDidHide(() => {
                            (async () => {
                                reject(
                                    shouldResume && (await shouldResume())
                                        ? InputFlowAction.resume
                                        : InputFlowAction.cancel,
                                );
                            })().catch(reject);
                        }),
                    );
                    if (this.current) {
                        this.current.dispose();
                    }
                    this.current = input;
                    this.current.show();
                },
            );
        } finally {
            disposables.forEach((d) => d.dispose());
        }
    }
}
