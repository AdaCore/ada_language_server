import { CancellationError, QuickPickItem } from 'vscode';
import { adaExtState, logger } from './extension';
import { InputFlowAction, MultiStepInput } from './multiStepInput';
import assert from 'assert';

interface SPARKOption extends QuickPickItem {
    cliArgs: string[];
}

const proofLevels: SPARKOption[] = [
    { label: '0', description: 'Fast, one prover (default)' },
    { label: '1', description: 'Fast, most provers' },
    { label: '2', description: 'Most provers' },
    { label: '3', description: 'Slower, most provers' },
    { label: '4', description: 'Slowest, most provers' },
].map((v) => ({ ...v, cliArgs: [`--level=${v.label}`] }));

const tmpDefaultProofLevel = proofLevels.find((v) => v.description?.includes('default'));
assert(tmpDefaultProofLevel, 'defaultProofLevel should not be null');
const defaultProofLevel: SPARKOption = tmpDefaultProofLevel;

const options: SPARKOption[] = [
    { label: 'Multiprocessing', cliArgs: ['-j0'] },
    { label: 'Do not report warnings', cliArgs: ['--warnings=off'] },
    { label: 'Report checks proved', cliArgs: ['--report=all'] },
    { label: 'Output info messages', cliArgs: ['--info'] },
    { label: 'Enable proof warnings', cliArgs: ['--proof-warnings=on'] },
] as const;

interface PickerState {
    proofLevel: SPARKOption;
    options: SPARKOption[];
}

/**
 * These are the applicable options when no prior selection has been made.
 */
const defaultPickerState: PickerState = {
    proofLevel: defaultProofLevel,
    options: [
        options[0], // Multiprocessing
    ],
};

interface SavedPickerState {
    proofLevelLabel: string;
    optionLabels: string[];
}

/**
 * Key for storing picker state in the workspace state map.
 */
const WS_STATE_KEY_PICKER = 'ada.spark.lastPickerState';

/**
 * Prompts the user to select GNATprove options through a multi-step input dialog.
 *
 * This function presents a two-step selection process:
 * 1. First step: Choose a proof level from available options
 * 2. Second step: Select additional SPARK options (multiple selection allowed)
 *
 * The function preserves user selections in workspace state for future use and
 * restores previously saved selections as defaults when the dialog is opened again.
 *
 * @returns A promise that resolves to an array of CLI arguments representing the selected options
 * @throws - {@link CancellationError} When the user cancels the selection process
 *
 * @example
 * ```typescript
 * try {
 *   const sparkArgs = await askSPARKOptions();
 *   console.log('Selected SPARK options:', sparkArgs);
 * } catch (error) {
 *   if (error instanceof CancellationError) {
 *     console.log('User cancelled the selection');
 *   }
 * }
 * ```
 */
export async function askSPARKOptions(): Promise<string[]> {
    const title = 'Select GNATprove Options';
    async function pickProofLevel(input: MultiStepInput, state: PickerState) {
        const choice: SPARKOption = await input.showQuickPick({
            title,
            step: 1,
            totalSteps: 2,
            placeholder: 'Choose the proof level',
            items: proofLevels,
            activeItems: state.proofLevel ?? defaultProofLevel,
        });
        state.proofLevel = choice;
        return (input: MultiStepInput) => pickOtherOptions(input, state);
    }

    async function pickOtherOptions(input: MultiStepInput, state: PickerState) {
        const choice: SPARKOption[] = await input.showQuickPick({
            title,
            step: 2,
            totalSteps: 2,
            placeholder: 'Select the desired options',
            items: options,
            canSelectMany: true,
            selectedItems: state.options,
        });
        state.options = choice;
    }

    const pickerState: PickerState = getSavedPickerState();
    try {
        logger.debug('Restored SPARK picker state: %j', pickerState);
        await MultiStepInput.run((input) => pickProofLevel(input, pickerState));
        logger.debug('User selected SPARK picker state: %j', pickerState);
        const toSave = {
            proofLevelLabel: pickerState.proofLevel.label,
            optionLabels: pickerState.options.map((o) => o.label),
        } satisfies SavedPickerState;
        /**
         * Save chosen selection for next usage
         */
        await adaExtState.context.workspaceState.update(WS_STATE_KEY_PICKER, toSave);
        logger.debug('Saved SPARK picker state: %j', toSave);
    } catch (err) {
        if (err == InputFlowAction.cancel) {
            // Selection was cancelled, interrupt the process
            throw new CancellationError();
        } else {
            throw err;
        }
    }

    return toCLIArgs(pickerState);
}

function getSavedPickerState() {
    const savedState: SavedPickerState | undefined =
        adaExtState.context.workspaceState.get(WS_STATE_KEY_PICKER);
    logger.debug('Retrieved saved SPARK picker state: %j', savedState);
    const pickerState: PickerState = savedState
        ? {
              proofLevel:
                  proofLevels.find((v) => v.label == savedState.proofLevelLabel) ??
                  defaultProofLevel,
              options: options.filter((o) => savedState.optionLabels?.find((v) => v == o.label)),
          }
        : defaultPickerState;
    return pickerState;
}

function toCLIArgs(choices: PickerState): string[] {
    const args: string[] = [];

    if (choices.proofLevel) {
        args.push(...choices.proofLevel.cliArgs);
    }
    if (choices.options) {
        args.push(...choices.options.flatMap((o) => o.cliArgs));
    }

    return args;
}

export function getLastSPARKOptions(): string[] {
    return toCLIArgs(getSavedPickerState());
}
