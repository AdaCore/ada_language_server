import { CancellationError, QuickPickItem } from 'vscode';
import { InputFlowAction, MultiStepInput } from './multiStepInput';

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

const defaultProofLevel = proofLevels.find((v) => v.description?.includes('default'));

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

let lastState: Partial<PickerState> = {};

export async function askSPARKOptions(): Promise<string[]> {
    const title = 'Select GNATprove Options';
    async function pickProofLevel(input: MultiStepInput, state: Partial<PickerState>) {
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

    async function pickOtherOptions(input: MultiStepInput, state: Partial<PickerState>) {
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

    try {
        const tmpState = { ...lastState };
        await MultiStepInput.run((input) => pickProofLevel(input, tmpState));
        /**
         * Save chosen selection for next usage
         */
        lastState = tmpState;
    } catch (err) {
        if (err == InputFlowAction.cancel) {
            // Selection was cancelled, interrupt the process
            throw new CancellationError();
        }
    }

    return toCLIArgs(lastState);
}

function toCLIArgs(choices: Partial<PickerState>): string[] {
    const args: string[] = [];

    if (choices.proofLevel) {
        args.push(...choices.proofLevel.cliArgs);
    }
    if (choices.options) {
        args.push(...choices.options.flatMap((o) => o.cliArgs));
    }

    return args;
}
