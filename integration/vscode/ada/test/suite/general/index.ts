import { runMochaTests } from '../utils';

export function run(): Promise<void> {
    return runMochaTests(__dirname);
}
