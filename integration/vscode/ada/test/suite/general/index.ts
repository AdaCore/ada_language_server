import { runMochaTestsuite } from './../utils';

export function run(): Promise<void> {
    return runMochaTestsuite('general', __dirname);
}
