import { runMochaTestsuite } from './../utils';

export function run(): Promise<void> {
    return runMochaTestsuite('gnattest', __dirname);
}
