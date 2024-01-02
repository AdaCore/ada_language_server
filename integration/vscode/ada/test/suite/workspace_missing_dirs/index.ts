import { runMochaTestsuite } from '../utils';

export function run(): Promise<void> {
    return runMochaTestsuite('workspace_missing_dirs', __dirname);
}
