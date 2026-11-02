import * as path from 'path';

let managedToolDirectories: readonly string[] = [];

/** Directories supplied by tools installed through the extension. */
export function setManagedToolDirectories(directories: readonly string[]) {
    managedToolDirectories = [...directories];
}

/** Normalize PATH and apply managed tools after workspace environment overrides. */
export function applyManagedToolEnvironment(
    env: NodeJS.ProcessEnv,
    platform: NodeJS.Platform = process.platform,
) {
    const windows = platform === 'win32';
    if (!managedToolDirectories.length && !windows) return;
    const delimiter = windows ? ';' : ':';
    const normalize = (directory: string) =>
        windows ? path.win32.normalize(directory).toLowerCase() : path.posix.normalize(directory);
    // Windows treats Path and PATH as the same variable, but a JavaScript
    // object does not. Pass a single, canonical PATH to child processes.
    const pathKey = windows
        ? (Object.keys(env).find((key) => key === 'PATH') ??
          Object.keys(env).find((key) => key.toLowerCase() === 'path'))
        : 'PATH';
    const current = env[pathKey ?? 'PATH'];
    const managed = new Set(managedToolDirectories.map(normalize));
    if (windows) {
        for (const key of Object.keys(env)) {
            if (key.toLowerCase() === 'path') delete env[key];
        }
    }
    if (!managedToolDirectories.length) {
        if (current !== undefined) env.PATH = current;
        return;
    }
    env.PATH = [
        ...managedToolDirectories,
        ...(current === undefined ? [] : current.split(delimiter)).filter(
            (directory) => !managed.has(normalize(directory)),
        ),
    ].join(delimiter);
}

/** Read Alire's Unix export format without evaluating shell expressions. */
export function parseAlireEnvironment(output: string): Record<string, string> {
    const env: Record<string, string> = {};
    for (const line of output.split(/\r?\n/)) {
        const match = /^export\s+(\w+)=(.*)$/.exec(line.trim());
        if (!match) continue;
        let value = match[2];
        const quote = value[0];
        if ((quote === '"' || quote === "'") && value.endsWith(quote)) {
            value = value.slice(1, -1);
            // Decode shell escapes in double quotes, preserving literal backslashes
            // before other characters (e.g. in Windows paths). Single quotes are literal.
            if (quote === '"') value = value.replace(/\\(["\\$`])/g, '$1');
        }
        env[match[1]] = value;
    }
    return env;
}
