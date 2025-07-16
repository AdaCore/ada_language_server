/**
 * This tool processes the nearby 'package-lock.json' file by removing the
 * "resolved" field of packages so that it becomes possible to resolve them in
 * a different registry than the one they were first resolved.
 */
const { writeFileSync } = require('fs');
const { join } = require('path');

lockPath = join(__dirname, 'package-lock.json');

lockData = require(lockPath);
const packages = lockData['packages'];
const keys = Object.keys(packages);
for (let index = 0; index < keys.length; index++) {
    const key = keys[index];
    const object = packages[key];
    if ('resolved' in object) {
        delete object['resolved'];
    }
}

writeFileSync(lockPath, JSON.stringify(lockData, null, 4) + '\n');
