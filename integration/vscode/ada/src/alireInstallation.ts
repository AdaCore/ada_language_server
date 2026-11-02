import { execFile } from 'child_process';
import { createHash } from 'crypto';
import { unzipSync } from 'fflate';
import { chmod, mkdir, mkdtemp, rename, rm, rmdir, writeFile } from 'fs/promises';
import * as path from 'path';
import { promisify } from 'util';
import { exe } from './helpers';

const LATEST_RELEASE_URL = 'https://api.github.com/repos/alire-project/alire/releases/latest';

export interface AlireRelease {
    url: string;
    sha256: string;
    executable: string;
}

const PLATFORMS: Record<string, string> = {
    'linux-x64': 'x86_64-linux',
    'linux-arm64': 'aarch64-linux',
    'darwin-x64': 'x86_64-macos',
    'darwin-arm64': 'aarch64-macos',
    'win32-x64': 'x86_64-windows',
};

/** Find the platform archive and its checksum in the latest stable release. */
export async function getAlireRelease(signal: AbortSignal): Promise<AlireRelease> {
    const { platform, arch } = process;
    // Windows 11 on ARM can run the official x64 binary through emulation.
    // https://learn.microsoft.com/en-us/windows/arm/apps-on-arm-x86-emulation
    const releaseArch = platform === 'win32' && arch === 'arm64' ? 'x64' : arch;
    const target = PLATFORMS[`${platform}-${releaseArch}`];
    if (!target) throw new Error(`No Alire download is available for ${platform}/${arch}.`);
    const metadata = await downloadAlireFile(LATEST_RELEASE_URL, signal);
    const release = JSON.parse(metadata.toString('utf8')) as {
        assets: { name: string; browser_download_url: string; digest: string | null }[];
    };
    const asset = release.assets.find(
        (asset) => asset.name.startsWith('alr-') && asset.name.endsWith(`-bin-${target}.zip`),
    );
    if (!asset) {
        throw new Error(`The latest Alire release has no download for ${platform}/${arch}.`);
    }
    if (!asset.digest || !/^sha256:[a-f0-9]{64}$/i.test(asset.digest)) {
        throw new Error('The Alire release does not provide a valid SHA-256 checksum.');
    }
    return {
        url: asset.browser_download_url,
        sha256: asset.digest.slice('sha256:'.length).toLowerCase(),
        executable: `alr${exe}`,
    };
}

/** Download the selected official release, with cancellation and a bounded timeout. */
export async function downloadAlire(release: AlireRelease, signal: AbortSignal): Promise<Buffer> {
    return downloadAlireFile(release.url, signal);
}

/** Bound both release discovery and archive downloads by the same timeout. */
async function downloadAlireFile(url: string, signal: AbortSignal): Promise<Buffer> {
    const controller = new AbortController();
    const cancel = () => controller.abort();
    signal.addEventListener('abort', cancel, { once: true });
    const timeout = setTimeout(cancel, 120_000);
    try {
        signal.throwIfAborted();
        const response = await fetch(url, {
            signal: controller.signal,
            headers: { 'User-Agent': 'ada-spark-vscode' },
        });
        if (!response.ok) throw new Error(`Alire download failed: HTTP ${response.status}.`);
        return Buffer.from(await response.arrayBuffer());
    } catch (error) {
        if (controller.signal.aborted && !signal.aborted) {
            throw new Error('The Alire download timed out. Please try again.');
        }
        throw error;
    } finally {
        clearTimeout(timeout);
        signal.removeEventListener('abort', cancel);
    }
}

/** Verify the archive and extract only the executable and its license. */
export function readAlireArchive(archive: Uint8Array, release: AlireRelease) {
    if (createHash('sha256').update(archive).digest('hex') !== release.sha256) {
        throw new Error('The Alire download failed its SHA-256 checksum check.');
    }
    const entry = `bin/${release.executable}`;
    const files = unzipSync(archive, {
        filter: (file) => file.name === entry || file.name === 'LICENSE.txt',
    });
    if (!files[entry]?.length)
        throw new Error('The Alire archive does not contain its executable.');
    return { executable: files[entry], license: files['LICENSE.txt'] };
}

/** Publish an installation only after checking that the executable can run. */
export async function installAlireArchive(
    archive: Uint8Array,
    release: AlireRelease,
    destination: string,
    env: NodeJS.ProcessEnv,
    signal: AbortSignal,
): Promise<string> {
    signal.throwIfAborted();
    const files = readAlireArchive(archive, release);
    await mkdir(path.dirname(destination), { recursive: true });
    const staging = await mkdtemp(path.join(path.dirname(destination), '.alire-'));
    try {
        await mkdir(path.join(staging, 'bin'));
        const executable = path.join(staging, 'bin', release.executable);
        await writeFile(executable, files.executable, { mode: 0o755 });
        if (process.platform !== 'win32') await chmod(executable, 0o755);
        if (files.license) await writeFile(path.join(staging, 'LICENSE.txt'), files.license);

        const { stdout } = await promisify(execFile)(executable, ['--version'], {
            env,
            signal,
            timeout: 10_000,
            windowsHide: true,
        });
        // The archive checksum identifies the release; --version checks it can run.
        if (!/^alr\s/m.test(stdout)) throw new Error('The downloaded Alire executable is invalid.');
        signal.throwIfAborted();
        // A directory selected by the user may already exist. Remove it only
        // if empty, preserving any files created there during the download.
        try {
            await rmdir(destination);
        } catch (error) {
            if ((error as NodeJS.ErrnoException).code !== 'ENOENT') throw error;
        }
        await rename(staging, destination);
        return path.join(destination, 'bin', release.executable);
    } finally {
        await rm(staging, { recursive: true, force: true });
    }
}
