import * as os from "node:os";

export const TERMINAL_NAME = "Musi";

export function getServerBinaryName(): string {
	return isWindows() ? "music_lsp.exe" : "music_lsp";
}

export function getCliBinaryName(): string {
	return isWindows() ? "musi.exe" : "musi";
}

export function isWindows(): boolean {
	return os.platform() === "win32";
}

export function getHomeDir(): string {
	return os.homedir();
}

/**
 * Get cargo bin directory path for current platform.
 * Returns `~/.cargo/bin` on Unix, `%USERPROFILE%\.cargo\bin` on Windows.
 */
export function getCargoBinDir(): string {
	const home = getHomeDir();
	return isWindows() ? `${home}\\.cargo\\bin` : `${home}/.cargo/bin`;
}
