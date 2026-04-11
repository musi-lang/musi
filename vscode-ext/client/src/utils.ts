import * as os from "node:os";

export const TERMINAL_NAME = "Musi";

export function getCliBinaryName(): string {
	return isWindows() ? "musi.exe" : "musi";
}

export function getLspBinaryName(): string {
	return isWindows() ? "musi_lsp.exe" : "musi_lsp";
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
