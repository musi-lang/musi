import * as os from "node:os";

/**
 * Get platform-appropriate binary name for `music_lsp`.
 * Returns `music_lsp.exe` on Windows, `music_lsp` otherwise.
 */
export function getServerBinaryName(): string {
	return os.platform() === "win32" ? "music_lsp.exe" : "music_lsp";
}

/**
 * Get platform-appropriate binary name for `musi` CLI.
 * Returns `musi.exe` on Windows, `musi` otherwise.
 */
export function getCliBinaryName(): string {
	return os.platform() === "win32" ? "musi.exe" : "musi";
}

/**
 * Check if current platform is Windows.
 */
export function isWindows(): boolean {
	return os.platform() === "win32";
}

/**
 * Get user's home directory in cross-platform manner.
 */
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
