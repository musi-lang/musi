import * as os from "node:os";

/**
 * Get platform-appropriate binary name for `musi_lsp`.
 * Returns `musi_lsp.exe` on Windows, `musi_lsp` otherwise.
 */
export function getServerBinaryName(): string {
	return os.platform() === "win32" ? "musi_lsp.exe" : "musi_lsp";
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
