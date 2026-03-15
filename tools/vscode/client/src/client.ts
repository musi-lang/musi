import * as vscode from "vscode";
import {
	LanguageClient,
	type LanguageClientOptions,
	type ServerOptions,
	TransportKind,
} from "vscode-languageclient/node";
import { getConfig } from "./config";

const _TIMEOUT_MS = 30_000;

let _client: LanguageClient | undefined;

function _buildServerOptions(serverPath: string): ServerOptions {
	return {
		run: { command: serverPath, transport: TransportKind.stdio },
		debug: { command: serverPath, transport: TransportKind.stdio },
	};
}

function _buildClientOptions(): LanguageClientOptions {
	const config = getConfig();

	const options: LanguageClientOptions = {
		documentSelector: [{ scheme: "file", language: "musi" }],
		synchronize: {
			fileEvents: vscode.workspace.createFileSystemWatcher("**/*.ms"),
		},
		outputChannel: vscode.window.createOutputChannel("Musi Language Server"),
	};

	if (config.traceServer !== "off") {
		options.traceOutputChannel = vscode.window.createOutputChannel(
			"Musi Language Server Trace",
		);
	}

	return options;
}

function _startWithTimeout(client: LanguageClient): Promise<void> {
	return Promise.race([
		client.start(),
		new Promise<never>((_, reject) =>
			setTimeout(
				() =>
					reject(
						new Error(
							`Language server failed to start within ${_TIMEOUT_MS / 1000} seconds`,
						),
					),
				_TIMEOUT_MS,
			),
		),
	]);
}

/**
 * Get current language client instance.
 * @returns active LanguageClient, or `undefined` if not started.
 */
export function getClient(): LanguageClient | undefined {
	return _client;
}

/**
 * Check if language client is currently running.
 */
export function isClientRunning(): boolean {
	return _client?.isRunning() ?? false;
}

/**
 * Create and start new language client connected to given server.
 * @param serverPath Absolute path to music_lsp binary.
 * @returns started LanguageClient instance.
 * @throws Error if server fails to start within timeout period.
 */
export async function createAndStartClient(
	serverPath: string,
): Promise<LanguageClient> {
	await stopClient();

	_client = new LanguageClient(
		"musiLsp",
		"Musi Language Server",
		_buildServerOptions(serverPath),
		_buildClientOptions(),
	);

	await _startWithTimeout(_client);
	return _client;
}

/**
 * Stop current language client and release resources.
 * Safe to call if no client is running.
 */
export async function stopClient() {
	if (!_client) {
		return;
	}

	try {
		await _client.stop();
	} catch (error) {
		console.warn("Failed to stop language client gracefully:", error);
	}
	_client = undefined;
}

/**
 * Restart language client with new server instance.
 * @param serverPath Absolute path to `music_lsp` binary.
 */
export async function restartClient(serverPath: string) {
	await createAndStartClient(serverPath);
}
