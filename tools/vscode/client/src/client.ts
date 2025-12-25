import * as vscode from "vscode";
import {
	LanguageClient,
	type LanguageClientOptions,
	type ServerOptions,
	TransportKind,
} from "vscode-languageclient/node";
import { getConfig } from "./config";

const _SERVER_START_TIMEOUT_MS = 30000;

let _client: LanguageClient | undefined;

export function getClient(): LanguageClient | undefined {
	return _client;
}

export function isClientRunning(): boolean {
	return _client?.isRunning() ?? false;
}

export async function createAndStartClient(
	serverPath: string,
): Promise<LanguageClient> {
	const config = getConfig();

	const serverOptions: ServerOptions = {
		run: { command: serverPath, transport: TransportKind.stdio },
		debug: { command: serverPath, transport: TransportKind.stdio },
	};

	const clientOptions: LanguageClientOptions = {
		documentSelector: [{ scheme: "file", language: "musi" }],
		synchronize: {
			fileEvents: vscode.workspace.createFileSystemWatcher("**/*.ms"),
		},
		initializationOptions: {
			diagnostics: { enable: config.diagnosticsEnabled },
			completion: { enable: config.completionEnabled },
			formatting: {
				enable: config.formattingEnabled,
				indentSize: config.formattingIndentSize,
			},
			inlayHints: { enable: config.inlayHintsEnabled },
			runtime: { path: config.runtimePath },
		},
	};

	if (config.traceServer !== "off") {
		clientOptions.traceOutputChannel = vscode.window.createOutputChannel(
			"Musi Language Server Trace",
		);
	}

	_client = new LanguageClient(
		"musiLsp",
		"Musi Language Server",
		serverOptions,
		clientOptions,
	);

	await Promise.race([
		_client.start(),
		new Promise<never>((_, reject) =>
			setTimeout(
				() => reject(new Error(`Server start timeout (${_SERVER_START_TIMEOUT_MS / 1000}s)`)),
				_SERVER_START_TIMEOUT_MS,
			),
		),
	]);

	return _client;
}

export async function stopClient() {
	if (_client) {
		try {
			await _client.stop();
		} catch (error) {
			console.warn("Error stopping client:", error);
		}
		_client = undefined;
	}
}

export async function restartClient(serverPath: string) {
	await stopClient();
	await createAndStartClient(serverPath);
}
