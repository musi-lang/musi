import * as vscode from "vscode";
import {
	LanguageClient,
	type LanguageClientOptions,
	type ServerOptions,
	State,
	TransportKind,
} from "vscode-languageclient/node";
import {
	findLspBinary,
	showLspNotFoundUI,
	showStaleLspBinaryUI,
} from "./bootstrap.ts";
import { getConfig } from "./config.ts";
import type { DiagnosticsController } from "./diagnostics.ts";
import type { StatusBar } from "./status.ts";

type LspStateListener = (isRunning: boolean) => void;

export class LspController implements vscode.Disposable {
	#client: LanguageClient | undefined;
	#statusBar: StatusBar;
	#diagnostics: DiagnosticsController;
	#onStateChange: LspStateListener;

	constructor(
		statusBar: StatusBar,
		diagnostics: DiagnosticsController,
		onStateChange: LspStateListener = () => {},
	) {
		this.#statusBar = statusBar;
		this.#diagnostics = diagnostics;
		this.#onStateChange = onStateChange;
	}

	isRunning(): boolean {
		return this.#client?.state === State.Running;
	}

	showOutput(): void {
		this.#client?.outputChannel.show();
	}

	async start(context: vscode.ExtensionContext): Promise<boolean> {
		const config = getConfig();
		if (!config.lspEnabled) {
			this.#diagnostics.setMode("full");
			this.#onStateChange(false);
			this.#statusBar.update("LSP disabled", "stopped");
			return false;
		}
		const server = findLspBinary();
		const serverPath = server.path;
		if (!serverPath) {
			this.#diagnostics.setMode("full");
			this.#onStateChange(false);
			if (server.staleWorkspacePath) {
				this.#statusBar.update("LSP stale", "error");
				await showStaleLspBinaryUI(
					server.staleWorkspacePath,
					server.freshnessPath,
				);
			} else {
				await showLspNotFoundUI();
			}
			return false;
		}

		const serverOptions: ServerOptions = {
			command: serverPath,
			args: [],
			transport: TransportKind.stdio,
		};
		const clientOptions: LanguageClientOptions = {
			documentSelector: [{ scheme: "file", language: "musi" }],
			outputChannelName: "Musi LSP",
			initializationOptions: {
				hover: {
					maximumLength: config.hoverMaximumLength,
				},
				inlayHints: {
					enabled: config.inlayHints.enabled,
					parameterNames: config.inlayHints.parameterNames,
					parameterNamesSuppressWhenArgumentMatchesName:
						config.inlayHints.parameterNamesSuppressWhenArgumentMatchesName,
					variableTypes: config.inlayHints.variableTypes,
					variableTypesSuppressWhenTypeMatchesName:
						config.inlayHints.variableTypesSuppressWhenTypeMatchesName,
				},
			},
		};
		const client = new LanguageClient(
			"musi-lsp",
			"Musi LSP",
			serverOptions,
			clientOptions,
		);
		client.onDidChangeState((event) => {
			if (event.newState === State.Running) {
				this.#diagnostics.setMode("manifest-only");
				this.#onStateChange(true);
				return;
			}
			if (event.newState === State.Stopped) {
				this.#diagnostics.setMode("full");
				this.#statusBar.update("LSP unavailable", "error");
				this.#onStateChange(false);
			}
		});

		try {
			context.subscriptions.push(client);
			await client.start();
			this.#client = client;
			this.#diagnostics.setMode("manifest-only");
			this.#onStateChange(true);
			return true;
		} catch (error) {
			this.#diagnostics.setMode("full");
			this.#statusBar.update("LSP unavailable", "error");
			this.#onStateChange(false);
			vscode.window
				.showErrorMessage(`Failed to start Musi LSP: ${String(error)}`)
				.then(undefined, (messageError: unknown) => {
					console.error(
						"[musi-vscode] failed to show LSP error message:",
						messageError,
					);
				});
			return false;
		}
	}

	async restart(context: vscode.ExtensionContext): Promise<boolean> {
		await this.stop();
		return this.start(context);
	}

	async stop(): Promise<void> {
		const client = this.#client;
		this.#client = undefined;
		this.#diagnostics.setMode("full");
		this.#onStateChange(false);
		if (client) {
			await client.stop();
		}
	}

	dispose() {
		this.stop().catch((error) => {
			console.error("[musi-vscode] failed to stop LSP client:", error);
		});
	}
}
