import * as vscode from "vscode";
import {
	LanguageClient,
	type LanguageClientOptions,
	type ServerOptions,
	State,
	TransportKind,
} from "vscode-languageclient/node";
import { findLspPath } from "./bootstrap.ts";
import type { DiagnosticsController } from "./diagnostics.ts";
import type { StatusBar } from "./status.ts";

export class LspController implements vscode.Disposable {
	#client: LanguageClient | undefined;
	#statusBar: StatusBar;
	#diagnostics: DiagnosticsController;

	constructor(statusBar: StatusBar, diagnostics: DiagnosticsController) {
		this.#statusBar = statusBar;
		this.#diagnostics = diagnostics;
	}

	isRunning(): boolean {
		return this.#client?.state === State.Running;
	}

	async start(context: vscode.ExtensionContext): Promise<boolean> {
		const serverPath = findLspPath();
		if (!serverPath) {
			this.#diagnostics.setMode("full");
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
				return;
			}
			if (event.newState === State.Stopped) {
				this.#diagnostics.setMode("full");
				this.#statusBar.update("LSP unavailable", "error");
			}
		});

		try {
			context.subscriptions.push(client);
			await client.start();
			this.#client = client;
			this.#diagnostics.setMode("manifest-only");
			return true;
		} catch (error) {
			this.#diagnostics.setMode("full");
			this.#statusBar.update("LSP unavailable", "error");
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
