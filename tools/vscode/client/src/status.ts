import * as vscode from "vscode";

export type StatusState = "loading" | "ready" | "error" | "stopped";

export class StatusBar {
	#statusBarItem: vscode.StatusBarItem;

	constructor() {
		this.#statusBarItem = vscode.window.createStatusBarItem(
			vscode.StatusBarAlignment.Right,
			100,
		);
		this.#statusBarItem.command = "musi.showLogs";
	}

	update(message: string, state: StatusState) {
		this.#statusBarItem.text = `$(play) Musi: ${message}`;

		switch (state) {
			case "loading":
				this.#statusBarItem.backgroundColor = undefined;
				this.#statusBarItem.color = new vscode.ThemeColor(
					"statusBarItem.warningForeground",
				);
				break;
			case "ready":
				this.#statusBarItem.backgroundColor = undefined;
				this.#statusBarItem.color = new vscode.ThemeColor(
					"statusBarItem.prominentForeground",
				);
				break;
			case "error":
				this.#statusBarItem.backgroundColor = new vscode.ThemeColor(
					"statusBarItem.errorBackground",
				);
				this.#statusBarItem.color = new vscode.ThemeColor("errorForeground");
				break;
			case "stopped":
				this.#statusBarItem.backgroundColor = undefined;
				this.#statusBarItem.color = new vscode.ThemeColor(
					"disabledForeground",
				);
				break;
		}

		this.#statusBarItem.show();
	}

	dispose() {
		this.#statusBarItem.dispose();
	}
}
