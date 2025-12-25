import * as vscode from "vscode";

/** Visual state of status bar indicator. */
export type StatusState = "loading" | "ready" | "error" | "stopped";

const _STATE_COLORS: Record<StatusState, { bg?: string; fg: string }> = {
	loading: { fg: "statusBarItem.warningForeground" },
	ready: { fg: "statusBarItem.prominentForeground" },
	error: { bg: "statusBarItem.errorBackground", fg: "errorForeground" },
	stopped: { fg: "disabledForeground" },
};

/**
 * Manages Musi status bar item in VS Code.
 * Displays connection state and provides click-to-action functionality.
 */
export class StatusBar {
	#item: vscode.StatusBarItem;

	/**
	 * Create new status bar manager.
	 * @param command Command to execute when status bar item is clicked.
	 */
	constructor(command = "musi.showLogs") {
		this.#item = vscode.window.createStatusBarItem(
			vscode.StatusBarAlignment.Right,
			100,
		);
		this.#item.command = command;
	}

	/**
	 * Update status bar with message and visual state.
	 * @param message Text to display (prefixed with `"Musi: "`).
	 * @param state Visual state determining colors.
	 */
	update(message: string, state: StatusState) {
		this.#item.text = `$(play) Musi: ${message}`;

		const colors = _STATE_COLORS[state];
		this.#item.backgroundColor = colors.bg
			? new vscode.ThemeColor(colors.bg)
			: undefined;
		this.#item.color = new vscode.ThemeColor(colors.fg);

		this.#item.show();
	}

	/**
	 * Dispose of status bar item and release resources.
	 */
	dispose() {
		this.#item.dispose();
	}
}
