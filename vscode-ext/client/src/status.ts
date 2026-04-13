import * as vscode from "vscode";

export type StatusState = "checking" | "ready" | "error" | "stopped";

const STATE_STYLE: Record<
	StatusState,
	{ bg?: string; fg: string; icon: string }
> = {
	checking: { fg: "statusBarItem.warningForeground", icon: "$(sync~spin)" },
	ready: { fg: "statusBarItem.prominentForeground", icon: "$(check)" },
	error: {
		bg: "statusBarItem.errorBackground",
		fg: "errorForeground",
		icon: "$(error)",
	},
	stopped: { fg: "disabledForeground", icon: "$(circle-slash)" },
};

export class StatusBar {
	#item: vscode.StatusBarItem;

	constructor(command = "musi.checkPackage") {
		this.#item = vscode.window.createStatusBarItem(
			vscode.StatusBarAlignment.Right,
			100,
		);
		this.#item.command = command;
	}

	update(message: string, state: StatusState) {
		const style = STATE_STYLE[state];
		this.#item.text = `${style.icon} Musi: ${message}`;
		const statusItem = this.#item as vscode.StatusBarItem & {
			backgroundColor: vscode.ThemeColor | undefined;
		};
		statusItem.backgroundColor = style.bg
			? new vscode.ThemeColor(style.bg)
			: undefined;
		this.#item.color = new vscode.ThemeColor(style.fg);
		this.#item.show();
	}

	dispose() {
		this.#item.dispose();
	}
}
