import { languages } from "vscode";

/**
 * @param {{ subscriptions: import("vscode").Disposable[]; }} context
 */
export function activate(context) {
	const provider = languages.registerCompletionItemProvider("musi", {
		provideCompletionItems(_document, _position, _token, _context) {
			return [];
		},
	});

	if (!context.subscriptions.includes(provider)) {
		context.subscriptions.push(provider);
	}
}

export function deactivate() {
	return undefined;
}
