import { languages } from "vscode";

/**
 * @param {{ subscriptions: import("vscode").Disposable[]; }} context
 */
export function activate(context) {
	const provider = languages.registerCompletionItemProvider(
		{ language: "musi", scheme: "file" },
		{
			provideCompletionItems(_document, _position, _token, _context) {
				return undefined;
			},
		}
	);

	context.subscriptions.push(provider);
}

export function deactivate() {
	return undefined;
}
