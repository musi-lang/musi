import { languages } from "vscode";

export function activate(context) {
	const provider = languages.registerCompletionItemProvider("musi", {
		provideCompletionItems(_document, _position, _token, _context) {
			return [];
		},
	});
	context.subscriptions.push(provider);
}

export function deactivate() {
	return;
}
