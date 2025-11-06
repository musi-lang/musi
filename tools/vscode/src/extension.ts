// biome-ignore lint/correctness/noUndeclaredDependencies: @types/vscode
import { type ExtensionContext, languages } from "vscode";

export function activate(context: ExtensionContext): void {
	const provider = languages.registerCompletionItemProvider("musi", {
		provideCompletionItems(_document, _position, _token, _context) {
			return [];
		},
	});
	context.subscriptions.push(provider);
}

export function deactivate(): void {
	return;
}
