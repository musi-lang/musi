import * as vscode from "vscode";

const TOKEN_TYPES = [
	"variable",
	"property",
	"function",
	"type",
	"enumMember",
	"typeParameter",
	"operator",
] as const;

const TOKEN_MODIFIERS = ["declaration", "readonly", "mutable"] as const;

export const MUSI_SEMANTIC_LEGEND = new vscode.SemanticTokensLegend(
	[...TOKEN_TYPES],
	[...TOKEN_MODIFIERS],
);

export class MusiSemanticTokensProvider
	implements vscode.DocumentSemanticTokensProvider
{
	provideDocumentSemanticTokens(
		_document: vscode.TextDocument,
		_token: vscode.CancellationToken,
	): vscode.ProviderResult<vscode.SemanticTokens> {
		const builder = new vscode.SemanticTokensBuilder(MUSI_SEMANTIC_LEGEND);
		return builder.build();
	}
}
