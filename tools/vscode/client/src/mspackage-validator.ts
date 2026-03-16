import * as vscode from "vscode";
import * as fs from "node:fs";
import * as path from "node:path";
import * as TOML from "smol-toml";

interface FieldSchema {
	type: string;
	enum?: string[];
	pattern?: string;
	default?: unknown;
}

interface SectionSchema {
	openType: string | null;
	fields: Map<string, FieldSchema>;
	subsections: Map<string, SectionSchema>;
}

type SchemaMap = Map<string, SectionSchema>;

let _schema: SchemaMap | null = null;
let _debounceTimer: ReturnType<typeof setTimeout> | undefined;

const DEBOUNCE_MS = 300;

function levenshtein(a: string, b: string): number {
	const m = a.length;
	const n = b.length;
	const dp: number[][] = Array.from({ length: m + 1 }, () =>
		Array(n + 1).fill(0),
	);
	for (let i = 0; i <= m; i++) dp[i][0] = i;
	for (let j = 0; j <= n; j++) dp[0][j] = j;
	for (let i = 1; i <= m; i++) {
		for (let j = 1; j <= n; j++) {
			dp[i][j] =
				a[i - 1] === b[j - 1]
					? dp[i - 1][j - 1]
					: 1 + Math.min(dp[i - 1][j], dp[i][j - 1], dp[i - 1][j - 1]);
		}
	}
	return dp[m][n];
}

function suggest(name: string, candidates: Iterable<string>): string | null {
	let best: string | null = null;
	let bestDist = 3;
	for (const c of candidates) {
		const d = levenshtein(name, c);
		if (d < bestDist) {
			bestDist = d;
			best = c;
		}
	}
	return best;
}

function parseSchemaToml(text: string): SchemaMap {
	const raw = TOML.parse(text) as Record<string, unknown>;
	const schema: SchemaMap = new Map();

	function ensureSection(path: string[]): SectionSchema {
		let current = schema;
		let section: SectionSchema | undefined;
		for (const segment of path) {
			if (!current.has(segment)) {
				current.set(segment, {
					openType: null,
					fields: new Map(),
					subsections: new Map(),
				});
			}
			section = current.get(segment)!;
			current = section.subsections;
		}
		return section!;
	}

	function walk(obj: Record<string, unknown>, pathParts: string[]) {
		for (const [key, value] of Object.entries(obj)) {
			if (
				value !== null &&
				typeof value === "object" &&
				!Array.isArray(value)
			) {
				const v = value as Record<string, unknown>;
				if ("type" in v && typeof v.type === "string") {
					const section = ensureSection(pathParts);
					const field: FieldSchema = { type: v.type };
					if (v.enum) field.enum = v.enum as string[];
					if (v.pattern) field.pattern = v.pattern as string;
					if ("default" in v) field.default = v.default;
					section.fields.set(key, field);
				} else if (key === "_type") {
					// handled below
				} else {
					walk(v, [...pathParts, key]);
				}
			}
		}
	}

	// First pass: collect _type markers
	function collectOpenTypes(
		obj: Record<string, unknown>,
		pathParts: string[],
	) {
		for (const [key, value] of Object.entries(obj)) {
			if (key === "_type" && typeof value === "string") {
				const section = ensureSection(pathParts);
				section.openType = value;
			} else if (
				value !== null &&
				typeof value === "object" &&
				!Array.isArray(value)
			) {
				const v = value as Record<string, unknown>;
				if (!("type" in v && typeof v.type === "string")) {
					collectOpenTypes(v, [...pathParts, key]);
				}
			}
		}
	}

	collectOpenTypes(raw, []);
	walk(raw, []);
	return schema;
}

function getSchema(extensionPath: string): SchemaMap {
	if (_schema) return _schema;
	const schemaPath = path.join(
		extensionPath,
		"schemas",
		"mspackage.schema.toml",
	);
	const text = fs.readFileSync(schemaPath, "utf-8");
	_schema = parseSchemaToml(text);
	return _schema;
}

function tomlTypeOf(value: unknown): string {
	if (typeof value === "string") return "string";
	if (typeof value === "number") {
		return Number.isInteger(value) ? "integer" : "float";
	}
	if (typeof value === "boolean") return "boolean";
	if (Array.isArray(value)) return "array";
	if (value instanceof Date) return "datetime";
	if (typeof value === "object" && value !== null) return "table";
	return "unknown";
}

function matchesType(schemaType: string, value: unknown): boolean {
	const actual = tomlTypeOf(value);
	// "string | boolean" style union types
	const allowed = schemaType.split("|").map((t) => t.trim());
	for (const t of allowed) {
		if (t === "string" && actual === "string") return true;
		if (t === "boolean" && actual === "boolean") return true;
		if (t === "number" && (actual === "integer" || actual === "float"))
			return true;
		if (t === "integer" && actual === "integer") return true;
		if (t === "float" && actual === "float") return true;
		if (t.startsWith("array") && actual === "array") return true;
		if (t.startsWith("table") && actual === "table") return true;
		if (t === "string | table of string") {
			if (actual === "string" || actual === "table") return true;
		}
	}
	return false;
}

function friendlyType(schemaType: string): string {
	return schemaType.split("|").map((t) => t.trim()).join(" or ");
}

interface TomlNode {
	start: { line: number; column: number };
	end: { line: number; column: number };
}

function findKeyRange(
	doc: vscode.TextDocument,
	sectionPath: string[],
	key: string,
): vscode.Range {
	const text = doc.getText();
	const lines = text.split("\n");

	// Build expected header patterns
	const headerPatterns: string[] = [];
	if (sectionPath.length > 0) {
		const joined = sectionPath.join(".");
		headerPatterns.push(`[${joined}]`);
		headerPatterns.push(`[[${joined}]]`);
	}

	let inSection = sectionPath.length === 0;
	for (let i = 0; i < lines.length; i++) {
		const trimmed = lines[i].trim();

		if (trimmed.startsWith("[")) {
			if (sectionPath.length === 0) {
				inSection = false;
				continue;
			}
			const headerMatch = trimmed.match(/^\[+\s*([^\]]+?)\s*\]+/);
			if (headerMatch) {
				const headerName = headerMatch[1];
				inSection = headerPatterns.some(
					(p) =>
						headerName === p.replace(/[\[\]]/g, "") ||
						headerName.startsWith(
							`${sectionPath.join(".")}.`,
						),
				);
				// Also match if header IS exactly the section
				if (headerName === sectionPath.join(".")) {
					inSection = true;
				}
			}
			continue;
		}

		if (inSection) {
			const keyMatch = trimmed.match(/^([a-zA-Z0-9_-]+)\s*=/);
			if (keyMatch && keyMatch[1] === key) {
				const col = lines[i].indexOf(key);
				return new vscode.Range(i, col, i, col + key.length);
			}
		}
	}

	return new vscode.Range(0, 0, 0, 0);
}

function findSectionRange(
	doc: vscode.TextDocument,
	sectionPath: string[],
): vscode.Range {
	const text = doc.getText();
	const lines = text.split("\n");
	const joined = sectionPath.join(".");

	for (let i = 0; i < lines.length; i++) {
		const trimmed = lines[i].trim();
		const headerMatch = trimmed.match(/^\[+\s*([^\]]+?)\s*\]+/);
		if (headerMatch && headerMatch[1] === joined) {
			const col = lines[i].indexOf(joined);
			return new vscode.Range(i, col, i, col + joined.length);
		}
	}

	return new vscode.Range(0, 0, 0, 0);
}

function validateDocument(
	doc: vscode.TextDocument,
	schema: SchemaMap,
): vscode.Diagnostic[] {
	const diagnostics: vscode.Diagnostic[] = [];
	let parsed: Record<string, unknown>;

	try {
		parsed = TOML.parse(doc.getText()) as Record<string, unknown>;
	} catch (e: unknown) {
		const msg = e instanceof Error ? e.message : String(e);
		const range = new vscode.Range(0, 0, 0, 0);
		diagnostics.push(
			new vscode.Diagnostic(range, `TOML parse error: ${msg}`, vscode.DiagnosticSeverity.Error),
		);
		return diagnostics;
	}

	function validateSection(
		obj: Record<string, unknown>,
		sectionSchema: SectionSchema | undefined,
		sectionPath: string[],
		allSections: SchemaMap,
	) {
		if (!sectionSchema) return;

		for (const [key, value] of Object.entries(obj)) {
			const fieldSchema = sectionSchema.fields.get(key);
			const childSection = sectionSchema.subsections.get(key);

			if (fieldSchema) {
				validateField(key, value, fieldSchema, sectionPath);
			} else if (childSection) {
				if (typeof value === "object" && value !== null && !Array.isArray(value)) {
					validateSection(
						value as Record<string, unknown>,
						childSection,
						[...sectionPath, key],
						childSection.subsections,
					);
				}
			} else if (sectionSchema.openType) {
				validateOpenTableValue(key, value, sectionSchema.openType, sectionPath);
			} else {
				const range = findKeyRange(doc, sectionPath, key);
				const known = [
					...sectionSchema.fields.keys(),
					...sectionSchema.subsections.keys(),
				];
				const hint = suggest(key, known);
				const section = sectionPath.length > 0 ? `[${sectionPath.join(".")}]` : "top level";
				let msg = `Unknown key '${key}' in ${section}.`;
				if (hint) msg += ` Did you mean '${hint}'?`;
				diagnostics.push(
					new vscode.Diagnostic(range, msg, vscode.DiagnosticSeverity.Warning),
				);
			}
		}
	}

	function validateField(
		key: string,
		value: unknown,
		field: FieldSchema,
		sectionPath: string[],
	) {
		const range = findKeyRange(doc, sectionPath, key);

		if (!matchesType(field.type, value)) {
			const actual = tomlTypeOf(value);
			diagnostics.push(
				new vscode.Diagnostic(
					range,
					`Expected ${friendlyType(field.type)}, got ${actual}.`,
					vscode.DiagnosticSeverity.Error,
				),
			);
			return;
		}

		if (field.enum && typeof value === "string") {
			if (!field.enum.includes(value)) {
				diagnostics.push(
					new vscode.Diagnostic(
						range,
						`Invalid value '${value}'. Expected one of: ${field.enum.join(", ")}.`,
						vscode.DiagnosticSeverity.Error,
					),
				);
			}
		}

		if (field.pattern && typeof value === "string") {
			try {
				const re = new RegExp(field.pattern);
				if (!re.test(value)) {
					diagnostics.push(
						new vscode.Diagnostic(
							range,
							`Value '${value}' doesn't match expected pattern: ${field.pattern}`,
							vscode.DiagnosticSeverity.Warning,
						),
					);
				}
			} catch {
				// Invalid regex in schema — skip
			}
		}
	}

	function validateOpenTableValue(
		key: string,
		value: unknown,
		openType: string,
		sectionPath: string[],
	) {
		const range = findKeyRange(doc, sectionPath, key);

		if (openType === "table of string") {
			if (typeof value !== "string") {
				diagnostics.push(
					new vscode.Diagnostic(
						range,
						`Expected string value, got ${tomlTypeOf(value)}.`,
						vscode.DiagnosticSeverity.Error,
					),
				);
			}
		} else if (openType === "table of array of string") {
			if (!Array.isArray(value)) {
				diagnostics.push(
					new vscode.Diagnostic(
						range,
						`Expected array of strings, got ${tomlTypeOf(value)}.`,
						vscode.DiagnosticSeverity.Error,
					),
				);
			}
		} else if (openType === "table of (string | task-object)") {
			if (typeof value !== "string" && tomlTypeOf(value) !== "table") {
				diagnostics.push(
					new vscode.Diagnostic(
						range,
						`Expected string or table, got ${tomlTypeOf(value)}.`,
						vscode.DiagnosticSeverity.Error,
					),
				);
			}
		} else if (openType === "string | table of string") {
			if (typeof value !== "string" && tomlTypeOf(value) !== "table") {
				diagnostics.push(
					new vscode.Diagnostic(
						range,
						`Expected string or table of strings, got ${tomlTypeOf(value)}.`,
						vscode.DiagnosticSeverity.Error,
					),
				);
			}
		}
	}

	// Check top-level keys
	const topLevelKnown = new Set<string>();
	for (const k of schema.keys()) topLevelKnown.add(k);

	for (const [key, value] of Object.entries(parsed)) {
		const sectionSchema = schema.get(key);
		if (!sectionSchema) {
			const range = findSectionRange(doc, [key]);
			const hint = suggest(key, topLevelKnown);
			let msg = `Unknown section '${key}'.`;
			if (hint) msg += ` Did you mean '${hint}'?`;
			diagnostics.push(
				new vscode.Diagnostic(range, msg, vscode.DiagnosticSeverity.Warning),
			);
			continue;
		}

		if (typeof value === "object" && value !== null && !Array.isArray(value)) {
			validateSection(
				value as Record<string, unknown>,
				sectionSchema,
				[key],
				sectionSchema.subsections,
			);
		} else if (Array.isArray(value)) {
			// [[references]] — array of tables
			for (const item of value) {
				if (typeof item === "object" && item !== null) {
					validateSection(
						item as Record<string, unknown>,
						sectionSchema,
						[key],
						sectionSchema.subsections,
					);
				}
			}
		}
	}

	return diagnostics;
}

export function registerMspackageValidator(
	context: vscode.ExtensionContext,
): vscode.DiagnosticCollection {
	const diagnosticCollection =
		vscode.languages.createDiagnosticCollection("musi-mspackage");
	context.subscriptions.push(diagnosticCollection);

	const schema = getSchema(context.extensionPath);

	function validate(doc: vscode.TextDocument) {
		if (!doc.fileName.endsWith("mspackage.toml")) return;
		const diagnostics = validateDocument(doc, schema);
		diagnosticCollection.set(doc.uri, diagnostics);
	}

	function debouncedValidate(doc: vscode.TextDocument) {
		if (_debounceTimer) clearTimeout(_debounceTimer);
		_debounceTimer = setTimeout(() => validate(doc), DEBOUNCE_MS);
	}

	// Validate already-open mspackage.toml files
	for (const doc of vscode.workspace.textDocuments) {
		validate(doc);
	}

	context.subscriptions.push(
		vscode.workspace.onDidOpenTextDocument(validate),
		vscode.workspace.onDidChangeTextDocument((e) =>
			debouncedValidate(e.document),
		),
		vscode.workspace.onDidSaveTextDocument(validate),
		vscode.workspace.onDidCloseTextDocument((doc) => {
			if (doc.fileName.endsWith("mspackage.toml")) {
				diagnosticCollection.delete(doc.uri);
			}
		}),
	);

	return diagnosticCollection;
}
