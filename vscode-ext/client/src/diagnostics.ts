import * as path from "node:path";
import * as vscode from "vscode";
import { getConfig } from "./config.ts";
import {
	activeDocumentUri,
	findOwningManifestPathForUri,
	loadPackageRoot,
} from "./manifest.ts";
import {
	type DiagnosticLabelPayload,
	type DiagnosticPayload,
	type DiagnosticRangePayload,
	runStructuredPackageCheck,
} from "./runner.ts";
import type { StatusBar } from "./status.ts";
import type { PackageRoot } from "./types.ts";

const CHECK_DEBOUNCE_MS = 250;

type PendingTimer = ReturnType<typeof setTimeout>;
type DiagnosticsMode = "disabled" | "full" | "manifest-only";

function toSeverity(value: string | undefined): vscode.DiagnosticSeverity {
	switch (value?.toLowerCase()) {
		case "warning":
			return vscode.DiagnosticSeverity.Warning;
		case "info":
			return vscode.DiagnosticSeverity.Information;
		case "hint":
			return vscode.DiagnosticSeverity.Hint;
		default:
			return vscode.DiagnosticSeverity.Error;
	}
}

function toPosition(point: {
	line: number;
	character: number;
}): vscode.Position {
	return new vscode.Position(
		Math.max(0, point.line - 1),
		Math.max(0, point.character - 1),
	);
}

function toRange(range: DiagnosticRangePayload | undefined): vscode.Range {
	if (!range) {
		return new vscode.Range(0, 0, 0, 1);
	}
	return new vscode.Range(toPosition(range.start), toPosition(range.end));
}

function normalizePath(pkg: PackageRoot, filePath: string | undefined): string {
	if (!filePath) {
		return pkg.manifestPath;
	}
	return path.isAbsolute(filePath)
		? filePath
		: path.join(pkg.rootDir, filePath);
}

function relatedInformation(
	pkg: PackageRoot,
	labels: readonly DiagnosticLabelPayload[] | undefined,
	primaryFile: string,
	primaryRange: vscode.Range,
): vscode.DiagnosticRelatedInformation[] {
	if (!labels) {
		return [];
	}

	return labels
		.map((label) => {
			const filePath = normalizePath(pkg, label.file);
			const range = toRange(label.range);
			if (filePath === primaryFile && range.isEqual(primaryRange)) {
				return undefined;
			}
			return new vscode.DiagnosticRelatedInformation(
				new vscode.Location(vscode.Uri.file(filePath), range),
				label.message ?? "related location",
			);
		})
		.filter((entry): entry is vscode.DiagnosticRelatedInformation => !!entry);
}

function toDiagnostic(
	pkg: PackageRoot,
	payload: DiagnosticPayload,
): {
	filePath: string;
	diagnostic: vscode.Diagnostic;
} {
	const primaryRange =
		payload.primaryRange ?? payload.range ?? payload.labels?.[0]?.range;
	const filePath = normalizePath(
		pkg,
		payload.file ?? payload.labels?.[0]?.file,
	);
	const range = toRange(primaryRange);
	const messageParts = [payload.message, ...(payload.notes ?? [])];
	if (payload.hint) {
		messageParts.push(`Hint: ${payload.hint}`);
	}
	const diagnostic = new vscode.Diagnostic(
		range,
		messageParts.join("\n"),
		toSeverity(payload.severity ?? payload.level),
	);
	diagnostic.source = "musi";
	if (payload.code !== undefined) {
		diagnostic.code = payload.code;
	}
	diagnostic.relatedInformation = relatedInformation(
		pkg,
		payload.labels,
		filePath,
		range,
	);
	return { filePath, diagnostic };
}

export class DiagnosticsController {
	#collection = vscode.languages.createDiagnosticCollection("musi");
	#statusBar: StatusBar;
	#timers = new Map<string, PendingTimer>();
	#running = new Map<string, AbortController>();
	#trackedFiles = new Map<string, Set<string>>();
	#mode: DiagnosticsMode = "full";

	constructor(statusBar: StatusBar) {
		this.#statusBar = statusBar;
	}

	setMode(mode: DiagnosticsMode) {
		this.#mode = mode;
	}

	mode() {
		return this.#mode;
	}

	dispose() {
		for (const timer of this.#timers.values()) {
			clearTimeout(timer);
		}
		for (const controller of this.#running.values()) {
			controller.abort();
		}
		this.#collection.dispose();
	}

	async refreshStatusForActiveEditor() {
		const activeUri = activeDocumentUri();
		const manifestPath = findOwningManifestPathForUri(activeUri);
		if (!manifestPath) {
			this.#statusBar.update("No owning musi.json", "stopped");
			return;
		}

		try {
			const pkg = await loadPackageRoot(manifestPath);
			this.#statusBar.update(`Ready: ${path.basename(pkg.rootDir)}`, "ready");
		} catch (error) {
			this.#statusBar.update("Invalid musi.json", "error");
			vscode.window.showErrorMessage(
				`Failed to read owning musi.json: ${String(error)}`,
			);
		}
	}

	scheduleDocumentCheck(document: vscode.TextDocument) {
		if (!(getConfig().checkOnSave && this.#shouldCheckDocument(document))) {
			return;
		}

		const manifestPath = findOwningManifestPathForUri(document.uri);
		if (!manifestPath) {
			this.#statusBar.update("No owning musi.json", "stopped");
			return;
		}

		const existing = this.#timers.get(manifestPath);
		if (existing) {
			clearTimeout(existing);
		}

		const timer = setTimeout(() => {
			this.checkManifestPath(manifestPath);
		}, CHECK_DEBOUNCE_MS);
		this.#timers.set(manifestPath, timer);
	}

	async checkActivePackage() {
		const manifestPath = findOwningManifestPathForUri(activeDocumentUri());
		if (!manifestPath) {
			vscode.window.showWarningMessage(
				"No owning musi.json for the active file.",
			);
			this.#statusBar.update("No owning musi.json", "stopped");
			return;
		}
		await this.checkManifestPath(manifestPath);
	}

	#shouldCheckDocument(document: vscode.TextDocument): boolean {
		if (this.#mode === "disabled") {
			return false;
		}
		if (this.#mode === "full") {
			return true;
		}
		return path.basename(document.uri.fsPath) === "musi.json";
	}

	async checkManifestPath(manifestPath: string) {
		let pkg: PackageRoot;
		try {
			pkg = await loadPackageRoot(manifestPath);
		} catch (error) {
			this.#statusBar.update("Invalid musi.json", "error");
			vscode.window.showErrorMessage(
				`Failed to read ${path.basename(manifestPath)}: ${String(error)}`,
			);
			return;
		}
		const running = this.#running.get(manifestPath);
		if (running) {
			running.abort();
		}

		const controller = new AbortController();
		this.#running.set(manifestPath, controller);
		this.#statusBar.update(
			`Checking ${path.basename(pkg.rootDir)}`,
			"checking",
		);

		try {
			const result = await runStructuredPackageCheck(pkg, controller.signal);
			if (controller.signal.aborted) {
				return;
			}
			this.publishPackageDiagnostics(pkg, result.payload.diagnostics);
			if (result.exitCode === 0) {
				this.#statusBar.update(`Ready: ${path.basename(pkg.rootDir)}`, "ready");
			} else {
				this.#statusBar.update(
					`Check failed: ${path.basename(pkg.rootDir)}`,
					"error",
				);
			}
		} catch (error) {
			if (controller.signal.aborted) {
				return;
			}
			this.publishPackageDiagnostics(pkg, []);
			this.#statusBar.update(
				`Diagnostics unavailable: ${path.basename(pkg.rootDir)}`,
				"error",
			);
			vscode.window.showErrorMessage(
				`Musi structured diagnostics failed for ${path.basename(pkg.rootDir)}: ${String(error)}`,
			);
		} finally {
			this.#running.delete(manifestPath);
		}
	}

	publishPackageDiagnostics(
		pkg: PackageRoot,
		diagnostics: readonly DiagnosticPayload[],
	) {
		const byFile = new Map<string, vscode.Diagnostic[]>();
		for (const entry of diagnostics) {
			const { filePath, diagnostic } = toDiagnostic(pkg, entry);
			const existing = byFile.get(filePath) ?? [];
			existing.push(diagnostic);
			byFile.set(filePath, existing);
		}

		const tracked =
			this.#trackedFiles.get(pkg.manifestPath) ?? new Set<string>();
		for (const oldFile of tracked) {
			if (!byFile.has(oldFile)) {
				this.#collection.delete(vscode.Uri.file(oldFile));
			}
		}

		for (const [filePath, fileDiagnostics] of byFile) {
			this.#collection.set(vscode.Uri.file(filePath), fileDiagnostics);
		}
		this.#trackedFiles.set(pkg.manifestPath, new Set(byFile.keys()));
	}
}
