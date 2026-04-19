import * as fs from "node:fs";
import * as path from "node:path";
import * as vscode from "vscode";
import type {
	MsPackageManifest,
	MsTaskDefinition,
	MsTaskSpec,
	PackageRoot,
} from "../types.ts";
import { packageEntry } from "./manifest-core.ts";

const MANIFEST_FILE = "musi.json";

function toTaskSpec(name: string, task: MsTaskDefinition): MsTaskSpec {
	if (typeof task === "string") {
		return {
			name,
			command: task,
			dependencies: [],
		};
	}

	const spec: MsTaskSpec = {
		name,
		command: task.command,
		dependencies: [...(task.dependencies ?? [])],
	};
	if (task.description !== undefined) {
		spec.description = task.description;
	}
	return spec;
}

function parseManifest(text: string): MsPackageManifest {
	return JSON.parse(text) as MsPackageManifest;
}

async function readManifest(manifestPath: string): Promise<MsPackageManifest> {
	const raw = await vscode.workspace.fs.readFile(vscode.Uri.file(manifestPath));
	return parseManifest(Buffer.from(raw).toString("utf8"));
}

function loadManifestSync(manifestPath: string): MsPackageManifest {
	return parseManifest(fs.readFileSync(manifestPath, "utf8"));
}

function toPackageRoot(
	manifestPath: string,
	manifest: MsPackageManifest,
): PackageRoot {
	return {
		manifestUri: vscode.Uri.file(manifestPath).toString(),
		manifestPath,
		rootDir: path.dirname(manifestPath),
		mainEntry: packageEntry(manifest),
		manifest,
	};
}

export async function loadPackageRoot(
	manifestPath: string,
): Promise<PackageRoot> {
	const manifest = await readManifest(manifestPath);
	return toPackageRoot(manifestPath, manifest);
}

export function loadPackageRootSync(manifestPath: string): PackageRoot {
	return toPackageRoot(manifestPath, loadManifestSync(manifestPath));
}

export function findOwningManifestPath(fsPath: string): string | undefined {
	let cursor = fs.statSync(fsPath, { throwIfNoEntry: false })?.isDirectory()
		? fsPath
		: path.dirname(fsPath);

	while (true) {
		const manifestPath = path.join(cursor, MANIFEST_FILE);
		if (fs.existsSync(manifestPath)) {
			return manifestPath;
		}

		const parent = path.dirname(cursor);
		if (parent === cursor) {
			return undefined;
		}
		cursor = parent;
	}
}

export function findOwningManifestPathForUri(
	uri: vscode.Uri | undefined,
): string | undefined {
	if (!uri || uri.scheme !== "file") {
		return undefined;
	}
	if (path.basename(uri.fsPath) === MANIFEST_FILE) {
		return uri.fsPath;
	}
	return findOwningManifestPath(uri.fsPath);
}

export function findWorkspaceManifestPathForUri(
	uri: vscode.Uri | undefined,
): string | undefined {
	if (!uri || uri.scheme !== "file") {
		return undefined;
	}
	let cursor = fs.statSync(uri.fsPath, { throwIfNoEntry: false })?.isDirectory()
		? uri.fsPath
		: path.dirname(uri.fsPath);
	let fallback: string | undefined;
	let workspace: string | undefined;
	while (true) {
		const manifestPath = path.join(cursor, MANIFEST_FILE);
		if (fs.existsSync(manifestPath)) {
			fallback ??= manifestPath;
			try {
				if (loadManifestSync(manifestPath).workspace !== undefined) {
					workspace = manifestPath;
				}
			} catch {
				// Invalid JSON is reported by normal manifest diagnostics.
			}
		}
		const parent = path.dirname(cursor);
		if (parent === cursor) {
			return workspace ?? fallback;
		}
		cursor = parent;
	}
}

export function activeDocumentUri(): vscode.Uri | undefined {
	return vscode.window.activeTextEditor?.document.uri;
}

export function getTaskSpec(
	pkg: PackageRoot,
	taskName: string,
): MsTaskSpec | undefined {
	const task = pkg.manifest.tasks?.[taskName];
	if (!task) {
		return undefined;
	}
	return toTaskSpec(taskName, task);
}

export function taskSpecs(pkg: PackageRoot): MsTaskSpec[] {
	return Object.entries(pkg.manifest.tasks ?? {}).map(([name, task]) =>
		toTaskSpec(name, task),
	);
}

export function taskPlan(pkg: PackageRoot, taskName: string): MsTaskSpec[] {
	const plan: MsTaskSpec[] = [];
	const seen = new Set<string>();
	const active = new Set<string>();

	const visit = (name: string) => {
		if (seen.has(name)) {
			return;
		}
		if (active.has(name)) {
			throw new Error(`task dependency cycle \`${name}\``);
		}
		const task = getTaskSpec(pkg, name);
		if (!task) {
			throw new Error(`unknown task \`${name}\``);
		}
		active.add(name);
		for (const dependency of task.dependencies) {
			visit(dependency);
		}
		active.delete(name);
		seen.add(name);
		plan.push(task);
	};

	visit(taskName);
	return plan;
}

export function taskNameLineMap(
	document: vscode.TextDocument,
): Map<string, number> {
	let manifest: MsPackageManifest;
	try {
		manifest = parseManifest(document.getText());
	} catch {
		return new Map();
	}

	if (!manifest.tasks) {
		return new Map();
	}

	const lines = new Map<string, number>();
	for (const name of Object.keys(manifest.tasks)) {
		const escaped = name.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
		const keyRegex = new RegExp(`"${escaped}"\\s*:`);
		for (let index = 0; index < document.lineCount; index += 1) {
			if (keyRegex.test(document.lineAt(index).text)) {
				lines.set(name, index);
				break;
			}
		}
	}

	return lines;
}
