import * as vscode from "vscode";
import { getClient } from "./client.ts";
import { buildExecutionRequest, spawnTestProcess } from "./runner.ts";

interface LspTestItem {
	id: string;
	label: string;
	kind: "suite" | "test";
	range: {
		start: { line: number; character: number };
		end: { line: number; character: number };
	};
}

interface DiscoverTestsResult {
	tests: LspTestItem[];
}

let _controller: vscode.TestController | undefined;
let _runInProgress = false;

export function registerTestController(context: vscode.ExtensionContext) {
	_controller = vscode.tests.createTestController("musiTests", "Musi Tests");
	context.subscriptions.push(_controller);

	_controller.createRunProfile(
		"Run",
		vscode.TestRunProfileKind.Run,
		runHandler,
		true,
	);

	_controller.resolveHandler = async (item) => {
		if (!item) {
			for (const doc of vscode.workspace.textDocuments) {
				if (doc.uri.fsPath.endsWith(".test.ms")) {
					await discoverTestsForFile(doc.uri);
				}
			}
			return;
		}
	};

	const watcher = vscode.workspace.createFileSystemWatcher("**/*.test.ms");
	watcher.onDidChange((uri) => discoverTestsForFile(uri));
	watcher.onDidCreate((uri) => discoverTestsForFile(uri));
	watcher.onDidDelete((uri) => {
		const existing = _controller?.items.get(uri.toString());
		if (existing) _controller?.items.delete(uri.toString());
	});
	context.subscriptions.push(watcher);

	context.subscriptions.push(
		vscode.workspace.onDidOpenTextDocument((doc) => {
			if (doc.uri.fsPath.endsWith(".test.ms")) {
				discoverTestsForFile(doc.uri);
			}
		}),
		vscode.workspace.onDidChangeTextDocument((e) => {
			if (e.document.uri.fsPath.endsWith(".test.ms")) {
				discoverTestsForFile(e.document.uri);
			}
		}),
		// Re-discover after LSP finishes analysis (diagnostics are published post-analysis,
		// so this is a reliable signal that musi/discoverTests will return results).
		vscode.languages.onDidChangeDiagnostics((e) => {
			for (const uri of e.uris) {
				if (uri.fsPath.endsWith(".test.ms")) {
					discoverTestsForFile(uri);
				}
			}
		}),
	);
}

async function discoverTestsForFile(uri: vscode.Uri) {
	if (_runInProgress) return;
	const client = getClient();
	if (!client || !_controller) return;

	try {
		const result = await client.sendRequest<DiscoverTestsResult>(
			"musi/discoverTests",
			{ uri: uri.toString() },
		);

		const fileName = uri.fsPath.split("/").pop() ?? uri.toString();
		const fileItem = _controller.createTestItem(uri.toString(), fileName, uri);

		const children: vscode.TestItem[] = [];
		for (const t of result.tests) {
			const child = _controller.createTestItem(t.id, t.label, uri);
			child.range = new vscode.Range(
				t.range.start.line,
				t.range.start.character,
				t.range.end.line,
				t.range.end.character,
			);
			children.push(child);
		}
		fileItem.children.replace(children);
		_controller.items.add(fileItem);
	} catch {
		// LSP not ready or file not analyzed yet - silently skip
	}
}

async function runHandler(
	request: vscode.TestRunRequest,
	token: vscode.CancellationToken,
) {
	if (!_controller) return;
	const run = _controller.createTestRun(request);
	const items = request.include ?? gatherAllItems(_controller);

	_runInProgress = true;
	try {
		for (const item of items) {
			if (token.isCancellationRequested) break;
			run.started(item);

			const file = item.uri?.fsPath;
			if (!file) {
				run.skipped(item);
				continue;
			}

			const execReq = buildExecutionRequest(file);
			const testName = item.parent ? item.label : undefined;
			const result = await spawnTestProcess(execReq, testName);

			const output = (result.stdout + result.stderr).replace(/\n/g, "\r\n");
			if (output) run.appendOutput(output, undefined, item);

			if (result.exitCode === 0) {
				run.passed(item);
			} else {
				run.failed(
					item,
					new vscode.TestMessage(
						result.stdout || result.stderr || "Test failed",
					),
				);
			}
		}
	} finally {
		_runInProgress = false;
		run.end();
	}
}

function gatherAllItems(controller: vscode.TestController): vscode.TestItem[] {
	const items: vscode.TestItem[] = [];
	controller.items.forEach((item) => {
		items.push(item);
	});
	return items;
}

export async function discoverAllOpenTestFiles() {
	for (const doc of vscode.workspace.textDocuments) {
		if (doc.uri.fsPath.endsWith(".test.ms")) {
			await discoverTestsForFile(doc.uri);
		}
	}
}

export function disposeTestController() {
	_controller?.dispose();
	_controller = undefined;
}
