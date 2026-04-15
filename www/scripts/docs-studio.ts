import { readdir, readFile, writeFile } from "node:fs/promises";
import { basename, join, relative, resolve } from "node:path";
import { marked } from "marked";

const repoRoot = resolve(import.meta.dirname, "..", "..");
const leadingSlashPattern = /^\/+/;
const allowedRoots = [
	{
		id: "language",
		label: "Language Docs",
		root: resolve(repoRoot, "docs", "what", "language"),
	},
	{
		id: "website",
		label: "Website Docs",
		root: resolve(repoRoot, "docs", "what", "website"),
	},
	{
		id: "landings",
		label: "Book Landings",
		root: resolve(repoRoot, "www", "src", "content", "book", "language"),
	},
] as const;

export type DocsStudioRootId = (typeof allowedRoots)[number]["id"];

export type DocsStudioFile = {
	path: string;
	root: DocsStudioRootId;
	title: string;
};

export function docsStudioRoots() {
	return allowedRoots.map(({ id, label, root }) => ({ id, label, root }));
}

export function normalizeDocsStudioPath(inputPath: string) {
	const normalizedInput = inputPath
		.replaceAll("\\", "/")
		.replace(leadingSlashPattern, "");
	const absolutePath = resolve(repoRoot, normalizedInput);

	for (const { root } of allowedRoots) {
		const relativePath = relative(root, absolutePath);
		if (relativePath !== "" && !relativePath.startsWith("..")) {
			return { absolutePath, relativePath };
		}
		if (absolutePath === root) {
			return { absolutePath, relativePath: "" };
		}
	}

	throw new Error(`docs studio path outside allowed roots: ${inputPath}`);
}

export async function listDocsStudioFiles() {
	const files: DocsStudioFile[] = [];

	for (const rootInfo of allowedRoots) {
		const rootFiles = await listMarkdownFiles(rootInfo.root);
		for (const absolutePath of rootFiles) {
			const path = relative(repoRoot, absolutePath).replaceAll("\\", "/");
			files.push({
				path,
				root: rootInfo.id,
				title: titleFromPath(path),
			});
		}
	}

	return files.sort((left, right) => left.path.localeCompare(right.path));
}

export async function readDocsStudioFile(inputPath: string) {
	const { absolutePath } = normalizeDocsStudioPath(inputPath);
	if (!absolutePath.endsWith(".md")) {
		throw new Error(`docs studio only edits Markdown files: ${inputPath}`);
	}
	return await readFile(absolutePath, "utf8");
}

export async function writeDocsStudioFile(inputPath: string, source: string) {
	const { absolutePath } = normalizeDocsStudioPath(inputPath);
	if (!absolutePath.endsWith(".md")) {
		throw new Error(`docs studio only edits Markdown files: ${inputPath}`);
	}
	await writeFile(absolutePath, source, "utf8");
}

export function renderDocsStudioMarkdown(source: string) {
	return marked.parse(source, { async: false }) as string;
}

async function listMarkdownFiles(root: string) {
	const files: string[] = [];

	for (const entry of await readdir(root, { withFileTypes: true })) {
		const path = join(root, entry.name);
		if (entry.isDirectory()) {
			files.push(...(await listMarkdownFiles(path)));
			continue;
		}
		if (entry.isFile() && entry.name.endsWith(".md")) {
			files.push(path);
		}
	}

	return files;
}

function titleFromPath(path: string) {
	return basename(path, ".md")
		.split("-")
		.map((part) => part.slice(0, 1).toUpperCase() + part.slice(1))
		.join(" ");
}

function jsonResponse(body: unknown, init?: ResponseInit) {
	return new Response(JSON.stringify(body), {
		...init,
		headers: {
			"content-type": "application/json; charset=utf-8",
			...init?.headers,
		},
	});
}

function htmlResponse(body: string, init?: ResponseInit) {
	return new Response(body, {
		...init,
		headers: {
			"content-type": "text/html; charset=utf-8",
			...init?.headers,
		},
	});
}

async function routeRequest(request: Request) {
	const url = new URL(request.url);

	if (url.pathname === "/api/files" && request.method === "GET") {
		return await filesResponse();
	}

	if (url.pathname === "/api/file" && request.method === "GET") {
		return await readFileResponse(url);
	}

	if (url.pathname === "/api/file" && request.method === "PUT") {
		return await writeFileResponse(request);
	}

	if (url.pathname === "/api/preview" && request.method === "POST") {
		return await previewResponse(request);
	}

	if (url.pathname === "/" && request.method === "GET") {
		return htmlResponse(docsStudioHtml());
	}

	return jsonResponse({ error: "not found" }, { status: 404 });
}

async function filesResponse() {
	return jsonResponse({ files: await listDocsStudioFiles() });
}

async function readFileResponse(url: URL) {
	const path = url.searchParams.get("path");
	if (!path) {
		return jsonResponse({ error: "missing path" }, { status: 400 });
	}
	return jsonResponse({ path, source: await readDocsStudioFile(path) });
}

async function writeFileResponse(request: Request) {
	const body = (await request.json()) as { path?: string; source?: string };
	if (!body.path || typeof body.source !== "string") {
		return jsonResponse({ error: "missing path or source" }, { status: 400 });
	}
	await writeDocsStudioFile(body.path, body.source);
	return jsonResponse({ ok: true });
}

async function previewResponse(request: Request) {
	const body = (await request.json()) as { source?: string };
	return jsonResponse({ html: renderDocsStudioMarkdown(body.source ?? "") });
}

function docsStudioHtml() {
	return `<!doctype html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>Musi Docs Studio</title>
  <style>
    body { margin: 0; font: 15px/1.5 ui-sans-serif, system-ui, sans-serif; background: #181412; color: #f5eee8; }
    header { border-bottom: 1px solid #6f5a52; padding: 12px 16px; }
    main { display: grid; grid-template-columns: 320px 1fr 1fr; min-height: calc(100vh - 54px); }
    nav, section { min-width: 0; border-right: 1px solid #6f5a52; }
    nav { overflow: auto; padding: 12px; }
    button { display: block; width: 100%; margin: 0 0 6px; padding: 8px; border: 1px solid #6f5a52; background: #241e1b; color: inherit; text-align: left; }
    button:hover, button:focus { border-color: #d38f7b; }
    textarea { box-sizing: border-box; width: 100%; height: 100%; min-height: 80vh; padding: 16px; border: 0; background: #211c19; color: inherit; font: 14px/1.5 ui-monospace, monospace; }
    article { padding: 16px; overflow: auto; }
    code, pre { background: #2c2521; }
  </style>
</head>
<body>
  <header><strong>Musi Docs Studio</strong> <span id="status">local only</span></header>
  <main>
    <nav id="files"></nav>
    <section><textarea id="editor" spellcheck="false"></textarea></section>
    <article id="preview"></article>
  </main>
  <script>
    let currentPath = "";
    const files = document.getElementById("files");
    const editor = document.getElementById("editor");
    const preview = document.getElementById("preview");
    const status = document.getElementById("status");
    async function loadFiles() {
      const response = await fetch("/api/files");
      const data = await response.json();
      files.replaceChildren(...data.files.map((file) => {
        const button = document.createElement("button");
        button.textContent = file.path;
        button.onclick = () => loadFile(file.path);
        return button;
      }));
    }
    async function loadFile(path) {
      currentPath = path;
      const response = await fetch("/api/file?path=" + encodeURIComponent(path));
      const data = await response.json();
      editor.value = data.source;
      status.textContent = path;
      await updatePreview();
    }
    async function updatePreview() {
      const response = await fetch("/api/preview", {
        method: "POST",
        headers: { "content-type": "application/json" },
        body: JSON.stringify({ source: editor.value }),
      });
      const data = await response.json();
      preview.innerHTML = data.html;
    }
    let saveTimer = 0;
    editor.addEventListener("input", () => {
      clearTimeout(saveTimer);
      saveTimer = setTimeout(async () => {
        await updatePreview();
        if (!currentPath) return;
        await fetch("/api/file", {
          method: "PUT",
          headers: { "content-type": "application/json" },
          body: JSON.stringify({ path: currentPath, source: editor.value }),
        });
        status.textContent = currentPath + " saved";
      }, 350);
    });
    loadFiles();
  </script>
</body>
</html>`;
}

if (import.meta.main) {
	const environment = Bun.env as { DOCS_STUDIO_PORT?: string };
	const port = Number(environment.DOCS_STUDIO_PORT ?? "4322");
	Bun.serve({
		hostname: "127.0.0.1",
		port,
		fetch: routeRequest,
	});
	console.log(`docs studio listening on http://127.0.0.1:${port}`);
}
