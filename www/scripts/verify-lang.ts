import { readdir, readFile } from "node:fs/promises";
import { join } from "node:path";

const distDir = join(import.meta.dir, "..", "dist");

const rootHtmlPattern = /<html\b(?<attributes>[^>]*)>/i;
const langAttributePattern = /\blang\s*=\s*(?<quote>["'])en\k<quote>/i;

async function htmlFiles(directory: string): Promise<string[]> {
	const entries = await readdir(directory, { withFileTypes: true });
	const paths: string[] = [];
	for (const entry of entries) {
		const path = join(directory, entry.name);
		if (entry.isDirectory()) {
			paths.push(...(await htmlFiles(path)));
			continue;
		}
		if (entry.name.endsWith(".html")) {
			paths.push(path);
		}
	}
	return paths;
}

async function main() {
	const files = await htmlFiles(distDir);
	if (files.length === 0) {
		throw new Error("no html files found in dist");
	}

	for (const file of files) {
		const html = await readFile(file, "utf8");
		const routePath = file.slice(distDir.length + 1);
		const match = html.match(rootHtmlPattern);
		const groups = match?.groups as { attributes?: string } | undefined;
		const attributes = groups?.attributes;

		if (!attributes) {
			throw new Error(`missing root html tag for ${routePath}`);
		}

		if (!langAttributePattern.test(attributes)) {
			throw new Error(
				`lang mismatch for ${routePath}: expected root lang="en"`,
			);
		}
	}

	console.log(`lang verified for ${files.length} html files`);
}

main().catch((error) => {
	console.error(error);
	process.exitCode = 1;
});
