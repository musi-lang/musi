import { readFile } from "node:fs/promises";
import { join } from "node:path";

const distDir = join(import.meta.dir, "..", "dist");

const routeChecks = [
	{ path: "index.html", lang: "en" },
	{ path: join("community", "index.html"), lang: "en" },
	{ path: join("learn", "index.html"), lang: "en" },
	{ path: join("install", "index.html"), lang: "en" },
] as const;

const htmlLangPattern = /<html lang="(?<lang>[^"]+)"/;

async function main() {
	for (const route of routeChecks) {
		const html = await readFile(join(distDir, route.path), "utf8");
		const match = html.match(htmlLangPattern);
		const groups = match?.groups as { lang?: string } | undefined;
		const lang = groups?.lang;

		if (lang !== route.lang) {
			throw new Error(
				`lang mismatch for ${route.path}: expected ${route.lang}, got ${lang ?? "missing"}`,
			);
		}
	}

	console.log("lang verified for representative prerendered routes");
}

main().catch((error) => {
	console.error(error);
	process.exitCode = 1;
});
