import { mkdir, readFile, writeFile } from "node:fs/promises";
import { dirname, join } from "node:path";
import { renderRoute } from "../src/prerender";
import { appRoutes } from "../src/routes";
import { buildHeadMarkup } from "../src/seo";

const distDir = join(import.meta.dir, "..", "dist");
const templatePath = join(distDir, "index.html");
const seoMarkerPattern =
	/<!-- SEO_HEAD_START -->([\s\S]*?)<!-- SEO_HEAD_END -->/;

function outputPath(routePath: string) {
	return routePath === "/"
		? join(distDir, "index.html")
		: join(distDir, routePath.slice(1), "index.html");
}

function buildSitemap(paths: string[]) {
	const today = new Date().toISOString().slice(0, 10);
	const items = paths
		.map(
			(path) =>
				`<url><loc>https://musi-lang.com${path}</loc><lastmod>${today}</lastmod></url>`,
		)
		.join("");
	return `<?xml version="1.0" encoding="UTF-8"?>\n<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">${items}</urlset>\n`;
}

async function main() {
	const template = await readFile(templatePath, "utf8");
	const paths = new Set<string>();

	for (const route of appRoutes) {
		const html = template
			.replace(
				seoMarkerPattern,
				`<!-- SEO_HEAD_START -->\n\t\t${buildHeadMarkup(route)}\n\t\t<!-- SEO_HEAD_END -->`,
			)
			.replace(
				'<div id="root"></div>',
				`<div id="root">${renderRoute(route)}</div>`,
			);
		const path = outputPath(route.path);
		await mkdir(dirname(path), { recursive: true });
		await writeFile(path, html, "utf8");
		paths.add(route.canonicalPath ?? route.path);
	}

	await writeFile(
		join(distDir, "sitemap.xml"),
		buildSitemap([...paths]),
		"utf8",
	);
	await writeFile(
		join(distDir, "robots.txt"),
		"User-agent: *\nAllow: /\n\nSitemap: https://musi-lang.com/sitemap.xml\n",
		"utf8",
	);
}

main().catch((error) => {
	console.error(error);
	process.exitCode = 1;
});
