import { resolve } from "node:path";
import react from "@vitejs/plugin-react";
import type { PluginOption, ViteDevServer } from "vite";
import { defineConfig } from "vitest/config";
import {
	generateContent,
	generatedContentPath,
	isWatchedContentPath,
	watchedContentPaths,
} from "./scripts/content-generator";

function musiContentPlugin(): PluginOption {
	let queue = Promise.resolve();

	async function regenerate(server: ViteDevServer, file: string) {
		queue = queue.then(async () => {
			try {
				await generateContent();
				const generatedModule =
					server.moduleGraph.getModuleById(generatedContentPath);
				if (generatedModule) {
					server.moduleGraph.invalidateModule(generatedModule);
				}
				server.ws.send({
					type: "full-reload",
					path: "*",
				});
			} catch (error) {
				const message =
					error instanceof Error
						? (error.stack ?? error.message)
						: String(error);
				server.config.logger.error(
					`[musi-content-watch] failed after ${file}\n${message}`,
				);
			}
		});

		await queue;
	}

	return {
		name: "musi-content-watch",
		configureServer(server) {
			server.watcher.add([...watchedContentPaths]);

			const onWatchedPathEvent = (file: string) => {
				if (file === generatedContentPath || !isWatchedContentPath(file)) {
					return;
				}

				regenerate(server, file).catch(() => undefined);
			};

			server.watcher.on("add", onWatchedPathEvent);
			server.watcher.on("change", onWatchedPathEvent);
			server.watcher.on("unlink", onWatchedPathEvent);

			server.httpServer?.once("close", () => {
				server.watcher.off("add", onWatchedPathEvent);
				server.watcher.off("change", onWatchedPathEvent);
				server.watcher.off("unlink", onWatchedPathEvent);
			});
		},
		async handleHotUpdate(ctx) {
			if (ctx.file === generatedContentPath) {
				return [];
			}

			if (!isWatchedContentPath(ctx.file)) {
				return;
			}

			await regenerate(ctx.server, ctx.file);
			return [];
		},
	};
}

export default defineConfig({
	root: resolve(import.meta.dirname),
	plugins: [react(), musiContentPlugin()],
	resolve: {
		alias: {
			"@": resolve(import.meta.dirname, "src"),
		},
	},
	build: {
		outDir: "dist",
		emptyOutDir: true,
		rolldownOptions: {
			output: {
				codeSplitting: true,
				manualChunks(id) {
					if (id.includes("src/generated-content.ts")) {
						return "content";
					}

					if (!id.includes("node_modules")) {
						return;
					}

					if (id.includes("@mantine/")) {
						return "vendor-mantine";
					}

					if (id.includes("react")) {
						return "vendor-react";
					}

					return "vendor";
				},
			},
		},
	},
	test: {
		environment: "jsdom",
		include: ["src/**/*.test.ts", "src/**/*.test.tsx", "scripts/**/*.test.ts"],
	},
});
