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
	},
	test: {
		environment: "jsdom",
		include: ["src/**/*.test.ts", "src/**/*.test.tsx", "scripts/**/*.test.ts"],
	},
});
