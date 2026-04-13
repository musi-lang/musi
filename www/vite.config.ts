import { exec } from "node:child_process";
import path from "node:path";
import react from "@vitejs/plugin-react";
import { defineConfig, type PluginOption } from "vite";
import { isWatchedContentPath } from "./scripts/content-generator";

function contentHotReloadPlugin(): PluginOption {
	return {
		name: "musi-content-generator",
		handleHotUpdate({ file }) {
			if (isWatchedContentPath(file)) {
				console.log(`Content file changed: ${file}. Regenerating...`);
				exec("bun run scripts/generate-content.ts", (err) => {
					if (err) {
						console.error(`Failed to generate content: ${err.message}`);
						return;
					}
					console.log("Content regenerated successfully.");
				});
			}
		},
	};
}

// https://vitejs.dev/config/
export default defineConfig({
	plugins: [react(), contentHotReloadPlugin()],
	resolve: {
		alias: {
			"@": path.resolve(__dirname, "./src"),
		},
	},
});
