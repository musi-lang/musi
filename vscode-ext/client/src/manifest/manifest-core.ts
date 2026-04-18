import type { MsPackageManifest } from "../types.ts";

export const DEFAULT_ENTRY = "index.ms";

export function packageEntry(manifest: MsPackageManifest): string {
	return manifest.entry ?? DEFAULT_ENTRY;
}
