import { coreSnippets } from "./core";
import { cCppDeveloperSnippets } from "./developers/c-cpp";
import { csharpDeveloperSnippets } from "./developers/csharp";
import { goDeveloperSnippets } from "./developers/go";
import { javaDeveloperSnippets } from "./developers/java";
import { javascriptTypeScriptDeveloperSnippets } from "./developers/js-ts";
import { luaDeveloperSnippets } from "./developers/lua";
import { pythonDeveloperSnippets } from "./developers/python";
import { rustDeveloperSnippets } from "./developers/rust";
import type { ContentSnippet } from "./types";

export type { ContentSnippet, SnippetEvidence } from "./types";

export const contentSnippets = [
	...coreSnippets,
	...cCppDeveloperSnippets,
	...csharpDeveloperSnippets,
	...goDeveloperSnippets,
	...javaDeveloperSnippets,
	...javascriptTypeScriptDeveloperSnippets,
	...luaDeveloperSnippets,
	...pythonDeveloperSnippets,
	...rustDeveloperSnippets,
] satisfies readonly ContentSnippet[];

export function snippetById(id: string) {
	return contentSnippets.find((snippet) => snippet.id === id);
}
