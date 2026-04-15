import type { RawBookPageDefinition } from "../types";
import { abstractionsPages } from "./abstractions";
import { advancedPages } from "./advanced";
import { corePages } from "./core";
import { dataPages } from "./data";
import { cCppDeveloperPages } from "./developers/c-cpp";
import { csharpDeveloperPages } from "./developers/csharp";
import { goDeveloperPages } from "./developers/go";
import { javaDeveloperPages } from "./developers/java";
import { javascriptTypeScriptDeveloperPages } from "./developers/js-ts";
import { luaDeveloperPages } from "./developers/lua";
import { pythonDeveloperPages } from "./developers/python";
import { rustDeveloperPages } from "./developers/rust";
import { effectsRuntimePages } from "./effects-runtime";
import { organizationPages } from "./organization";
import { startPages } from "./start";
import { typesPages } from "./types";

export const rawBookPages = [
	...startPages,
	...corePages,
	...dataPages,
	...organizationPages,
	...typesPages,
	...abstractionsPages,
	...effectsRuntimePages,
	...advancedPages,
	...cCppDeveloperPages,
	...csharpDeveloperPages,
	...goDeveloperPages,
	...javaDeveloperPages,
	...javascriptTypeScriptDeveloperPages,
	...luaDeveloperPages,
	...pythonDeveloperPages,
	...rustDeveloperPages,
] satisfies readonly RawBookPageDefinition[];
