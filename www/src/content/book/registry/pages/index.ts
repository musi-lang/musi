import type { RawBookPageDefinition } from "../types";
import { abstractionsPages } from "./abstractions";
import { advancedPages } from "./advanced";
import { corePages } from "./core";
import { dataPages } from "./data";
import { c99DeveloperPages } from "./developers/c99";
import { cpp17DeveloperPages } from "./developers/cpp17";
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
	...c99DeveloperPages,
	...cpp17DeveloperPages,
	...csharpDeveloperPages,
	...goDeveloperPages,
	...javaDeveloperPages,
	...javascriptTypeScriptDeveloperPages,
	...luaDeveloperPages,
	...pythonDeveloperPages,
	...rustDeveloperPages,
] satisfies readonly RawBookPageDefinition[];
