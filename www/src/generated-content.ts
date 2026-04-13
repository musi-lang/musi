export interface GeneratedHeading {
	depth: number;
	id: string;
	text: string;
}

export interface GeneratedDoc {
	id: string;
	kind: "part" | "chapter";
	partId: string;
	partTitle: string;
	path: string;
	canonicalPath: string;
	aliases: string[];
	questions: { label: string; href: string }[];
	title: string;
	description: string;
	descriptionHtml: string;
	group: string;
	section: string;
	order: number;
	slug: string;
	summary: string;
	summaryHtml: string;
	headings: GeneratedHeading[];
	html: string;
}

export const renderedSnippets = {
	"homeSampleHtml": "<div class=\"code-tabs\" data-example-id=\"home-intro\">\n<div class=\"code-tabs-meta\">\n<p class=\"code-tabs-caption\">A real-world example: A function checking a JSON response and parsing coordinates.</p>\n<p class=\"code-tabs-note\">Musi is expression-oriented. Types, bindings, and branches flow sequentially.</p>\n</div>\n<section role=\"tabpanel\" class=\"code-panel\">\n<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> json </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> import</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> \"@std/json\"</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> Point</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> data</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> {</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">  x : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Float</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">  y : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Float</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">};</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> parseLocation</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> (payload : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">String</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">) : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Result</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Point</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">, </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">String</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">] </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">  case</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> json.parse(payload) </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">of</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> (</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">  | </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">.Ok</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(doc) </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">=></span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">      let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> x </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> doc.field(</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\">\"x\"</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">).asFloat().unwrapOr(</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">0.0</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">);</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">      let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> y </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> doc.field(</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\">\"y\"</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">).asFloat().unwrapOr(</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">0.0</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">);</span></span>\n<span class=\"line\"><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">      .Ok</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">({ x </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> x, y </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> y })</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">  | </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">.Err</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(e) </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">=></span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> .Err</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(e)</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">  );</span></span></code></pre>\n</section>\n</div>",
	"installSourceHtml": "<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">git</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> clone</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> https://github.com/musi-lang/musi.git</span></span>\n<span class=\"line\"><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">cd</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> musi</span></span>\n<span class=\"line\"><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">cargo</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> build</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> --release</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">export</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> PATH</span><span style=\"color:#D73A49;--shiki-dark:#F97583\">=</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\">\"/path/to/musi/target/release:</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">$PATH</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\">\"</span></span></code></pre>",
	"quickstartHtml": "<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">musi</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> new</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> hello</span></span>\n<span class=\"line\"><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">cd</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> hello</span></span>\n<span class=\"line\"><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">musi</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> run</span></span></code></pre>"
} as const;

export const renderedDocs = [
	{
		"id": "start",
		"kind": "part",
		"partId": "start",
		"partTitle": "start",
		"path": "/docs/start",
		"canonicalPath": "/docs/start",
		"aliases": [],
		"questions": [],
		"title": "Start",
		"description": "Setup, first file, packages, and imports.",
		"group": "Start",
		"section": "Start",
		"order": 1,
		"slug": "start",
		"summary": "Use this part to get Musi running and move from one file into package work.",
		"descriptionHtml": "Setup, first file, packages, and imports.",
		"headings": [
			{
				"depth": 2,
				"id": "read-this-part",
				"text": "Read this part"
			}
		],
		"html": "<p>Start here if Musi is new to you. These chapters cover setup, first code, package shape, and imports in the order most developers need them.</p>\n<h2 id=\"read-this-part\"><a href=\"#read-this-part\">Read this part</a></h2><p>Move through install, first program, files and packages, then imports. Each chapter ends with a small exercise and a next step.</p>\n",
		"summaryHtml": "Use this part to get Musi running and move from one file into package work."
	},
	{
		"id": "core-language",
		"kind": "part",
		"partId": "core-language",
		"partTitle": "core-language",
		"path": "/docs/core-language",
		"canonicalPath": "/docs/core-language",
		"aliases": [],
		"questions": [],
		"title": "Core language",
		"description": "Expressions, operators, functions, data, and updates.",
		"group": "Core language",
		"section": "Core language",
		"order": 2,
		"slug": "core-language",
		"summary": "Read this part to learn how Musi code flows and how data is shaped.",
		"descriptionHtml": "Expressions, operators, functions, data, and updates.",
		"headings": [
			{
				"depth": 2,
				"id": "read-this-part",
				"text": "Read this part"
			}
		],
		"html": "<p>This part covers the syntax you read every day: expressions, calls, data, records, arrays, and pattern matching.</p>\n<h2 id=\"read-this-part\"><a href=\"#read-this-part\">Read this part</a></h2><p>Start with expressions and bindings, then operators, functions, data, and updates. The goal is to remove syntax guessing early.</p>\n",
		"summaryHtml": "Read this part to learn how Musi code flows and how data is shaped."
	},
	{
		"id": "types-and-abstractions",
		"kind": "part",
		"partId": "types-and-abstractions",
		"partTitle": "types-and-abstractions",
		"path": "/docs/types-and-abstractions",
		"canonicalPath": "/docs/types-and-abstractions",
		"aliases": [],
		"questions": [],
		"title": "Types and abstractions",
		"description": "Types, constraints, effects, quote, and foreign bindings.",
		"group": "Types and abstractions",
		"section": "Types and abstractions",
		"order": 3,
		"slug": "types-and-abstractions",
		"summary": "Use this part once you want reusable APIs, constraints, and effectful code.",
		"descriptionHtml": "Types, constraints, effects, quote, and foreign bindings.",
		"headings": [
			{
				"depth": 2,
				"id": "read-this-part",
				"text": "Read this part"
			}
		],
		"html": "<p>This part groups the features that often raise follow-up questions: types, classes, effects, quote, and foreign declarations.</p>\n<h2 id=\"read-this-part\"><a href=\"#read-this-part\">Read this part</a></h2><p>Move from types into classes, then effects, quote, and foreign declarations. Each chapter keeps syntax and practical use close together.</p>\n",
		"summaryHtml": "Use this part once you want reusable APIs, constraints, and effectful code."
	},
	{
		"id": "tooling",
		"kind": "part",
		"partId": "tooling",
		"partTitle": "tooling",
		"path": "/docs/tooling",
		"canonicalPath": "/docs/tooling",
		"aliases": [],
		"questions": [],
		"title": "Tooling",
		"description": "Standard library, testing, and command-line workflows.",
		"group": "Tooling",
		"section": "Tooling",
		"order": 4,
		"slug": "tooling",
		"summary": "Use this part for package commands, tests, and the standard library split.",
		"descriptionHtml": "Standard library, testing, and command-line workflows.",
		"headings": [
			{
				"depth": 2,
				"id": "read-this-part",
				"text": "Read this part"
			}
		],
		"html": "<p>This part covers the standard library, foundation modules, testing, and the command flow between <code>music</code> and <code>musi</code>.</p>\n<h2 id=\"read-this-part\"><a href=\"#read-this-part\">Read this part</a></h2><p>Start with <code>@std</code> versus <code>musi:*</code>, then move into tests and run/build commands.</p>\n",
		"summaryHtml": "Use this part for package commands, tests, and the standard library split."
	},
	{
		"id": "questions",
		"kind": "part",
		"partId": "questions",
		"partTitle": "questions",
		"path": "/docs/questions",
		"canonicalPath": "/docs/questions",
		"aliases": [],
		"questions": [],
		"title": "Common questions",
		"description": "Direct answers to common Musi tasks.",
		"group": "Questions",
		"section": "Questions",
		"order": 5,
		"slug": "questions",
		"summary": "Use this part when you know the task you want, but not the chapter name.",
		"descriptionHtml": "Direct answers to common Musi tasks.",
		"headings": [
			{
				"depth": 2,
				"id": "read-this-part",
				"text": "Read this part"
			}
		],
		"html": "<p>This part is task-first. Jump here when your question starts with “how do I...?” and you want the shortest path to the right chapter.</p>\n<h2 id=\"read-this-part\"><a href=\"#read-this-part\">Read this part</a></h2><p>Use the task list below, then follow the linked chapter for syntax, explanation, and exercises.</p>\n",
		"summaryHtml": "Use this part when you know the task you want, but not the chapter name."
	},
	{
		"id": "getting-started",
		"kind": "chapter",
		"partId": "start",
		"partTitle": "start",
		"path": "/docs/start/getting-started",
		"canonicalPath": "/docs/start/getting-started",
		"aliases": [
			"/docs/getting-started"
		],
		"questions": [
			{
				"label": "How do I install Musi and learn the <code>music</code> / <code>musi</code> split?",
				"href": "/docs/start/getting-started"
			}
		],
		"title": "Getting started",
		"description": "Install the tools, know what they do, and start with the right command.",
		"group": "Start",
		"section": "Start",
		"order": 1,
		"slug": "getting-started",
		"summary": "Install, PATH setup, and the difference between <code>musi</code> and <code>music</code>.",
		"descriptionHtml": "Install the tools, know what they do, and start with the right command.",
		"headings": [
			{
				"depth": 2,
				"id": "two-commands",
				"text": "Two commands"
			},
			{
				"depth": 2,
				"id": "first-setup-pass",
				"text": "First setup pass"
			},
			{
				"depth": 2,
				"id": "try-it",
				"text": "Try it"
			},
			{
				"depth": 2,
				"id": "next-step",
				"text": "Next step"
			}
		],
		"html": "<p>Musi has two CLI entry points. Learn that split first and the rest of the toolchain gets simpler.</p>\n<h2 id=\"two-commands\"><a href=\"#two-commands\">Two commands</a></h2><ul>\n<li><code>musi</code> works at package level. Use it for <code>run</code>, <code>check</code>, <code>build</code>, and <code>test</code>.</li>\n<li><code>music</code> works on one source graph or built artifact directly.</li>\n</ul>\n<p>If you already know tools like <code>cargo</code>, <code>npm</code>, or <code>dotnet</code>, <code>musi</code> fills that role. <code>music</code> is closer to a direct file runner.</p>\n<h2 id=\"first-setup-pass\"><a href=\"#first-setup-pass\">First setup pass</a></h2><p>Start at the <a href=\"/install\">install page</a>, then follow this order:</p>\n<ul>\n<li>install binaries and PATH entries</li>\n<li>create a package</li>\n<li>add first expressions</li>\n<li>run <code>musi check</code> and <code>musi run</code></li>\n</ul>\n<p>If you want the shortest feedback loop, start with one file and <code>music</code>. Move to <code>musi</code> once you want package commands and shared project structure.</p>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><div class=\"try-block\"><ol><li>Install Rust and libffi.</li><li>Build Musi from source.</li><li>Run <code>music check index.ms</code> once <code>music</code> is on PATH.</li></ol></div><h2 id=\"next-step\"><a href=\"#next-step\">Next step</a></h2><p>Open <a href=\"/install\">Install</a>, make one command work, then continue to <a href=\"/docs/start/first-program\">First program</a>.</p>\n",
		"summaryHtml": "Install, PATH setup, and the difference between <code>musi</code> and <code>music</code>."
	},
	{
		"id": "first-program",
		"kind": "chapter",
		"partId": "start",
		"partTitle": "start",
		"path": "/docs/start/first-program",
		"canonicalPath": "/docs/start/first-program",
		"aliases": [
			"/docs/first-program"
		],
		"questions": [
			{
				"label": "How do I write and run the smallest Musi file?",
				"href": "/docs/start/first-program"
			}
		],
		"title": "First program",
		"description": "Write a small file and read it as expressions.",
		"group": "Start",
		"section": "Start",
		"order": 2,
		"slug": "first-program",
		"summary": "A first Musi file without extra ceremony.",
		"descriptionHtml": "Write a small file and read it as expressions.",
		"headings": [
			{
				"depth": 2,
				"id": "smallest-runnable-file",
				"text": "Smallest runnable file"
			},
			{
				"depth": 2,
				"id": "add-one-function",
				"text": "Add one function"
			},
			{
				"depth": 2,
				"id": "try-it",
				"text": "Try it"
			},
			{
				"depth": 2,
				"id": "next-step",
				"text": "Next step"
			}
		],
		"html": "<p>Musi files are read as expressions.\nRead top to bottom. Each expression leaves a result for the next one to use.</p>\n<h2 id=\"smallest-runnable-file\"><a href=\"#smallest-runnable-file\">Smallest runnable file</a></h2><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> answer </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 42</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">answer;</span></span></code></pre><p><code>let</code> binds a name. <code>;</code> ends an expression. That is enough to start writing and running Musi code.</p>\n<h2 id=\"add-one-function\"><a href=\"#add-one-function\">Add one function</a></h2><div class=\"code-tabs\" data-example-id=\"double-function\">\n<div class=\"code-tabs-meta\">\n<p class=\"code-tabs-caption\">Same small task across multiple languages. Musi keeps it as an expression-oriented <code>let</code> binding.</p>\n<p class=\"code-tabs-note\">Like doubling a recipe: same operation, different kitchens. Musi keeps the function as a normal binding so it reads like other definitions.</p>\n</div>\n<section role=\"tabpanel\" class=\"code-panel\">\n<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> twice</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> (x : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">) : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> x </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">+</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> x;</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> answer </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> twice</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">21</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">);</span></span></code></pre>\n</section>\n</div><p>Functions use the same <code>let</code> form as values. You do not switch to a separate declaration syntax.</p>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><div class=\"try-block\"><ol><li>Create <code>index.ms</code>.</li><li>Bind one value with <code>let</code>.</li><li>Run it with <code>music check index.ms</code>.</li></ol></div><h2 id=\"next-step\"><a href=\"#next-step\">Next step</a></h2><p>Type the snippets above, run them with <code>music</code>, then move to <a href=\"/docs/start/files-packages-and-entry\">Files, packages, and entry</a>.</p>\n",
		"summaryHtml": "A first Musi file without extra ceremony."
	},
	{
		"id": "files-packages-and-entry",
		"kind": "chapter",
		"partId": "start",
		"partTitle": "start",
		"path": "/docs/start/files-packages-and-entry",
		"canonicalPath": "/docs/start/files-packages-and-entry",
		"aliases": [
			"/docs/files-packages-and-entry"
		],
		"questions": [
			{
				"label": "How do I switch from one file to a package?",
				"href": "/docs/start/files-packages-and-entry"
			}
		],
		"title": "Files, packages, and entry",
		"description": "Know what <code>musi new</code> creates and what <code>musi run</code> looks for.",
		"group": "Start",
		"section": "Start",
		"order": 3,
		"slug": "files-packages-and-entry",
		"summary": "Packages, <code>musi.json</code>, and the resolved entry file.",
		"descriptionHtml": "Know what <code>musi new</code> creates and what <code>musi run</code> looks for.",
		"headings": [
			{
				"depth": 2,
				"id": "package-workflow",
				"text": "Package workflow"
			},
			{
				"depth": 2,
				"id": "why-packages-help",
				"text": "Why packages help"
			},
			{
				"depth": 2,
				"id": "direct-mode",
				"text": "Direct mode"
			},
			{
				"depth": 2,
				"id": "try-it",
				"text": "Try it"
			},
			{
				"depth": 2,
				"id": "next-step",
				"text": "Next step"
			}
		],
		"html": "<p>Use plain files while you experiment. Use a package once you want repeatable commands and a stable project root.</p>\n<h2 id=\"package-workflow\"><a href=\"#package-workflow\">Package workflow</a></h2><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">musi</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> run</span></span>\n<span class=\"line\"><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">musi</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> check</span></span>\n<span class=\"line\"><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">musi</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> build</span></span>\n<span class=\"line\"><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">musi</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> test</span></span></code></pre><ul>\n<li><code>musi</code> reads package config, resolves the entry file, and runs project commands.</li>\n<li><code>music</code> stays useful for direct checks on one source file or built artifact.</li>\n</ul>\n<h2 id=\"why-packages-help\"><a href=\"#why-packages-help\">Why packages help</a></h2><p>Packages remove repeated path handling. They also give everyone on a project the same command surface.</p>\n<h2 id=\"direct-mode\"><a href=\"#direct-mode\">Direct mode</a></h2><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">music</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> check</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> index.ms</span></span>\n<span class=\"line\"><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">music</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> build</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> index.ms</span></span>\n<span class=\"line\"><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">music</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> run</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> index.seam</span></span></code></pre><p>Use direct mode for one-off experiments, parser checks, and small examples that do not need package metadata yet.</p>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><div class=\"try-block\"><ol><li>Create a package with <code>musi new hello</code>.</li><li>Open the generated entry file.</li><li>Run <code>musi run</code> from the package root.</li></ol></div><h2 id=\"next-step\"><a href=\"#next-step\">Next step</a></h2><p>Create a package, confirm which entry file is used, then continue to <a href=\"/docs/start/imports-and-packages\">Imports and packages</a>.</p>\n",
		"summaryHtml": "Packages, <code>musi.json</code>, and the resolved entry file."
	},
	{
		"id": "imports-and-packages",
		"kind": "chapter",
		"partId": "start",
		"partTitle": "start",
		"path": "/docs/start/imports-and-packages",
		"canonicalPath": "/docs/start/imports-and-packages",
		"aliases": [
			"/docs/imports-and-packages"
		],
		"questions": [
			{
				"label": "How do I import <code>@std</code> modules and when do I use <code>musi:*</code>?",
				"href": "/docs/start/imports-and-packages"
			}
		],
		"title": "Imports and packages",
		"description": "Import modules and use the main namespace families.",
		"group": "Core language",
		"section": "Core language",
		"order": 4,
		"slug": "imports-and-packages",
		"summary": "Import expressions, <code>@std</code>, and the <code>musi:*</code> foundation namespace.",
		"descriptionHtml": "Import modules and use the main namespace families.",
		"headings": [
			{
				"depth": 2,
				"id": "default-rule",
				"text": "Default rule"
			},
			{
				"depth": 2,
				"id": "example",
				"text": "Example"
			},
			{
				"depth": 2,
				"id": "try-it",
				"text": "Try it"
			},
			{
				"depth": 2,
				"id": "next-step",
				"text": "Next step"
			}
		],
		"html": "<p>Imports are expressions. Bind them with <code>let</code>, then use the imported value like any other name.</p>\n<h2 id=\"default-rule\"><a href=\"#default-rule\">Default rule</a></h2><ul>\n<li>Start with <code>@std</code>.</li>\n<li>Reach for <code>musi:*</code> only when you need lower-level foundation modules.</li>\n</ul>\n<p>That keeps application code on the standard library path and keeps compiler-facing pieces explicit.</p>\n<h2 id=\"example\"><a href=\"#example\">Example</a></h2><div class=\"code-tabs\" data-example-id=\"import-stdlib\">\n<div class=\"code-tabs-meta\">\n<p class=\"code-tabs-caption\">Import the standard library, then reach the family you need. Musi keeps stdlib access explicit through <code>@std</code>.</p>\n<p class=\"code-tabs-note\">Like checking out a toolbox before work: import once, then use the tools by name. In Musi, imports are values you can pass around.</p>\n</div>\n<section role=\"tabpanel\" class=\"code-panel\">\n<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> Option</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> import</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> \"@std/option\"</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> value </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> Option</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">.some(</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">1</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">);</span></span></code></pre>\n</section>\n</div><p>Imports can appear anywhere an expression can appear, but keeping them near the top of a file is still the easiest style to read.</p>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><div class=\"try-block\"><ol><li>Import <code>@std/option</code> in one file.</li><li>Bind the module with <code>let</code>.</li><li>Call one exported function from it.</li></ol></div><h2 id=\"next-step\"><a href=\"#next-step\">Next step</a></h2><p>Replace duplicated snippets with imported names, then continue to <a href=\"/docs/core-language/expressions-and-bindings\">Expressions and bindings</a>.</p>\n",
		"summaryHtml": "Import expressions, <code>@std</code>, and the <code>musi:*</code> foundation namespace."
	},
	{
		"id": "expressions-and-bindings",
		"kind": "chapter",
		"partId": "core-language",
		"partTitle": "core-language",
		"path": "/docs/core-language/expressions-and-bindings",
		"canonicalPath": "/docs/core-language/expressions-and-bindings",
		"aliases": [
			"/docs/expressions-and-bindings"
		],
		"questions": [
			{
				"label": "How do I read a Musi file top to bottom?",
				"href": "/docs/core-language/expressions-and-bindings"
			}
		],
		"title": "Expressions and bindings",
		"description": "Read Musi through <code>let</code>, sequences, and <code>case</code>.",
		"group": "Core language",
		"section": "Core language",
		"order": 5,
		"slug": "expressions-and-bindings",
		"summary": "The base reading model for names, sequences, and branching.",
		"descriptionHtml": "Read Musi through <code>let</code>, sequences, and <code>case</code>.",
		"headings": [
			{
				"depth": 2,
				"id": "sequencing",
				"text": "Sequencing"
			},
			{
				"depth": 2,
				"id": "branching",
				"text": "Branching"
			},
			{
				"depth": 2,
				"id": "try-it",
				"text": "Try it"
			},
			{
				"depth": 2,
				"id": "next-step",
				"text": "Next step"
			}
		],
		"html": "<p>Expressions and bindings are the core reading model in Musi. Bind a name, then keep reading downward.</p>\n<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> port </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 8080</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">port;</span></span></code></pre><h2 id=\"sequencing\"><a href=\"#sequencing\">Sequencing</a></h2><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">  let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> base </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 8000</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">  base </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">+</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 80</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">);</span></span></code></pre><p><code>;</code> separates expressions. Parentheses can group a sequence into one larger expression.</p>\n<h2 id=\"branching\"><a href=\"#branching\">Branching</a></h2><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> Port</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> data</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> {</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">  | </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Configured</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">  | </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Default</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">};</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> port : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Port</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> .Configured</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">8080</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">);</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">case</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> port </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">of</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> (</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">| </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">.Configured</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(port) </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">=></span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> port</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">| </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">.Default</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> =></span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 3000</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">);</span></span></code></pre><p><code>case ... of</code> handles branching. Match on shape, then return a value from each branch.</p>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><div class=\"try-block\"><ol><li>Write two <code>let</code> bindings.</li><li>Add a <code>case</code> expression under them.</li><li>Read the file top to bottom and check the final result.</li></ol></div><h2 id=\"next-step\"><a href=\"#next-step\">Next step</a></h2><p>Read the two snippets, then move to <a href=\"/docs/core-language/operators-and-literals\">Operators and literals</a>.</p>\n",
		"summaryHtml": "The base reading model for names, sequences, and branching."
	},
	{
		"id": "operators-and-literals",
		"kind": "chapter",
		"partId": "core-language",
		"partTitle": "core-language",
		"path": "/docs/core-language/operators-and-literals",
		"canonicalPath": "/docs/core-language/operators-and-literals",
		"aliases": [
			"/docs/operators-and-literals"
		],
		"questions": [
			{
				"label": "How do operators, records, and arrays fit in ordinary expressions?",
				"href": "/docs/core-language/operators-and-literals"
			}
		],
		"title": "Operators and literals",
		"description": "Read numbers, strings, records, arrays, and operator chains in the same surface.",
		"group": "Core language",
		"section": "Core language",
		"order": 6,
		"slug": "operators-and-literals",
		"summary": "Literal forms and operator precedence in ordinary expressions.",
		"descriptionHtml": "Read numbers, strings, records, arrays, and operator chains in the same surface.",
		"headings": [
			{
				"depth": 2,
				"id": "common-forms",
				"text": "Common forms"
			},
			{
				"depth": 3,
				"id": "operator-intent",
				"text": "Operator intent"
			},
			{
				"depth": 2,
				"id": "examples",
				"text": "Examples"
			},
			{
				"depth": 2,
				"id": "try-it",
				"text": "Try it"
			},
			{
				"depth": 2,
				"id": "next-step",
				"text": "Next step"
			}
		],
		"html": "<p>Operators and literals cover most day-one code: numbers, strings, arrays, records, comparisons, and small calculations.</p>\n<h2 id=\"common-forms\"><a href=\"#common-forms\">Common forms</a></h2><ul>\n<li>numeric literals like <code>8080</code></li>\n<li>string literals like <code>&quot;ready&quot;</code></li>\n<li>symbolic operators like <code>+</code>, <code>-</code>, <code>*</code>, <code>/</code>, <code>=</code>, <code>&lt;=</code>, <code>&gt;=</code>, and <code>/=</code></li>\n<li>word-like operators like <code>and</code>, <code>or</code>, <code>not</code>, <code>shl</code>, <code>shr</code>, and <code>xor</code></li>\n</ul>\n<h3 id=\"operator-intent\"><a href=\"#operator-intent\">Operator intent</a></h3><ul>\n<li><code>and</code>, <code>or</code>, <code>not</code>, and <code>xor</code> are word operators whose meaning depends on operand types and operator definitions in scope.</li>\n<li><code>shl</code> and <code>shr</code> are shift operators.</li>\n<li><code>/=</code> means not equal. It is not divide-and-assign.</li>\n</ul>\n<p>If you are coming from languages where <code>/=</code> means divide-and-assign, use <code>x := x / y</code> when you want reassignment from division.</p>\n<h2 id=\"examples\"><a href=\"#examples\">Examples</a></h2><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> port </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 8080</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> label </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> \"ready\"</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> next </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> port </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">+</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 1</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> same </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> next </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> port </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">+</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 1</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> capped </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> port </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">&#x3C;=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 9000</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> masked </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 1</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> shl</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 3</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span></code></pre><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> point </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> { x </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 3</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">, y </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 4</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> };</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> values </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> [</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">1</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">, </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">2</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">, </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">3</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">];</span></span></code></pre><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> point3 </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> { </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">...</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">point, z </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 5</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> };</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> extended </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> [</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">0</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">, </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">...</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">values];</span></span></code></pre><h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><div class=\"try-block\"><ol><li>Make one record literal and one array literal.</li><li>Apply one operator expression.</li><li>Create one spread-based update.</li></ol></div><h2 id=\"next-step\"><a href=\"#next-step\">Next step</a></h2><p>Combine a few literals and operators in one file, then continue to <a href=\"/docs/core-language/functions-and-calls\">Functions and calls</a>.</p>\n",
		"summaryHtml": "Literal forms and operator precedence in ordinary expressions."
	},
	{
		"id": "functions-and-calls",
		"kind": "chapter",
		"partId": "core-language",
		"partTitle": "core-language",
		"path": "/docs/core-language/functions-and-calls",
		"canonicalPath": "/docs/core-language/functions-and-calls",
		"aliases": [
			"/docs/functions-and-calls"
		],
		"questions": [
			{
				"label": "How do I define functions and recursion with <code>let</code>?",
				"href": "/docs/core-language/functions-and-calls"
			}
		],
		"title": "Functions and calls",
		"description": "Define functions with <code>let</code>, call them normally, and use <code>let rec</code> for recursion.",
		"group": "Core language",
		"section": "Core language",
		"order": 7,
		"slug": "functions-and-calls",
		"summary": "Functions, calls, and recursion without extra control syntax.",
		"descriptionHtml": "Define functions with <code>let</code>, call them normally, and use <code>let rec</code> for recursion.",
		"headings": [
			{
				"depth": 2,
				"id": "basic-call",
				"text": "Basic call"
			},
			{
				"depth": 2,
				"id": "recursion",
				"text": "Recursion"
			},
			{
				"depth": 2,
				"id": "try-it",
				"text": "Try it"
			},
			{
				"depth": 2,
				"id": "next-step",
				"text": "Next step"
			}
		],
		"html": "<p>Functions are values you can bind, pass, and call.</p>\n<h2 id=\"basic-call\"><a href=\"#basic-call\">Basic call</a></h2><div class=\"code-tabs\" data-example-id=\"double-function\">\n<div class=\"code-tabs-meta\">\n<p class=\"code-tabs-caption\">Same small task across multiple languages. Musi keeps it as an expression-oriented <code>let</code> binding.</p>\n<p class=\"code-tabs-note\">Like doubling a recipe: same operation, different kitchens. Musi keeps the function as a normal binding so it reads like other definitions.</p>\n</div>\n<section role=\"tabpanel\" class=\"code-panel\">\n<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> twice</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> (x : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">) : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> x </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">+</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> x;</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> answer </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> twice</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">21</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">);</span></span></code></pre>\n</section>\n</div><p>Calls look ordinary: <code>name(args)</code>. Function definitions use the same <code>let</code> syntax as other bindings.</p>\n<h2 id=\"recursion\"><a href=\"#recursion\">Recursion</a></h2><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> rec</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> loop</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> (x : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">) : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">  case</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> x </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">of</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> (</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">  | </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">0</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> =></span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 0</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">  | </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">_</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> =></span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> loop</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(x </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">-</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 1</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">)</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">  );</span></span></code></pre><p>Use <code>let rec</code> when a function needs to refer to itself.</p>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><div class=\"try-block\"><ol><li>Write one function with <code>let</code>.</li><li>Call it with one argument.</li><li>Add <code>let rec</code> and check recursive flow.</li></ol></div><h2 id=\"next-step\"><a href=\"#next-step\">Next step</a></h2><p>Bind and call a function, then add the recursive form and continue to <a href=\"/docs/core-language/data-and-pattern-matching\">Data and pattern matching</a>.</p>\n",
		"summaryHtml": "Functions, calls, and recursion without extra control syntax."
	},
	{
		"id": "data-and-pattern-matching",
		"kind": "chapter",
		"partId": "core-language",
		"partTitle": "core-language",
		"path": "/docs/core-language/data-and-pattern-matching",
		"canonicalPath": "/docs/core-language/data-and-pattern-matching",
		"aliases": [
			"/docs/data-and-pattern-matching"
		],
		"questions": [
			{
				"label": "How do I model variants and branch with <code>case</code>?",
				"href": "/docs/core-language/data-and-pattern-matching"
			}
		],
		"title": "Data and pattern matching",
		"description": "Define sums with <code>data</code>, construct variants, and read them with <code>case</code>.",
		"group": "Core language",
		"section": "Core language",
		"order": 8,
		"slug": "data-and-pattern-matching",
		"summary": "Data definitions, constructors, and pattern matching.",
		"descriptionHtml": "Define sums with <code>data</code>, construct variants, and read them with <code>case</code>.",
		"headings": [
			{
				"depth": 2,
				"id": "match-first",
				"text": "Match first"
			},
			{
				"depth": 2,
				"id": "define-and-construct",
				"text": "Define and construct"
			},
			{
				"depth": 2,
				"id": "compare",
				"text": "Compare"
			},
			{
				"depth": 2,
				"id": "try-it",
				"text": "Try it"
			},
			{
				"depth": 2,
				"id": "next-step",
				"text": "Next step"
			}
		],
		"html": "<p>Use <code>data</code> to model bounded domains directly in code.\nThe <code>case</code> form reads shape by shape and keeps branching explicit.</p>\n<h2 id=\"match-first\"><a href=\"#match-first\">Match first</a></h2><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">case</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> port </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">of</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> (</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">| </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">.Configured</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(value) </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">=></span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> value</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">| </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">.Default</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> =></span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 3000</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">);</span></span></code></pre><p>Use <code>data</code> when a value can be one of several known shapes.</p>\n<h2 id=\"define-and-construct\"><a href=\"#define-and-construct\">Define and construct</a></h2><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> Port</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> data</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> {</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">  | </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Configured</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">  | </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Default</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">};</span></span></code></pre><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> port : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Port</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> .Configured</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">8080</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">);</span></span></code></pre><h2 id=\"compare\"><a href=\"#compare\">Compare</a></h2><div class=\"code-tabs\" data-example-id=\"data-named-record\">\n<div class=\"code-tabs-meta\">\n<p class=\"code-tabs-caption\">Use named fields directly in a <code>data</code> definition, then construct values from that shape.</p>\n<p class=\"code-tabs-note\">Like filling out a passport form: named boxes with known defaults. This form keeps field intent explicit in the type itself.</p>\n</div>\n<section role=\"tabpanel\" class=\"code-panel\">\n<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> User</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> data</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> {</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">  name : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">String</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">  age : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 0</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">};</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> user : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">User</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> { name </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> \"Ada\"</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> };</span></span></code></pre>\n</section>\n</div><h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><div class=\"try-block\"><ol><li>Define a <code>data</code> type with two cases.</li><li>Construct one value.</li><li>Match it with <code>case ... of</code>.</li></ol></div><h2 id=\"next-step\"><a href=\"#next-step\">Next step</a></h2><p>Read the three snippets in order, then continue to <a href=\"/docs/core-language/records-arrays-and-mutation\">Records and arrays</a>.</p>\n",
		"summaryHtml": "Data definitions, constructors, and pattern matching."
	},
	{
		"id": "records-arrays-and-mutation",
		"kind": "chapter",
		"partId": "core-language",
		"partTitle": "core-language",
		"path": "/docs/core-language/records-arrays-and-mutation",
		"canonicalPath": "/docs/core-language/records-arrays-and-mutation",
		"aliases": [
			"/docs/records-arrays-and-mutation"
		],
		"questions": [
			{
				"label": "How do I update records and arrays without guessing syntax?",
				"href": "/docs/core-language/records-arrays-and-mutation"
			}
		],
		"title": "Records and arrays",
		"description": "Use record literals, arrays, and explicit spread forms.",
		"group": "Core language",
		"section": "Core language",
		"order": 9,
		"slug": "records-arrays-and-mutation",
		"summary": "Structured values and the current writeable-data surface.",
		"descriptionHtml": "Use record literals, arrays, and explicit spread forms.",
		"headings": [
			{
				"depth": 2,
				"id": "update-by-copy",
				"text": "Update by copy"
			},
			{
				"depth": 2,
				"id": "compare",
				"text": "Compare"
			},
			{
				"depth": 2,
				"id": "musi-note",
				"text": "Musi note"
			},
			{
				"depth": 2,
				"id": "try-it",
				"text": "Try it"
			},
			{
				"depth": 2,
				"id": "next-step",
				"text": "Next step"
			}
		],
		"html": "<p>Records and arrays are ordinary values with predictable update patterns.</p>\n<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> point </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> { x </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 3</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">, y </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 4</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> };</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> values </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> [</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">1</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">, </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">2</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">, </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">3</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">];</span></span></code></pre><h2 id=\"update-by-copy\"><a href=\"#update-by-copy\">Update by copy</a></h2><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> point3 </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> { </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">...</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">point, z </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 5</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> };</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> extended </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> [</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">0</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">, </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">...</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">values];</span></span></code></pre><h2 id=\"compare\"><a href=\"#compare\">Compare</a></h2><div class=\"code-tabs\" data-example-id=\"record-array-spread\">\n<div class=\"code-tabs-meta\">\n<p class=\"code-tabs-caption\">Build structured values, spread them, and update selected fields in one expression flow.</p>\n<p class=\"code-tabs-note\">Like copying a form and editing only one line instead of rewriting everything. Spread/update keeps the unchanged parts intact. Musi also has a nested record-update form inspired by F# and OCaml; the note below covers that separately.</p>\n</div>\n<section role=\"tabpanel\" class=\"code-panel\">\n<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> xs </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> [</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">1</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">, </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">2</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">];</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> ys </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> [</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">0</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">, </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">...</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">xs, </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">3</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">];</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> p </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> { x </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 1</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">, y </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 2</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> };</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> q </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> { </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">...</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">p, y </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 9</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> };</span></span></code></pre>\n</section>\n</div><h2 id=\"musi-note\"><a href=\"#musi-note\">Musi note</a></h2><p>Musi also supports nested record update syntax such as <code>let r := p.{ x := 3 };</code> and <code>let r := p.{ ...q, y := 9 };</code>.\nThat form is separate from the cross-language comparison above and comes from the same record-update family used in F# and OCaml.</p>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><div class=\"try-block\"><ol><li>Create one record value.</li><li>Create one array value.</li><li>Build a new value with spread syntax.</li></ol></div><h2 id=\"next-step\"><a href=\"#next-step\">Next step</a></h2><p>Create a base value then build one spread-based variant, then continue to <a href=\"/docs/types-and-abstractions/types\">Types and generics</a>.</p>\n",
		"summaryHtml": "Structured values and the current writeable-data surface."
	},
	{
		"id": "types",
		"kind": "chapter",
		"partId": "types-and-abstractions",
		"partTitle": "types-and-abstractions",
		"path": "/docs/types-and-abstractions/types",
		"canonicalPath": "/docs/types-and-abstractions/types",
		"aliases": [
			"/docs/types"
		],
		"questions": [
			{
				"label": "How do I add type annotations and generics?",
				"href": "/docs/types-and-abstractions/types"
			}
		],
		"title": "Types and generics",
		"description": "Read type annotations and generic parameters in the same surface as values.",
		"group": "Types",
		"section": "Types",
		"order": 10,
		"slug": "types",
		"summary": "Type annotations, generic parameters, and direct type application.",
		"descriptionHtml": "Read type annotations and generic parameters in the same surface as values.",
		"headings": [
			{
				"depth": 2,
				"id": "generics",
				"text": "Generics"
			},
			{
				"depth": 2,
				"id": "compare",
				"text": "Compare"
			},
			{
				"depth": 2,
				"id": "try-it",
				"text": "Try it"
			},
			{
				"depth": 2,
				"id": "next-step",
				"text": "Next step"
			}
		],
		"html": "<p>Types in Musi appear near values and functions.\nYou can read types without switching to a separate declaration section.</p>\n<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> port : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 8080</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> identityFn</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">T</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">] (input : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">T</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">) : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">T</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> input;</span></span></code></pre><h2 id=\"generics\"><a href=\"#generics\">Generics</a></h2><p>Add annotations to values and functions, then apply generics where reusable behavior is needed.\nStart with concrete types first. Reach for generics after the single-type version already makes sense.</p>\n<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">identityFn[</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">](port);</span></span></code></pre><h2 id=\"compare\"><a href=\"#compare\">Compare</a></h2><div class=\"code-tabs\" data-example-id=\"generic-constraint\">\n<div class=\"code-tabs-meta\">\n<p class=\"code-tabs-caption\">Constrain polymorphic code to capabilities that must exist at call sites.</p>\n<p class=\"code-tabs-note\">Like requiring a driving license before renting a car: callers must provide the needed capability. Musi writes that requirement with <code>where T : Eq</code> before the result type annotation.</p>\n</div>\n<section role=\"tabpanel\" class=\"code-panel\">\n<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> requireEq</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">T</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">] (x : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">T</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">) </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">where</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> T</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Eq</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">T</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> x;</span></span></code></pre>\n</section>\n</div><h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><div class=\"try-block\"><ol><li>Add a type annotation to one binding.</li><li>Add a generic parameter to one function.</li><li>Apply that generic explicitly.</li></ol></div><h2 id=\"next-step\"><a href=\"#next-step\">Next step</a></h2><p>Try the two snippets before moving to <a href=\"/docs/types-and-abstractions/classes-instances-and-laws\">Classes and instances</a>.</p>\n",
		"summaryHtml": "Type annotations, generic parameters, and direct type application."
	},
	{
		"id": "classes-instances-and-laws",
		"kind": "chapter",
		"partId": "types-and-abstractions",
		"partTitle": "types-and-abstractions",
		"path": "/docs/types-and-abstractions/classes-instances-and-laws",
		"canonicalPath": "/docs/types-and-abstractions/classes-instances-and-laws",
		"aliases": [
			"/docs/classes-instances-and-laws"
		],
		"questions": [
			{
				"label": "How do I define a class and add an instance?",
				"href": "/docs/types-and-abstractions/classes-instances-and-laws"
			}
		],
		"title": "Classes and instances",
		"description": "Read the class surface and define instances.",
		"group": "Abstractions",
		"section": "Abstractions",
		"order": 11,
		"slug": "classes-instances-and-laws",
		"summary": "Classes, methods, and instance declarations.",
		"descriptionHtml": "Read the class surface and define instances.",
		"headings": [
			{
				"depth": 2,
				"id": "class",
				"text": "Class"
			},
			{
				"depth": 2,
				"id": "instance",
				"text": "Instance"
			},
			{
				"depth": 2,
				"id": "compare",
				"text": "Compare"
			},
			{
				"depth": 2,
				"id": "try-it",
				"text": "Try it"
			},
			{
				"depth": 2,
				"id": "next-step",
				"text": "Next step"
			}
		],
		"html": "<p>Classes define shared behavior names.\nInstances provide concrete implementations for those behavior names.</p>\n<h2 id=\"class\"><a href=\"#class\">Class</a></h2><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> Eq</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">T</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">] </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> class</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> {</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">  let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> (</span><span style=\"color:#D73A49;--shiki-dark:#F97583\">=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">) (a : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">T</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">, b : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">T</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">) : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Bool</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">  law</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> reflexive</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> (x : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">T</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">) </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> .True</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">};</span></span></code></pre><h2 id=\"instance\"><a href=\"#instance\">Instance</a></h2><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> eqInt </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> instance</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> Eq</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">] {</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">  let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> (</span><span style=\"color:#D73A49;--shiki-dark:#F97583\">=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">) (a : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">, b : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">) : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Bool</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> .True</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">};</span></span></code></pre><h2 id=\"compare\"><a href=\"#compare\">Compare</a></h2><div class=\"code-tabs\" data-example-id=\"class-instance\">\n<div class=\"code-tabs-meta\">\n<p class=\"code-tabs-caption\">Define shared behavior once, then attach concrete implementations per type.</p>\n<p class=\"code-tabs-note\">Like one wall-socket standard with different appliance designs behind the plug. Declare one behavior shape, then implement it per type.</p>\n</div>\n<section role=\"tabpanel\" class=\"code-panel\">\n<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> Eq</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">T</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">] </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> class</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> {</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">  let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> (</span><span style=\"color:#D73A49;--shiki-dark:#F97583\">=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">) (a : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">T</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">, b : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">T</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">) : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Bool</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">};</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> eqInt </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> instance</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> Eq</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">] {</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">  let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> (</span><span style=\"color:#D73A49;--shiki-dark:#F97583\">=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">) (a : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">, b : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">) : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Bool</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> .True</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">};</span></span></code></pre>\n</section>\n</div><p>Use a class when you want one operation shape to work across different types. Use an instance when you want to supply that behavior for one concrete type.</p>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><div class=\"try-block\"><ol><li>Define one class.</li><li>Add one instance for <code>Int</code>.</li><li>Call the behavior through the class surface.</li></ol></div><h2 id=\"next-step\"><a href=\"#next-step\">Next step</a></h2><p>Review both snippets, then continue to <a href=\"/docs/types-and-abstractions/effects-and-handlers\">Effects and handlers</a>.</p>\n",
		"summaryHtml": "Classes, methods, and instance declarations."
	},
	{
		"id": "effects-and-handlers",
		"kind": "chapter",
		"partId": "types-and-abstractions",
		"partTitle": "types-and-abstractions",
		"path": "/docs/types-and-abstractions/effects-and-handlers",
		"canonicalPath": "/docs/types-and-abstractions/effects-and-handlers",
		"aliases": [
			"/docs/effects-and-handlers"
		],
		"questions": [
			{
				"label": "How do I define effects, handle them, and use <code>resume</code>?",
				"href": "/docs/types-and-abstractions/effects-and-handlers"
			}
		],
		"title": "Effects and handlers",
		"description": "Use <code>effect</code>, <code>perform</code>, <code>handle</code>, and <code>resume</code> as part of normal Musi code.",
		"group": "Effects",
		"section": "Effects",
		"order": 12,
		"slug": "effects-and-handlers",
		"summary": "The main Musi differentiator, shown with real syntax.",
		"descriptionHtml": "Use <code>effect</code>, <code>perform</code>, <code>handle</code>, and <code>resume</code> as part of normal Musi code.",
		"headings": [
			{
				"depth": 2,
				"id": "handle-first",
				"text": "Handle first"
			},
			{
				"depth": 2,
				"id": "define-effect",
				"text": "Define effect"
			},
			{
				"depth": 2,
				"id": "perform-operation",
				"text": "Perform operation"
			},
			{
				"depth": 2,
				"id": "compare",
				"text": "Compare"
			},
			{
				"depth": 2,
				"id": "try-it",
				"text": "Try it"
			},
			{
				"depth": 2,
				"id": "next-step",
				"text": "Next step"
			}
		],
		"html": "<p>Effects are part of ordinary Musi code. Define an effect, <code>perform</code> an operation, then <code>handle</code> it at a boundary.</p>\n<h2 id=\"handle-first\"><a href=\"#handle-first\">Handle first</a></h2><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">handle</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> perform</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> console.readln() using console {</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">  value </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">=></span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> value;</span></span>\n<span class=\"line\"><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">  readln</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(k) </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">=></span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> resume</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> \"ok\"</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">};</span></span></code></pre><p>Handlers decide what to do with an operation request. <code>resume</code> continues execution with a value.</p>\n<h2 id=\"define-effect\"><a href=\"#define-effect\">Define effect</a></h2><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> console </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> effect</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> {</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">  let</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> readln</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> () : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">String</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">};</span></span></code></pre><h2 id=\"perform-operation\"><a href=\"#perform-operation\">Perform operation</a></h2><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">perform</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> console.readln();</span></span></code></pre><h2 id=\"compare\"><a href=\"#compare\">Compare</a></h2><div class=\"code-tabs\" data-example-id=\"effect-handle\">\n<div class=\"code-tabs-meta\">\n<p class=\"code-tabs-caption\">Capture side-effect requests in one place, then resolve them through handlers.</p>\n<p class=\"code-tabs-note\">At small scale this can look like callback wiring, but at larger scale handlers keep policy at boundaries and reduce plumbing across call chains.</p>\n</div>\n<section role=\"tabpanel\" class=\"code-panel\">\n<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> console </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> effect</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> {</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">  let</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> readln</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> () : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">String</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">};</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">handle</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> perform</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> console.readln() </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">with</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> console </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">of</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> (</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">| </span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">readln</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(k) </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">=></span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> resume</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> \"ok\"</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">| value </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">=></span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> value</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">);</span></span></code></pre>\n</section>\n</div><p>Effects are useful when direct calls would otherwise drag boundary logic through many layers of code.</p>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><div class=\"try-block\"><ol><li>Define one effect with one operation.</li><li>Perform that operation.</li><li>Handle it and use <code>resume</code> once.</li></ol></div><h2 id=\"next-step\"><a href=\"#next-step\">Next step</a></h2><p>Read all three snippets end-to-end, then continue to <a href=\"/docs/types-and-abstractions/attributes-and-foreign\">Attributes and foreign declarations</a>.</p>\n",
		"summaryHtml": "The main Musi differentiator, shown with real syntax."
	},
	{
		"id": "quote-and-syntax",
		"kind": "chapter",
		"partId": "types-and-abstractions",
		"partTitle": "types-and-abstractions",
		"path": "/docs/types-and-abstractions/quote-and-syntax",
		"canonicalPath": "/docs/types-and-abstractions/quote-and-syntax",
		"aliases": [
			"/docs/quote-and-syntax"
		],
		"questions": [
			{
				"label": "How do <code>quote</code> and splice work in real Musi code?",
				"href": "/docs/types-and-abstractions/quote-and-syntax"
			}
		],
		"title": "Quote and syntax values",
		"description": "Use quote and splice for reusable code templates and syntax-driven workflows.",
		"group": "Abstractions",
		"section": "Abstractions",
		"order": 14,
		"slug": "quote-and-syntax",
		"summary": "Quoted expressions, splice forms, and practical metaprogramming patterns.",
		"descriptionHtml": "Use quote and splice for reusable code templates and syntax-driven workflows.",
		"headings": [
			{
				"depth": 2,
				"id": "without-quote",
				"text": "Without quote"
			},
			{
				"depth": 2,
				"id": "with-quote-and-splice",
				"text": "With quote and splice"
			},
			{
				"depth": 2,
				"id": "small-forms",
				"text": "Small forms"
			},
			{
				"depth": 2,
				"id": "compare",
				"text": "Compare"
			},
			{
				"depth": 2,
				"id": "try-it",
				"text": "Try it"
			},
			{
				"depth": 2,
				"id": "next-step",
				"text": "Next step"
			}
		],
		"html": "<p>Quote syntax lets you treat code as data, then splice values or sub-expressions into that code shape.</p>\n<h2 id=\"without-quote\"><a href=\"#without-quote\">Without quote</a></h2><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> addOne</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> (x : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">) : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> x </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">+</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 1</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> addTwo</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> (x : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">) : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> x </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">+</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 2</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span></code></pre><h2 id=\"with-quote-and-splice\"><a href=\"#with-quote-and-splice\">With quote and splice</a></h2><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> addTemplate </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> quote </span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(x </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">+</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> #(delta));</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> addOneSyntax </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> quote </span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(#(x) </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">+</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 1</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">);</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> addTwoSyntax </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> quote </span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(#(x) </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">+</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 2</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">);</span></span></code></pre><p>Use <code>quote</code> when code shape itself is data you want to build or transform.</p>\n<h2 id=\"small-forms\"><a href=\"#small-forms\">Small forms</a></h2><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">quote </span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(x </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">+</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 1</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">);</span></span></code></pre><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">quote </span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">{</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">  x;</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">};</span></span></code></pre><h2 id=\"compare\"><a href=\"#compare\">Compare</a></h2><div class=\"code-tabs\" data-example-id=\"quote-metaprogramming\">\n<div class=\"code-tabs-meta\">\n<p class=\"code-tabs-caption\">Build or transform code structure itself, not only runtime values.</p>\n<p class=\"code-tabs-note\">When a language lacks first-class quote/splice, the closest equivalent is usually macros, AST builders, or plain code generators.</p>\n</div>\n<section role=\"tabpanel\" class=\"code-panel\">\n<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> addTemplate </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> quote </span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(#(x) </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">+</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> #(delta));</span></span></code></pre>\n</section>\n</div><p>Splice forms such as <code>#name</code> and <code>#(expr)</code> are only valid inside quote contexts.</p>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><div class=\"try-block\"><ol><li>Write one <code>quote</code> expression.</li><li>Splice one value into it.</li><li>Compare the quoted shape with duplicated handwritten code.</li></ol></div><h2 id=\"next-step\"><a href=\"#next-step\">Next step</a></h2><p>Take one duplicated helper pair, replace it with one quoted template, then continue to <a href=\"/docs/tooling/foundation-and-standard-library\">Foundation and standard library</a>.</p>\n",
		"summaryHtml": "Quoted expressions, splice forms, and practical metaprogramming patterns."
	},
	{
		"id": "attributes-and-foreign",
		"kind": "chapter",
		"partId": "types-and-abstractions",
		"partTitle": "types-and-abstractions",
		"path": "/docs/types-and-abstractions/attributes-and-foreign",
		"canonicalPath": "/docs/types-and-abstractions/attributes-and-foreign",
		"aliases": [
			"/docs/attributes-and-foreign"
		],
		"questions": [
			{
				"label": "How do I declare foreign functions and attributes?",
				"href": "/docs/types-and-abstractions/attributes-and-foreign"
			}
		],
		"title": "Attributes and foreign declarations",
		"description": "Use stable public attributes, reserved compiler attributes, and foreign bindings.",
		"group": "Abstractions",
		"section": "Abstractions",
		"order": 13,
		"slug": "attributes-and-foreign",
		"summary": "Stable public attrs, reserved compiler attrs, and foreign bindings.",
		"descriptionHtml": "Use stable public attributes, reserved compiler attributes, and foreign bindings.",
		"headings": [
			{
				"depth": 2,
				"id": "foreign-binding",
				"text": "Foreign binding"
			},
			{
				"depth": 2,
				"id": "public-attributes",
				"text": "Public attributes"
			},
			{
				"depth": 2,
				"id": "reserved-compiler-attributes",
				"text": "Reserved compiler attributes"
			},
			{
				"depth": 2,
				"id": "try-it",
				"text": "Try it"
			},
			{
				"depth": 2,
				"id": "next-step",
				"text": "Next step"
			}
		],
		"html": "<p>Attributes are plain metadata on declarations. Most are public. Two are compiler-owned: <code>@known</code> and <code>@intrinsic</code>.</p>\n<h2 id=\"foreign-binding\"><a href=\"#foreign-binding\">Foreign binding</a></h2><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">foreign</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> \"c\" </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> puts</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> (msg : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">CString</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">) : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span></code></pre><h2 id=\"public-attributes\"><a href=\"#public-attributes\">Public attributes</a></h2><p>Use these when a declaration needs explicit metadata:</p>\n<ul>\n<li><code>@link</code></li>\n<li><code>@when</code></li>\n<li><code>@repr</code></li>\n<li><code>@layout</code></li>\n<li><code>@frozen</code></li>\n<li><code>@hot</code></li>\n<li><code>@cold</code></li>\n<li><code>@deprecated</code></li>\n<li><code>@since</code></li>\n</ul>\n<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">@</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">link</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(name </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> \"c\"</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">) </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">foreign</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> \"c\" </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> puts</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> (msg : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">CString</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">) : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span></code></pre><p><code>@frozen</code> is ABI/layout promise for exported non-opaque <code>data</code>. It does not mean immutability.</p>\n<p><code>@hot</code> and <code>@cold</code> are optimizer hints on callable declarations. They do not change semantics.</p>\n<h2 id=\"reserved-compiler-attributes\"><a href=\"#reserved-compiler-attributes\">Reserved compiler attributes</a></h2><p>Use reserved attrs only inside foundation/compiler-owned modules:</p>\n<ul>\n<li><code>@known(name := &quot;...&quot;)</code></li>\n<li><code>@intrinsic(name := &quot;...&quot;)</code></li>\n</ul>\n<p><code>@known</code> marks compiler-known bindings such as <code>Type</code> or <code>CString</code>.</p>\n<p><code>@intrinsic</code> marks compiler-owned runtime hooks in <code>musi:intrinsics</code>. It is not general user metadata.</p>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><div class=\"try-block\"><ol><li>Declare one <code>foreign</code> binding.</li><li>Add one attribute to it.</li><li>Keep the declaration minimal and explicit.</li></ol></div><h2 id=\"next-step\"><a href=\"#next-step\">Next step</a></h2><p>Read foreign examples first, then continue to <a href=\"/docs/types-and-abstractions/quote-and-syntax\">Quote and syntax values</a>.</p>\n",
		"summaryHtml": "Stable public attrs, reserved compiler attrs, and foreign bindings."
	},
	{
		"id": "foundation-and-standard-library",
		"kind": "chapter",
		"partId": "tooling",
		"partTitle": "tooling",
		"path": "/docs/tooling/foundation-and-standard-library",
		"canonicalPath": "/docs/tooling/foundation-and-standard-library",
		"aliases": [
			"/docs/foundation-and-standard-library"
		],
		"questions": [
			{
				"label": "How do <code>@std</code> and <code>musi:*</code> differ?",
				"href": "/docs/tooling/foundation-and-standard-library"
			}
		],
		"title": "Foundation and standard library",
		"description": "Know when to use <code>@std</code> and when you are looking at lower-level foundation names.",
		"group": "Tooling",
		"section": "Tooling",
		"order": 15,
		"slug": "foundation-and-standard-library",
		"summary": "The standard library family and the lower-level foundation namespace.",
		"descriptionHtml": "Know when to use <code>@std</code> and when you are looking at lower-level foundation names.",
		"headings": [
			{
				"depth": 2,
				"id": "default-split",
				"text": "Default split"
			},
			{
				"depth": 2,
				"id": "compare",
				"text": "Compare"
			},
			{
				"depth": 2,
				"id": "try-it",
				"text": "Try it"
			},
			{
				"depth": 2,
				"id": "next-step",
				"text": "Next step"
			}
		],
		"html": "<p>Most user code starts in <code>@std</code>.\n<code>musi:*</code> is the lower-level family when you need foundation-level capabilities.</p>\n<h2 id=\"default-split\"><a href=\"#default-split\">Default split</a></h2><ul>\n<li><code>@std</code> for everyday work</li>\n<li><code>musi:*</code> for core-level operations</li>\n</ul>\n<p>Most code should stay in <code>@std</code>. Reach for <code>musi:*</code> when you are working near the language runtime boundary or lower-level compiler-facing tools.</p>\n<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> configured </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> Option</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">.some[</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">](</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">8080</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">);</span></span>\n<span class=\"line\"><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Option</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">.unwrapOr[</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">](configured, </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">3000</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">);</span></span></code></pre><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> parsed </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> Result</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">.ok[</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">, </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">String</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">](</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">8080</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">);</span></span>\n<span class=\"line\"><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Result</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">.unwrapOr[</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">, </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">String</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">](parsed, </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">3000</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">);</span></span></code></pre><h2 id=\"compare\"><a href=\"#compare\">Compare</a></h2><div class=\"code-tabs\" data-example-id=\"import-stdlib\">\n<div class=\"code-tabs-meta\">\n<p class=\"code-tabs-caption\">Import the standard library, then reach the family you need. Musi keeps stdlib access explicit through <code>@std</code>.</p>\n<p class=\"code-tabs-note\">Like checking out a toolbox before work: import once, then use the tools by name. In Musi, imports are values you can pass around.</p>\n</div>\n<section role=\"tabpanel\" class=\"code-panel\">\n<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> Option</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> import</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> \"@std/option\"</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> value </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> Option</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">.some(</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">1</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">);</span></span></code></pre>\n</section>\n</div><h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><div class=\"try-block\"><ol><li>Import one <code>@std</code> module.</li><li>Import one <code>musi:*</code> module.</li><li>Write down which one belongs in app code.</li></ol></div><h2 id=\"next-step\"><a href=\"#next-step\">Next step</a></h2><p>Move one project import to <code>@std</code>, then continue to <a href=\"/docs/tooling/testing-and-running\">Testing and running</a>.</p>\n",
		"summaryHtml": "The standard library family and the lower-level foundation namespace."
	},
	{
		"id": "testing-and-running",
		"kind": "chapter",
		"partId": "tooling",
		"partTitle": "tooling",
		"path": "/docs/tooling/testing-and-running",
		"canonicalPath": "/docs/tooling/testing-and-running",
		"aliases": [
			"/docs/testing-and-running"
		],
		"questions": [
			{
				"label": "How do I run tests, packages, and direct files?",
				"href": "/docs/tooling/testing-and-running"
			}
		],
		"title": "Testing and running",
		"description": "Run a package, run tests, and use the direct CLI when needed.",
		"group": "Tooling",
		"section": "Tooling",
		"order": 16,
		"slug": "testing-and-running",
		"summary": "The main commands for package work and direct file work.",
		"descriptionHtml": "Run a package, run tests, and use the direct CLI when needed.",
		"headings": [
			{
				"depth": 2,
				"id": "test-shape",
				"text": "Test shape"
			},
			{
				"depth": 2,
				"id": "package-commands",
				"text": "Package commands"
			},
			{
				"depth": 2,
				"id": "direct-commands",
				"text": "Direct commands"
			},
			{
				"depth": 2,
				"id": "compare",
				"text": "Compare"
			},
			{
				"depth": 2,
				"id": "try-it",
				"text": "Try it"
			},
			{
				"depth": 2,
				"id": "next-step",
				"text": "Next step"
			}
		],
		"html": "<p>Testing and execution are split by scope:</p>\n<ul>\n<li>package scope with <code>musi</code></li>\n<li>direct source/artifact scope with <code>music</code></li>\n</ul>\n<h2 id=\"test-shape\"><a href=\"#test-shape\">Test shape</a></h2><ul>\n<li>keep tests in <code>*.test.ms</code></li>\n<li>expose each test with exported <code>test</code></li>\n</ul>\n<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> Testing</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> import</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> \"@std/testing\"</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">export</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> let</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> test</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> () </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span></span>\n<span class=\"line\"><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">  Testing</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">.it(</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\">\"adds values\"</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">, </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Testing</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">.toBe(</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">1</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> +</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 2</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">, </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">3</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">));</span></span></code></pre><h2 id=\"package-commands\"><a href=\"#package-commands\">Package commands</a></h2><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">musi</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> run</span></span>\n<span class=\"line\"><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">musi</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> check</span></span>\n<span class=\"line\"><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">musi</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> build</span></span>\n<span class=\"line\"><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">musi</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> test</span></span></code></pre><h2 id=\"direct-commands\"><a href=\"#direct-commands\">Direct commands</a></h2><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">music</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> check</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> index.ms</span></span>\n<span class=\"line\"><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">music</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> build</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> index.ms</span></span>\n<span class=\"line\"><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">music</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> run</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> index.seam</span></span></code></pre><h2 id=\"compare\"><a href=\"#compare\">Compare</a></h2><div class=\"code-tabs\" data-example-id=\"testing-entry\">\n<div class=\"code-tabs-meta\">\n<p class=\"code-tabs-caption\">A small test entry should read like ordinary code. Musi uses <code>export let test ()</code> inside <code>*.test.ms</code> files.</p>\n<p class=\"code-tabs-note\">Think smoke detector checks: small, repeatable, and run regularly. Musi discovers these by file name and runs them with <code>musi test</code>.</p>\n</div>\n<section role=\"tabpanel\" class=\"code-panel\">\n<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> Testing</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> import</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> \"@std/testing\"</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">export</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> let</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> test</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> () </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> Testing</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">.it(</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\">\"adds values\"</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">, </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Testing</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">.toBe(</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">1</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> +</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 2</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">, </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">3</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">));</span></span></code></pre>\n</section>\n</div><p>Use <code>musi</code> for normal project work. Use <code>music</code> when you want to run or inspect one source file or built artifact directly.</p>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><div class=\"try-block\"><ol><li>Create one <code>*.test.ms</code> file.</li><li>Export a <code>test</code> binding.</li><li>Run <code>musi test</code> and <code>music check index.ms</code>.</li></ol></div><h2 id=\"next-step\"><a href=\"#next-step\">Next step</a></h2><p>Run one package command and one direct command, then revisit any chapter where behavior is unclear.</p>\n<p>See <a href=\"/reference\">Reference</a> for source, grammar, extension, and issue links.</p>\n",
		"summaryHtml": "The main commands for package work and direct file work."
	},
	{
		"id": "common-questions",
		"kind": "chapter",
		"partId": "questions",
		"partTitle": "questions",
		"path": "/docs/questions/common-questions",
		"canonicalPath": "/docs/questions/common-questions",
		"aliases": [
			"/docs/common-questions"
		],
		"questions": [],
		"title": "Common questions",
		"description": "Task-first links into the Musi book.",
		"group": "Questions",
		"section": "Questions",
		"order": 6,
		"slug": "common-questions",
		"summary": "Task-first links for setup, imports, types, effects, and testing.",
		"descriptionHtml": "Task-first links into the Musi book.",
		"headings": [
			{
				"depth": 2,
				"id": "setup-and-files",
				"text": "Setup and files"
			},
			{
				"depth": 2,
				"id": "syntax-and-data",
				"text": "Syntax and data"
			},
			{
				"depth": 2,
				"id": "types-and-effects",
				"text": "Types and effects"
			},
			{
				"depth": 2,
				"id": "tooling",
				"text": "Tooling"
			},
			{
				"depth": 2,
				"id": "try-it",
				"text": "Try it"
			}
		],
		"html": "<p>Use this page when you know the task you need, but not where it lives in the book.</p>\n<h2 id=\"setup-and-files\"><a href=\"#setup-and-files\">Setup and files</a></h2><ul>\n<li>Install Musi and learn <code>music</code> versus <code>musi</code>: <a href=\"/docs/start/getting-started\">Getting started</a></li>\n<li>Run the smallest file: <a href=\"/docs/start/first-program\">First program</a></li>\n<li>Move into package work: <a href=\"/docs/start/files-packages-and-entry\">Files, packages, and entry</a></li>\n</ul>\n<h2 id=\"syntax-and-data\"><a href=\"#syntax-and-data\">Syntax and data</a></h2><ul>\n<li>Import from <code>@std</code> or <code>musi:*</code>: <a href=\"/docs/start/imports-and-packages\">Imports and packages</a></li>\n<li>Read files top to bottom: <a href=\"/docs/core-language/expressions-and-bindings\">Expressions and bindings</a></li>\n<li>Model variants and branch: <a href=\"/docs/core-language/data-and-pattern-matching\">Data and pattern matching</a></li>\n<li>Update records and arrays: <a href=\"/docs/core-language/records-arrays-and-mutation\">Records and arrays</a></li>\n</ul>\n<h2 id=\"types-and-effects\"><a href=\"#types-and-effects\">Types and effects</a></h2><ul>\n<li>Add type annotations and generics: <a href=\"/docs/types-and-abstractions/types\">Types and generics</a></li>\n<li>Define classes and instances: <a href=\"/docs/types-and-abstractions/classes-instances-and-laws\">Classes and instances</a></li>\n<li>Handle effects and use <code>resume</code>: <a href=\"/docs/types-and-abstractions/effects-and-handlers\">Effects and handlers</a></li>\n<li>Use <code>quote</code> and splice: <a href=\"/docs/types-and-abstractions/quote-and-syntax\">Quote and syntax values</a></li>\n<li>Declare foreign bindings: <a href=\"/docs/types-and-abstractions/attributes-and-foreign\">Attributes and foreign declarations</a></li>\n</ul>\n<h2 id=\"tooling\"><a href=\"#tooling\">Tooling</a></h2><ul>\n<li>Understand <code>@std</code> and <code>musi:*</code>: <a href=\"/docs/tooling/foundation-and-standard-library\">Foundation and standard library</a></li>\n<li>Run tests and packages: <a href=\"/docs/tooling/testing-and-running\">Testing and running</a></li>\n</ul>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><div class=\"try-block\"><ol><li>Pick one question from this page.</li><li>Follow its linked chapter.</li><li>Add the shown syntax to a scratch file.</li></ol></div>\n",
		"summaryHtml": "Task-first links for setup, imports, types, effects, and testing."
	}
] satisfies GeneratedDoc[];
