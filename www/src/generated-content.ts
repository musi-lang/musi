export interface GeneratedHeading {
	depth: number;
	id: string;
	text: string;
}

export interface GeneratedDoc {
	locale: "en";
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
	"homeSampleHtml": "<div class=\"code-tabs\" data-example-id=\"home-intro\">\n<div class=\"code-tabs-meta\">\n<p class=\"code-tabs-caption\">A real-world example: A function checking a JSON response and parsing coordinates.</p>\n<p class=\"code-tabs-note\">Musi is expression-oriented. Types, bindings, and branches flow sequentially.</p>\n</div>\n<section role=\"tabpanel\" class=\"code-panel\">\n<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> json </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> import</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> \"@std/json\"</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> Point</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> data</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> {</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">  x : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Float</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">  y : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Float</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">};</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> parseLocation</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> (payload : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">String</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">) : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Result</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Point</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">, </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">String</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">] </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">  case</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> json.</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">parse</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(payload) </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">of</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> (</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">  | </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">.Ok</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(doc) </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">=></span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">      let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> x </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> doc.</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">field</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\">\"x\"</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">).</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">asFloat</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">().</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">unwrapOr</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">0.0</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">);</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">      let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> y </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> doc.</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">field</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\">\"y\"</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">).</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">asFloat</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">().</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">unwrapOr</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">0.0</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">);</span></span>\n<span class=\"line\"><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">      .Ok</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">({ x </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> x, y </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> y })</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">  | </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">.Err</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(e) </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">=></span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> .Err</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(e)</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">  );</span></span></code></pre>\n</section>\n</div>",
	"installSourceHtml": "<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">git</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> clone</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> https://github.com/musi-lang/musi.git</span></span>\n<span class=\"line\"><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">cd</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> musi</span></span>\n<span class=\"line\"><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">cargo</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> build</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> --release</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">export</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> PATH</span><span style=\"color:#D73A49;--shiki-dark:#F97583\">=</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\">\"/path/to/musi/target/release:</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">$PATH</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\">\"</span></span></code></pre>",
	"quickstartHtml": "<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">musi</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> new</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> hello</span></span>\n<span class=\"line\"><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">cd</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> hello</span></span>\n<span class=\"line\"><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">musi</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> run</span></span></code></pre>"
} as const;

export const renderedDocs = [
	{
		"locale": "en",
		"id": "start",
		"kind": "part",
		"partId": "start",
		"partTitle": "start",
		"path": "/learn/language/start",
		"canonicalPath": "/learn/language/start",
		"aliases": [],
		"questions": [],
		"title": "Start",
		"description": "Set up Musi, write the first file, and learn the core reading model.",
		"group": "Start",
		"section": "Start",
		"order": 1,
		"slug": "start",
		"summary": "Begin with setup, one file, one binding model, one expression model, then explicit mutation.",
		"descriptionHtml": "Set up Musi, write the first file, and learn the core reading model.",
		"headings": [
			{
				"depth": 2,
				"id": "what",
				"text": "What"
			},
			{
				"depth": 2,
				"id": "why",
				"text": "Why"
			},
			{
				"depth": 2,
				"id": "how",
				"text": "How"
			}
		],
		"html": "<p>Start here if Musi is new. These chapters are intentionally small.</p>\n<h2 id=\"what\"><a href=\"#what\">What</a></h2><p>This part establishes the first mental model.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>Beginners need one idea at a time.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><p>Read these chapters in order.</p>\n",
		"summaryHtml": "Begin with setup, one file, one binding model, one expression model, then explicit mutation."
	},
	{
		"locale": "en",
		"id": "core",
		"kind": "part",
		"partId": "core",
		"partTitle": "core",
		"path": "/learn/language/core",
		"canonicalPath": "/learn/language/core",
		"aliases": [],
		"questions": [],
		"title": "Core syntax",
		"description": "Build comfort with literals, operators, ranges, functions, calls, and methods.",
		"group": "Core syntax",
		"section": "Core syntax",
		"order": 2,
		"slug": "core",
		"summary": "Learn everyday syntax in very small pieces so nothing stacks too early.",
		"descriptionHtml": "Build comfort with literals, operators, ranges, functions, calls, and methods.",
		"headings": [
			{
				"depth": 2,
				"id": "what",
				"text": "What"
			},
			{
				"depth": 2,
				"id": "why",
				"text": "Why"
			},
			{
				"depth": 2,
				"id": "how",
				"text": "How"
			}
		],
		"html": "<p>This part teaches the syntax you read every day.</p>\n<h2 id=\"what\"><a href=\"#what\">What</a></h2><p>Literals, operators, ranges, functions, calls, methods.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>These forms should feel ordinary before larger features arrive.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><p>Work forward one chapter at a time.</p>\n",
		"summaryHtml": "Learn everyday syntax in very small pieces so nothing stacks too early."
	},
	{
		"locale": "en",
		"id": "data",
		"kind": "part",
		"partId": "data",
		"partTitle": "data",
		"path": "/learn/language/data",
		"canonicalPath": "/learn/language/data",
		"aliases": [],
		"questions": [],
		"title": "Data",
		"description": "Work with records, arrays, slices, and patterns one step at a time.",
		"group": "Data",
		"section": "Data",
		"order": 3,
		"slug": "data",
		"summary": "Keep data-shape learning readable by separating records, sequences, and patterns.",
		"descriptionHtml": "Work with records, arrays, slices, and patterns one step at a time.",
		"headings": [
			{
				"depth": 2,
				"id": "what",
				"text": "What"
			},
			{
				"depth": 2,
				"id": "why",
				"text": "Why"
			},
			{
				"depth": 2,
				"id": "how",
				"text": "How"
			}
		],
		"html": "<p>This part teaches data shapes without overloading the reader.</p>\n<h2 id=\"what\"><a href=\"#what\">What</a></h2><p>Records, arrays, slices, patterns.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>Shape-heavy syntax becomes much easier when split.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><p>Learn the value shape first. Match on it later.</p>\n",
		"summaryHtml": "Keep data-shape learning readable by separating records, sequences, and patterns."
	},
	{
		"locale": "en",
		"id": "organization",
		"kind": "part",
		"partId": "organization",
		"partTitle": "organization",
		"path": "/learn/language/organization",
		"canonicalPath": "/learn/language/organization",
		"aliases": [],
		"questions": [],
		"title": "Code organization",
		"description": "Move from single files into packages, imports, and exports.",
		"group": "Code organization",
		"section": "Code organization",
		"order": 4,
		"slug": "organization",
		"summary": "Grow from one file to packages without changing the mental model of code flow.",
		"descriptionHtml": "Move from single files into packages, imports, and exports.",
		"headings": [
			{
				"depth": 2,
				"id": "what",
				"text": "What"
			},
			{
				"depth": 2,
				"id": "why",
				"text": "Why"
			},
			{
				"depth": 2,
				"id": "how",
				"text": "How"
			}
		],
		"html": "<p>This part explains how Musi code grows.</p>\n<h2 id=\"what\"><a href=\"#what\">What</a></h2><p>Files, packages, imports, exports.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>Readers should understand one file before package ceremony.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><p>Move from files to packages only when the reason is clear.</p>\n",
		"summaryHtml": "Grow from one file to packages without changing the mental model of code flow."
	},
	{
		"locale": "en",
		"id": "types",
		"kind": "part",
		"partId": "types",
		"partTitle": "types",
		"path": "/learn/language/types",
		"canonicalPath": "/learn/language/types",
		"aliases": [],
		"questions": [],
		"title": "Types",
		"description": "Add type information, understand inference, and write small generic helpers.",
		"group": "Types",
		"section": "Types",
		"order": 5,
		"slug": "types",
		"summary": "Introduce types gradually: explicit first, inferred second, generic third.",
		"descriptionHtml": "Add type information, understand inference, and write small generic helpers.",
		"headings": [
			{
				"depth": 2,
				"id": "what",
				"text": "What"
			},
			{
				"depth": 2,
				"id": "why",
				"text": "Why"
			},
			{
				"depth": 2,
				"id": "how",
				"text": "How"
			}
		],
		"html": "<p>This part keeps type learning incremental.</p>\n<h2 id=\"what\"><a href=\"#what\">What</a></h2><p>Annotations, inference, generics.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>Beginners should not meet all type ideas at once.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><p>Use each chapter to stabilize one type concept.</p>\n",
		"summaryHtml": "Introduce types gradually: explicit first, inferred second, generic third."
	},
	{
		"locale": "en",
		"id": "abstractions",
		"kind": "part",
		"partId": "abstractions",
		"partTitle": "abstractions",
		"path": "/learn/language/abstractions",
		"canonicalPath": "/learn/language/abstractions",
		"aliases": [],
		"questions": [],
		"title": "Abstractions",
		"description": "Learn classes, instances, and laws without object-model confusion.",
		"group": "Abstractions",
		"section": "Abstractions",
		"order": 6,
		"slug": "abstractions",
		"summary": "Separate behavior shape, concrete implementation, and semantic law into distinct chapters.",
		"descriptionHtml": "Learn classes, instances, and laws without object-model confusion.",
		"headings": [
			{
				"depth": 2,
				"id": "what",
				"text": "What"
			},
			{
				"depth": 2,
				"id": "why",
				"text": "Why"
			},
			{
				"depth": 2,
				"id": "how",
				"text": "How"
			}
		],
		"html": "<p>This part explains shared behavior in small steps.</p>\n<h2 id=\"what\"><a href=\"#what\">What</a></h2><p>Classes, instances, laws.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>Readers should not confuse contracts with implementations.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><p>Learn class first, instance second, law third.</p>\n",
		"summaryHtml": "Separate behavior shape, concrete implementation, and semantic law into distinct chapters."
	},
	{
		"locale": "en",
		"id": "effects-runtime",
		"kind": "part",
		"partId": "effects-runtime",
		"partTitle": "effects-runtime",
		"path": "/learn/language/effects-runtime",
		"canonicalPath": "/learn/language/effects-runtime",
		"aliases": [],
		"questions": [],
		"title": "Effects and runtime",
		"description": "Understand effects, using, handlers, foundation, runtime, and stdlib layering.",
		"group": "Effects and runtime",
		"section": "Effects and runtime",
		"order": 7,
		"slug": "effects-runtime",
		"summary": "Make effect flow explicit, then place runtime and stdlib on top of that model.",
		"descriptionHtml": "Understand effects, using, handlers, foundation, runtime, and stdlib layering.",
		"headings": [
			{
				"depth": 2,
				"id": "what",
				"text": "What"
			},
			{
				"depth": 2,
				"id": "why",
				"text": "Why"
			},
			{
				"depth": 2,
				"id": "how",
				"text": "How"
			}
		],
		"html": "<p>This part explains capability flow and library layering.</p>\n<h2 id=\"what\"><a href=\"#what\">What</a></h2><p>Effects, using, handlers, foundation, runtime, stdlib.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>These topics only stay readable when their boundaries stay clear.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><p>Keep capability ideas separate from host-boundary ideas.</p>\n",
		"summaryHtml": "Make effect flow explicit, then place runtime and stdlib on top of that model."
	},
	{
		"locale": "en",
		"id": "advanced",
		"kind": "part",
		"partId": "advanced",
		"partTitle": "advanced",
		"path": "/learn/language/advanced",
		"canonicalPath": "/learn/language/advanced",
		"aliases": [],
		"questions": [],
		"title": "Advanced and tooling",
		"description": "Finish with attributes, foreign bindings, quote, testing, and tool workflow.",
		"group": "Advanced and tooling",
		"section": "Advanced and tooling",
		"order": 8,
		"slug": "advanced",
		"summary": "Keep sharp or advanced topics late, after ordinary code already feels natural.",
		"descriptionHtml": "Finish with attributes, foreign bindings, quote, testing, and tool workflow.",
		"headings": [
			{
				"depth": 2,
				"id": "what",
				"text": "What"
			},
			{
				"depth": 2,
				"id": "why",
				"text": "Why"
			},
			{
				"depth": 2,
				"id": "how",
				"text": "How"
			}
		],
		"html": "<p>This part collects later-stage topics.</p>\n<h2 id=\"what\"><a href=\"#what\">What</a></h2><p>Attributes, foreign, quote, testing, tooling.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>These ideas become easier once core code already feels stable.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><p>Treat them as boundary tools, not beginner syntax.</p>\n",
		"summaryHtml": "Keep sharp or advanced topics late, after ordinary code already feels natural."
	},
	{
		"locale": "en",
		"id": "getting-started",
		"kind": "chapter",
		"partId": "start",
		"partTitle": "Start",
		"path": "/learn/language/start/getting-started",
		"canonicalPath": "/learn/language/start/getting-started",
		"aliases": [
			"/docs/language/start/getting-started"
		],
		"questions": [],
		"title": "Getting started",
		"description": "Install prerequisites, build Musi from source, and learn when to use music versus musi.",
		"group": "Start",
		"section": "Start",
		"order": 1,
		"slug": "getting-started",
		"summary": "Install tools, build Musi, and learn the two command lanes.",
		"descriptionHtml": "Install prerequisites, build Musi from source, and learn when to use music versus musi.",
		"headings": [
			{
				"depth": 2,
				"id": "what",
				"text": "What"
			},
			{
				"depth": 2,
				"id": "why",
				"text": "Why"
			},
			{
				"depth": 2,
				"id": "how",
				"text": "How"
			},
			{
				"depth": 2,
				"id": "try-it",
				"text": "Try it"
			},
			{
				"depth": 2,
				"id": "common-mistake",
				"text": "Common mistake"
			},
			{
				"depth": 2,
				"id": "next",
				"text": "Next"
			}
		],
		"html": "<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">git</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> clone</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> https://github.com/musi-lang/musi.git</span></span>\n<span class=\"line\"><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">cd</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> musi</span></span>\n<span class=\"line\"><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">cargo</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> build</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> --release</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">export</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> PATH</span><span style=\"color:#D73A49;--shiki-dark:#F97583\">=</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\">\"/path/to/musi/target/release:</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">$PATH</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\">\"</span></span></code></pre><h2 id=\"what\"><a href=\"#what\">What</a></h2><p>Musi is currently source-first. You build the repo, then put <code>music</code> and <code>musi</code> on <code>PATH</code>.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>Beginners need one reliable setup story before they learn anything else.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><ul>\n<li>Build once.</li>\n<li>Add the release directory to <code>PATH</code>.</li>\n<li>Use <code>music</code> for direct file work.</li>\n<li>Use <code>musi</code> for package work.</li>\n</ul>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><ul>\n<li>Build the repository.</li>\n<li>Run one <code>music check index.ms</code>.</li>\n<li>Run one <code>musi new hello</code>.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not treat <code>music</code> and <code>musi</code> as duplicates.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/start/first-program\">First program</a>.</p>\n",
		"summaryHtml": "Install tools, build Musi, and learn the two command lanes."
	},
	{
		"locale": "en",
		"id": "first-program",
		"kind": "chapter",
		"partId": "start",
		"partTitle": "Start",
		"path": "/learn/language/start/first-program",
		"canonicalPath": "/learn/language/start/first-program",
		"aliases": [
			"/docs/language/start/first-program"
		],
		"questions": [],
		"title": "First program",
		"description": "Create the smallest useful Musi file and run it with the direct command lane.",
		"group": "Start",
		"section": "Start",
		"order": 2,
		"slug": "first-program",
		"summary": "Write one file, bind one value, and run it end to end.",
		"descriptionHtml": "Create the smallest useful Musi file and run it with the direct command lane.",
		"headings": [
			{
				"depth": 2,
				"id": "what",
				"text": "What"
			},
			{
				"depth": 2,
				"id": "why",
				"text": "Why"
			},
			{
				"depth": 2,
				"id": "how",
				"text": "How"
			},
			{
				"depth": 2,
				"id": "try-it",
				"text": "Try it"
			},
			{
				"depth": 2,
				"id": "common-mistake",
				"text": "Common mistake"
			},
			{
				"depth": 2,
				"id": "next",
				"text": "Next"
			}
		],
		"html": "<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> answer </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 42</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">answer;</span></span></code></pre><h2 id=\"what\"><a href=\"#what\">What</a></h2><p>A small Musi file can be just a few bindings and one final expression.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>Scripting-language readers already know top-to-bottom file flow. Musi keeps that feel.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><ul>\n<li>Bind one value with <code>let</code>.</li>\n<li>End the file with the result you want.</li>\n<li>Validate it with <code>music check index.ms</code>.</li>\n</ul>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><ul>\n<li>Create <code>index.ms</code>.</li>\n<li>Bind one value.</li>\n<li>End the file with that value.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not look for a <code>main</code> function in a tiny scratch file.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/start/values-and-let\">Values and let</a>.</p>\n",
		"summaryHtml": "Write one file, bind one value, and run it end to end."
	},
	{
		"locale": "en",
		"id": "values-and-let",
		"kind": "chapter",
		"partId": "start",
		"partTitle": "Start",
		"path": "/learn/language/start/values-and-let",
		"canonicalPath": "/learn/language/start/values-and-let",
		"aliases": [
			"/docs/language/start/values-and-let"
		],
		"questions": [],
		"title": "Values and let",
		"description": "Learn Musi’s core binding form before adding more syntax.",
		"group": "Start",
		"section": "Start",
		"order": 3,
		"slug": "values-and-let",
		"summary": "Bind names with let and read the file top to bottom.",
		"descriptionHtml": "Learn Musi’s core binding form before adding more syntax.",
		"headings": [
			{
				"depth": 2,
				"id": "what",
				"text": "What"
			},
			{
				"depth": 2,
				"id": "why",
				"text": "Why"
			},
			{
				"depth": 2,
				"id": "how",
				"text": "How"
			},
			{
				"depth": 2,
				"id": "try-it",
				"text": "Try it"
			},
			{
				"depth": 2,
				"id": "common-mistake",
				"text": "Common mistake"
			},
			{
				"depth": 2,
				"id": "next",
				"text": "Next"
			}
		],
		"html": "<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> port </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 8080</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">port;</span></span></code></pre><h2 id=\"what\"><a href=\"#what\">What</a></h2><p><code>let</code> introduces a name and a value.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>Functions, methods, modules, and many other definitions also start from <code>let</code>, so one form matters a lot.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><ul>\n<li>Read <code>let name := value;</code> as a value binding.</li>\n<li>Keep names close to where they are used.</li>\n<li>Prefer small bindings while learning.</li>\n</ul>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><ul>\n<li>Bind one number.</li>\n<li>Bind a second value from the first.</li>\n<li>End the file with the second value.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not assume <code>let</code> only means a top-level constant.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/start/blocks-and-expressions\">Blocks and expressions</a>.</p>\n",
		"summaryHtml": "Bind names with let and read the file top to bottom."
	},
	{
		"locale": "en",
		"id": "blocks-and-expressions",
		"kind": "chapter",
		"partId": "start",
		"partTitle": "Start",
		"path": "/learn/language/start/blocks-and-expressions",
		"canonicalPath": "/learn/language/start/blocks-and-expressions",
		"aliases": [
			"/docs/language/start/blocks-and-expressions"
		],
		"questions": [],
		"title": "Blocks and expressions",
		"description": "Learn block flow before adding mutation or larger control forms.",
		"group": "Start",
		"section": "Start",
		"order": 4,
		"slug": "blocks-and-expressions",
		"summary": "Understand how grouped expressions produce one final value.",
		"descriptionHtml": "Learn block flow before adding mutation or larger control forms.",
		"headings": [
			{
				"depth": 2,
				"id": "what",
				"text": "What"
			},
			{
				"depth": 2,
				"id": "why",
				"text": "Why"
			},
			{
				"depth": 2,
				"id": "how",
				"text": "How"
			},
			{
				"depth": 2,
				"id": "try-it",
				"text": "Try it"
			},
			{
				"depth": 2,
				"id": "common-mistake",
				"text": "Common mistake"
			},
			{
				"depth": 2,
				"id": "next",
				"text": "Next"
			}
		],
		"html": "<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">  let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> base </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 8000</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">  base </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">+</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 80</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">);</span></span></code></pre><h2 id=\"what\"><a href=\"#what\">What</a></h2><p>A block groups expressions and yields one final result.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>Expression-oriented code becomes much easier once you stop expecting every intermediate line to “return nothing”.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><ul>\n<li>Put helper bindings first.</li>\n<li>Put the final value last.</li>\n<li>Read the whole block as one result-producing expression.</li>\n</ul>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><ul>\n<li>Create a block.</li>\n<li>Add one helper binding.</li>\n<li>Return one arithmetic expression.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not treat every grouped form like JavaScript braces.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/start/mutation\">Mutation</a>.</p>\n",
		"summaryHtml": "Understand how grouped expressions produce one final value."
	},
	{
		"locale": "en",
		"id": "mutation",
		"kind": "chapter",
		"partId": "start",
		"partTitle": "Start",
		"path": "/learn/language/start/mutation",
		"canonicalPath": "/learn/language/start/mutation",
		"aliases": [
			"/docs/language/start/mutation"
		],
		"questions": [],
		"title": "Mutation",
		"description": "Learn Musi’s explicit mutation surface without mixing it into every lesson.",
		"group": "Start",
		"section": "Start",
		"order": 5,
		"slug": "mutation",
		"summary": "Use mut only when changing a value helps more than rebuilding it.",
		"descriptionHtml": "Learn Musi’s explicit mutation surface without mixing it into every lesson.",
		"headings": [
			{
				"depth": 2,
				"id": "what",
				"text": "What"
			},
			{
				"depth": 2,
				"id": "why",
				"text": "Why"
			},
			{
				"depth": 2,
				"id": "how",
				"text": "How"
			},
			{
				"depth": 2,
				"id": "try-it",
				"text": "Try it"
			},
			{
				"depth": 2,
				"id": "common-mistake",
				"text": "Common mistake"
			},
			{
				"depth": 2,
				"id": "next",
				"text": "Next"
			}
		],
		"html": "<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> counter </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> mut</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 1</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">counter </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 2</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">counter;</span></span></code></pre><h2 id=\"what\"><a href=\"#what\">What</a></h2><p>Mutation is explicit. You mark a value as mutable, then assign a new value to it.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>Visible mutation makes it easier to tell stable values from changing state.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><ul>\n<li>Start with <code>let x := mut 1;</code>.</li>\n<li>Reassign with <code>x := 2;</code>.</li>\n<li>Prefer rebuilding immutable values when that stays clearer.</li>\n</ul>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><ul>\n<li>Create one mutable integer.</li>\n<li>Update it once.</li>\n<li>Compare it with an immutable rewrite.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not add <code>mut</code> by reflex.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/core/literals\">Literals</a>.</p>\n",
		"summaryHtml": "Use mut only when changing a value helps more than rebuilding it."
	},
	{
		"locale": "en",
		"id": "literals",
		"kind": "chapter",
		"partId": "core",
		"partTitle": "Core syntax",
		"path": "/learn/language/core/literals",
		"canonicalPath": "/learn/language/core/literals",
		"aliases": [
			"/docs/language/core/literals"
		],
		"questions": [],
		"title": "Literals",
		"description": "Meet Musi’s everyday literal values before mixing them with operators.",
		"group": "Core syntax",
		"section": "Core syntax",
		"order": 6,
		"slug": "literals",
		"summary": "Start with numbers, strings, booleans, and templates.",
		"descriptionHtml": "Meet Musi’s everyday literal values before mixing them with operators.",
		"headings": [
			{
				"depth": 2,
				"id": "what",
				"text": "What"
			},
			{
				"depth": 2,
				"id": "why",
				"text": "Why"
			},
			{
				"depth": 2,
				"id": "how",
				"text": "How"
			},
			{
				"depth": 2,
				"id": "try-it",
				"text": "Try it"
			},
			{
				"depth": 2,
				"id": "common-mistake",
				"text": "Common mistake"
			},
			{
				"depth": 2,
				"id": "next",
				"text": "Next"
			}
		],
		"html": "<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> port </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 8080</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> label </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> \"ready\"</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> next </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> port </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">+</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 1</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> same </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> next </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> port </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">+</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 1</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> capped </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> port </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">&#x3C;=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 9000</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> masked </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 1</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> shl</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 3</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span></code></pre><h2 id=\"what\"><a href=\"#what\">What</a></h2><p>Literals are values written directly in source.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>They give readers a stable base before more structural syntax arrives.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><ul>\n<li>Start with numbers and strings.</li>\n<li>Add booleans next.</li>\n<li>Treat templates as text-building expressions.</li>\n</ul>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><ul>\n<li>Bind one integer.</li>\n<li>Bind one string.</li>\n<li>Add one small template.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not learn literals, operators, and data shapes all at once.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/core/operators\">Operators</a>.</p>\n",
		"summaryHtml": "Start with numbers, strings, booleans, and templates."
	},
	{
		"locale": "en",
		"id": "operators",
		"kind": "chapter",
		"partId": "core",
		"partTitle": "Core syntax",
		"path": "/learn/language/core/operators",
		"canonicalPath": "/learn/language/core/operators",
		"aliases": [
			"/docs/language/core/operators"
		],
		"questions": [],
		"title": "Operators",
		"description": "Add operators after literals so expressions stay readable.",
		"group": "Core syntax",
		"section": "Core syntax",
		"order": 7,
		"slug": "operators",
		"summary": "Read arithmetic, comparison, and logic in ordinary expressions.",
		"descriptionHtml": "Add operators after literals so expressions stay readable.",
		"headings": [
			{
				"depth": 2,
				"id": "what",
				"text": "What"
			},
			{
				"depth": 2,
				"id": "why",
				"text": "Why"
			},
			{
				"depth": 2,
				"id": "how",
				"text": "How"
			},
			{
				"depth": 2,
				"id": "try-it",
				"text": "Try it"
			},
			{
				"depth": 2,
				"id": "common-mistake",
				"text": "Common mistake"
			},
			{
				"depth": 2,
				"id": "next",
				"text": "Next"
			}
		],
		"html": "<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> port </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 8080</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> label </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> \"ready\"</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> next </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> port </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">+</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 1</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> same </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> next </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> port </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">+</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 1</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> capped </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> port </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">&#x3C;=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 9000</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> masked </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 1</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> shl</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 3</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span></code></pre><h2 id=\"what\"><a href=\"#what\">What</a></h2><p>Operators combine values into larger expressions.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>Small operator expressions are often the fastest way to read ordinary value flow.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><ul>\n<li>Start with arithmetic.</li>\n<li>Add comparison.</li>\n<li>Keep early examples small.</li>\n</ul>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><ul>\n<li>Compute one sum.</li>\n<li>Compare two values.</li>\n<li>Add one boolean result.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not stack too many operator families in one early example.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/core/ranges\">Ranges</a>.</p>\n",
		"summaryHtml": "Read arithmetic, comparison, and logic in ordinary expressions."
	},
	{
		"locale": "en",
		"id": "ranges",
		"kind": "chapter",
		"partId": "core",
		"partTitle": "Core syntax",
		"path": "/learn/language/core/ranges",
		"canonicalPath": "/learn/language/core/ranges",
		"aliases": [
			"/docs/language/core/ranges"
		],
		"questions": [],
		"title": "Ranges",
		"description": "Learn Musi’s range operators in isolation before they appear inside larger code.",
		"group": "Core syntax",
		"section": "Core syntax",
		"order": 8,
		"slug": "ranges",
		"summary": "Read open, closed, and spread-like range forms without guessing.",
		"descriptionHtml": "Learn Musi’s range operators in isolation before they appear inside larger code.",
		"headings": [
			{
				"depth": 2,
				"id": "what",
				"text": "What"
			},
			{
				"depth": 2,
				"id": "why",
				"text": "Why"
			},
			{
				"depth": 2,
				"id": "how",
				"text": "How"
			},
			{
				"depth": 2,
				"id": "try-it",
				"text": "Try it"
			},
			{
				"depth": 2,
				"id": "common-mistake",
				"text": "Common mistake"
			},
			{
				"depth": 2,
				"id": "next",
				"text": "Next"
			}
		],
		"html": "<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> closed </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 0</span><span style=\"color:#D73A49;--shiki-dark:#F97583\">..</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">10</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> halfOpen </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 0</span><span style=\"color:#D73A49;--shiki-dark:#F97583\">..&#x3C;</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">10</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">closed;</span></span></code></pre><h2 id=\"what\"><a href=\"#what\">What</a></h2><p>Ranges describe ordered spans such as <code>0..10</code> or <code>0..&lt;10</code>.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>Dot-heavy syntax is easy to misread unless you learn range forms separately from spread forms.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><ul>\n<li>Read <code>..</code> as a range form.</li>\n<li>Read <code>..&lt;</code> as a boundary-sensitive form.</li>\n<li>Keep <code>...</code> mentally separate.</li>\n</ul>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><ul>\n<li>Write one <code>0..10</code>.</li>\n<li>Write one <code>0..&lt;10</code>.</li>\n<li>Explain the difference to yourself.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not assume every dotted form is a range.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/core/functions\">Functions</a>.</p>\n",
		"summaryHtml": "Read open, closed, and spread-like range forms without guessing."
	},
	{
		"locale": "en",
		"id": "functions",
		"kind": "chapter",
		"partId": "core",
		"partTitle": "Core syntax",
		"path": "/learn/language/core/functions",
		"canonicalPath": "/learn/language/core/functions",
		"aliases": [
			"/docs/language/core/functions"
		],
		"questions": [],
		"title": "Functions",
		"description": "Write one reusable function and read its shape without extra abstraction noise.",
		"group": "Core syntax",
		"section": "Core syntax",
		"order": 9,
		"slug": "functions",
		"summary": "Define plain functions with let before learning calls or methods.",
		"descriptionHtml": "Write one reusable function and read its shape without extra abstraction noise.",
		"headings": [
			{
				"depth": 2,
				"id": "what",
				"text": "What"
			},
			{
				"depth": 2,
				"id": "why",
				"text": "Why"
			},
			{
				"depth": 2,
				"id": "how",
				"text": "How"
			},
			{
				"depth": 2,
				"id": "try-it",
				"text": "Try it"
			},
			{
				"depth": 2,
				"id": "common-mistake",
				"text": "Common mistake"
			},
			{
				"depth": 2,
				"id": "next",
				"text": "Next"
			}
		],
		"html": "<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> twice</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> (x : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">) : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> x </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">+</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> x;</span></span>\n<span class=\"line\"><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">twice</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">21</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">);</span></span></code></pre><h2 id=\"what\"><a href=\"#what\">What</a></h2><p>A function is a <code>let</code> binding with parameters.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>Beginners learn faster when function syntax grows out of the same form as ordinary value bindings.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><ul>\n<li>Name the function after <code>let</code>.</li>\n<li>Put parameters in parentheses.</li>\n<li>Add a result type when it helps.</li>\n</ul>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><ul>\n<li>Write one one-argument function.</li>\n<li>Call it once.</li>\n<li>Rename the parameter more clearly.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not jump to methods or classes before plain functions feel ordinary.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/core/calls\">Calls</a>.</p>\n",
		"summaryHtml": "Define plain functions with let before learning calls or methods."
	},
	{
		"locale": "en",
		"id": "calls",
		"kind": "chapter",
		"partId": "core",
		"partTitle": "Core syntax",
		"path": "/learn/language/core/calls",
		"canonicalPath": "/learn/language/core/calls",
		"aliases": [
			"/docs/language/core/calls"
		],
		"questions": [],
		"title": "Calls",
		"description": "Learn call syntax as its own reading skill before adding dot calls.",
		"group": "Core syntax",
		"section": "Core syntax",
		"order": 10,
		"slug": "calls",
		"summary": "Call functions directly and follow argument flow left to right.",
		"descriptionHtml": "Learn call syntax as its own reading skill before adding dot calls.",
		"headings": [
			{
				"depth": 2,
				"id": "what",
				"text": "What"
			},
			{
				"depth": 2,
				"id": "why",
				"text": "Why"
			},
			{
				"depth": 2,
				"id": "how",
				"text": "How"
			},
			{
				"depth": 2,
				"id": "try-it",
				"text": "Try it"
			},
			{
				"depth": 2,
				"id": "common-mistake",
				"text": "Common mistake"
			},
			{
				"depth": 2,
				"id": "next",
				"text": "Next"
			}
		],
		"html": "<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> twice</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> (x : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">) : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> x </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">+</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> x;</span></span>\n<span class=\"line\"><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">twice</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">21</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">);</span></span></code></pre><h2 id=\"what\"><a href=\"#what\">What</a></h2><p>A call passes arguments to a callable value.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>Definitions and calls look different. Splitting them into separate chapters lowers mental load.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><ul>\n<li>Read the callee first.</li>\n<li>Read the argument list second.</li>\n<li>Keep first call sites small.</li>\n</ul>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><ul>\n<li>Call one function.</li>\n<li>Feed its result into a second call.</li>\n<li>Compare the clearer version.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not read a call as text substitution.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/core/methods\">Methods</a>.</p>\n",
		"summaryHtml": "Call functions directly and follow argument flow left to right."
	},
	{
		"locale": "en",
		"id": "methods",
		"kind": "chapter",
		"partId": "core",
		"partTitle": "Core syntax",
		"path": "/learn/language/core/methods",
		"canonicalPath": "/learn/language/core/methods",
		"aliases": [
			"/docs/language/core/methods"
		],
		"questions": [],
		"title": "Methods",
		"description": "Learn Musi’s attached-method model after plain functions and calls.",
		"group": "Core syntax",
		"section": "Core syntax",
		"order": 11,
		"slug": "methods",
		"summary": "Use receiver-prefixed methods and dot calls without needing an impl block.",
		"descriptionHtml": "Learn Musi’s attached-method model after plain functions and calls.",
		"headings": [
			{
				"depth": 2,
				"id": "what",
				"text": "What"
			},
			{
				"depth": 2,
				"id": "why",
				"text": "Why"
			},
			{
				"depth": 2,
				"id": "how",
				"text": "How"
			},
			{
				"depth": 2,
				"id": "try-it",
				"text": "Try it"
			},
			{
				"depth": 2,
				"id": "common-mistake",
				"text": "Common mistake"
			},
			{
				"depth": 2,
				"id": "next",
				"text": "Next"
			}
		],
		"html": "<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> (</span><span style=\"color:#E36209;--shiki-dark:#FFAB70\">self</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> : Int).</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">abs</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> () : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> self;</span></span></code></pre><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> one </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 1</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">one.</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">abs</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">();</span></span></code></pre><h2 id=\"what\"><a href=\"#what\">What</a></h2><p>Attached methods use receiver-prefixed declarations such as <code>let (self : Int).abs () := ...</code>.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>This keeps attachment explicit without adding an <code>impl</code> wrapper.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><ul>\n<li>Declare the receiver first.</li>\n<li>Put the method name after the dot in the declaration head.</li>\n<li>Call it with dot syntax.</li>\n</ul>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><ul>\n<li>Define one attached method.</li>\n<li>Call it with dot syntax.</li>\n<li>Compare it with an equivalent free function.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not assume dot call falls back to a free function.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/data/records\">Records</a>.</p>\n",
		"summaryHtml": "Use receiver-prefixed methods and dot calls without needing an impl block."
	},
	{
		"locale": "en",
		"id": "records",
		"kind": "chapter",
		"partId": "data",
		"partTitle": "Data",
		"path": "/learn/language/data/records",
		"canonicalPath": "/learn/language/data/records",
		"aliases": [
			"/docs/language/data/records"
		],
		"questions": [],
		"title": "Records",
		"description": "Use records for labeled data before mixing in arrays or pattern matching.",
		"group": "Data",
		"section": "Data",
		"order": 12,
		"slug": "records",
		"summary": "Build named-field values and access fields directly.",
		"descriptionHtml": "Use records for labeled data before mixing in arrays or pattern matching.",
		"headings": [
			{
				"depth": 2,
				"id": "what",
				"text": "What"
			},
			{
				"depth": 2,
				"id": "why",
				"text": "Why"
			},
			{
				"depth": 2,
				"id": "how",
				"text": "How"
			},
			{
				"depth": 2,
				"id": "try-it",
				"text": "Try it"
			},
			{
				"depth": 2,
				"id": "common-mistake",
				"text": "Common mistake"
			},
			{
				"depth": 2,
				"id": "next",
				"text": "Next"
			}
		],
		"html": "<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> point </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> { x </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 3</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">, y </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 4</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> };</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> values </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> [</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">1</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">, </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">2</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">, </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">3</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">];</span></span></code></pre><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> point3 </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> { </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">...</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">point, z </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 5</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> };</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> extended </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> [</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">0</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">, </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">...</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">values];</span></span></code></pre><h2 id=\"what\"><a href=\"#what\">What</a></h2><p>Records group named fields into one value.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>Field names make structure visible without adding too much syntax at once.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><ul>\n<li>Build a record with <code>{ field := value }</code>.</li>\n<li>Read fields with dot access.</li>\n<li>Use spread when building an updated copy helps.</li>\n</ul>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><ul>\n<li>Make one record with two fields.</li>\n<li>Read one field.</li>\n<li>Build a spread-updated copy.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not treat records as magical namespaces first.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/data/arrays-and-slices\">Arrays and slices</a>.</p>\n",
		"summaryHtml": "Build named-field values and access fields directly."
	},
	{
		"locale": "en",
		"id": "arrays-and-slices",
		"kind": "chapter",
		"partId": "data",
		"partTitle": "Data",
		"path": "/learn/language/data/arrays-and-slices",
		"canonicalPath": "/learn/language/data/arrays-and-slices",
		"aliases": [
			"/docs/language/data/arrays-and-slices"
		],
		"questions": [],
		"title": "Arrays and slices",
		"description": "Read sequence-shaped data without mixing it into pattern syntax yet.",
		"group": "Data",
		"section": "Data",
		"order": 13,
		"slug": "arrays-and-slices",
		"summary": "Store ordered values and learn where slices fit.",
		"descriptionHtml": "Read sequence-shaped data without mixing it into pattern syntax yet.",
		"headings": [
			{
				"depth": 2,
				"id": "what",
				"text": "What"
			},
			{
				"depth": 2,
				"id": "why",
				"text": "Why"
			},
			{
				"depth": 2,
				"id": "how",
				"text": "How"
			},
			{
				"depth": 2,
				"id": "try-it",
				"text": "Try it"
			},
			{
				"depth": 2,
				"id": "common-mistake",
				"text": "Common mistake"
			},
			{
				"depth": 2,
				"id": "next",
				"text": "Next"
			}
		],
		"html": "<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> point </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> { x </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 3</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">, y </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 4</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> };</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> values </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> [</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">1</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">, </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">2</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">, </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">3</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">];</span></span></code></pre><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> Slice</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> import</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> \"@std/slice\"</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Slice</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">.</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">concat</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">]([</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">1</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">], [</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">2</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">, </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">3</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">]);</span></span></code></pre><h2 id=\"what\"><a href=\"#what\">What</a></h2><p>Arrays hold ordered items. Slices describe sequence-oriented helper work and views.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>Sequences are easier to learn when they are not mixed with records and patterns at the same time.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><ul>\n<li>Build arrays with <code>[a, b, c]</code>.</li>\n<li>Use spread to build related arrays.</li>\n<li>Reach for <code>@std/slice</code> helpers when you need sequence operations.</li>\n</ul>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><ul>\n<li>Create one array.</li>\n<li>Build a second array with spread.</li>\n<li>Call one <code>Slice</code> helper.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not assume arrays and slices are the same word for the same thing.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/data/patterns\">Patterns</a>.</p>\n",
		"summaryHtml": "Store ordered values and learn where slices fit."
	},
	{
		"locale": "en",
		"id": "patterns",
		"kind": "chapter",
		"partId": "data",
		"partTitle": "Data",
		"path": "/learn/language/data/patterns",
		"canonicalPath": "/learn/language/data/patterns",
		"aliases": [
			"/docs/language/data/patterns"
		],
		"questions": [],
		"title": "Patterns",
		"description": "Learn pattern matching after records and arrays, not before.",
		"group": "Data",
		"section": "Data",
		"order": 14,
		"slug": "patterns",
		"summary": "Use case and destructuring to branch on data shape.",
		"descriptionHtml": "Learn pattern matching after records and arrays, not before.",
		"headings": [
			{
				"depth": 2,
				"id": "what",
				"text": "What"
			},
			{
				"depth": 2,
				"id": "why",
				"text": "Why"
			},
			{
				"depth": 2,
				"id": "how",
				"text": "How"
			},
			{
				"depth": 2,
				"id": "try-it",
				"text": "Try it"
			},
			{
				"depth": 2,
				"id": "common-mistake",
				"text": "Common mistake"
			},
			{
				"depth": 2,
				"id": "next",
				"text": "Next"
			}
		],
		"html": "<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> Port</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> data</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> {</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">  | </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Configured</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">  | </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Default</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">};</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> port : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Port</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> .Configured</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">8080</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">);</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">case</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> port </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">of</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> (</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">| </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">.Configured</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(port) </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">=></span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> port</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">| </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">.Default</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> =></span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 3000</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">);</span></span></code></pre><h2 id=\"what\"><a href=\"#what\">What</a></h2><p>Patterns inspect structured values by shape.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>Pattern matching lands much better once you already understand the shapes you are matching.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><ul>\n<li>Define a small <code>data</code> type.</li>\n<li>Construct one value.</li>\n<li>Match it with <code>case ... of</code>.</li>\n</ul>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><ul>\n<li>Define a two-case <code>data</code> type.</li>\n<li>Construct each case once.</li>\n<li>Return different values from a <code>case</code>.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not start with deeply nested patterns while simple constructor matching still feels new.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/organization/files\">Files</a>.</p>\n",
		"summaryHtml": "Use case and destructuring to branch on data shape."
	},
	{
		"locale": "en",
		"id": "files",
		"kind": "chapter",
		"partId": "organization",
		"partTitle": "Code organization",
		"path": "/learn/language/organization/files",
		"canonicalPath": "/learn/language/organization/files",
		"aliases": [
			"/docs/language/organization/files"
		],
		"questions": [],
		"title": "Files",
		"description": "Separate file reading from package structure to reduce beginner overload.",
		"group": "Code organization",
		"section": "Code organization",
		"order": 15,
		"slug": "files",
		"summary": "Know what a single file means before building a package.",
		"descriptionHtml": "Separate file reading from package structure to reduce beginner overload.",
		"headings": [
			{
				"depth": 2,
				"id": "what",
				"text": "What"
			},
			{
				"depth": 2,
				"id": "why",
				"text": "Why"
			},
			{
				"depth": 2,
				"id": "how",
				"text": "How"
			},
			{
				"depth": 2,
				"id": "try-it",
				"text": "Try it"
			},
			{
				"depth": 2,
				"id": "common-mistake",
				"text": "Common mistake"
			},
			{
				"depth": 2,
				"id": "next",
				"text": "Next"
			}
		],
		"html": "<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> answer </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 42</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">answer;</span></span></code></pre><h2 id=\"what\"><a href=\"#what\">What</a></h2><p>A Musi file is a direct unit of source.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>Readers usually understand one file before they understand package manifests.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><ul>\n<li>Start with one <code>index.ms</code>.</li>\n<li>Keep related code close while learning.</li>\n<li>Move to package structure only when it helps.</li>\n</ul>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><ul>\n<li>Create a scratch file.</li>\n<li>Rename it once.</li>\n<li>Run <code>music check</code> on it.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not jump into manifest details before one-file flow feels ordinary.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/organization/packages\">Packages</a>.</p>\n",
		"summaryHtml": "Know what a single file means before building a package."
	},
	{
		"locale": "en",
		"id": "packages",
		"kind": "chapter",
		"partId": "organization",
		"partTitle": "Code organization",
		"path": "/learn/language/organization/packages",
		"canonicalPath": "/learn/language/organization/packages",
		"aliases": [
			"/docs/language/organization/packages"
		],
		"questions": [],
		"title": "Packages",
		"description": "Learn package roots and entry files after single-file work makes sense.",
		"group": "Code organization",
		"section": "Code organization",
		"order": 16,
		"slug": "packages",
		"summary": "Move from one file to package-managed code without changing mental models.",
		"descriptionHtml": "Learn package roots and entry files after single-file work makes sense.",
		"headings": [
			{
				"depth": 2,
				"id": "what",
				"text": "What"
			},
			{
				"depth": 2,
				"id": "why",
				"text": "Why"
			},
			{
				"depth": 2,
				"id": "how",
				"text": "How"
			},
			{
				"depth": 2,
				"id": "try-it",
				"text": "Try it"
			},
			{
				"depth": 2,
				"id": "common-mistake",
				"text": "Common mistake"
			},
			{
				"depth": 2,
				"id": "next",
				"text": "Next"
			}
		],
		"html": "<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">musi</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> new</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> hello</span></span>\n<span class=\"line\"><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">cd</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> hello</span></span>\n<span class=\"line\"><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">musi</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> run</span></span></code></pre><h2 id=\"what\"><a href=\"#what\">What</a></h2><p>A package groups files, dependencies, and entry settings under <code>musi.json</code>.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>Package structure matters once code grows, but it is too much ceremony for a first file.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><ul>\n<li>Create a package with <code>musi new hello</code>.</li>\n<li>Inspect the generated structure.</li>\n<li>Use <code>musi run</code>, <code>musi check</code>, and <code>musi test</code>.</li>\n</ul>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><ul>\n<li>Create one package.</li>\n<li>Open its entry file.</li>\n<li>Run <code>musi run</code>.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not assume package work replaces direct file work.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/organization/imports-and-exports\">Imports and exports</a>.</p>\n",
		"summaryHtml": "Move from one file to package-managed code without changing mental models."
	},
	{
		"locale": "en",
		"id": "imports-and-exports",
		"kind": "chapter",
		"partId": "organization",
		"partTitle": "Code organization",
		"path": "/learn/language/organization/imports-and-exports",
		"canonicalPath": "/learn/language/organization/imports-and-exports",
		"aliases": [
			"/docs/language/organization/imports-and-exports"
		],
		"questions": [],
		"title": "Imports and exports",
		"description": "Use imports and exports after package shape is clear.",
		"group": "Code organization",
		"section": "Code organization",
		"order": 17,
		"slug": "imports-and-exports",
		"summary": "Bring code in explicitly and expose only what other files need.",
		"descriptionHtml": "Use imports and exports after package shape is clear.",
		"headings": [
			{
				"depth": 2,
				"id": "what",
				"text": "What"
			},
			{
				"depth": 2,
				"id": "why",
				"text": "Why"
			},
			{
				"depth": 2,
				"id": "how",
				"text": "How"
			},
			{
				"depth": 2,
				"id": "try-it",
				"text": "Try it"
			},
			{
				"depth": 2,
				"id": "common-mistake",
				"text": "Common mistake"
			},
			{
				"depth": 2,
				"id": "next",
				"text": "Next"
			}
		],
		"html": "<div class=\"code-tabs\" data-example-id=\"import-stdlib\">\n<div class=\"code-tabs-meta\">\n<p class=\"code-tabs-caption\">Import the standard library, then reach the family you need. Musi keeps stdlib access explicit through <code>@std</code>.</p>\n<p class=\"code-tabs-note\">Like checking out a toolbox before work: import once, then use the tools by name. In Musi, imports are values you can pass around.</p>\n</div>\n<section role=\"tabpanel\" class=\"code-panel\">\n<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> Option</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> import</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> \"@std/option\"</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> value </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> Option</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">.</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">some</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">1</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">);</span></span></code></pre>\n</section>\n</div><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">export</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> answer </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 42</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> Local</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> import</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> \"./index.ms\"</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Local</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">.</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">answer</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span></code></pre><h2 id=\"what\"><a href=\"#what\">What</a></h2><p>Imports bring values or modules into scope. Exports mark names other files may use.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>Explicit module boundaries make dependency flow visible.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><ul>\n<li>Import with <code>let Name := import &quot;spec&quot;;</code>.</li>\n<li>Export only names you want to publish.</li>\n<li>Prefer small public surfaces.</li>\n</ul>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><ul>\n<li>Import one <code>@std</code> module.</li>\n<li>Export one helper.</li>\n<li>Use it from a second file.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not use imports as a substitute for naming discipline.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/types/type-annotations\">Type annotations</a>.</p>\n",
		"summaryHtml": "Bring code in explicitly and expose only what other files need."
	},
	{
		"locale": "en",
		"id": "type-annotations",
		"kind": "chapter",
		"partId": "types",
		"partTitle": "Types",
		"path": "/learn/language/types/type-annotations",
		"canonicalPath": "/learn/language/types/type-annotations",
		"aliases": [
			"/docs/language/types/type-annotations"
		],
		"questions": [],
		"title": "Type annotations",
		"description": "Introduce type annotations before inference and generics.",
		"group": "Types",
		"section": "Types",
		"order": 18,
		"slug": "type-annotations",
		"summary": "Add type information where it helps readers and tools.",
		"descriptionHtml": "Introduce type annotations before inference and generics.",
		"headings": [
			{
				"depth": 2,
				"id": "what",
				"text": "What"
			},
			{
				"depth": 2,
				"id": "why",
				"text": "Why"
			},
			{
				"depth": 2,
				"id": "how",
				"text": "How"
			},
			{
				"depth": 2,
				"id": "try-it",
				"text": "Try it"
			},
			{
				"depth": 2,
				"id": "common-mistake",
				"text": "Common mistake"
			},
			{
				"depth": 2,
				"id": "next",
				"text": "Next"
			}
		],
		"html": "<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> port : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 8080</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> identityFn</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">T</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">] (input : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">T</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">) : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">T</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> input;</span></span></code></pre><h2 id=\"what\"><a href=\"#what\">What</a></h2><p>Type annotations tell readers and the compiler what kind of value or result you expect.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>They provide anchors before inference and generic code enter the picture.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><ul>\n<li>Annotate important bindings first.</li>\n<li>Annotate public helpers before tiny locals.</li>\n<li>Use annotations for clarity, not decoration.</li>\n</ul>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><ul>\n<li>Annotate one integer binding.</li>\n<li>Annotate one function result.</li>\n<li>Compare readability.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not annotate everything by habit.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/types/type-inference\">Type inference</a>.</p>\n",
		"summaryHtml": "Add type information where it helps readers and tools."
	},
	{
		"locale": "en",
		"id": "type-inference",
		"kind": "chapter",
		"partId": "types",
		"partTitle": "Types",
		"path": "/learn/language/types/type-inference",
		"canonicalPath": "/learn/language/types/type-inference",
		"aliases": [
			"/docs/language/types/type-inference"
		],
		"questions": [],
		"title": "Type inference",
		"description": "Learn how inference reduces repetition without hiding too much.",
		"group": "Types",
		"section": "Types",
		"order": 19,
		"slug": "type-inference",
		"summary": "See what Musi can infer so you know when to write less.",
		"descriptionHtml": "Learn how inference reduces repetition without hiding too much.",
		"headings": [
			{
				"depth": 2,
				"id": "what",
				"text": "What"
			},
			{
				"depth": 2,
				"id": "why",
				"text": "Why"
			},
			{
				"depth": 2,
				"id": "how",
				"text": "How"
			},
			{
				"depth": 2,
				"id": "try-it",
				"text": "Try it"
			},
			{
				"depth": 2,
				"id": "common-mistake",
				"text": "Common mistake"
			},
			{
				"depth": 2,
				"id": "next",
				"text": "Next"
			}
		],
		"html": "<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> port : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 8080</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> next </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> port </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">+</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 1</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">next;</span></span></code></pre><h2 id=\"what\"><a href=\"#what\">What</a></h2><p>Inference fills in some type information from surrounding code.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>Good inference keeps code shorter, but beginners need to know where convenience stops.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><ul>\n<li>Start from an annotated example.</li>\n<li>Remove one obvious annotation at a time.</li>\n<li>Add annotations back when clarity drops.</li>\n</ul>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><ul>\n<li>Write one annotated binding.</li>\n<li>Remove one obvious annotation.</li>\n<li>Keep one non-obvious annotation.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not guess what inference can do in a hard example.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/types/generics\">Generics</a>.</p>\n",
		"summaryHtml": "See what Musi can infer so you know when to write less."
	},
	{
		"locale": "en",
		"id": "generics",
		"kind": "chapter",
		"partId": "types",
		"partTitle": "Types",
		"path": "/learn/language/types/generics",
		"canonicalPath": "/learn/language/types/generics",
		"aliases": [
			"/docs/language/types/generics"
		],
		"questions": [],
		"title": "Generics",
		"description": "Introduce type parameters after annotations and inference make sense.",
		"group": "Types",
		"section": "Types",
		"order": 20,
		"slug": "generics",
		"summary": "Write reusable functions over many types without losing clarity.",
		"descriptionHtml": "Introduce type parameters after annotations and inference make sense.",
		"headings": [
			{
				"depth": 2,
				"id": "what",
				"text": "What"
			},
			{
				"depth": 2,
				"id": "why",
				"text": "Why"
			},
			{
				"depth": 2,
				"id": "how",
				"text": "How"
			},
			{
				"depth": 2,
				"id": "try-it",
				"text": "Try it"
			},
			{
				"depth": 2,
				"id": "common-mistake",
				"text": "Common mistake"
			},
			{
				"depth": 2,
				"id": "next",
				"text": "Next"
			}
		],
		"html": "<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> port : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 8080</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> identityFn</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">T</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">] (input : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">T</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">) : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">T</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> input;</span></span></code></pre><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">identityFn[</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">](port);</span></span></code></pre><h2 id=\"what\"><a href=\"#what\">What</a></h2><p>Generics let one definition work across many types by using type parameters such as <code>T</code>.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>Without generics, reusable code quickly turns into copy-paste or overly concrete helpers.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><ul>\n<li>Add a type parameter list like <code>[T]</code>.</li>\n<li>Use that parameter in inputs and outputs.</li>\n<li>Apply the generic explicitly when it helps reading.</li>\n</ul>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><ul>\n<li>Write one generic identity function.</li>\n<li>Call it with <code>Int</code>.</li>\n<li>Call it with another type.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not add several type parameters before one-parameter generic code feels natural.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/abstractions/classes\">Classes</a>.</p>\n",
		"summaryHtml": "Write reusable functions over many types without losing clarity."
	},
	{
		"locale": "en",
		"id": "classes",
		"kind": "chapter",
		"partId": "abstractions",
		"partTitle": "Abstractions",
		"path": "/learn/language/abstractions/classes",
		"canonicalPath": "/learn/language/abstractions/classes",
		"aliases": [
			"/docs/language/abstractions/classes"
		],
		"questions": [],
		"title": "Classes",
		"description": "Start abstraction work with class declarations alone before instances or laws.",
		"group": "Abstractions",
		"section": "Abstractions",
		"order": 21,
		"slug": "classes",
		"summary": "Describe shared behavior with class declarations.",
		"descriptionHtml": "Start abstraction work with class declarations alone before instances or laws.",
		"headings": [
			{
				"depth": 2,
				"id": "what",
				"text": "What"
			},
			{
				"depth": 2,
				"id": "why",
				"text": "Why"
			},
			{
				"depth": 2,
				"id": "how",
				"text": "How"
			},
			{
				"depth": 2,
				"id": "try-it",
				"text": "Try it"
			},
			{
				"depth": 2,
				"id": "common-mistake",
				"text": "Common mistake"
			},
			{
				"depth": 2,
				"id": "next",
				"text": "Next"
			}
		],
		"html": "<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> Eq</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">T</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">] </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> class</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> {</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">  let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> (</span><span style=\"color:#D73A49;--shiki-dark:#F97583\">=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">) (a : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">T</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">, b : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">T</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">) : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Bool</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">  law</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> reflexive</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> (x : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">T</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">) </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> .True</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">};</span></span></code></pre><h2 id=\"what\"><a href=\"#what\">What</a></h2><p>A class describes a behavior surface that different types can implement.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>Beginners need to separate “what behavior should exist” from “how a type implements it”.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><ul>\n<li>Define one class with one operation.</li>\n<li>Read it as a contract shape.</li>\n<li>Keep the first class tiny.</li>\n</ul>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><ul>\n<li>Define one class member.</li>\n<li>Name the behavior clearly.</li>\n<li>List the types that should implement it.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not read classes here as inheritance trees.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/abstractions/instances\">Instances</a>.</p>\n",
		"summaryHtml": "Describe shared behavior with class declarations."
	},
	{
		"locale": "en",
		"id": "instances",
		"kind": "chapter",
		"partId": "abstractions",
		"partTitle": "Abstractions",
		"path": "/learn/language/abstractions/instances",
		"canonicalPath": "/learn/language/abstractions/instances",
		"aliases": [
			"/docs/language/abstractions/instances"
		],
		"questions": [],
		"title": "Instances",
		"description": "Learn instance declarations only after classes make sense on their own.",
		"group": "Abstractions",
		"section": "Abstractions",
		"order": 22,
		"slug": "instances",
		"summary": "Attach concrete behavior to concrete types.",
		"descriptionHtml": "Learn instance declarations only after classes make sense on their own.",
		"headings": [
			{
				"depth": 2,
				"id": "what",
				"text": "What"
			},
			{
				"depth": 2,
				"id": "why",
				"text": "Why"
			},
			{
				"depth": 2,
				"id": "how",
				"text": "How"
			},
			{
				"depth": 2,
				"id": "try-it",
				"text": "Try it"
			},
			{
				"depth": 2,
				"id": "common-mistake",
				"text": "Common mistake"
			},
			{
				"depth": 2,
				"id": "next",
				"text": "Next"
			}
		],
		"html": "<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> eqInt </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> instance</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> Eq</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">] {</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">  let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> (</span><span style=\"color:#D73A49;--shiki-dark:#F97583\">=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">) (a : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">, b : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">) : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Bool</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> .True</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">};</span></span></code></pre><h2 id=\"what\"><a href=\"#what\">What</a></h2><p>An instance says how one specific type satisfies a class.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>Splitting classes from instances reduces mental overload.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><ul>\n<li>Choose one class.</li>\n<li>Choose one concrete type.</li>\n<li>Implement the required members in one instance block.</li>\n</ul>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><ul>\n<li>Take a tiny class.</li>\n<li>Add one instance for <code>Int</code>.</li>\n<li>Use it once.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not define several classes and instances at once during early practice.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/abstractions/laws\">Laws</a>.</p>\n",
		"summaryHtml": "Attach concrete behavior to concrete types."
	},
	{
		"locale": "en",
		"id": "laws",
		"kind": "chapter",
		"partId": "abstractions",
		"partTitle": "Abstractions",
		"path": "/learn/language/abstractions/laws",
		"canonicalPath": "/learn/language/abstractions/laws",
		"aliases": [
			"/docs/language/abstractions/laws"
		],
		"questions": [],
		"title": "Laws",
		"description": "Add semantic expectations after class and instance basics.",
		"group": "Abstractions",
		"section": "Abstractions",
		"order": 23,
		"slug": "laws",
		"summary": "Use laws to document the meaning of an abstraction, not just its shape.",
		"descriptionHtml": "Add semantic expectations after class and instance basics.",
		"headings": [
			{
				"depth": 2,
				"id": "what",
				"text": "What"
			},
			{
				"depth": 2,
				"id": "why",
				"text": "Why"
			},
			{
				"depth": 2,
				"id": "how",
				"text": "How"
			},
			{
				"depth": 2,
				"id": "try-it",
				"text": "Try it"
			},
			{
				"depth": 2,
				"id": "common-mistake",
				"text": "Common mistake"
			},
			{
				"depth": 2,
				"id": "next",
				"text": "Next"
			}
		],
		"html": "<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> Eq</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">T</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">] </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> class</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> {</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">  let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> (</span><span style=\"color:#D73A49;--shiki-dark:#F97583\">=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">) (a : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">T</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">, b : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">T</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">) : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Bool</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">  law</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> reflexive</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> (x : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">T</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">) </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> .True</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">};</span></span></code></pre><h2 id=\"what\"><a href=\"#what\">What</a></h2><p>Laws express behavioral expectations for a class.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>Signatures alone do not explain semantics.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><ul>\n<li>Keep early laws small.</li>\n<li>Express one idea per law.</li>\n<li>Explain each law in plain language.</li>\n</ul>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><ul>\n<li>Read one class law.</li>\n<li>Restate it in plain English.</li>\n<li>Imagine a bad instance that breaks it.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not write vague laws.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/effects-runtime/effects\">Effects</a>.</p>\n",
		"summaryHtml": "Use laws to document the meaning of an abstraction, not just its shape."
	},
	{
		"locale": "en",
		"id": "effects",
		"kind": "chapter",
		"partId": "effects-runtime",
		"partTitle": "Effects and runtime",
		"path": "/learn/language/effects-runtime/effects",
		"canonicalPath": "/learn/language/effects-runtime/effects",
		"aliases": [
			"/docs/language/effects-runtime/effects"
		],
		"questions": [],
		"title": "Effects",
		"description": "Introduce effect vocabulary before using clauses or handlers.",
		"group": "Effects and runtime",
		"section": "Effects and runtime",
		"order": 24,
		"slug": "effects",
		"summary": "Understand effects as requests for work, not immediate hidden side effects.",
		"descriptionHtml": "Introduce effect vocabulary before using clauses or handlers.",
		"headings": [
			{
				"depth": 2,
				"id": "what",
				"text": "What"
			},
			{
				"depth": 2,
				"id": "why",
				"text": "Why"
			},
			{
				"depth": 2,
				"id": "how",
				"text": "How"
			},
			{
				"depth": 2,
				"id": "try-it",
				"text": "Try it"
			},
			{
				"depth": 2,
				"id": "common-mistake",
				"text": "Common mistake"
			},
			{
				"depth": 2,
				"id": "next",
				"text": "Next"
			}
		],
		"html": "<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> console </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> effect</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> {</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">  let</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> readln</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> () : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">String</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">};</span></span></code></pre><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">perform</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> console.</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">readln</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">();</span></span></code></pre><h2 id=\"what\"><a href=\"#what\">What</a></h2><p>An effect describes operations that code may request.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>Visible effect requests are easier to reason about than hidden ambient side effects.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><ul>\n<li>Define a small effect.</li>\n<li>Perform one operation from it.</li>\n<li>Read the operation as a request, not magic global access.</li>\n</ul>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><ul>\n<li>Define one effect.</li>\n<li>Perform one operation.</li>\n<li>Describe what must now handle it.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not try to learn full handlers before the request model is stable.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/effects-runtime/using\">Using</a>.</p>\n",
		"summaryHtml": "Understand effects as requests for work, not immediate hidden side effects."
	},
	{
		"locale": "en",
		"id": "using",
		"kind": "chapter",
		"partId": "effects-runtime",
		"partTitle": "Effects and runtime",
		"path": "/learn/language/effects-runtime/using",
		"canonicalPath": "/learn/language/effects-runtime/using",
		"aliases": [
			"/docs/language/effects-runtime/using"
		],
		"questions": [],
		"title": "Using",
		"description": "Learn using syntax before full handlers so capability requirements stay visible.",
		"group": "Effects and runtime",
		"section": "Effects and runtime",
		"order": 25,
		"slug": "using",
		"summary": "Read and write using clauses as explicit capability flow.",
		"descriptionHtml": "Learn using syntax before full handlers so capability requirements stay visible.",
		"headings": [
			{
				"depth": 2,
				"id": "what",
				"text": "What"
			},
			{
				"depth": 2,
				"id": "why",
				"text": "Why"
			},
			{
				"depth": 2,
				"id": "how",
				"text": "How"
			},
			{
				"depth": 2,
				"id": "try-it",
				"text": "Try it"
			},
			{
				"depth": 2,
				"id": "common-mistake",
				"text": "Common mistake"
			},
			{
				"depth": 2,
				"id": "next",
				"text": "Next"
			}
		],
		"html": "<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> readClosed</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> (x : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">) : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">String</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> using</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> { </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Console</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> } </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">  perform</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> State</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">.</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">readln</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">();</span></span></code></pre><h2 id=\"what\"><a href=\"#what\">What</a></h2><p><code>using</code> spells out effect requirements in declarations and types.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>Capability flow stays readable only when requirements stay explicit.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><ul>\n<li>Read <code>using { Console }</code> as a required capability set.</li>\n<li>Put <code>using</code> where the signature should expose that need.</li>\n<li>Keep early examples to one capability.</li>\n</ul>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><ul>\n<li>Write one signature with <code>using</code>.</li>\n<li>Read it aloud in plain English.</li>\n<li>Compare it with a hidden requirement.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not treat <code>using</code> as decoration.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/effects-runtime/handlers\">Handlers</a>.</p>\n",
		"summaryHtml": "Read and write using clauses as explicit capability flow."
	},
	{
		"locale": "en",
		"id": "handlers",
		"kind": "chapter",
		"partId": "effects-runtime",
		"partTitle": "Effects and runtime",
		"path": "/learn/language/effects-runtime/handlers",
		"canonicalPath": "/learn/language/effects-runtime/handlers",
		"aliases": [
			"/docs/language/effects-runtime/handlers"
		],
		"questions": [],
		"title": "Handlers",
		"description": "Handle effects after the effect and using model are already clear.",
		"group": "Effects and runtime",
		"section": "Effects and runtime",
		"order": 26,
		"slug": "handlers",
		"summary": "Resolve performed effects at the boundary where policy belongs.",
		"descriptionHtml": "Handle effects after the effect and using model are already clear.",
		"headings": [
			{
				"depth": 2,
				"id": "what",
				"text": "What"
			},
			{
				"depth": 2,
				"id": "why",
				"text": "Why"
			},
			{
				"depth": 2,
				"id": "how",
				"text": "How"
			},
			{
				"depth": 2,
				"id": "try-it",
				"text": "Try it"
			},
			{
				"depth": 2,
				"id": "common-mistake",
				"text": "Common mistake"
			},
			{
				"depth": 2,
				"id": "next",
				"text": "Next"
			}
		],
		"html": "<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">handle</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> perform</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> console.</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">readln</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">() </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">using</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> console {</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">  value </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">=></span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> value;</span></span>\n<span class=\"line\"><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">  readln</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(k) </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">=></span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> resume</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> \"ok\"</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">};</span></span></code></pre><h2 id=\"what\"><a href=\"#what\">What</a></h2><p>A handler decides what to do when effect operations are performed.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>This keeps core logic separate from boundary policy.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><ul>\n<li>Wrap effectful work in <code>handle ... using ...</code>.</li>\n<li>Provide a value clause.</li>\n<li>Provide the effect clauses you need.</li>\n</ul>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><ul>\n<li>Perform one effect operation.</li>\n<li>Handle it once.</li>\n<li>Change the handled result.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not push all business logic into handlers.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/effects-runtime/foundation\">Foundation</a>.</p>\n",
		"summaryHtml": "Resolve performed effects at the boundary where policy belongs."
	},
	{
		"locale": "en",
		"id": "foundation",
		"kind": "chapter",
		"partId": "effects-runtime",
		"partTitle": "Effects and runtime",
		"path": "/learn/language/effects-runtime/foundation",
		"canonicalPath": "/learn/language/effects-runtime/foundation",
		"aliases": [
			"/docs/language/effects-runtime/foundation"
		],
		"questions": [],
		"title": "Foundation",
		"description": "Separate language foundation from runtime and stdlib layers.",
		"group": "Effects and runtime",
		"section": "Effects and runtime",
		"order": 27,
		"slug": "foundation",
		"summary": "Understand what belongs to musi:core before reaching for stdlib modules.",
		"descriptionHtml": "Separate language foundation from runtime and stdlib layers.",
		"headings": [
			{
				"depth": 2,
				"id": "what",
				"text": "What"
			},
			{
				"depth": 2,
				"id": "why",
				"text": "Why"
			},
			{
				"depth": 2,
				"id": "how",
				"text": "How"
			},
			{
				"depth": 2,
				"id": "try-it",
				"text": "Try it"
			},
			{
				"depth": 2,
				"id": "common-mistake",
				"text": "Common mistake"
			},
			{
				"depth": 2,
				"id": "next",
				"text": "Next"
			}
		],
		"html": "<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> Core</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> import</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> \"musi:core\"</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Core</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span></code></pre><h2 id=\"what\"><a href=\"#what\">What</a></h2><p><code>musi:core</code> holds foundation-level language concepts.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>Beginners need a clear answer to “what is built in” versus “what is library code”.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><ul>\n<li>Treat <code>musi:core</code> as low-level foundation.</li>\n<li>Reach for it rarely in ordinary app code.</li>\n<li>Prefer <code>@std</code> for day-to-day helpers.</li>\n</ul>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><ul>\n<li>Import one foundation module.</li>\n<li>Name why it feels lower-level.</li>\n<li>Compare it with <code>@std</code>.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not build ordinary app APIs directly on foundation modules when a clearer stdlib layer exists.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/effects-runtime/runtime\">Runtime</a>.</p>\n",
		"summaryHtml": "Understand what belongs to musi:core before reaching for stdlib modules."
	},
	{
		"locale": "en",
		"id": "runtime",
		"kind": "chapter",
		"partId": "effects-runtime",
		"partTitle": "Effects and runtime",
		"path": "/learn/language/effects-runtime/runtime",
		"canonicalPath": "/learn/language/effects-runtime/runtime",
		"aliases": [
			"/docs/language/effects-runtime/runtime"
		],
		"questions": [],
		"title": "Runtime",
		"description": "Learn what runtime-backed imports are for and why they are separate from stdlib helpers.",
		"group": "Effects and runtime",
		"section": "Effects and runtime",
		"order": 28,
		"slug": "runtime",
		"summary": "Use musi:runtime for runtime-backed capabilities and host services.",
		"descriptionHtml": "Learn what runtime-backed imports are for and why they are separate from stdlib helpers.",
		"headings": [
			{
				"depth": 2,
				"id": "what",
				"text": "What"
			},
			{
				"depth": 2,
				"id": "why",
				"text": "Why"
			},
			{
				"depth": 2,
				"id": "how",
				"text": "How"
			},
			{
				"depth": 2,
				"id": "try-it",
				"text": "Try it"
			},
			{
				"depth": 2,
				"id": "common-mistake",
				"text": "Common mistake"
			},
			{
				"depth": 2,
				"id": "next",
				"text": "Next"
			}
		],
		"html": "<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> Runtime</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> import</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> \"musi:runtime\"</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Runtime</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">.</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">envGet</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\">\"HOME\"</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">);</span></span></code></pre><h2 id=\"what\"><a href=\"#what\">What</a></h2><p><code>musi:runtime</code> is the boundary for runtime-backed services.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>Host-powered behavior has different boundaries than pure helpers.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><ul>\n<li>Read <code>musi:runtime</code> imports as lower-level capability surfaces.</li>\n<li>Prefer <code>@std</code> wrappers in ordinary code.</li>\n<li>Reach lower only when you are building boundaries.</li>\n</ul>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><ul>\n<li>Import <code>musi:runtime</code>.</li>\n<li>Call one helper.</li>\n<li>Compare it with <code>@std/env</code>.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not confuse <code>musi:runtime</code> with general-purpose stdlib.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/effects-runtime/stdlib\">Stdlib</a>.</p>\n",
		"summaryHtml": "Use musi:runtime for runtime-backed capabilities and host services."
	},
	{
		"locale": "en",
		"id": "stdlib",
		"kind": "chapter",
		"partId": "effects-runtime",
		"partTitle": "Effects and runtime",
		"path": "/learn/language/effects-runtime/stdlib",
		"canonicalPath": "/learn/language/effects-runtime/stdlib",
		"aliases": [
			"/docs/language/effects-runtime/stdlib"
		],
		"questions": [],
		"title": "Stdlib",
		"description": "Place the standard library on top of foundation and runtime so the layering stays clear.",
		"group": "Effects and runtime",
		"section": "Effects and runtime",
		"order": 29,
		"slug": "stdlib",
		"summary": "Reach for @std modules first in ordinary application code.",
		"descriptionHtml": "Place the standard library on top of foundation and runtime so the layering stays clear.",
		"headings": [
			{
				"depth": 2,
				"id": "what",
				"text": "What"
			},
			{
				"depth": 2,
				"id": "why",
				"text": "Why"
			},
			{
				"depth": 2,
				"id": "how",
				"text": "How"
			},
			{
				"depth": 2,
				"id": "try-it",
				"text": "Try it"
			},
			{
				"depth": 2,
				"id": "common-mistake",
				"text": "Common mistake"
			},
			{
				"depth": 2,
				"id": "next",
				"text": "Next"
			}
		],
		"html": "<div class=\"code-tabs\" data-example-id=\"import-stdlib\">\n<div class=\"code-tabs-meta\">\n<p class=\"code-tabs-caption\">Import the standard library, then reach the family you need. Musi keeps stdlib access explicit through <code>@std</code>.</p>\n<p class=\"code-tabs-note\">Like checking out a toolbox before work: import once, then use the tools by name. In Musi, imports are values you can pass around.</p>\n</div>\n<section role=\"tabpanel\" class=\"code-panel\">\n<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> Option</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> import</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> \"@std/option\"</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> value </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> Option</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">.</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">some</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">1</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">);</span></span></code></pre>\n</section>\n</div><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> Testing</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> import</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> \"@std/testing\"</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">export</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> let</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> test</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> () </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span></span>\n<span class=\"line\"><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">  Testing</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">.</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">it</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\">\"adds values\"</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">, </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Testing</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">.</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">toBe</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">1</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> +</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 2</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">, </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">3</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">));</span></span></code></pre><h2 id=\"what\"><a href=\"#what\">What</a></h2><p><code>@std</code> is the user-facing standard library.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>Most users should learn one clear library surface first.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><ul>\n<li>Import focused stdlib packages directly.</li>\n<li>Prefer modules like <code>@std/option</code> and <code>@std/testing</code>.</li>\n<li>Use the root catalog only when it reads better.</li>\n</ul>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><ul>\n<li>Import one focused <code>@std</code> module.</li>\n<li>Call one helper.</li>\n<li>Add one test helper import.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not reach for low-level <code>musi:*</code> imports when a clear <code>@std</code> module already solves the job.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/advanced/attributes\">Attributes</a>.</p>\n",
		"summaryHtml": "Reach for @std modules first in ordinary application code."
	},
	{
		"locale": "en",
		"id": "attributes",
		"kind": "chapter",
		"partId": "advanced",
		"partTitle": "Advanced and tooling",
		"path": "/learn/language/advanced/attributes",
		"canonicalPath": "/learn/language/advanced/attributes",
		"aliases": [
			"/docs/language/advanced/attributes"
		],
		"questions": [],
		"title": "Attributes",
		"description": "Introduce attributes separately from foreign declarations so each concept stays narrow.",
		"group": "Advanced and tooling",
		"section": "Advanced and tooling",
		"order": 30,
		"slug": "attributes",
		"summary": "Use attributes when the compiler or runtime needs explicit extra metadata.",
		"descriptionHtml": "Introduce attributes separately from foreign declarations so each concept stays narrow.",
		"headings": [
			{
				"depth": 2,
				"id": "what",
				"text": "What"
			},
			{
				"depth": 2,
				"id": "why",
				"text": "Why"
			},
			{
				"depth": 2,
				"id": "how",
				"text": "How"
			},
			{
				"depth": 2,
				"id": "try-it",
				"text": "Try it"
			},
			{
				"depth": 2,
				"id": "common-mistake",
				"text": "Common mistake"
			},
			{
				"depth": 2,
				"id": "next",
				"text": "Next"
			}
		],
		"html": "<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">@</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">link</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(name </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> \"c\"</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">) </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">foreign</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> \"c\" </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> puts</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> (msg : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">CString</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">) : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span></code></pre><h2 id=\"what\"><a href=\"#what\">What</a></h2><p>Attributes attach structured metadata to declarations.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>Readers should see attributes as explicit metadata, not mysterious decoration.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><ul>\n<li>Put the attribute on the declaration it modifies.</li>\n<li>Keep first uses small.</li>\n<li>Use attributes only when plain syntax cannot say the same thing clearly.</li>\n</ul>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><ul>\n<li>Read one attributed declaration.</li>\n<li>Identify what the attribute changes.</li>\n<li>Decide whether plain syntax could have expressed it.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not collect attributes casually.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/advanced/foreign\">Foreign</a>.</p>\n",
		"summaryHtml": "Use attributes when the compiler or runtime needs explicit extra metadata."
	},
	{
		"locale": "en",
		"id": "foreign",
		"kind": "chapter",
		"partId": "advanced",
		"partTitle": "Advanced and tooling",
		"path": "/learn/language/advanced/foreign",
		"canonicalPath": "/learn/language/advanced/foreign",
		"aliases": [
			"/docs/language/advanced/foreign"
		],
		"questions": [],
		"title": "Foreign",
		"description": "Keep foreign declarations separate from general attributes so boundary thinking stays clear.",
		"group": "Advanced and tooling",
		"section": "Advanced and tooling",
		"order": 31,
		"slug": "foreign",
		"summary": "Declare foreign bindings at the runtime boundary, not inside ordinary domain code.",
		"descriptionHtml": "Keep foreign declarations separate from general attributes so boundary thinking stays clear.",
		"headings": [
			{
				"depth": 2,
				"id": "what",
				"text": "What"
			},
			{
				"depth": 2,
				"id": "why",
				"text": "Why"
			},
			{
				"depth": 2,
				"id": "how",
				"text": "How"
			},
			{
				"depth": 2,
				"id": "try-it",
				"text": "Try it"
			},
			{
				"depth": 2,
				"id": "common-mistake",
				"text": "Common mistake"
			},
			{
				"depth": 2,
				"id": "next",
				"text": "Next"
			}
		],
		"html": "<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">foreign</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> \"c\" </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> puts</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> (msg : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">CString</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">) : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span></code></pre><h2 id=\"what\"><a href=\"#what\">What</a></h2><p>Foreign declarations bind Musi code to external symbols and ABIs.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>Cross-language boundaries are powerful but sharp, so they belong at edges.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><ul>\n<li>Declare the ABI explicitly.</li>\n<li>Keep the surface narrow.</li>\n<li>Wrap foreign calls behind cleaner helpers when possible.</li>\n</ul>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><ul>\n<li>Read one foreign declaration.</li>\n<li>List the boundary facts it exposes.</li>\n<li>Sketch the wrapper you would place around it.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not scatter foreign declarations through ordinary business code.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/advanced/quote-and-syntax\">Quote and syntax</a>.</p>\n",
		"summaryHtml": "Declare foreign bindings at the runtime boundary, not inside ordinary domain code."
	},
	{
		"locale": "en",
		"id": "quote-and-syntax",
		"kind": "chapter",
		"partId": "advanced",
		"partTitle": "Advanced and tooling",
		"path": "/learn/language/advanced/quote-and-syntax",
		"canonicalPath": "/learn/language/advanced/quote-and-syntax",
		"aliases": [
			"/docs/language/advanced/quote-and-syntax"
		],
		"questions": [],
		"title": "Quote and syntax",
		"description": "Introduce quote and syntax work late so beginners are not overloaded too early.",
		"group": "Advanced and tooling",
		"section": "Advanced and tooling",
		"order": 32,
		"slug": "quote-and-syntax",
		"summary": "Treat code as data only after ordinary code reading feels natural.",
		"descriptionHtml": "Introduce quote and syntax work late so beginners are not overloaded too early.",
		"headings": [
			{
				"depth": 2,
				"id": "what",
				"text": "What"
			},
			{
				"depth": 2,
				"id": "why",
				"text": "Why"
			},
			{
				"depth": 2,
				"id": "how",
				"text": "How"
			},
			{
				"depth": 2,
				"id": "try-it",
				"text": "Try it"
			},
			{
				"depth": 2,
				"id": "common-mistake",
				"text": "Common mistake"
			},
			{
				"depth": 2,
				"id": "next",
				"text": "Next"
			}
		],
		"html": "<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">quote </span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(x </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">+</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 1</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">);</span></span></code></pre><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> addTemplate </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> quote </span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(x </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">+</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> #(delta));</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> addOneSyntax </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> quote </span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(#(x) </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">+</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 1</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">);</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> addTwoSyntax </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> quote </span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(#(x) </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">+</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 2</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">);</span></span></code></pre><h2 id=\"what\"><a href=\"#what\">What</a></h2><p><code>quote</code> lets you work with code shape as data.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>Metaprogramming adds a second mental layer, so it belongs late in the learning path.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><ul>\n<li>Start with one quoted expression.</li>\n<li>Add one splice.</li>\n<li>Compare it with duplicated handwritten code.</li>\n</ul>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><ul>\n<li>Write one quoted expression.</li>\n<li>Splice one value into it.</li>\n<li>Explain what stays syntax and what becomes runtime data.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not reach for quote just because duplication exists.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/advanced/testing\">Testing</a>.</p>\n",
		"summaryHtml": "Treat code as data only after ordinary code reading feels natural."
	},
	{
		"locale": "en",
		"id": "testing",
		"kind": "chapter",
		"partId": "advanced",
		"partTitle": "Advanced and tooling",
		"path": "/learn/language/advanced/testing",
		"canonicalPath": "/learn/language/advanced/testing",
		"aliases": [
			"/docs/language/advanced/testing"
		],
		"questions": [],
		"title": "Testing",
		"description": "Teach tests before the wider command surface so the workflow stays concrete.",
		"group": "Advanced and tooling",
		"section": "Advanced and tooling",
		"order": 33,
		"slug": "testing",
		"summary": "Write small package tests that read like ordinary code.",
		"descriptionHtml": "Teach tests before the wider command surface so the workflow stays concrete.",
		"headings": [
			{
				"depth": 2,
				"id": "what",
				"text": "What"
			},
			{
				"depth": 2,
				"id": "why",
				"text": "Why"
			},
			{
				"depth": 2,
				"id": "how",
				"text": "How"
			},
			{
				"depth": 2,
				"id": "try-it",
				"text": "Try it"
			},
			{
				"depth": 2,
				"id": "common-mistake",
				"text": "Common mistake"
			},
			{
				"depth": 2,
				"id": "next",
				"text": "Next"
			}
		],
		"html": "<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> Testing</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> import</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> \"@std/testing\"</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">export</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> let</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> test</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> () </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span></span>\n<span class=\"line\"><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">  Testing</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">.</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">it</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\">\"adds values\"</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">, </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Testing</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">.</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">toBe</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">1</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> +</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 2</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">, </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">3</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">));</span></span></code></pre><h2 id=\"what\"><a href=\"#what\">What</a></h2><p>Musi tests live in <code>*.test.ms</code> files and usually export a <code>test</code> binding.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>Beginners should see that tests are ordinary code, not a second language.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><ul>\n<li>Create a <code>*.test.ms</code> file.</li>\n<li>Import <code>@std/testing</code>.</li>\n<li>Export one small test binding.</li>\n</ul>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><ul>\n<li>Create one test file.</li>\n<li>Write one passing test.</li>\n<li>Run <code>musi test</code>.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not wait for a large project before adding tests.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/advanced/running-and-tooling\">Running and tooling</a>.</p>\n",
		"summaryHtml": "Write small package tests that read like ordinary code."
	},
	{
		"locale": "en",
		"id": "running-and-tooling",
		"kind": "chapter",
		"partId": "advanced",
		"partTitle": "Advanced and tooling",
		"path": "/learn/language/advanced/running-and-tooling",
		"canonicalPath": "/learn/language/advanced/running-and-tooling",
		"aliases": [
			"/docs/language/advanced/running-and-tooling"
		],
		"questions": [],
		"title": "Running and tooling",
		"description": "Bring the learning path back to commands and workflow once language basics are in place.",
		"group": "Advanced and tooling",
		"section": "Advanced and tooling",
		"order": 34,
		"slug": "running-and-tooling",
		"summary": "Finish with the everyday command flow for checking, running, and building code.",
		"descriptionHtml": "Bring the learning path back to commands and workflow once language basics are in place.",
		"headings": [
			{
				"depth": 2,
				"id": "what",
				"text": "What"
			},
			{
				"depth": 2,
				"id": "why",
				"text": "Why"
			},
			{
				"depth": 2,
				"id": "how",
				"text": "How"
			},
			{
				"depth": 2,
				"id": "try-it",
				"text": "Try it"
			},
			{
				"depth": 2,
				"id": "common-mistake",
				"text": "Common mistake"
			},
			{
				"depth": 2,
				"id": "next",
				"text": "Next"
			}
		],
		"html": "<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">musi</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> run</span></span>\n<span class=\"line\"><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">musi</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> check</span></span>\n<span class=\"line\"><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">musi</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> build</span></span>\n<span class=\"line\"><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">musi</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> test</span></span></code></pre><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">music</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> check</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> index.ms</span></span>\n<span class=\"line\"><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">music</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> build</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> index.ms</span></span>\n<span class=\"line\"><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">music</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> run</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> index.seam</span></span></code></pre><h2 id=\"what\"><a href=\"#what\">What</a></h2><p>Musi’s tooling surface stays small on purpose.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>The book should end with a practical loop: edit, check, run, test.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><ul>\n<li>Use <code>musi</code> inside package roots.</li>\n<li>Use <code>music</code> for direct file work.</li>\n<li>Run checks often.</li>\n</ul>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><ul>\n<li>Run <code>musi check</code>.</li>\n<li>Run <code>musi test</code>.</li>\n<li>Run <code>music check index.ms</code>.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not memorize commands without connecting them to context.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>You now have the full beginner path. Revisit earlier chapters, then move into deeper reference docs when needed.</p>\n",
		"summaryHtml": "Finish with the everyday command flow for checking, running, and building code."
	}
] satisfies GeneratedDoc[];

