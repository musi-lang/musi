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
	"homeSampleHtml": "<div class=\"code-tabs\" data-example-id=\"home-intro\">\n<div class=\"code-tabs-meta\">\n<p class=\"code-tabs-caption\">Start with one value, one function, and one final result. This matches Musi&#39;s beginner path.</p>\n<p class=\"code-tabs-note\">Musi reads top to bottom. Bind values with <code>let</code>, define small functions the same way, then end with the result you want.</p>\n</div>\n<section role=\"tabpanel\" class=\"code-panel\">\n<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> base </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 21</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> twice</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> (x : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">) : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> x </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">+</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> x;</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> answer </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> twice</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(base);</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">answer;</span></span></code></pre>\n</section>\n</div>",
	"installCurlHtml": "<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">curl</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> -fsSL</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> https://raw.githubusercontent.com/musi-lang/musi/main/install.sh</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> |</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> sh</span></span></code></pre>",
	"installPowershellHtml": "<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span>powershell -NoProfile -ExecutionPolicy Bypass -Command \"irm https://raw.githubusercontent.com/musi-lang/musi/main/install.ps1 | iex\"</span></span></code></pre>",
	"installCargoHtml": "<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">git</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> clone</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> https://github.com/musi-lang/musi.git</span></span>\n<span class=\"line\"><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">cd</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> musi</span></span>\n<span class=\"line\"><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">cargo</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> install</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> --locked</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> --force</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> --path</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> crates/music</span></span>\n<span class=\"line\"><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">cargo</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> install</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> --locked</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> --force</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> --path</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> crates/musi</span></span></code></pre>",
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
		"html": "<p>Start here if Musi is new, but expect more than a checklist.\nThis part gives you first working model of the language: files read top to bottom, <code>let</code> names values, blocks produce results, and mutation is explicit instead of ambient.\nThe goal is quick traction without burying you under package structure or advanced abstractions.</p>\n<h2 id=\"what\"><a href=\"#what\">What</a></h2><p>This part teaches first Musi habits you will reuse everywhere: install the toolchain, write one file, bind values, read expressions, and recognize when state changes are explicit.\nEach chapter stays narrow, but each one should answer a real beginner question instead of only naming a topic.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>Too little detail creates immediate &quot;how do I actually do this?&quot; gaps.\nToo much detail creates wall-of-text fatigue before the core reading model even lands.\nThis section aims for middle ground: enough examples to make first files feel practical, but small enough that every page still unlocks one new idea at a time.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><p>Read these chapters in order.\nTry each example in a scratch file, keep names concrete, and stop to restate what value a file or block produces before moving on.\nIf one page still feels fuzzy, repeat its tiny example before piling on the next concept.</p>\n",
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
		"html": "<p>This part turns first-file syntax into everyday working code.\nYou will move from direct values into operators, ranges, functions, calls, and methods, always with examples that look like real Musi rather than token drills.\nBy the end, small expression-oriented code should feel normal instead of novel.</p>\n<h2 id=\"what\"><a href=\"#what\">What</a></h2><p>This section teaches core surface you read all the time: direct values, value transformations, reusable functions, ordinary calls, and receiver-led methods.\nThese are not isolated grammar facts; they are building blocks for nearly every later chapter.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>Readers need enough substance here to stop asking basic &quot;how do I compute this?&quot; or &quot;how do I call that?&quot; questions later.\nAt same time, this section should not become a full reference manual before users can even read a tiny file comfortably.\nThe right balance is concrete examples with one clear lesson per chapter.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><p>Work through chapters in order and keep rewriting examples in your own words.\nName intermediate results, trace values left to right through calls, and compare plain functions with methods so you can choose the clearer style in real code.\nIf one form still feels mechanical, keep it small until the reading pattern becomes boring.</p>\n",
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
		"html": "<p>This part introduces values with shape.\nRecords, arrays, slices, and patterns all answer different kinds of data questions, and the examples should help you feel when to reach for each one.\nThe section is about practical structure, not abstract taxonomy.</p>\n<h2 id=\"what\"><a href=\"#what\">What</a></h2><p>This section teaches labeled data, ordered data, and branching on data shape.\nYou will construct values, update them, and then decide what to do when shape changes the result.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>Once values stop being just numbers or strings, users need examples that answer &quot;how should I model this?&quot; not only &quot;what punctuation exists?&quot;\nIf the docs stay too thin here, beginners end up asking how to update one field, how to represent a list, or how to branch on constructor cases.\nThis section should answer those questions with compact but meaningful examples.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><p>Start with records and arrays before pattern matching.\nGet comfortable recognizing named fields versus ordered positions, then move into <code>match</code> once those shapes already mean something to you.\nWhen examples grow, keep asking what data shape each construct is making visible.</p>\n",
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
		"html": "<p>This part explains how Musi code grows past one scratch file.\nFiles stay important, but packages, imports, and exports become necessary once code needs boundaries, reuse, and repeatable commands.\nThe goal is to grow code organization without breaking the simple mental model from start chapters.</p>\n<h2 id=\"what\"><a href=\"#what\">What</a></h2><p>This section teaches file-level thinking, package structure, and explicit module boundaries.\nYou will see where code lives, how projects are created, and how names move across file edges.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>Many guides overload readers by mixing package ceremony into first syntax lesson.\nThat is wrong direction.\nUsers should first understand one file well, then learn how to organize many files without losing track of what each boundary is for.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><p>Read these pages as scaling story.\nFirst stabilize one-file flow, then move to package commands, then add imports and exports where code genuinely needs boundaries.\nAt each step, ask whether the new structure solves a real organization problem instead of adding ceremony for its own sake.</p>\n",
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
		"html": "<p>This part adds type information gradually.\nYou will start with explicit annotations, then see where inference reduces repetition, then use generics to reuse one definition across many types.\nThe emphasis stays on readable, practical code rather than abstract type-system tour.</p>\n<h2 id=\"what\"><a href=\"#what\">What</a></h2><p>This section teaches three related tools: annotations for explicit boundaries, inference for obvious cases, and generics for reusable typed definitions.\nTogether they answer how Musi code stays clear as values and functions become less trivial.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>Users need enough type detail to write and read real code, but too much theory too early causes drop-off.\nThis section aims for decision-making help: when should I annotate, when can I omit, and when is a generic definition worth it?\nThat is more useful than a giant catalog of type features.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><p>Learn one move at a time.\nAdd types where clarity rises, remove only what surrounding code makes obvious, and introduce generic parameters only after you can already read annotated functions comfortably.\nKeep examples small enough that every type choice still has an obvious reason.</p>\n",
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
		"html": "<p>This part moves from plain functions into reusable behavior contracts.\nClasses describe shared behavior, instances provide concrete implementations, and laws explain what those abstractions are supposed to mean.\nThe section works only if each layer stays distinct.</p>\n<h2 id=\"what\"><a href=\"#what\">What</a></h2><p>This section teaches three abstraction layers: contract, implementation, and semantic expectation.\nThose layers are connected, but they are not interchangeable.\nUnderstanding the separation is more important than memorizing every keyword.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>Abstraction chapters become overwhelming when readers cannot tell whether they are looking at behavior shape, specific implementation, or mathematical expectation.\nToo little explanation here leads straight to &quot;what is class for?&quot; or &quot;where does real behavior live?&quot;\nThis section should answer those questions with small examples before complexity grows.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><p>Read class page first, then instance page, then law page.\nFor each example, ask what stays generic, what becomes concrete, and what property should hold across all correct implementations.\nIf that separation is blurry, stay with the small <code>Eq</code>-style examples until it is not.</p>\n",
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
		"html": "<p>This part explains explicit capability flow and the layers built around it.\nEffects describe requested work, <code>using</code> surfaces required capabilities, handlers resolve requests, and foundation/runtime/stdlib pages place imports in their proper layer.\nThe goal is to make boundary thinking readable instead of mystical.</p>\n<h2 id=\"what\"><a href=\"#what\">What</a></h2><p>This section teaches effect requests, capability requirements, request handling, and module layering from core to runtime to stdlib.\nIt is one of richest parts of the book, but every page should still answer one practical question.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>Effects and runtime topics become intimidating when all boundaries are introduced at once.\nUsers then ask whether something is built in, imported, handled, runtime-backed, or just standard library code.\nThis section prevents that pile-up by separating each concern while keeping one coherent model of explicit capability flow.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><p>Follow the order.\nLearn effect requests before handlers, understand <code>using</code> before resolving capabilities, and keep module layers distinct when reading imports.\nWhenever a page feels abstract, come back to concrete question: what work is being requested, and who is responsible for providing it?</p>\n",
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
		"html": "<p>This part finishes the learning path with boundary and workflow tools.\nAttributes, foreign declarations, quote, testing, and command workflow all matter, but they matter most after ordinary Musi code already feels stable.\nThese pages should feel like practical extensions, not sudden genre change.</p>\n<h2 id=\"what\"><a href=\"#what\">What</a></h2><p>This section teaches metadata, host-boundary declarations, code-as-data tools, testing surface, and everyday command workflow.\nEach topic matters, but none of them should replace the simpler habits from earlier chapters.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>If advanced topics appear too early, beginners get overwhelmed and leave.\nIf they never appear with concrete examples, later-stage users still have to ask how Musi handles tooling, native integration, or metaprogramming.\nThis section answers those needs without turning into a wall of detail.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><p>Treat these chapters as boundary tools.\nReach for them when ordinary code is already clear and you need metadata, external integration, generated syntax, verification, or repeatable workflow.\nKeep the examples grounded in practical use so the advanced surface feels purposeful rather than ornamental.</p>\n",
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
		"description": "Install prerequisites, install Musi with script or Cargo, and learn when to use music versus musi.",
		"group": "Start",
		"section": "Start",
		"order": 1,
		"slug": "getting-started",
		"summary": "Install tools, install Musi, and learn the two command lanes.",
		"descriptionHtml": "Install prerequisites, install Musi with script or Cargo, and learn when to use music versus musi.",
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
		"html": "<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">curl</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> -fsSL</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> https://raw.githubusercontent.com/musi-lang/musi/main/install.sh</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> |</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> sh</span></span>\n<span class=\"line\"><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">music</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> check</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> index.ms</span></span>\n<span class=\"line\"><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">musi</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> new</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> hello</span></span></code></pre><h2 id=\"what\"><a href=\"#what\">What</a></h2><p>Getting started in Musi means setting up one toolchain, then learning that there are two command lanes.\nUse <code>music</code> when you are working directly with one file. Use <code>musi</code> when you are working inside a package.\nThat split matters early because it tells you whether you are experimenting with syntax or managing a project.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>Many &quot;how do I run this?&quot; questions come from not knowing which command lane owns which job.\nIf the install page only says &quot;install Musi,&quot; beginners still do not know whether to reach for <code>music check index.ms</code> or <code>musi run</code>.\nThis page should make the mental model concrete: one binary for direct file work, one binary for package workflow.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><p>Read the first command block as fastest path to a working install, then keep the Cargo path as fallback when you want a local clone or explicit build.\nAfter installation, verify <code>music</code> first with a single-file check command.\nThen create one package and run <code>musi</code> inside it so the two lanes become separate muscle memory instead of one blurred command surface.</p>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><ul>\n<li>Install Musi with one command path.</li>\n<li>Run <code>music check index.ms</code> on a scratch file.</li>\n<li>Create one package with <code>musi new hello</code> and run it.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not treat <code>music</code> and <code>musi</code> as duplicate names for same workflow.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/start/first-program\">First program</a> to use the direct file lane on the smallest possible Musi file.</p>\n",
		"summaryHtml": "Install tools, install Musi, and learn the two command lanes."
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
		"html": "<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> answer </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 42</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">answer;</span></span></code></pre><h2 id=\"what\"><a href=\"#what\">What</a></h2><p>A first Musi program can be only a binding and a final expression.\nThere is no extra ceremony here: no wrapper function, no package manifest, and no boilerplate runtime setup.\nThe file reads top to bottom, and the last expression is the result you are asking Musi to evaluate.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>This is first meaningful win for new readers.\nIf the first page immediately adds packages, imports, or larger syntax, beginners stop learning the language and start fighting setup detail.\nA two-line program proves the core reading model first: bind a value, then use it.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><p>Read <code>let answer := 42;</code> as &quot;introduce a name for a value I care about.&quot;\nRead the final <code>answer;</code> line as &quot;this is the result of the file.&quot;\nOnce that shape feels normal, you can change the value, add one more binding above it, or swap the final line for a larger expression without changing the mental model.</p>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><ul>\n<li>Create <code>index.ms</code> with one <code>let</code> binding.</li>\n<li>End the file with the bound name.</li>\n<li>Run <code>music check index.ms</code> to confirm the file shape.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not go hunting for a mandatory <code>main</code> just because other languages require one.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/start/values-and-let\">Values and let</a> to make that first binding pattern do real work.</p>\n",
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
		"description": "Learn Musi's core binding form before adding more syntax.",
		"group": "Start",
		"section": "Start",
		"order": 3,
		"slug": "values-and-let",
		"summary": "Bind names with let and read the file top to bottom.",
		"descriptionHtml": "Learn Musi&#39;s core binding form before adding more syntax.",
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
		"html": "<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> port </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 8080</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> nextPort </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> port </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">+</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 1</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">nextPort;</span></span></code></pre><h2 id=\"what\"><a href=\"#what\">What</a></h2><p><code>let</code> is Musi&#39;s everyday binding form.\nYou use it for plain values, and later you will see same starting shape again for functions, methods, modules, and other definitions.\nThat makes <code>let</code> worth learning deeply instead of treating it as throwaway beginner syntax.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>Users ask &quot;how do I name a value and reuse it?&quot; before they ask bigger questions.\nA good answer is not only syntax shape, but also why the shape stays important as the language grows.\nOnce <code>let</code> feels ordinary, many later chapters stop looking like brand-new grammar and start looking like familiar definitions with more detail attached.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><p>Read <code>let port := 8080;</code> as one stable binding between a name and a value.\nKeep the name close to where you will use it so the file stays readable without scrolling around for context.\nThen end with <code>port;</code> or derive one more binding from it so you can feel the difference between introducing a value and using one.</p>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><ul>\n<li>Bind one number with <code>let</code>.</li>\n<li>Add a second binding derived from first.</li>\n<li>End the file with derived value.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not assume <code>let</code> is only for top-level constants and not for the rest of language surface.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/start/blocks-and-expressions\">Blocks and expressions</a> to see how several bindings can still produce one final value.</p>\n",
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
		"html": "<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">  let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> base </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 8000</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">  let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> offset </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 80</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">  base </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">+</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> offset</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">);</span></span></code></pre><h2 id=\"what\"><a href=\"#what\">What</a></h2><p>A block groups several steps and still counts as one expression.\nInside the block you can introduce helper bindings, and the last line becomes the value the whole block produces.\nThat makes Musi feel expression-first even when the code has multiple stages.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>Beginners often expect grouped code to behave like statement braces from JavaScript or C-family languages.\nThat expectation causes confusion about &quot;where does this return from?&quot; or &quot;why is last line not ignored?&quot;\nLearning block result flow early prevents that confusion before you meet <code>match</code>, handlers, or larger definitions built from same idea.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><p>Read the block from top to bottom.\n<code>let base := 8000;</code> is local setup. <code>base + 80</code> is not a random trailing line; it is result of whole block.\nWhen you write your own block, put setup first, keep one clear final expression last, and ask what value the whole group should produce.</p>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><ul>\n<li>Create one block with a helper binding.</li>\n<li>Put arithmetic expression last.</li>\n<li>Replace last line and see how block result changes.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not read grouped Musi code as if only explicit <code>return</code> can produce a value.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/start/mutation\">Mutation</a> to see what changes when a value is meant to vary over time.</p>\n",
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
		"description": "Learn Musi's explicit mutation surface without mixing it into every lesson.",
		"group": "Start",
		"section": "Start",
		"order": 5,
		"slug": "mutation",
		"summary": "Use mut only when changing a value helps more than rebuilding it.",
		"descriptionHtml": "Learn Musi&#39;s explicit mutation surface without mixing it into every lesson.",
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
		"html": "<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> counter </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> mut</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 1</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">counter </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> counter </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">+</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 1</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">counter;</span></span></code></pre><h2 id=\"what\"><a href=\"#what\">What</a></h2><p>Mutation in Musi is explicit.\nA value becomes mutable only when you mark it with <code>mut</code>, and reassignment uses same <code>:=</code> surface in a clearly state-changing position.\nThat keeps changing state visible instead of quietly blending it into ordinary bindings.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>New users need to know both that mutation exists and that Musi does not want it everywhere.\nIf docs only show immutable examples, people ask how to update counters or accumulators.\nIf docs present mutation as default, readers miss one of the language&#39;s clarity wins: stable values stay stable unless you opt into change.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><p>Read <code>let counter := mut 1;</code> as creation of one mutable cell with initial value <code>1</code>.\nRead <code>counter := 2;</code> as reassignment of existing mutable value, not creation of a second binding.\nWhen writing real code, start by asking whether a new immutable value would read better; choose <code>mut</code> when step-by-step updates make the intent clearer.</p>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><ul>\n<li>Create one mutable counter.</li>\n<li>Reassign it once.</li>\n<li>Rewrite same tiny task with immutable bindings and compare readability.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not add <code>mut</code> automatically just because value changes in other languages.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/core/literals\">Literals</a> to build up the everyday values you will bind, transform, and compare.</p>\n",
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
		"description": "Meet Musi's everyday literal values before mixing them with operators.",
		"group": "Core syntax",
		"section": "Core syntax",
		"order": 6,
		"slug": "literals",
		"summary": "Start with numbers, strings, booleans, and templates.",
		"descriptionHtml": "Meet Musi&#39;s everyday literal values before mixing them with operators.",
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
		"html": "<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> port </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 8080</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> label </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> \"ready\"</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> enabled </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> .True</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">label;</span></span></code></pre><h2 id=\"what\"><a href=\"#what\">What</a></h2><p>Literals are values written directly in source: numbers, strings, booleans, and other small values you can read without another definition step.\nThis example mixes a few literal kinds with nearby derived bindings so you can see what is written directly and what is computed from it.\nThe language stays readable when you can spot that difference quickly.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>Users rarely ask for &quot;literal theory.&quot; They ask how to write a port number, a label, or a comparison flag in working code.\nA useful chapter connects literal syntax to ordinary tasks instead of listing token categories.\nThat lowers early friction before operators, ranges, and structured data show up.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><p>Read <code>8080</code> and <code>&quot;ready&quot;</code> as direct source values.\nThen notice how nearby bindings such as <code>next</code>, <code>same</code>, and <code>capped</code> are built from those literals rather than introducing brand-new syntax categories.\nWhen learning, start with small direct values, then derive one or two computed bindings so you can tell where literal writing stops and expression building begins.</p>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><ul>\n<li>Bind one integer and one string.</li>\n<li>Add one boolean comparison from them or nearby values.</li>\n<li>Rename bindings so result reads like small real code instead of token practice.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not try to learn literals, operators, and data shapes as one giant syntax dump.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/core/operators\">Operators</a> to turn those direct values into useful expressions.</p>\n",
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
		"html": "<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> port </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 8080</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> next </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> port </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">+</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 1</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> same </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> next </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 8081</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> capped </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> next </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">&#x3C;=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 9000</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span></code></pre><h2 id=\"what\"><a href=\"#what\">What</a></h2><p>Operators let you combine existing values into new results.\nIn this example, arithmetic, equality, ordering, and bit-shift forms all stay inside ordinary <code>let</code> bindings so the code still reads like plain Musi, not like a separate operator sublanguage.\nThe point is not memorizing every symbol at once; it is seeing operators as part of normal expression flow.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>Most &quot;how do I compute this?&quot; questions start here.\nUsers want to add one, compare two values, or build a flag, and they need examples that look like real code rather than isolated operator tables.\nShowing operators next to named bindings answers both syntax question and readability question.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><p>Read <code>port + 1</code> as value transformation, <code>next = port + 1</code> as equality check, and <code>port &lt;= 9000</code> as a guard-like comparison that still returns a value.\nThe <code>shl</code> example shows that named operator forms follow same expression pattern.\nWhen writing your own code, start with named inputs, then build one operator expression per binding so the meaning stays obvious.</p>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><ul>\n<li>Start from one numeric binding.</li>\n<li>Add one arithmetic result and one comparison result.</li>\n<li>Read both results aloud as values, not hidden control flow.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not pack several unrelated operators into one line before each result has a clear name.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/core/ranges\">Ranges</a> to see how Musi writes &quot;from here to there&quot; without guessing at endpoint meaning.</p>\n",
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
		"description": "Learn Musi's range operators in isolation before they appear inside larger code.",
		"group": "Core syntax",
		"section": "Core syntax",
		"order": 8,
		"slug": "ranges",
		"summary": "Read open, closed, and spread-like range forms without guessing.",
		"descriptionHtml": "Learn Musi&#39;s range operators in isolation before they appear inside larger code.",
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
		"html": "<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> closed </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 0</span><span style=\"color:#D73A49;--shiki-dark:#F97583\">..</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">10</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> halfOpen </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 0</span><span style=\"color:#D73A49;--shiki-dark:#F97583\">..&#x3C;</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">10</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">halfOpen;</span></span></code></pre><h2 id=\"what\"><a href=\"#what\">What</a></h2><p>Ranges are compact value forms for spans such as &quot;zero through ten&quot; or &quot;zero up to but not including ten.&quot;\nMusi makes the endpoint choice visible in the operator itself, which is why this topic deserves its own page instead of being buried inside larger examples.\nThe syntax is small, but the meaning matters.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>Readers often trip on off-by-one errors long before they trip on advanced language features.\nIf docs rush past range syntax, users still have to ask whether end value is included, excluded, or context dependent.\nThis chapter should make those decisions visible early so later APIs that consume ranges feel predictable.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><p>Read <code>0..10</code> as closed range and <code>0..&lt;10</code> as half-open range.\nThe example ends with <code>closed;</code> so you can see range syntax is still ordinary expression-producing code.\nWhen choosing between forms, decide first whether end value belongs inside result, then pick operator that states that choice directly.</p>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><ul>\n<li>Create one closed range.</li>\n<li>Create one half-open range with same endpoints.</li>\n<li>Write down which values differ between them.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not assume every range form means same endpoint behavior with different punctuation.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/core/functions\">Functions</a> to package repeated expression logic behind a reusable name.</p>\n",
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
		"html": "<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> twice</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> (x : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">) : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> x </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">+</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> x;</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> answer </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> twice</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">21</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">);</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">answer;</span></span></code></pre><h2 id=\"what\"><a href=\"#what\">What</a></h2><p>A function in Musi is still a <code>let</code> binding, but now the bound thing takes inputs and returns a result.\nThat continuity matters: you are not leaving basic syntax behind, only adding parameters and usually a result type.\nThe page&#39;s example stays tiny on purpose so the function shape is easier to see than the arithmetic.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>People ask &quot;how do I reuse logic?&quot; almost immediately after first successful file.\nA helpful answer should show that functions are ordinary named values with a clearer shape, not a new top-level declaration family to memorize.\nThat keeps the learning curve flatter when later chapters add calls, methods, or recursion.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><p>Read <code>let twice (x : Int) : Int := x + x;</code> as three pieces: function name, parameter list, and result expression.\nThen read <code>twice(21);</code> as proof that function definition and use stay close together.\nWhen writing your own first functions, keep one parameter, one small body, and one obvious return value until the shape feels automatic.</p>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><ul>\n<li>Write one one-argument function.</li>\n<li>Call it once with literal input.</li>\n<li>Rename function or parameter to make intent clearer.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not jump to methods, classes, or generic helpers before plain function flow feels normal.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/core/calls\">Calls</a> to focus on what function application looks like in everyday code.</p>\n",
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
		"html": "<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> greet</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> (name : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">String</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">) : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">String</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> name;</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> message </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> greet</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\">\"Musi\"</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">);</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">message;</span></span></code></pre><h2 id=\"what\"><a href=\"#what\">What</a></h2><p>A call applies a function to arguments.\nThis page reuses a tiny function example because the point is not more syntax surface; the point is learning to read <code>twice(21)</code> as value flow from argument into function and back out as result.\nCalls show up everywhere, so this reading habit must become boring fast.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>Many beginner questions are really call-reading questions: &quot;where does this value go?&quot;, &quot;what is input here?&quot;, or &quot;why are parentheses here but not there?&quot;\nIf call syntax is only mentioned in passing, those questions keep interrupting later chapters.\nA short focused chapter pays off because functions, constructors, stdlib helpers, and methods all build on same habit of tracking inputs and outputs.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><p>Look at definition first, then read call left to right.\n<code>twice</code> names the function, <code>(21)</code> supplies one argument, and the whole expression evaluates to returned result.\nWhen calls get larger, keep naming intermediate values so you are still reading one input step at a time rather than decoding a pile of nested punctuation.</p>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><ul>\n<li>Write one small function.</li>\n<li>Call it with one literal argument.</li>\n<li>Bind call result to a name before doing anything larger.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not confuse function definition syntax with function call syntax just because both sit near same name.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/core/methods\">Methods</a> to see how Musi attaches behavior to a receiver and calls it with dot syntax.</p>\n",
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
		"description": "Learn Musi's attached-method model after plain functions and calls.",
		"group": "Core syntax",
		"section": "Core syntax",
		"order": 11,
		"slug": "methods",
		"summary": "Use receiver-prefixed methods and dot calls without needing an impl block.",
		"descriptionHtml": "Learn Musi&#39;s attached-method model after plain functions and calls.",
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
		"html": "<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> (</span><span style=\"color:#E36209;--shiki-dark:#FFAB70\">self</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> : Int).</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">abs</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> () : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> self;</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> one </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 1</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">one.</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">abs</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">();</span></span></code></pre><h2 id=\"what\"><a href=\"#what\">What</a></h2><p>Methods in Musi are receiver-prefixed function definitions.\nInstead of inventing a separate <code>impl</code> block or class body, the receiver appears right in the definition, and the call site uses dot syntax on the value.\nThat keeps method behavior close to ordinary function behavior while making receiver visible.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>Users coming from object-heavy languages expect one model; users coming from functional languages expect another.\nThis chapter should show that Musi&#39;s method surface is simpler than both expectations: define behavior with a receiver parameter, then call it from the value you already have.\nThat answers &quot;how do I write <code>x.abs()</code>?&quot; without dragging in more abstraction machinery.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><p>Read <code>let (self : Int).abs () : Int := self;</code> as a function whose first visible role is receiver.\nThen read <code>one.abs();</code> as method call on value <code>one</code>, not as magical property lookup.\nWhen deciding between plain function and method, prefer method when receiver-led reading is clearer at call site.</p>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><ul>\n<li>Define one receiver method for a simple type.</li>\n<li>Call it from a named value.</li>\n<li>Compare that call with equivalent plain function style.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not assume methods require a separate container type declaration before they can exist.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/data/records\">Records</a> to move from scalar values into labeled data.</p>\n",
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
		"html": "<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> point </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> { x </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 3</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">, y </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 4</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> };</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> moved </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> { </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">...</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">point, y </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 9</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> };</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">moved;</span></span></code></pre><h2 id=\"what\"><a href=\"#what\">What</a></h2><p>Records let you store related values under field names.\nThe first snippet shows record construction with named fields, and the second shows how to build a new record from an existing one with spread update.\nTogether they cover the two questions beginners actually ask: how to make one, and how to change one without losing everything else.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>Plain numbers and strings stop being enough as soon as data has roles such as <code>x</code>, <code>y</code>, <code>name</code>, or <code>port</code>.\nIf docs only mention record syntax once and move on, users still need to ask how to update one field while keeping rest intact.\nRecords are where labeled data starts feeling practical instead of theoretical.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><p>Read <code>{ x := 3, y := 4 }</code> as one value with two named fields.\nThen read <code>{ ...point, z := 5 }</code> as &quot;copy existing record shape, then override or add selected fields.&quot;\nWhen writing your own records, choose field names that make access obvious and use spread when you want a new value that mostly keeps old data.</p>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><ul>\n<li>Create one record with two named fields.</li>\n<li>Build a second record with spread update.</li>\n<li>Change one field name or value to reflect a real domain example.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not rebuild an entire record by hand when only one or two fields need to change.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/data/arrays-and-slices\">Arrays and slices</a> to compare labeled record data with ordered sequence data.</p>\n",
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
		"html": "<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> Slice</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> import</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> \"@std/slice\"</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> values </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> [</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">1</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">, </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">2</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">, </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">3</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">];</span></span>\n<span class=\"line\"><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Slice</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">.</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">concat</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">](values, [</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">4</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">]);</span></span></code></pre><h2 id=\"what\"><a href=\"#what\">What</a></h2><p>Arrays store ordered values, while slice helpers let you work with sequence-shaped data without inventing your own low-level operations each time.\nThis page pairs a tiny literal array with one stdlib slice example so you can see both local syntax and the broader sequence workflow.\nOrdered data is common enough that users need more than a token example.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>Beginners quickly ask how to represent &quot;a list of things&quot; and how to combine or pass that list around.\nIf the docs only show <code>[1, 2, 3]</code>, they still do not know what happens next.\nShowing array creation next to <code>@std/slice</code> answers both shape question and &quot;what do I do with this afterward?&quot; question.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><p>Read <code>[1, 2, 3]</code> as one ordered value where position matters more than field names.\nThen read <code>Slice.concat[Int]([1], [2, 3]);</code> as a normal library call over sequence values, not special built-in mutation.\nWhen writing real code, create arrays locally, then reach for slice helpers when you need to combine, traverse, or transform them in clearer steps.</p>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><ul>\n<li>Bind one small array literal.</li>\n<li>Concatenate two arrays with slice helper.</li>\n<li>Rename arrays so their roles are obvious.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not use arrays when named fields would explain the data better than positions.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/data/patterns\">Patterns</a> to branch on data shape once records and sequences already feel familiar.</p>\n",
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
		"summary": "Use match and destructuring to branch on data shape.",
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
		"html": "<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> Port</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> data</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> {</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">  | </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Configured</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">  | </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Default</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">};</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> port : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Port</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> .Configured</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">8080</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">);</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">match</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> port</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> (</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">| </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">.Configured</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(value) </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">=></span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> value</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">| </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">.Default</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> =></span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 3000</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">);</span></span></code></pre><h2 id=\"what\"><a href=\"#what\">What</a></h2><p>Patterns let code react to value shape instead of only to raw scalar comparisons.\nThis example defines a small <code>Port</code> data type, constructs one value, and then uses <code>match</code> to branch on whether a configured value exists.\nIt is first complete example where data definition, construction, and branching all line up around one real decision.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>Users ask &quot;how do I handle different cases?&quot; as soon as data can vary.\nIf docs rush into nested destructuring or advanced matching too early, readers get lost before simple constructor matching is stable.\nThis page should teach the core win: pattern matching keeps data-dependent branching explicit and readable.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><p>Read <code>.Configured(8080)</code> and <code>.Default</code> as two possible shapes of same type.\nThen read each <code>match</code> arm as answer for one shape, with extracted values such as <code>port</code> made available only in arm that matches.\nWhen writing your own patterns, start with two or three clear cases and ask what value each branch should produce.</p>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><ul>\n<li>Define one data type with two cases.</li>\n<li>Construct one variant value.</li>\n<li>Write <code>match</code> expression that returns different result for each shape.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not begin with deeply nested pattern trees before constructor-level branching feels easy.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/organization/files\">Files</a> to see how these language forms live inside ordinary source files.</p>\n",
		"summaryHtml": "Use match and destructuring to branch on data shape."
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
		"html": "<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> answer </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 42</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">answer;</span></span></code></pre><h2 id=\"what\"><a href=\"#what\">What</a></h2><p>A Musi file is direct unit of source code.\nThe smallest useful file can hold bindings and a final expression, and that same top-to-bottom reading model continues even after programs get larger.\nThis page exists so package structure never hides the simpler truth that code still starts in files.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>People often jump into package layout too early and lose track of where language code actually lives.\nIf docs only explain package commands, beginners still ask what one file means, what belongs in it, and how much ceremony it needs.\nAnchoring on the file keeps later organization topics grounded.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><p>Read the example as one self-contained source file.\nEverything important is visible in one place: one binding, one final result, one clear evaluation path.\nWhen learning, create scratch files freely, keep related code close together, and only reach for multi-file organization when the single-file story starts feeling cramped.</p>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><ul>\n<li>Create one scratch Musi file.</li>\n<li>Put one binding and one final expression in it.</li>\n<li>Check it directly with <code>music check</code>.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not assume package structure replaces the need to understand one-file evaluation flow.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/organization/packages\">Packages</a> to see when a single file stops being enough.</p>\n",
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
		"html": "<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">musi</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> new</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> hello</span></span>\n<span class=\"line\"><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">cd</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> hello</span></span>\n<span class=\"line\"><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">musi</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> run</span></span></code></pre><h2 id=\"what\"><a href=\"#what\">What</a></h2><p>A package groups source files, manifest data, and package-level commands under one project root.\nThe <code>musi new hello</code> flow shows what package work looks like when you are no longer just checking one scratch file.\nThis is about project shape, not new language semantics.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>Users need a clear moment where they switch from &quot;I am learning syntax in one file&quot; to &quot;I am building a project I will rerun, test, and grow.&quot;\nWithout that transition, <code>musi run</code>, <code>musi test</code>, and manifest concepts feel arbitrary.\nA practical package example gives those commands a home.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><p>Read the command sequence as package lifecycle: create project, move into root, then run package entry point.\nAfter that, inspect generated structure and connect it back to earlier file model: package entry is still just Musi source, now managed by project tooling.\nUse package workflow when code needs multiple files, dependency tracking, or repeatable commands.</p>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><ul>\n<li>Create one package with <code>musi new</code>.</li>\n<li>Open generated entry file.</li>\n<li>Run <code>musi run</code> from package root.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not assume package workflow makes direct <code>music</code> file work obsolete.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/organization/imports-and-exports\">Imports and exports</a> to connect files without turning everything public.</p>\n",
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
		"html": "<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">export</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> answer </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 42</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> Option</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> import</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> \"@std/option\"</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> Local</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> import</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> \"./index.ms\"</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Local</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">.</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">answer</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span></code></pre><h2 id=\"what\"><a href=\"#what\">What</a></h2><p>Imports bring code or modules into scope, and exports decide which names other files may use.\nThis page pairs a standard-library import with a local export/import cycle so the boundary is visible from both directions.\nThat makes module flow concrete instead of abstract.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>As soon as code crosses file boundaries, users ask two questions: &quot;how do I bring this in?&quot; and &quot;how do I expose that out?&quot;\nIf docs only answer one of them, people still end up guessing about module ownership.\nClear import/export examples help keep dependencies explicit and public surfaces small.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><p>Read <code>let Option := import &quot;@std/option&quot;;</code> as binding imported module to a local name you can call through.\nThen read <code>export let answer := 42;</code> as deliberate publication of one binding, not automatic exposure of whole file.\nWhen organizing code, import only what you need, export only what other files truly depend on, and keep local helpers unexported by default.</p>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><ul>\n<li>Import one <code>@std</code> module into a file.</li>\n<li>Export one helper from another file.</li>\n<li>Use exported name through local import.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not use imports and exports as substitute for deciding which names should stay local.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/types/type-annotations\">Type annotations</a> to make important value shapes explicit once code spans more than one toy file.</p>\n",
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
		"html": "<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> port : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 8080</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> twice</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> (x : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">) : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> x </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">+</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> x;</span></span>\n<span class=\"line\"><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">twice</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(port);</span></span></code></pre><h2 id=\"what\"><a href=\"#what\">What</a></h2><p>Type annotations tell both reader and compiler what kind of value a binding or function expects.\nThis example shows them on a simple value and on a generic function, which is enough to see the surface without drowning in type theory.\nAnnotations are there to clarify important boundaries, not to decorate every name.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>Users ask for annotations when code stops being self-evident.\nA raw value such as <code>8080</code> may be obvious, but function parameters, return shapes, and reused helpers benefit from visible type anchors.\nThis chapter should show where annotations earn their keep before inference and generics complicate the picture.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><p>Read <code>let port : Int := 8080;</code> as ordinary binding with an explicit type promise inserted between name and value.\nThen read function annotation positions separately: parameter types explain inputs, result type explains output.\nWhen writing your own code, annotate public or non-obvious boundaries first, then stop once the code becomes clearer rather than noisier.</p>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><ul>\n<li>Add type annotation to one value binding.</li>\n<li>Add parameter or result type to one function.</li>\n<li>Compare readability before and after.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not annotate everything by reflex when only a few boundaries actually need explanation.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/types/type-inference\">Type inference</a> to see where Musi can safely fill in detail for you.</p>\n",
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
		"html": "<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> port : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 8080</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> next </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> port </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">+</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 1</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">next;</span></span></code></pre><h2 id=\"what\"><a href=\"#what\">What</a></h2><p>Type inference lets Musi recover some type information from surrounding code so you do not have to repeat every obvious fact.\nThe example keeps one explicit <code>Int</code> annotation and then omits it on derived binding <code>next</code>.\nThat contrast is whole lesson: inference is convenience anchored by context, not magic guesswork.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>Users want shorter code, but they also want to know when shorter code stops being clear.\nIf docs celebrate inference without boundaries, beginners start guessing what compiler can or cannot recover.\nA small example with one kept annotation and one omitted annotation teaches better instinct than a broad promise.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><p>Read annotated <code>port</code> as source of type information.\nThen read <code>let next := port + 1;</code> as value whose type becomes obvious because operands already constrain it.\nWhen editing real code, remove only the annotations that feel redundant after you can still explain the type from nearby information.</p>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><ul>\n<li>Start with one annotated value.</li>\n<li>Add derived binding without annotation.</li>\n<li>Put annotation back if meaning becomes harder to read.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not rely on inference in examples you cannot explain by tracing the surrounding code.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/types/generics\">Generics</a> to reuse one definition across many types once the annotation story is clear.</p>\n",
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
		"html": "<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> identityFn</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">T</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">] (input : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">T</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">) : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">T</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> input;</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> port : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 8080</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">identityFn[</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">](port);</span></span></code></pre><h2 id=\"what\"><a href=\"#what\">What</a></h2><p>Generics let one definition work across many concrete types by abstracting over a type parameter such as <code>T</code>.\nThis page keeps the example deliberately small: identity function in generic form, then one explicit application at <code>Int</code>.\nThat is enough to teach the core move without turning the chapter into a full type-system reference.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>Sooner or later users ask how to avoid copy-pasting same function for several types.\nGenerics answer that need, but they become overwhelming when introduced before annotations and inference are stable.\nHere the goal is practical reuse: one function shape, many compatible inputs.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><p>Read <code>[T]</code> as parameterization over a type, not over a runtime value.\nThen trace where <code>T</code> appears in input and output positions to understand what stays same across all uses.\nAt the call site, <code>identityFn[Int](port)</code> makes type application explicit; that can be useful whenever you want the chosen type to be obvious to reader.</p>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><ul>\n<li>Write one generic function with <code>[T]</code>.</li>\n<li>Call it once with <code>Int</code>.</li>\n<li>Call it again with another obvious type if available.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not introduce several type parameters before one-parameter generic code feels easy to read.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/abstractions/classes\">Classes</a> to move from reusable functions to reusable behavior contracts.</p>\n",
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
		"html": "<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> Eq</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">T</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">] </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> class</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> {</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">  let</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> (=)</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> (a : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">T</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">, b : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">T</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">) : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Bool</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">  law</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> reflexive</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> (x : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">T</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">) </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> .True</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">};</span></span></code></pre><h2 id=\"what\"><a href=\"#what\">What</a></h2><p>A class describes a behavior surface for values of some type.\nIn this example, <code>Eq[T]</code> says that values of type <code>T</code> can be compared for equality, and the law names one semantic expectation that should hold.\nThe key idea is contract first: what operations and guarantees exist before any one concrete implementation appears.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>Users coming from inheritance-heavy languages can misread classes immediately.\nThis page should prevent that by showing classes as behavior descriptions, not object hierarchies.\nOnce that distinction is clear, later instance and law pages feel like natural follow-ups instead of confusing add-ons.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><p>Read <code>let Eq[T] := class { ... };</code> as definition of shared capability over some type parameter <code>T</code>.\nInside it, focus first on operation shape <code>let (=) ... : Bool;</code>, then on the law as statement about meaning rather than syntax decoration.\nWhen writing your own first class, keep member count tiny and choose behavior that several concrete types could plausibly share.</p>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><ul>\n<li>Define one class with one operation.</li>\n<li>Name the behavior after what callers need.</li>\n<li>List one or two concrete types that should satisfy it.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not read Musi classes as inheritance trees with hidden fields or subclass state.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/abstractions/instances\">Instances</a> to see how one concrete type fulfills that behavior contract.</p>\n",
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
		"html": "<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> eqInt </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> instance</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> Eq</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">] {</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">  let</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> (=)</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> (a : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">, b : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">) : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Bool</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> .True</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">};</span></span></code></pre><h2 id=\"what\"><a href=\"#what\">What</a></h2><p>An instance says how one concrete type satisfies one class.\nWhere the class page defined behavior shape, this page fills in actual implementation for <code>Eq[Int]</code>.\nThat split is important because it keeps reusable abstraction separate from concrete decision.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>If users only see class declarations, they still ask where the real behavior lives.\nIf they see classes and instances collapsed together too early, they lose the difference between contract and implementation.\nA tiny <code>Int</code> instance makes the handoff between those layers easy to follow.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><p>Read <code>instance Eq[Int]</code> as commitment to implement <code>Eq</code> behavior for the specific type <code>Int</code>.\nInside the block, compare member names with class definition and notice that instance must satisfy required surface.\nWhen writing your own first instance, pick one small class and one obvious concrete type so the mapping from contract to implementation is immediate.</p>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><ul>\n<li>Take one small class definition.</li>\n<li>Add one instance for <code>Int</code> or another simple type.</li>\n<li>Use same member names so relation stays obvious.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not define several classes and several instances at once before one mapping feels clear.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/abstractions/laws\">Laws</a> to capture what those abstractions are supposed to mean, not only how they are spelled.</p>\n",
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
		"html": "<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> Eq</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">T</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">] </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> class</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> {</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">  let</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> (=)</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> (a : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">T</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">, b : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">T</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">) : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Bool</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">  law</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> reflexive</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> (x : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">T</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">) </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> .True</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">};</span></span></code></pre><h2 id=\"what\"><a href=\"#what\">What</a></h2><p>Laws document semantic expectations of an abstraction.\nIn the <code>Eq</code> example, <code>reflexive</code> states that comparing a value with itself should succeed.\nThat means class definitions can communicate not only available operations, but also what correct behavior is supposed to preserve.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>Without laws, abstractions can be technically implemented yet still behave in surprising or inconsistent ways.\nUsers need to know that Musi has a place to state these expectations close to the abstraction itself.\nThat keeps class design from becoming only a bag of function signatures.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><p>Read a law as part of abstraction meaning.\nIt is not there to add another callable member; it tells implementers and readers what behavior should hold across instances.\nWhen adding laws, start with one obvious property that would help another reader tell correct implementation from suspicious one.</p>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><ul>\n<li>Look at one class operation.</li>\n<li>Write one law that expresses expected behavior of that operation.</li>\n<li>Ask how an instance could violate it.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not treat laws as decorative comments that can say anything without relation to the operations they describe.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/effects-runtime/effects\">Effects</a> to shift from shared behavior contracts into explicit requests for external work.</p>\n",
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
		"html": "<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> console </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> effect</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> {</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">  let</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> readln</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> () : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">String</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">};</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">request</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> console.</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">readln</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">();</span></span></code></pre><h2 id=\"what\"><a href=\"#what\">What</a></h2><p>An effect describes operations code may request, and <code>request</code> issues one of those requests.\nThis pair of snippets keeps model intentionally small: first define console capability, then request one read operation from it.\nThat is core effect story before handlers, runtime imports, or stdlib layering enter scene.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>Users often know hidden side effects from other languages, but Musi wants capability flow to stay visible.\nIf docs jump straight to handlers, the basic question &quot;what is an effect?&quot; never gets a clean answer.\nThis page should make one thing obvious: effectful code is asking for work that something else must eventually provide.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><p>Read effect block as declaration of available operations, not as immediate implementation.\nThen read <code>request console.readln();</code> as explicit request made from code that depends on that capability.\nWhen writing your own examples, keep one effect and one operation at first so the request model stays sharper than the surrounding syntax.</p>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><ul>\n<li>Define one effect with one operation.</li>\n<li>Request that operation once.</li>\n<li>Explain in words what still needs to handle the request.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not skip straight to full handlers before the &quot;request for work&quot; model feels stable.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/effects-runtime/using\">Using</a> to make required capabilities visible in function signatures.</p>\n",
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
		"html": "<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> readClosed</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> (x : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">) : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">String</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> using</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> { </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Console</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> } </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">  request</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> State</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">.</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">readln</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">();</span></span></code></pre><h2 id=\"what\"><a href=\"#what\">What</a></h2><p><code>using</code> clauses make capability requirements visible in the type-level surface of a definition.\nA function that requests effectful work can say so directly instead of leaving the dependency ambient or hidden.\nThat makes effect requirements part of the signature a reader sees first.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>Once users understand effect requests, the next question is &quot;how do I know this function needs that capability?&quot;\nA signature-level answer scales better than relying on comments or hidden convention.\nThis page should show that effectful code advertises its needs instead of surprising the caller later.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><p>Read <code>using { Console }</code> as requirement attached to the function, not as runtime argument list.\nEven if the example is small, the lesson is practical: signatures can tell you what must be available before the body can request certain operations.\nWhen designing APIs, add <code>using</code> where capability dependence is real and useful for callers to know up front.</p>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><ul>\n<li>Write one function signature with <code>using</code> clause.</li>\n<li>Add one requested effect inside body.</li>\n<li>Compare signature with equivalent hidden-dependency story.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not treat <code>using</code> as optional decoration when function genuinely depends on capability availability.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/effects-runtime/handlers\">Handlers</a> to resolve those requests at a boundary that can choose policy.</p>\n",
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
		"summary": "Resolve requested effects at the boundary where policy belongs.",
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
		"html": "<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">handle</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> request</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> console.</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">readln</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">() </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">using</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> console {</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">  value </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">=></span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> value;</span></span>\n<span class=\"line\"><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">  readln</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(k) </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">=></span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> resume</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> \"ok\"</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">};</span></span></code></pre><h2 id=\"what\"><a href=\"#what\">What</a></h2><p>A handler decides what to do with requested operations.\nThis example handles a console read request, gives a value path for normal completion, and uses <code>resume</code> to continue computation after operation match is handled.\nIt is first place where effect requests meet concrete policy.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>Users ask &quot;where does the effect actually get answered?&quot; right after they see <code>request</code>.\nHandlers are that answer, but they are easier to learn once effect and <code>using</code> ideas are already clear.\nA small handler example keeps focus on resolution flow instead of broad control abstractions.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><p>Read <code>handle ... using console</code> as boundary around effectful computation.\nInside the handler, <code>value =&gt; value;</code> covers normal completion, while <code>readln(k) =&gt; resume &quot;ok&quot;;</code> covers specific requested operation and chooses how computation continues.\nWhen writing your own handlers, start with one operation and one simple resume path so control flow stays easy to trace.</p>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><ul>\n<li>Wrap one requested operation in a handler.</li>\n<li>Add one operation case.</li>\n<li>Resume computation with a concrete replacement value.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not mix several effects and several policies into first handler example.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/effects-runtime/foundation\">Foundation</a> to separate language-level core from runtime and stdlib layers built above it.</p>\n",
		"summaryHtml": "Resolve requested effects at the boundary where policy belongs."
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
		"html": "<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> Core</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> import</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> \"musi:core\"</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Core</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span></code></pre><h2 id=\"what\"><a href=\"#what\">What</a></h2><p><code>musi:core</code> is language foundation layer.\nIt names the lowest-level built-in surface that exists before you start reaching for runtime-backed modules or standard-library conveniences.\nThis page matters because &quot;what is built in?&quot; and &quot;what comes from libraries?&quot; are different questions.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>Users get overwhelmed when docs mention effects, runtime, and stdlib as if they are one blurred toolbox.\nA foundation page prevents that blur by giving core layer its own place in the model.\nOnce readers know what belongs to the base layer, later imports make more sense.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><p>Read <code>let Core := import &quot;musi:core&quot;;</code> as explicit access to foundational language surface.\nThen ask what kind of code needs this layer directly: mostly infrastructure, lower-level libraries, or explanation of system boundaries rather than ordinary app code.\nWhen teaching or writing app code, prefer clearer higher-level modules unless you specifically need the foundational layer.</p>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><ul>\n<li>Import <code>musi:core</code> once.</li>\n<li>Note what kind of code would reach for it directly.</li>\n<li>Compare that role with a higher-level stdlib import.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not assume foundational modules are where everyday application code should start by default.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/effects-runtime/runtime\">Runtime</a> to see where host-backed capabilities enter the picture.</p>\n",
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
		"html": "<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> Runtime</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> import</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> \"musi:runtime\"</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Runtime</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">.</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">envGet</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\">\"HOME\"</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">);</span></span></code></pre><h2 id=\"what\"><a href=\"#what\">What</a></h2><p><code>musi:runtime</code> exposes runtime-backed capabilities tied to the host environment.\nThe example uses <code>envGet(&quot;HOME&quot;)</code>, which is good because it looks like something ordinary code might want while still clearly depending on runtime presence.\nThis is where Musi crosses from pure language surface into host-connected services.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>Users need to know why runtime imports are separate from both foundation and <code>@std</code> modules.\nIf docs flatten those layers together, it becomes hard to tell what is portable language code and what depends on runtime support.\nA concrete runtime import makes that boundary easier to reason about.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><p>Read <code>import &quot;musi:runtime&quot;</code> as explicit opt-in to host-backed functionality.\nThen read <code>Runtime.envGet(&quot;HOME&quot;)</code> as normal call over imported module, with extra understanding that result depends on surrounding runtime environment.\nUse runtime modules when code truly needs host services, and keep that dependency visible instead of smuggling it in through unrelated helpers.</p>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><ul>\n<li>Import <code>musi:runtime</code>.</li>\n<li>Call one runtime-backed function.</li>\n<li>Explain what part of result depends on host environment.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not present runtime-backed imports as interchangeable with pure stdlib helpers.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/effects-runtime/stdlib\">Stdlib</a> to see what ordinary application code usually reaches for first.</p>\n",
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
		"html": "<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> Option</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> import</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> \"@std/option\"</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> configured </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> Option</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">.</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">some</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">](</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">8080</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">);</span></span>\n<span class=\"line\"><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Option</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">.</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">unwrapOr</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">](configured, </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">3000</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">);</span></span></code></pre><h2 id=\"what\"><a href=\"#what\">What</a></h2><p>The standard library gives ordinary application code ready-made modules such as <code>@std/option</code> and <code>@std/testing</code>.\nThis page pairs a simple import example with a testing import to show stdlib as practical toolbox, not abstract layer diagram.\nFor most day-to-day code, this is friendliest layer to reach for first.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>After foundation and runtime pages, users need a clear answer to &quot;what do I usually import in normal code?&quot;\nThat answer is often <code>@std</code>.\nPutting stdlib in its own chapter prevents the lower layers from looking like default entry point for common tasks.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><p>Read <code>let Option := import &quot;@std/option&quot;;</code> as explicit acquisition of a higher-level module designed for ordinary code.\nThen notice that testing uses same import model: <code>@std/testing</code> is still just a module you bind and call through.\nWhen writing app code, start from <code>@std</code> modules, then move downward only when you truly need lower-level control.</p>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><ul>\n<li>Import one <code>@std</code> module.</li>\n<li>Call one exported helper from it.</li>\n<li>Compare that import with one <code>musi:*</code> import and decide which belongs in app code.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not reach for foundation or runtime modules first when a clearer <code>@std</code> module already fits the job.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/advanced/attributes\">Attributes</a> to finish the learning path with boundary and tooling features.</p>\n",
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
		"html": "<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">@</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">link</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(name </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> \"c\"</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">) </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">foreign</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> \"c\" </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> puts</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> (msg : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">CString</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">) : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span></code></pre><h2 id=\"what\"><a href=\"#what\">What</a></h2><p>Attributes attach explicit metadata to declarations.\nThis example keeps that concrete by placing <code>@link(...)</code> on a foreign declaration, where the extra metadata clearly changes how surrounding system should treat the declaration.\nAttributes are not everyday syntax, but they are important when code must talk to tooling, compiler, or runtime machinery.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>Users eventually ask how to express non-local facts such as linkage, platform detail, or compiler-facing metadata.\nThose questions should not clutter beginner chapters, but they still need a clean answer.\nAn attribute chapter gives those answers without pretending attributes are part of normal domain modeling.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><p>Read the attribute as metadata attached to declaration that follows it.\nThen read the foreign declaration itself and separate two concerns: the declaration says what binding exists, the attribute says extra information needed for that binding to work correctly in broader system.\nWhen using attributes, keep them narrow, explicit, and close to declarations that truly require them.</p>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><ul>\n<li>Add one attribute to a boundary-facing declaration.</li>\n<li>State what behavior the attribute is trying to influence.</li>\n<li>Remove it mentally and decide what information would then be missing.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not use attributes to hide core business logic that should be visible in ordinary code structure.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/advanced/foreign\">Foreign</a> to focus on declarations that cross out of Musi entirely.</p>\n",
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
		"html": "<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">foreign</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> \"c\" </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> puts</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> (msg : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">CString</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">) : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span></code></pre><h2 id=\"what\"><a href=\"#what\">What</a></h2><p>Foreign declarations describe bindings implemented outside Musi.\nThe example names a C function and its Musi-facing type so code on Musi side can call across boundary with explicit contract.\nThis is advanced because it is about integration, not about core language flow.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>Users working near system boundaries need to know how Musi reaches native code without pretending that boundary is ordinary function definition.\nIf docs bury foreign declarations under attribute notes or runtime pages, the integration story stays fuzzy.\nA dedicated page keeps the riskier cross-language surface explicit.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><p>Read <code>foreign &quot;c&quot;</code> as declaration of external implementation source.\nThen read remainder of line as ordinary Musi-facing name and type surface that callers will see on Musi side.\nWhen adding foreign bindings, keep signatures minimal, verify types carefully, and isolate these declarations near integration boundaries instead of scattering them through domain code.</p>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><ul>\n<li>Declare one foreign binding.</li>\n<li>Identify language/runtime boundary it crosses.</li>\n<li>Explain what Musi side promises about arguments and result.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not treat foreign declarations as casual shortcut for code that could stay inside normal Musi modules.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/advanced/quote-and-syntax\">Quote and syntax</a> to see how Musi can represent code itself as data.</p>\n",
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
		"html": "<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> addTemplate </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> quote </span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(x </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">+</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> #(delta));</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> addOneSyntax </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> quote </span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(#(x) </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">+</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 1</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">);</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">addOneSyntax;</span></span></code></pre><h2 id=\"what\"><a href=\"#what\">What</a></h2><p><code>quote</code> turns code shape into syntax data you can inspect, build, or reuse.\nThe first snippet shows simplest quoted expression, and the second shows interpolation with <code>#(...)</code> inside quoted form.\nThis chapter belongs late because it asks you to reason about code as data rather than just running code.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>Metaprogramming questions show up after ordinary code already feels familiar.\nAt that point users need examples that explain both power and boundary: quoting is useful, but it is not default way to write everyday logic.\nA focused page keeps this tool available without overwhelming readers who are still stabilizing basic syntax.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><p>Read <code>quote (x + 1);</code> as syntax value representing expression shape.\nThen read <code>#(delta)</code> or <code>#(x)</code> inside quoted form as splice points where surrounding values contribute pieces to generated syntax.\nWhen experimenting, start with very small quoted expressions and ask what syntax object each quote should represent before building larger templates.</p>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><ul>\n<li>Quote one simple expression.</li>\n<li>Add one splice inside a quoted template.</li>\n<li>Compare quoted template with handwritten equivalent shape.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not reach for quote when an ordinary function or data value already solves the problem more directly.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/advanced/testing\">Testing</a> to come back from metaprogramming into practical project workflow.</p>\n",
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
		"html": "<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> Testing</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> import</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> \"@std/testing\"</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">export</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> let</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> test</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> () </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span></span>\n<span class=\"line\"><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">  Testing</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">.</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">it</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\">\"adds values\"</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">, </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Testing</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">.</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">toBe</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">1</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> +</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 2</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">, </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">3</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">));</span></span></code></pre><h2 id=\"what\"><a href=\"#what\">What</a></h2><p>Musi tests are ordinary code organized for discovery and execution by tooling.\nThis example keeps that promise visible: import testing helpers, export a <code>test</code> binding, and express one expectation in same language surface you already know.\nTesting becomes easier to adopt when it does not require a second mini-language.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>Users need confidence loop, not just syntax reference.\nIf the docs explain features but never show how to check them, learners still ask how to verify a package change or protect against regressions.\nA tiny test example gives them a habit they can keep using as code grows.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><p>Read <code>let Testing := import &quot;@std/testing&quot;;</code> as setup of helpers, then read exported <code>test</code> binding as entry point tooling will discover.\nThe assertion itself is ordinary function-style code, which means testing builds on same import, call, and expression patterns from earlier chapters.\nWhen writing first tests, keep each one tiny and named around one behavior you want confidence in.</p>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><ul>\n<li>Create one <code>*.test.ms</code> file.</li>\n<li>Export one <code>test</code> binding.</li>\n<li>Check one small behavior with stdlib testing helper.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not wait for a large project before learning the test shape; tiny examples benefit from it too.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/advanced/running-and-tooling\">Running and tooling</a> to tie learning back to everyday commands.</p>\n",
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
		"html": "<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">music</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> check</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> index.ms</span></span>\n<span class=\"line\"><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">musi</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> run</span></span>\n<span class=\"line\"><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">musi</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> test</span></span></code></pre><h2 id=\"what\"><a href=\"#what\">What</a></h2><p>Musi has everyday commands for package workflow and direct file workflow.\nThis page brings them together at end of language path so readers can connect all earlier examples to routine habits: check code, run packages, build outputs, and run tests.\nThe command surface is small enough to learn, but distinct enough to deserve a final summary.</p>\n<h2 id=\"why\"><a href=\"#why\">Why</a></h2><p>After learning syntax, users still need operational confidence.\nThey want to know which command to run when checking a file, when to use package commands, and how testing fits into normal iteration.\nA workflow chapter turns scattered command knowledge into repeatable practice.</p>\n<h2 id=\"how\"><a href=\"#how\">How</a></h2><p>Read <code>musi run</code>, <code>musi check</code>, <code>musi build</code>, and <code>musi test</code> as package-root commands for project lifecycle.\nRead <code>music check index.ms</code>, <code>music build index.ms</code>, and <code>music run index.seam</code> as direct lane for single-file or lower-level work.\nWhen in doubt, ask first whether you are inside a package or handling one file directly; that decision usually picks the right command family immediately.</p>\n<h2 id=\"try-it\"><a href=\"#try-it\">Try it</a></h2><ul>\n<li>Run one direct <code>music check</code> on a scratch file.</li>\n<li>Run one package command inside generated project.</li>\n<li>Use <code>musi test</code> after adding one tiny test.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not memorize commands as flat list; group them by direct-file lane versus package lane.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue back through any chapter you need, now that you have both language model and workflow model tied together.</p>\n",
		"summaryHtml": "Finish with the everyday command flow for checking, running, and building code."
	}
] satisfies GeneratedDoc[];

