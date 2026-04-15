export interface GeneratedHeading {
	depth: number;
	id: string;
	text: string;
}

export interface GeneratedDoc {
	locale: "en";
	id: string;
	kind: "part" | "section" | "chapter";
	parentId: string | null;
	depth: number;
	treePath: string[];
	childIds: string[];
	partId: string;
	partTitle: string;
	sectionId: string | null;
	sectionTitle: string | null;
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
	"homeSampleHtml": "<div class=\"code-tabs\" data-example-id=\"home-intro\"><div class=\"code-tabs-meta\"><p class=\"code-tabs-caption\">Start with one value, one function, and one final result. This matches Musi&#39;s beginner path.</p><p class=\"code-tabs-note\">Musi reads top to bottom. Bind values with <code>let</code>, define small functions the same way, then end with the result you want.</p></div><section role=\"tabpanel\" class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> base </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 21</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> twice</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> (x : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">) : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> :=</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> x </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">+</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> x;</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> answer </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> twice</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(base);</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">answer;</span></span></code></pre></section></div>",
	"installCurlHtml": "<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#702C00;--shiki-dark:#B392F0\">curl</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> -fsSL</span><span style=\"color:#032563;--shiki-dark:#9ECBFF\"> https://raw.githubusercontent.com/musi-lang/musi/main/install.sh</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> |</span><span style=\"color:#702C00;--shiki-dark:#B392F0\"> sh</span></span></code></pre></section></div>",
	"installPowershellHtml": "<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span>powershell -NoProfile -ExecutionPolicy Bypass -Command \"irm https://raw.githubusercontent.com/musi-lang/musi/main/install.ps1 | iex\"</span></span></code></pre></section></div>",
	"installCargoHtml": "<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#702C00;--shiki-dark:#B392F0\">git</span><span style=\"color:#032563;--shiki-dark:#9ECBFF\"> clone</span><span style=\"color:#032563;--shiki-dark:#9ECBFF\"> https://github.com/musi-lang/musi.git</span></span>\n<span class=\"line\"><span style=\"color:#023B95;--shiki-dark:#79B8FF\">cd</span><span style=\"color:#032563;--shiki-dark:#9ECBFF\"> musi</span></span>\n<span class=\"line\"><span style=\"color:#702C00;--shiki-dark:#B392F0\">cargo</span><span style=\"color:#032563;--shiki-dark:#9ECBFF\"> install</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> --locked</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> --force</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> --path</span><span style=\"color:#032563;--shiki-dark:#9ECBFF\"> crates/music</span></span>\n<span class=\"line\"><span style=\"color:#702C00;--shiki-dark:#B392F0\">cargo</span><span style=\"color:#032563;--shiki-dark:#9ECBFF\"> install</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> --locked</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> --force</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> --path</span><span style=\"color:#032563;--shiki-dark:#9ECBFF\"> crates/musi</span></span></code></pre></section></div>",
	"quickstartHtml": "<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#702C00;--shiki-dark:#B392F0\">musi</span><span style=\"color:#032563;--shiki-dark:#9ECBFF\"> new</span><span style=\"color:#032563;--shiki-dark:#9ECBFF\"> hello</span></span>\n<span class=\"line\"><span style=\"color:#023B95;--shiki-dark:#79B8FF\">cd</span><span style=\"color:#032563;--shiki-dark:#9ECBFF\"> hello</span></span>\n<span class=\"line\"><span style=\"color:#702C00;--shiki-dark:#B392F0\">musi</span><span style=\"color:#032563;--shiki-dark:#9ECBFF\"> run</span></span></code></pre></section></div>"
} as const;

export const renderedDocs = [
	{
		"locale": "en",
		"id": "start",
		"kind": "part",
		"parentId": null,
		"depth": 0,
		"treePath": [
			"start"
		],
		"childIds": [
			"start-foundations"
		],
		"partId": "start",
		"partTitle": "start",
		"sectionId": null,
		"sectionTitle": null,
		"path": "/learn/book/start",
		"canonicalPath": "/learn/book/start",
		"aliases": [
			"/docs/book/start",
			"/learn/book/start",
			"/docs/language/start",
			"/learn/language/start"
		],
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
				"id": "path-through-this-part",
				"text": "Path Through This Part"
			},
			{
				"depth": 2,
				"id": "what-this-part-solves",
				"text": "What This Part Solves"
			},
			{
				"depth": 2,
				"id": "how-to-read-it",
				"text": "How to Read It"
			}
		],
		"html": "<p>Start here if Musi is new, but expect more than a checklist.\nThis part gives you the first working model of the language: files read top to bottom, <code>let</code> names values, blocks produce results, and mutation stays explicit instead of ambient.\nThe goal is quick traction without burying you under package structure or advanced abstractions.</p>\n<p>Think of it like setting up a small workbench before you build the rest of the room.\nYou need one file, one binding model, and one expression model first, because the later chapters assume those habits already feel normal.</p>\n<h2 id=\"path-through-this-part\"><a href=\"#path-through-this-part\">Path Through This Part</a></h2><p>This part teaches first Musi habits you will reuse everywhere: install the toolchain, write one file, bind values, read expressions, and recognize when state changes are explicit.\nEach chapter stays narrow, but each one should answer a real beginner question instead of only naming a topic.</p>\n<h2 id=\"what-this-part-solves\"><a href=\"#what-this-part-solves\">What This Part Solves</a></h2><p>Too little detail creates immediate &quot;how do I actually do this?&quot; gaps.\nToo much detail creates wall-of-text fatigue before the core reading model even lands.\nThis section aims for middle ground: enough examples to make first files feel practical, but small enough that every page still unlocks one new idea at a time.</p>\n<h2 id=\"how-to-read-it\"><a href=\"#how-to-read-it\">How to Read It</a></h2><p>Read these chapters in order.\nTry each example in a scratch file, keep names concrete, and stop to restate what value a file or block produces before moving on.\nIf one page still feels fuzzy, repeat its tiny example before piling on the next concept.</p>\n",
		"summaryHtml": "Begin with setup, one file, one binding model, one expression model, then explicit mutation."
	},
	{
		"locale": "en",
		"id": "core",
		"kind": "part",
		"parentId": null,
		"depth": 0,
		"treePath": [
			"core"
		],
		"childIds": [
			"core-expressions",
			"core-functions"
		],
		"partId": "core",
		"partTitle": "core",
		"sectionId": null,
		"sectionTitle": null,
		"path": "/learn/book/core",
		"canonicalPath": "/learn/book/core",
		"aliases": [
			"/docs/book/core",
			"/learn/book/core",
			"/docs/language/core",
			"/learn/language/core"
		],
		"questions": [],
		"title": "Core Syntax",
		"description": "Build comfort with literals, operators, ranges, functions, calls, and methods.",
		"group": "Core Syntax",
		"section": "Core Syntax",
		"order": 2,
		"slug": "core",
		"summary": "Learn everyday syntax in very small pieces so nothing stacks too early.",
		"descriptionHtml": "Build comfort with literals, operators, ranges, functions, calls, and methods.",
		"headings": [
			{
				"depth": 2,
				"id": "path-through-this-part",
				"text": "Path Through This Part"
			},
			{
				"depth": 2,
				"id": "what-this-part-solves",
				"text": "What This Part Solves"
			},
			{
				"depth": 2,
				"id": "how-to-read-it",
				"text": "How to Read It"
			}
		],
		"html": "<p>This part turns first-file syntax into everyday working code.\nYou move from direct values into operators, ranges, functions, calls, and methods, always with examples that look like real Musi rather than token drills.\nBy the end, small expression-oriented code should feel normal instead of novel.</p>\n<h2 id=\"path-through-this-part\"><a href=\"#path-through-this-part\">Path Through This Part</a></h2><p>This section teaches the surface you read all the time: direct values, value transformations, reusable functions, ordinary calls, and receiver-led methods.\nThese are not isolated grammar facts; they are building blocks for nearly every later chapter.</p>\n<h2 id=\"what-this-part-solves\"><a href=\"#what-this-part-solves\">What This Part Solves</a></h2><p>Readers need enough substance here to stop asking basic &quot;how do I compute this?&quot; or &quot;how do I call that?&quot; questions later.\nAt the same time, this section should not become a full reference manual before users can even read a tiny file comfortably.\nThe right balance is concrete examples with one clear lesson per chapter.</p>\n<h2 id=\"how-to-read-it\"><a href=\"#how-to-read-it\">How to Read It</a></h2><p>Work through chapters in order and keep rewriting examples in your own words.\nName intermediate results, trace values left to right through calls, and compare plain functions with methods so you can choose the clearer style in real code.\nIf one form still feels mechanical, keep it small until the reading pattern becomes boring.</p>\n",
		"summaryHtml": "Learn everyday syntax in very small pieces so nothing stacks too early."
	},
	{
		"locale": "en",
		"id": "data",
		"kind": "part",
		"parentId": null,
		"depth": 0,
		"treePath": [
			"data"
		],
		"childIds": [
			"data-modeling"
		],
		"partId": "data",
		"partTitle": "data",
		"sectionId": null,
		"sectionTitle": null,
		"path": "/learn/book/data",
		"canonicalPath": "/learn/book/data",
		"aliases": [
			"/docs/book/data",
			"/learn/book/data",
			"/docs/language/data",
			"/learn/language/data"
		],
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
				"id": "path-through-this-part",
				"text": "Path Through This Part"
			},
			{
				"depth": 2,
				"id": "what-this-part-solves",
				"text": "What This Part Solves"
			},
			{
				"depth": 2,
				"id": "how-to-read-it",
				"text": "How to Read It"
			}
		],
		"html": "<p>This part introduces values with shape.\nRecords, arrays, slices, and patterns all answer different kinds of data questions, and the examples should help you feel when to reach for each one.\nThe section is about practical structure, not abstract taxonomy.</p>\n<h2 id=\"path-through-this-part\"><a href=\"#path-through-this-part\">Path Through This Part</a></h2><p>This section teaches labeled data, ordered data, and branching on data shape.\nYou will construct values, update them, and then decide what to do when shape changes the result.</p>\n<h2 id=\"what-this-part-solves\"><a href=\"#what-this-part-solves\">What This Part Solves</a></h2><p>Once values stop being just numbers or strings, users need examples that answer &quot;how should I model this?&quot; not only &quot;what punctuation exists?&quot;\nIf the docs stay too thin here, beginners end up asking how to update one field, how to represent a list, or how to branch on constructor cases.\nThis section should answer those questions with compact but meaningful examples.</p>\n<h2 id=\"how-to-read-it\"><a href=\"#how-to-read-it\">How to Read It</a></h2><p>Start with records and arrays before pattern matching.\nGet comfortable recognizing named fields versus ordered positions, then move into <code>match</code> once those shapes already mean something to you.\nWhen examples grow, keep asking what data shape each construct is making visible.</p>\n",
		"summaryHtml": "Keep data-shape learning readable by separating records, sequences, and patterns."
	},
	{
		"locale": "en",
		"id": "organization",
		"kind": "part",
		"parentId": null,
		"depth": 0,
		"treePath": [
			"organization"
		],
		"childIds": [
			"organization-modules"
		],
		"partId": "organization",
		"partTitle": "organization",
		"sectionId": null,
		"sectionTitle": null,
		"path": "/learn/book/organization",
		"canonicalPath": "/learn/book/organization",
		"aliases": [
			"/docs/book/organization",
			"/learn/book/organization",
			"/docs/language/organization",
			"/learn/language/organization"
		],
		"questions": [],
		"title": "Code Organization",
		"description": "Move from single files into packages, imports, and exports.",
		"group": "Code Organization",
		"section": "Code Organization",
		"order": 4,
		"slug": "organization",
		"summary": "Grow from one file to packages without changing the mental model of code flow.",
		"descriptionHtml": "Move from single files into packages, imports, and exports.",
		"headings": [
			{
				"depth": 2,
				"id": "path-through-this-part",
				"text": "Path Through This Part"
			},
			{
				"depth": 2,
				"id": "what-this-part-solves",
				"text": "What This Part Solves"
			},
			{
				"depth": 2,
				"id": "how-to-read-it",
				"text": "How to Read It"
			}
		],
		"html": "<p>This part explains how Musi code grows past one scratch file.\nFiles stay important, but packages, imports, and exports become necessary once code needs boundaries, reuse, and repeatable commands.\nThe goal is to grow code organization without breaking the simple mental model from start chapters.</p>\n<h2 id=\"path-through-this-part\"><a href=\"#path-through-this-part\">Path Through This Part</a></h2><p>This section teaches file-level thinking, package structure, and explicit module boundaries.\nYou will see where code lives, how projects are created, and how names move across file edges.</p>\n<h2 id=\"what-this-part-solves\"><a href=\"#what-this-part-solves\">What This Part Solves</a></h2><p>Many guides overload readers by mixing package ceremony into first syntax lesson.\nThat is wrong direction.\nUsers should first understand one file well, then learn how to organize many files without losing track of what each boundary is for.</p>\n<h2 id=\"how-to-read-it\"><a href=\"#how-to-read-it\">How to Read It</a></h2><p>Read these pages as scaling story.\nFirst stabilize one-file flow, then move to package commands, then add imports and exports where code genuinely needs boundaries.\nAt each step, ask whether the new structure solves a real organization problem instead of adding ceremony for its own sake.</p>\n",
		"summaryHtml": "Grow from one file to packages without changing the mental model of code flow."
	},
	{
		"locale": "en",
		"id": "types",
		"kind": "part",
		"parentId": null,
		"depth": 0,
		"treePath": [
			"types"
		],
		"childIds": [
			"types-foundations"
		],
		"partId": "types",
		"partTitle": "types",
		"sectionId": null,
		"sectionTitle": null,
		"path": "/learn/book/types",
		"canonicalPath": "/learn/book/types",
		"aliases": [
			"/docs/book/types",
			"/learn/book/types",
			"/docs/language/types",
			"/learn/language/types"
		],
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
				"id": "path-through-this-part",
				"text": "Path Through This Part"
			},
			{
				"depth": 2,
				"id": "what-this-part-solves",
				"text": "What This Part Solves"
			},
			{
				"depth": 2,
				"id": "how-to-read-it",
				"text": "How to Read It"
			}
		],
		"html": "<p>This part adds type information gradually.\nYou start with explicit annotations, then see where inference reduces repetition, then use generics to reuse one definition across many types.\nThe emphasis stays on readable, practical code rather than an abstract type-system tour.</p>\n<p>Think of types as labels, molds, and rules.\nSome pages name the label on one value, some pages show the mold that fits many values, and some pages show the rule sheet that keeps callers honest.</p>\n<h2 id=\"path-through-this-part\"><a href=\"#path-through-this-part\">Path Through This Part</a></h2><p>This section teaches three related tools: annotations for explicit boundaries, inference for obvious cases, and generics for reusable typed definitions.\nIt introduces callable type spelling such as <code>T -&gt; U</code> and <code>T ~&gt; U</code>, so readers can connect type chapters back to functions and effects.\nIt also makes the <code>:</code> story explicit: annotations use <code>:</code>, constraints use <code>where</code>, and named variant payloads use constructor-style declarations such as <code>| Configured(port : Int)</code>.</p>\n<h2 id=\"what-this-part-solves\"><a href=\"#what-this-part-solves\">What This Part Solves</a></h2><p>Users need enough type detail to write and read real code, but too much theory too early causes drop-off.\nThis section aims for decision-making help: when should I annotate, when can I omit, when is a generic definition worth it, and how do callable types fit into effectful code?\nThat is more useful than a giant catalog of type features.</p>\n<h2 id=\"how-to-read-it\"><a href=\"#how-to-read-it\">How to Read It</a></h2><p>Learn one move at a time.\nAdd types where clarity rises, remove only what surrounding code makes obvious, and introduce generic parameters only after you can already read annotated functions comfortably.\nKeep examples small enough that every type choice still has an obvious reason.</p>\n<p>After <code>forall</code>, the section introduces practical dependent types: value parameters in type lists, indexed variant results, <code>partial</code> for runtime-only definitions, and <code>~=</code> for type equality.</p>\n",
		"summaryHtml": "Introduce types gradually: explicit first, inferred second, generic third."
	},
	{
		"locale": "en",
		"id": "abstractions",
		"kind": "part",
		"parentId": null,
		"depth": 0,
		"treePath": [
			"abstractions"
		],
		"childIds": [
			"abstractions-laws"
		],
		"partId": "abstractions",
		"partTitle": "abstractions",
		"sectionId": null,
		"sectionTitle": null,
		"path": "/learn/book/abstractions",
		"canonicalPath": "/learn/book/abstractions",
		"aliases": [
			"/docs/book/abstractions",
			"/learn/book/abstractions",
			"/docs/language/abstractions",
			"/learn/language/abstractions"
		],
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
				"id": "path-through-this-part",
				"text": "Path Through This Part"
			},
			{
				"depth": 2,
				"id": "what-this-part-solves",
				"text": "What This Part Solves"
			},
			{
				"depth": 2,
				"id": "how-to-read-it",
				"text": "How to Read It"
			}
		],
		"html": "<p>This part moves from plain functions into reusable behavior contracts.\nClasses describe shared behavior, instances provide concrete implementations, and laws explain what those abstractions are supposed to mean.\nThe section works only if each layer stays distinct.</p>\n<p>Think of it like a vehicle family, one car, and the road rule that says the car still has to satisfy a real-world promise before it counts as correct.\nThat is the same split the chapter pages keep: shape first, implementation second, law last.</p>\n<h2 id=\"path-through-this-part\"><a href=\"#path-through-this-part\">Path Through This Part</a></h2><p>This section teaches three abstraction layers: contract, implementation, and semantic expectation.\nThose layers are connected, but they are not interchangeable.\nUnderstanding the separation is more important than memorizing every keyword.</p>\n<h2 id=\"what-this-part-solves\"><a href=\"#what-this-part-solves\">What This Part Solves</a></h2><p>Abstraction chapters become overwhelming when readers cannot tell whether they are looking at behavior shape, specific implementation, or mathematical expectation.\nToo little explanation here leads straight to &quot;what is class for?&quot; or &quot;where does real behavior live?&quot;\nThis section should answer those questions with small examples before complexity grows.</p>\n<h2 id=\"how-to-read-it\"><a href=\"#how-to-read-it\">How to Read It</a></h2><p>Read class page first, then instance page, then law page.\nFor each example, ask what stays generic, what becomes concrete, and what property should hold across all correct implementations.\nIf that separation is blurry, stay with the small <code>Eq</code>-style examples until it is not.</p>\n",
		"summaryHtml": "Separate behavior shape, concrete implementation, and semantic law into distinct chapters."
	},
	{
		"locale": "en",
		"id": "effects-runtime",
		"kind": "part",
		"parentId": null,
		"depth": 0,
		"treePath": [
			"effects-runtime"
		],
		"childIds": [
			"effects-handling",
			"effects-runtime-model"
		],
		"partId": "effects-runtime",
		"partTitle": "effects-runtime",
		"sectionId": null,
		"sectionTitle": null,
		"path": "/learn/book/effects-runtime",
		"canonicalPath": "/learn/book/effects-runtime",
		"aliases": [
			"/docs/book/effects-runtime",
			"/learn/book/effects-runtime",
			"/docs/language/effects-runtime",
			"/learn/language/effects-runtime"
		],
		"questions": [],
		"title": "Effects and Runtime",
		"description": "Understand effects, using, handlers, foundation, runtime, and stdlib layering.",
		"group": "Effects and Runtime",
		"section": "Effects and Runtime",
		"order": 7,
		"slug": "effects-runtime",
		"summary": "Make effect flow explicit, then place runtime and stdlib on top of that model.",
		"descriptionHtml": "Understand effects, using, handlers, foundation, runtime, and stdlib layering.",
		"headings": [
			{
				"depth": 2,
				"id": "path-through-this-part",
				"text": "Path Through This Part"
			},
			{
				"depth": 2,
				"id": "what-this-part-solves",
				"text": "What This Part Solves"
			},
			{
				"depth": 2,
				"id": "how-to-read-it",
				"text": "How to Read It"
			}
		],
		"html": "<p>This part explains explicit capability flow and the layers built around it.\nEffects describe requested work, <code>using</code> surfaces required capabilities, handlers resolve requests, and foundation/runtime/stdlib pages place imports in their proper layer.\nThe goal is to make boundary thinking readable instead of mystical.</p>\n<h2 id=\"path-through-this-part\"><a href=\"#path-through-this-part\">Path Through This Part</a></h2><p>This section teaches effect requests, capability requirements, request handling, and module layering from core to runtime to stdlib.\nIt is one of richest parts of the book, but every page should still answer one practical question.</p>\n<h2 id=\"what-this-part-solves\"><a href=\"#what-this-part-solves\">What This Part Solves</a></h2><p>Effects and runtime topics become intimidating when all boundaries are introduced at once.\nUsers then ask whether something is built in, imported, handled, runtime-backed, or just standard library code.\nThis section prevents that pile-up by separating each concern while keeping one coherent model of explicit capability flow.</p>\n<h2 id=\"how-to-read-it\"><a href=\"#how-to-read-it\">How to Read It</a></h2><p>Follow the order.\nLearn effect requests before handlers, understand <code>using</code> before resolving capabilities, and keep module layers distinct when reading imports.\nWhenever a page feels abstract, come back to concrete question: what work is being requested, and who is responsible for providing it?</p>\n",
		"summaryHtml": "Make effect flow explicit, then place runtime and stdlib on top of that model."
	},
	{
		"locale": "en",
		"id": "developers",
		"kind": "part",
		"parentId": null,
		"depth": 0,
		"treePath": [
			"developers"
		],
		"childIds": [
			"developers-guides"
		],
		"partId": "developers",
		"partTitle": "developers",
		"sectionId": null,
		"sectionTitle": null,
		"path": "/learn/book/developers",
		"canonicalPath": "/learn/book/developers",
		"aliases": [
			"/docs/book/developers",
			"/learn/book/developers",
			"/docs/language/developers",
			"/learn/language/developers",
			"/learn/guides"
		],
		"questions": [],
		"title": "Musi for Developers",
		"description": "Map common habits from other languages into Musi code.",
		"group": "Musi for Developers",
		"section": "Musi for Developers",
		"order": 9,
		"slug": "developers",
		"summary": "Translate familiar habits into Musi without carrying over syntax that does not fit.",
		"descriptionHtml": "Map common habits from other languages into Musi code.",
		"headings": [
			{
				"depth": 2,
				"id": "choose-your-starting-point",
				"text": "Choose Your Starting Point"
			},
			{
				"depth": 2,
				"id": "shared-musi-model",
				"text": "Shared Musi Model"
			},
			{
				"depth": 2,
				"id": "guides",
				"text": "Guides"
			}
		],
		"html": "<p>This part is for readers who already write software in another language.\nEach guide starts from habits you already have, then shows how Musi writes the same idea with expressions, <code>let</code>, data constructors, pattern matching, effects, packages, and explicit unsafe boundaries.</p>\n<p>Rust gets the deepest comparison because the mutability model and type-driven habits need a slower translation.\nThat guide goes further into <code>let mut x</code>, value-based mutation, and the way Musi keeps the mutable part visible instead of scattered through the whole binding model.</p>\n<h2 id=\"choose-your-starting-point\"><a href=\"#choose-your-starting-point\">Choose Your Starting Point</a></h2><p>Pick the language you use most often.\nThe guide will not teach that language back to you; it names the habits that usually transfer well and the ones that need a different Musi shape.</p>\n<h2 id=\"shared-musi-model\"><a href=\"#shared-musi-model\">Shared Musi Model</a></h2><p>Musi code reads as values flowing through expressions.\nBlocks produce values, <code>match</code> chooses by shape, records keep named fields, and <code>request</code> asks an effect handler for work.\nThere is no <code>return</code> keyword in ordinary function bodies.\nThe final expression is the value.</p>\n<h2 id=\"guides\"><a href=\"#guides\">Guides</a></h2><ul>\n<li><a href=\"/learn/book/developers/guides/c\">Musi for C Developers</a></li>\n<li><a href=\"/learn/book/developers/guides/csharp\">Musi for C# Developers</a></li>\n<li><a href=\"/learn/book/developers/guides/cpp\">Musi for C++ Developers</a></li>\n<li><a href=\"/learn/book/developers/guides/go\">Musi for Go Developers</a></li>\n<li><a href=\"/learn/book/developers/guides/java\">Musi for Java Developers</a></li>\n<li><a href=\"/learn/book/developers/guides/javascript\">Musi for JavaScript Developers</a></li>\n<li><a href=\"/learn/book/developers/guides/python\">Musi for Python Developers</a></li>\n<li><a href=\"/learn/book/developers/guides/rust\">Musi for Rust Developers</a></li>\n<li><a href=\"/learn/book/developers/guides/typescript\">Musi for TypeScript Developers</a></li>\n</ul>\n",
		"summaryHtml": "Translate familiar habits into Musi without carrying over syntax that does not fit."
	},
	{
		"locale": "en",
		"id": "advanced",
		"kind": "part",
		"parentId": null,
		"depth": 0,
		"treePath": [
			"advanced"
		],
		"childIds": [
			"advanced-interop",
			"advanced-meta-tooling"
		],
		"partId": "advanced",
		"partTitle": "advanced",
		"sectionId": null,
		"sectionTitle": null,
		"path": "/learn/book/advanced",
		"canonicalPath": "/learn/book/advanced",
		"aliases": [
			"/docs/book/advanced",
			"/learn/book/advanced",
			"/docs/language/advanced",
			"/learn/language/advanced"
		],
		"questions": [],
		"title": "Advanced and Tooling",
		"description": "Finish with attributes, foreign bindings, quote, testing, and tool workflow.",
		"group": "Advanced and Tooling",
		"section": "Advanced and Tooling",
		"order": 8,
		"slug": "advanced",
		"summary": "Keep sharp or advanced topics late, after ordinary code already feels natural.",
		"descriptionHtml": "Finish with attributes, foreign bindings, quote, testing, and tool workflow.",
		"headings": [
			{
				"depth": 2,
				"id": "path-through-this-part",
				"text": "Path Through This Part"
			},
			{
				"depth": 2,
				"id": "what-this-part-solves",
				"text": "What This Part Solves"
			},
			{
				"depth": 2,
				"id": "how-to-read-it",
				"text": "How to Read It"
			}
		],
		"html": "<p>This part finishes the learning path with boundary and workflow tools.\nAttributes, foreign declarations, unsafe FFI, quote, testing, and command workflow all matter, but they matter most after ordinary Musi code already feels stable.\nThese pages should feel like practical extensions, not sudden genre changes.</p>\n<p>Think of this part as the control room and the service hatch.\nIt tells you where metadata lives, where native work crosses a boundary, and how to keep that work small and visible.</p>\n<h2 id=\"path-through-this-part\"><a href=\"#path-through-this-part\">Path Through This Part</a></h2><p>This section teaches metadata, host-boundary declarations, unsafe FFI boundaries, code-as-data tools, testing surface, and everyday command workflow.\nIt distinguishes attribute families explicitly, so advanced pages answer more than &quot;yes, attributes exist&quot;.</p>\n<h2 id=\"what-this-part-solves\"><a href=\"#what-this-part-solves\">What This Part Solves</a></h2><p>If advanced topics appear too early, beginners get overwhelmed and leave.\nIf they never appear with concrete examples, later-stage users still have to ask how Musi handles metadata, native integration, generated syntax, verification, or repeatable workflow.\nThis section answers those needs without turning into a wall of detail.</p>\n<h2 id=\"how-to-read-it\"><a href=\"#how-to-read-it\">How to Read It</a></h2><p>Treat these chapters as boundary tools.\nReach for them when ordinary code is already clear and you need metadata, external integration, generated syntax, verification, or repeatable workflow.\nFollow visible example sources when one chapter shows boundary syntax you want to trace back into repo code.</p>\n",
		"summaryHtml": "Keep sharp or advanced topics late, after ordinary code already feels natural."
	},
	{
		"locale": "en",
		"id": "start-foundations",
		"kind": "section",
		"parentId": "start",
		"depth": 1,
		"treePath": [
			"start",
			"start-foundations"
		],
		"childIds": [
			"getting-started",
			"first-program",
			"values-and-let",
			"blocks-and-expressions",
			"mutation"
		],
		"partId": "start",
		"partTitle": "Start",
		"sectionId": "start-foundations",
		"sectionTitle": "Foundations",
		"path": "/learn/book/start/foundations",
		"canonicalPath": "/learn/book/start/foundations",
		"aliases": [
			"/docs/book/start/foundations",
			"/learn/book/start/foundations",
			"/docs/language/start/foundations",
			"/learn/language/start/foundations"
		],
		"questions": [],
		"title": "Foundations",
		"description": "Install Musi, read one file, and learn expression-first flow.",
		"group": "Start",
		"section": "Foundations",
		"order": 1,
		"slug": "foundations",
		"summary": "First setup and first-file habits: values, blocks, and explicit mutation.",
		"descriptionHtml": "Install Musi, read one file, and learn expression-first flow.",
		"headings": [],
		"html": "<p>Install Musi, read one file, and learn expression-first flow.</p>",
		"summaryHtml": "First setup and first-file habits: values, blocks, and explicit mutation."
	},
	{
		"locale": "en",
		"id": "core-expressions",
		"kind": "section",
		"parentId": "core",
		"depth": 1,
		"treePath": [
			"core",
			"core-expressions"
		],
		"childIds": [
			"literals",
			"tuples-and-unit",
			"operators",
			"ranges"
		],
		"partId": "core",
		"partTitle": "Core Syntax",
		"sectionId": "core-expressions",
		"sectionTitle": "Expressions",
		"path": "/learn/book/core/expressions",
		"canonicalPath": "/learn/book/core/expressions",
		"aliases": [
			"/docs/book/core/expressions",
			"/learn/book/core/expressions",
			"/docs/language/core/expressions",
			"/learn/language/core/expressions"
		],
		"questions": [],
		"title": "Expressions",
		"description": "Learn literals, tuples, operators, and ranges for everyday calculations.",
		"group": "Core Syntax",
		"section": "Expressions",
		"order": 1,
		"slug": "expressions",
		"summary": "Core value and operator forms used across normal Musi code.",
		"descriptionHtml": "Learn literals, tuples, operators, and ranges for everyday calculations.",
		"headings": [],
		"html": "<p>Learn literals, tuples, operators, and ranges for everyday calculations.</p>",
		"summaryHtml": "Core value and operator forms used across normal Musi code."
	},
	{
		"locale": "en",
		"id": "core-functions",
		"kind": "section",
		"parentId": "core",
		"depth": 1,
		"treePath": [
			"core",
			"core-functions"
		],
		"childIds": [
			"functions",
			"lambdas",
			"calls",
			"methods"
		],
		"partId": "core",
		"partTitle": "Core Syntax",
		"sectionId": "core-functions",
		"sectionTitle": "Functions and Calls",
		"path": "/learn/book/core/functions-and-calls",
		"canonicalPath": "/learn/book/core/functions-and-calls",
		"aliases": [
			"/docs/book/core/functions-and-calls",
			"/learn/book/core/functions-and-calls",
			"/docs/language/core/functions-and-calls",
			"/learn/language/core/functions-and-calls"
		],
		"questions": [],
		"title": "Functions and Calls",
		"description": "Define reusable functions, write lambdas, and choose call and method forms.",
		"group": "Core Syntax",
		"section": "Functions and Calls",
		"order": 2,
		"slug": "functions-and-calls",
		"summary": "Function declarations, lambdas, and call-site reading patterns.",
		"descriptionHtml": "Define reusable functions, write lambdas, and choose call and method forms.",
		"headings": [],
		"html": "<p>Define reusable functions, write lambdas, and choose call and method forms.</p>",
		"summaryHtml": "Function declarations, lambdas, and call-site reading patterns."
	},
	{
		"locale": "en",
		"id": "data-modeling",
		"kind": "section",
		"parentId": "data",
		"depth": 1,
		"treePath": [
			"data",
			"data-modeling"
		],
		"childIds": [
			"records",
			"indexing-and-fields",
			"data-definitions",
			"arrays-and-slices",
			"patterns"
		],
		"partId": "data",
		"partTitle": "Data",
		"sectionId": "data-modeling",
		"sectionTitle": "Data Modeling",
		"path": "/learn/book/data/modeling",
		"canonicalPath": "/learn/book/data/modeling",
		"aliases": [
			"/docs/book/data/modeling",
			"/learn/book/data/modeling",
			"/docs/language/data/modeling",
			"/learn/language/data/modeling"
		],
		"questions": [],
		"title": "Data Modeling",
		"description": "Model records, variants, collections, and matching patterns.",
		"group": "Data and Patterns",
		"section": "Data Modeling",
		"order": 1,
		"slug": "modeling",
		"summary": "Build domain data with records and variants, then read and transform with patterns.",
		"descriptionHtml": "Model records, variants, collections, and matching patterns.",
		"headings": [],
		"html": "<p>Model records, variants, collections, and matching patterns.</p>",
		"summaryHtml": "Build domain data with records and variants, then read and transform with patterns."
	},
	{
		"locale": "en",
		"id": "organization-modules",
		"kind": "section",
		"parentId": "organization",
		"depth": 1,
		"treePath": [
			"organization",
			"organization-modules"
		],
		"childIds": [
			"files",
			"packages",
			"imports-and-exports"
		],
		"partId": "organization",
		"partTitle": "Code Organization",
		"sectionId": "organization-modules",
		"sectionTitle": "Files and Modules",
		"path": "/learn/book/organization/modules",
		"canonicalPath": "/learn/book/organization/modules",
		"aliases": [
			"/docs/book/organization/modules",
			"/learn/book/organization/modules",
			"/docs/language/organization/modules",
			"/learn/language/organization/modules"
		],
		"questions": [],
		"title": "Files and Modules",
		"description": "Structure files, packages, and imports so projects stay navigable.",
		"group": "Organization and Modules",
		"section": "Files and Modules",
		"order": 1,
		"slug": "modules",
		"summary": "Project layout, package boundaries, and import/export discipline.",
		"descriptionHtml": "Structure files, packages, and imports so projects stay navigable.",
		"headings": [],
		"html": "<p>Structure files, packages, and imports so projects stay navigable.</p>",
		"summaryHtml": "Project layout, package boundaries, and import/export discipline."
	},
	{
		"locale": "en",
		"id": "types-foundations",
		"kind": "section",
		"parentId": "types",
		"depth": 1,
		"treePath": [
			"types",
			"types-foundations"
		],
		"childIds": [
			"type-annotations",
			"callable-types",
			"type-inference",
			"generics",
			"type-tests-and-casts",
			"forall-types",
			"dependent-types"
		],
		"partId": "types",
		"partTitle": "Types",
		"sectionId": "types-foundations",
		"sectionTitle": "Type Foundations",
		"path": "/learn/book/types/foundations",
		"canonicalPath": "/learn/book/types/foundations",
		"aliases": [
			"/docs/book/types/foundations",
			"/learn/book/types/foundations",
			"/docs/language/types/foundations",
			"/learn/language/types/foundations"
		],
		"questions": [],
		"title": "Type Foundations",
		"description": "Type annotations, inference, generics, callable types, and checks.",
		"group": "Types and Generic Design",
		"section": "Type Foundations",
		"order": 1,
		"slug": "foundations",
		"summary": "Type reading and design patterns from everyday annotations to advanced forms.",
		"descriptionHtml": "Type annotations, inference, generics, callable types, and checks.",
		"headings": [],
		"html": "<p>Type annotations, inference, generics, callable types, and checks.</p>",
		"summaryHtml": "Type reading and design patterns from everyday annotations to advanced forms."
	},
	{
		"locale": "en",
		"id": "abstractions-laws",
		"kind": "section",
		"parentId": "abstractions",
		"depth": 1,
		"treePath": [
			"abstractions",
			"abstractions-laws"
		],
		"childIds": [
			"classes",
			"instances",
			"laws"
		],
		"partId": "abstractions",
		"partTitle": "Abstractions",
		"sectionId": "abstractions-laws",
		"sectionTitle": "Classes, Instances, and Laws",
		"path": "/learn/book/abstractions/classes-instances-laws",
		"canonicalPath": "/learn/book/abstractions/classes-instances-laws",
		"aliases": [
			"/docs/book/abstractions/classes-instances-laws",
			"/learn/book/abstractions/classes-instances-laws",
			"/docs/language/abstractions/classes-instances-laws",
			"/learn/language/abstractions/classes-instances-laws"
		],
		"questions": [],
		"title": "Classes, Instances, and Laws",
		"description": "Define reusable interfaces, implementations, and behavior constraints.",
		"group": "Abstractions",
		"section": "Classes and Laws",
		"order": 1,
		"slug": "classes-instances-laws",
		"summary": "Shared behavior contracts and law-oriented implementation discipline.",
		"descriptionHtml": "Define reusable interfaces, implementations, and behavior constraints.",
		"headings": [],
		"html": "<p>Define reusable interfaces, implementations, and behavior constraints.</p>",
		"summaryHtml": "Shared behavior contracts and law-oriented implementation discipline."
	},
	{
		"locale": "en",
		"id": "effects-handling",
		"kind": "section",
		"parentId": "effects-runtime",
		"depth": 1,
		"treePath": [
			"effects-runtime",
			"effects-handling"
		],
		"childIds": [
			"effects",
			"using",
			"handlers"
		],
		"partId": "effects-runtime",
		"partTitle": "Effects and Runtime",
		"sectionId": "effects-handling",
		"sectionTitle": "Effect Handling",
		"path": "/learn/book/effects-runtime/handling",
		"canonicalPath": "/learn/book/effects-runtime/handling",
		"aliases": [
			"/docs/book/effects-runtime/handling",
			"/learn/book/effects-runtime/handling",
			"/docs/language/effects-runtime/handling",
			"/learn/language/effects-runtime/handling"
		],
		"questions": [],
		"title": "Effect Handling",
		"description": "Request operations and handle them with explicit handler scopes.",
		"group": "Effects and Runtime",
		"section": "Effect Handling",
		"order": 1,
		"slug": "handling",
		"summary": "Effect requests, using scopes, and handler composition.",
		"descriptionHtml": "Request operations and handle them with explicit handler scopes.",
		"headings": [],
		"html": "<p>Request operations and handle them with explicit handler scopes.</p>",
		"summaryHtml": "Effect requests, using scopes, and handler composition."
	},
	{
		"locale": "en",
		"id": "effects-runtime-model",
		"kind": "section",
		"parentId": "effects-runtime",
		"depth": 1,
		"treePath": [
			"effects-runtime",
			"effects-runtime-model"
		],
		"childIds": [
			"foundation",
			"runtime",
			"stdlib"
		],
		"partId": "effects-runtime",
		"partTitle": "Effects and Runtime",
		"sectionId": "effects-runtime-model",
		"sectionTitle": "Runtime Model",
		"path": "/learn/book/effects-runtime/runtime-model",
		"canonicalPath": "/learn/book/effects-runtime/runtime-model",
		"aliases": [
			"/docs/book/effects-runtime/runtime-model",
			"/learn/book/effects-runtime/runtime-model",
			"/docs/language/effects-runtime/runtime-model",
			"/learn/language/effects-runtime/runtime-model"
		],
		"questions": [],
		"title": "Runtime Model",
		"description": "Understand runtime boundaries and stdlib effect patterns.",
		"group": "Effects and Runtime",
		"section": "Runtime Model",
		"order": 2,
		"slug": "runtime-model",
		"summary": "Runtime behavior, operational model, and standard library integration.",
		"descriptionHtml": "Understand runtime boundaries and stdlib effect patterns.",
		"headings": [],
		"html": "<p>Understand runtime boundaries and stdlib effect patterns.</p>",
		"summaryHtml": "Runtime behavior, operational model, and standard library integration."
	},
	{
		"locale": "en",
		"id": "developers-guides",
		"kind": "section",
		"parentId": "developers",
		"depth": 1,
		"treePath": [
			"developers",
			"developers-guides"
		],
		"childIds": [
			"developers-rust",
			"musi-for-python-developers",
			"musi-for-java-developers",
			"musi-for-javascript-developers",
			"musi-for-typescript-developers",
			"musi-for-c-developers",
			"musi-for-cpp-developers",
			"musi-for-csharp-developers",
			"musi-for-go-developers"
		],
		"partId": "developers",
		"partTitle": "Musi for Developers",
		"sectionId": "developers-guides",
		"sectionTitle": "Language Guides",
		"path": "/learn/book/developers/guides",
		"canonicalPath": "/learn/book/developers/guides",
		"aliases": [
			"/docs/book/developers/guides",
			"/learn/book/developers/guides",
			"/docs/language/developers/guides",
			"/learn/language/developers/guides"
		],
		"questions": [],
		"title": "Language Guides",
		"description": "Map existing language habits to Musi with practical side-by-side guidance.",
		"group": "Musi for Developers",
		"section": "Language Guides",
		"order": 1,
		"slug": "guides",
		"summary": "Comparative guides for teams moving from other language ecosystems.",
		"descriptionHtml": "Map existing language habits to Musi with practical side-by-side guidance.",
		"headings": [],
		"html": "<p>Map existing language habits to Musi with practical side-by-side guidance.</p>",
		"summaryHtml": "Comparative guides for teams moving from other language ecosystems."
	},
	{
		"locale": "en",
		"id": "advanced-interop",
		"kind": "section",
		"parentId": "advanced",
		"depth": 1,
		"treePath": [
			"advanced",
			"advanced-interop"
		],
		"childIds": [
			"attributes",
			"foreign",
			"unsafe-and-ffi"
		],
		"partId": "advanced",
		"partTitle": "Advanced and Tooling",
		"sectionId": "advanced-interop",
		"sectionTitle": "Interop and Safety",
		"path": "/learn/book/advanced/interop",
		"canonicalPath": "/learn/book/advanced/interop",
		"aliases": [
			"/docs/book/advanced/interop",
			"/learn/book/advanced/interop",
			"/docs/language/advanced/interop",
			"/learn/language/advanced/interop"
		],
		"questions": [],
		"title": "Interop and Safety",
		"description": "Use attributes, foreign declarations, and unsafe FFI boundaries.",
		"group": "Advanced and Tooling",
		"section": "Interop and Safety",
		"order": 1,
		"slug": "interop",
		"summary": "Native interop boundaries and explicit unsafe operations.",
		"descriptionHtml": "Use attributes, foreign declarations, and unsafe FFI boundaries.",
		"headings": [],
		"html": "<p>Use attributes, foreign declarations, and unsafe FFI boundaries.</p>",
		"summaryHtml": "Native interop boundaries and explicit unsafe operations."
	},
	{
		"locale": "en",
		"id": "advanced-meta-tooling",
		"kind": "section",
		"parentId": "advanced",
		"depth": 1,
		"treePath": [
			"advanced",
			"advanced-meta-tooling"
		],
		"childIds": [
			"operator-forms",
			"quote-and-syntax",
			"templates-and-splices",
			"testing",
			"running-and-tooling"
		],
		"partId": "advanced",
		"partTitle": "Advanced and Tooling",
		"sectionId": "advanced-meta-tooling",
		"sectionTitle": "Meta and Tooling",
		"path": "/learn/book/advanced/meta-tooling",
		"canonicalPath": "/learn/book/advanced/meta-tooling",
		"aliases": [
			"/docs/book/advanced/meta-tooling",
			"/learn/book/advanced/meta-tooling",
			"/docs/language/advanced/meta-tooling",
			"/learn/language/advanced/meta-tooling"
		],
		"questions": [],
		"title": "Meta and Tooling",
		"description": "Operator declarations, syntax tools, templates, testing, and run flow.",
		"group": "Advanced and Tooling",
		"section": "Meta and Tooling",
		"order": 2,
		"slug": "meta-tooling",
		"summary": "Syntax-oriented authoring tools and day-to-day development workflow.",
		"descriptionHtml": "Operator declarations, syntax tools, templates, testing, and run flow.",
		"headings": [],
		"html": "<p>Operator declarations, syntax tools, templates, testing, and run flow.</p>",
		"summaryHtml": "Syntax-oriented authoring tools and day-to-day development workflow."
	},
	{
		"locale": "en",
		"id": "developers-rust",
		"kind": "section",
		"parentId": "developers-guides",
		"depth": 2,
		"treePath": [
			"developers",
			"developers-guides",
			"developers-rust"
		],
		"childIds": [
			"musi-for-rust-developers",
			"rust-values-functions",
			"rust-mutation",
			"rust-records-structs",
			"rust-enums-data",
			"rust-traits-classes-laws",
			"rust-generics",
			"rust-results-effects",
			"rust-modules-packages",
			"rust-unsafe-ffi",
			"rust-testing-tooling"
		],
		"partId": "developers",
		"partTitle": "Musi for Developers",
		"sectionId": "developers-rust",
		"sectionTitle": "Musi for Rust Developers",
		"path": "/learn/book/developers/guides/rust",
		"canonicalPath": "/learn/book/developers/guides/rust",
		"aliases": [
			"/docs/book/developers/guides/rust",
			"/learn/book/developers/guides/rust",
			"/docs/book/developers/rust",
			"/learn/book/developers/rust",
			"/docs/language/developers/rust-guide",
			"/learn/language/developers/rust-guide"
		],
		"questions": [],
		"title": "Musi for Rust Developers",
		"description": "Translate Rust habits into Musi's expression, data, abstraction, and mutation model.",
		"group": "Musi for Developers",
		"section": "Rust Developers",
		"order": 1,
		"slug": "rust",
		"summary": "Rust-specific contrasts for mutation, data, traits, effects, and native boundaries.",
		"descriptionHtml": "Translate Rust habits into Musi&#39;s expression, data, abstraction, and mutation model.",
		"headings": [],
		"html": "<p>Translate Rust habits into Musi&#39;s expression, data, abstraction, and mutation model.</p>",
		"summaryHtml": "Rust-specific contrasts for mutation, data, traits, effects, and native boundaries."
	},
	{
		"locale": "en",
		"id": "getting-started",
		"kind": "chapter",
		"parentId": "start-foundations",
		"depth": 2,
		"treePath": [
			"start",
			"start-foundations",
			"getting-started"
		],
		"childIds": [],
		"partId": "start",
		"partTitle": "Start",
		"sectionId": "start-foundations",
		"sectionTitle": "Foundations",
		"path": "/learn/book/start/foundations/getting-started",
		"canonicalPath": "/learn/book/start/foundations/getting-started",
		"aliases": [
			"/docs/book/start/foundations/getting-started",
			"/learn/book/start/foundations/getting-started",
			"/docs/book/start/getting-started",
			"/learn/book/start/getting-started",
			"/docs/language/start/getting-started",
			"/learn/language/start/getting-started"
		],
		"questions": [],
		"title": "Getting Started",
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
				"id": "first-file",
				"text": "First File"
			},
			{
				"depth": 2,
				"id": "package-start",
				"text": "Package Start"
			},
			{
				"depth": 2,
				"id": "mistake-to-avoid",
				"text": "Mistake to Avoid"
			}
		],
		"html": "<p>Getting started in Musi has two tracks: direct file work and package work. The commands are separate because they answer different questions.</p>\n<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#702C00;--shiki-dark:#B392F0\">curl</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> -fsSL</span><span style=\"color:#032563;--shiki-dark:#9ECBFF\"> https://raw.githubusercontent.com/musi-lang/musi/main/install.sh</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> |</span><span style=\"color:#702C00;--shiki-dark:#B392F0\"> sh</span></span>\n<span class=\"line\"><span style=\"color:#702C00;--shiki-dark:#B392F0\">music</span><span style=\"color:#032563;--shiki-dark:#9ECBFF\"> check</span><span style=\"color:#032563;--shiki-dark:#9ECBFF\"> index.ms</span></span>\n<span class=\"line\"><span style=\"color:#702C00;--shiki-dark:#B392F0\">musi</span><span style=\"color:#032563;--shiki-dark:#9ECBFF\"> new</span><span style=\"color:#032563;--shiki-dark:#9ECBFF\"> hello</span></span></code></pre></section></div><p><code>music</code> is the direct-file tool. Use it when one entry file is enough. It is the fast lane for learning syntax and testing a small idea.</p>\n<p><code>musi</code> is the package tool. Use it when a directory has <code>musi.json</code>, package imports, tasks, tests, targets, and project-level configuration.</p>\n<p>Think of it like a notebook and a workshop. <code>music check index.ms</code> is the notebook: one page, one idea, fast feedback. <code>musi run</code> is the workshop: project layout, dependencies, and repeatable commands.</p>\n<h2 id=\"first-file\"><a href=\"#first-file\">First File</a></h2><p>Create <code>index.ms</code> with one exported <code>main</code> when you want to run it later, or one plain expression when you only want to check syntax.</p>\n<h2 id=\"package-start\"><a href=\"#package-start\">Package Start</a></h2><p>Use <code>musi new hello</code> when the code needs a manifest. From that point, prefer <code>musi check</code>, <code>musi run</code>, and <code>musi test</code> inside the package.</p>\n<h2 id=\"mistake-to-avoid\"><a href=\"#mistake-to-avoid\">Mistake to Avoid</a></h2><p>Do not treat <code>music</code> and <code>musi</code> as duplicate spellings. Pick by scope: one file uses <code>music</code>; one package uses <code>musi</code>.</p>\n<p>Continue to <a href=\"/learn/book/start/foundations/first-program\">First Program</a>.</p>\n",
		"summaryHtml": "Install tools, install Musi, and learn the two command lanes."
	},
	{
		"locale": "en",
		"id": "first-program",
		"kind": "chapter",
		"parentId": "start-foundations",
		"depth": 2,
		"treePath": [
			"start",
			"start-foundations",
			"first-program"
		],
		"childIds": [],
		"partId": "start",
		"partTitle": "Start",
		"sectionId": "start-foundations",
		"sectionTitle": "Foundations",
		"path": "/learn/book/start/foundations/first-program",
		"canonicalPath": "/learn/book/start/foundations/first-program",
		"aliases": [
			"/docs/book/start/foundations/first-program",
			"/learn/book/start/foundations/first-program",
			"/docs/book/start/first-program",
			"/learn/book/start/first-program",
			"/docs/language/start/first-program",
			"/learn/language/start/first-program"
		],
		"questions": [],
		"title": "First Program",
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
				"id": "reading-a-file",
				"text": "Reading a File"
			}
		],
		"html": "<p>A Musi program can be small enough to read in one breath: bind a value, end with a value, and let the tool check the file.</p>\n<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> answer </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 42</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">answer;</span></span></code></pre></section></div><p>Read the semicolon after <code>let answer := 42;</code> as &quot;this statement is done.&quot; Read the final <code>answer;</code> as the value the file leaves behind. That final-expression habit appears everywhere in Musi.</p>\n<p>C-like languages often teach a program as a function that returns. Musi starts smaller: an expression can already be the result. Functions come next, once naming a reusable action matters.</p>\n<h2 id=\"reading-a-file\"><a href=\"#reading-a-file\">Reading a File</a></h2><p>Read top to bottom. Names become available after their <code>let</code>. Later expressions use earlier names. There is no hidden global setup in this example, and there is no ordinary <code>return</code> keyword at the end.</p>\n<p>Continue to <a href=\"/learn/book/start/foundations/values-and-let\">Values and Let</a>.</p>\n",
		"summaryHtml": "Write one file, bind one value, and run it end to end."
	},
	{
		"locale": "en",
		"id": "values-and-let",
		"kind": "chapter",
		"parentId": "start-foundations",
		"depth": 2,
		"treePath": [
			"start",
			"start-foundations",
			"values-and-let"
		],
		"childIds": [],
		"partId": "start",
		"partTitle": "Start",
		"sectionId": "start-foundations",
		"sectionTitle": "Foundations",
		"path": "/learn/book/start/foundations/values-and-let",
		"canonicalPath": "/learn/book/start/foundations/values-and-let",
		"aliases": [
			"/docs/book/start/foundations/values-and-let",
			"/learn/book/start/foundations/values-and-let",
			"/docs/book/start/values-and-let",
			"/learn/book/start/values-and-let",
			"/docs/language/start/values-and-let",
			"/learn/language/start/values-and-let"
		],
		"questions": [],
		"title": "Values and Let",
		"description": "Learn what `let` means in Musi, how names are introduced, and when `let rec` matters.",
		"group": "Start",
		"section": "Start",
		"order": 3,
		"slug": "values-and-let",
		"summary": "Use `let` to name values, define callables, and understand when recursion needs `let rec`.",
		"descriptionHtml": "Learn what <code>let</code> means in Musi, how names are introduced, and when <code>let rec</code> matters.",
		"headings": [
			{
				"depth": 2,
				"id": "recursive-names",
				"text": "Recursive Names"
			}
		],
		"html": "<p><code>let</code> introduces a name. That one idea carries plain values, functions, data definitions, effects, classes, and many other declarations.</p>\n<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> port </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 8080</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> nextPort </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> port </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">+</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 1</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">nextPort;</span></span></code></pre></section></div><p>Read <code>let port := 8080;</code> as &quot;bind the name <code>port</code> to this value.&quot; The later binding can use it because the first binding is already in scope. Nothing here mutates <code>port</code>; a new name receives the next value.</p>\n<p>A spreadsheet analogy helps: one cell names an input, another cell derives from it, and the formula tells you which value depends on which.</p>\n<h2 id=\"recursive-names\"><a href=\"#recursive-names\">Recursive Names</a></h2><p>Use <code>let rec</code> when a definition must refer to itself while being defined. That is how repeated work can be expressed without making loops the first tool.</p>\n<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> rec</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> loop</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> (x : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">) : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> :=</span></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">  match</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> x</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> (</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">  | </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">0</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> =></span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 0</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">  | </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">_</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> =></span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> loop</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(x </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">-</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 1</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">)</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">  );</span></span></code></pre></section></div><p><code>loop</code> can call itself because the definition starts with <code>let rec</code>. Read the match arms as the two roads through the calculation: stop at zero, or call <code>loop</code> again with a smaller number.</p>\n<p>Continue to <a href=\"/learn/book/start/foundations/blocks-and-expressions\">Blocks and Expressions</a>.</p>\n",
		"summaryHtml": "Use <code>let</code> to name values, define callables, and understand when recursion needs <code>let rec</code>."
	},
	{
		"locale": "en",
		"id": "blocks-and-expressions",
		"kind": "chapter",
		"parentId": "start-foundations",
		"depth": 2,
		"treePath": [
			"start",
			"start-foundations",
			"blocks-and-expressions"
		],
		"childIds": [],
		"partId": "start",
		"partTitle": "Start",
		"sectionId": "start-foundations",
		"sectionTitle": "Foundations",
		"path": "/learn/book/start/foundations/blocks-and-expressions",
		"canonicalPath": "/learn/book/start/foundations/blocks-and-expressions",
		"aliases": [
			"/docs/book/start/foundations/blocks-and-expressions",
			"/learn/book/start/foundations/blocks-and-expressions",
			"/docs/book/start/blocks-and-expressions",
			"/learn/book/start/blocks-and-expressions",
			"/docs/language/start/blocks-and-expressions",
			"/learn/language/start/blocks-and-expressions"
		],
		"questions": [],
		"title": "Blocks and Expressions",
		"description": "Learn how blocks produce results, why Musi does not need `return`, and how to model repetition without loop statements.",
		"group": "Start",
		"section": "Start",
		"order": 4,
		"slug": "blocks-and-expressions",
		"summary": "Treat a block as one expression with setup at the top and the result at the bottom.",
		"descriptionHtml": "Learn how blocks produce results, why Musi does not need <code>return</code>, and how to model repetition without loop statements.",
		"headings": [
			{
				"depth": 2,
				"id": "local-names",
				"text": "Local Names"
			}
		],
		"html": "<p>A block groups several steps and produces one value. That makes blocks useful anywhere Musi expects an expression.</p>\n<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(</span></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">  let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> base </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 8000</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">  let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> offset </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 80</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">  base </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">+</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> offset</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">);</span></span></code></pre></section></div><p>Read the block like a small workbench. <code>base</code> and <code>offset</code> are tools laid out inside the bench. <code>base + offset</code> is the item that leaves the bench. The surrounding code sees only the final value.</p>\n<p>This matters because the same block shape appears in function bodies, match arms, handlers, and unsafe scopes. Once &quot;last expression wins&quot; feels natural, many later features become smaller.</p>\n<h2 id=\"local-names\"><a href=\"#local-names\">Local Names</a></h2><p>Names introduced inside a block stay inside that block. Use a block when a calculation needs helper names but outside code should only see the result.</p>\n<p>Continue to <a href=\"/learn/book/start/foundations/mutation\">Mutation</a>.</p>\n",
		"summaryHtml": "Treat a block as one expression with setup at the top and the result at the bottom."
	},
	{
		"locale": "en",
		"id": "mutation",
		"kind": "chapter",
		"parentId": "start-foundations",
		"depth": 2,
		"treePath": [
			"start",
			"start-foundations",
			"mutation"
		],
		"childIds": [],
		"partId": "start",
		"partTitle": "Start",
		"sectionId": "start-foundations",
		"sectionTitle": "Foundations",
		"path": "/learn/book/start/foundations/mutation",
		"canonicalPath": "/learn/book/start/foundations/mutation",
		"aliases": [
			"/docs/book/start/foundations/mutation",
			"/learn/book/start/foundations/mutation",
			"/docs/book/start/mutation",
			"/learn/book/start/mutation",
			"/docs/language/start/mutation",
			"/learn/language/start/mutation"
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
				"id": "use-it-sparingly",
				"text": "Use It Sparingly"
			}
		],
		"html": "<p>Mutation is explicit and value-based. A binding can hold a mutable value, but ordinary <code>let</code> still means &quot;name this value.&quot;</p>\n<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> counter </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> mut</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 1</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">counter </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> counter </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">+</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 1</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">counter;</span></span></code></pre></section></div><p>Read <code>let counter := mut 1;</code> as &quot;counter starts with a mutable value.&quot; Read <code>counter := counter + 1;</code> as &quot;replace the current value with the next value.&quot; The update is visible at the assignment site.</p>\n<p>A real-world counter works the same way. The clicker is the same object in your hand, but its displayed number changes.</p>\n<h2 id=\"use-it-sparingly\"><a href=\"#use-it-sparingly\">Use It Sparingly</a></h2><p>Mutation is useful for counters, cursors, buffers, and small accumulators. Prefer fresh names when the next value is just another step in a pipeline.</p>\n<p>Continue to <a href=\"/learn/book/core/expressions/literals\">Literals</a>.</p>\n",
		"summaryHtml": "Use mut only when changing a value helps more than rebuilding it."
	},
	{
		"locale": "en",
		"id": "literals",
		"kind": "chapter",
		"parentId": "core-expressions",
		"depth": 2,
		"treePath": [
			"core",
			"core-expressions",
			"literals"
		],
		"childIds": [],
		"partId": "core",
		"partTitle": "Core Syntax",
		"sectionId": "core-expressions",
		"sectionTitle": "Expressions",
		"path": "/learn/book/core/expressions/literals",
		"canonicalPath": "/learn/book/core/expressions/literals",
		"aliases": [
			"/docs/book/core/expressions/literals",
			"/learn/book/core/expressions/literals",
			"/docs/book/core/literals",
			"/learn/book/core/literals",
			"/docs/language/core/literals",
			"/learn/language/core/literals"
		],
		"questions": [],
		"title": "Literals",
		"description": "Meet Musi's everyday literal values before mixing them with operators.",
		"group": "Core Syntax",
		"section": "Core Syntax",
		"order": 6,
		"slug": "literals",
		"summary": "Start with numbers, strings, booleans, runes, and template text.",
		"descriptionHtml": "Meet Musi&#39;s everyday literal values before mixing them with operators.",
		"headings": [
			{
				"depth": 2,
				"id": "reading-model",
				"text": "Reading Model"
			},
			{
				"depth": 2,
				"id": "practical-rule",
				"text": "Practical Rule"
			}
		],
		"html": "<p>Literals are values written directly in source: numbers, strings, booleans, tuples, arrays, records, and unit. Move them behind names when the value has domain meaning.</p>\n<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> port </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 8080</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> label </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#032563;--shiki-dark:#9ECBFF\"> \"ready\"</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> enabled </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> .True</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">label;</span></span></code></pre></section></div><h2 id=\"reading-model\"><a href=\"#reading-model\">Reading Model</a></h2><p>Read the example from top to bottom. The first visible name gives the reader a handle, the following expressions show how values move, and the final expression shows what leaves the example.</p>\n<h2 id=\"practical-rule\"><a href=\"#practical-rule\">Practical Rule</a></h2><p>Use this form when it makes value movement clearer than copying habits from another language. Prefer the smallest form that still tells the reader where names, types, effects, and boundaries live.</p>\n<p>Continue to <a href=\"/learn/book/core/expressions/tuples-and-unit\">Tuples and Unit</a>.</p>\n",
		"summaryHtml": "Start with numbers, strings, booleans, runes, and template text."
	},
	{
		"locale": "en",
		"id": "tuples-and-unit",
		"kind": "chapter",
		"parentId": "core-expressions",
		"depth": 2,
		"treePath": [
			"core",
			"core-expressions",
			"tuples-and-unit"
		],
		"childIds": [],
		"partId": "core",
		"partTitle": "Core Syntax",
		"sectionId": "core-expressions",
		"sectionTitle": "Expressions",
		"path": "/learn/book/core/expressions/tuples-and-unit",
		"canonicalPath": "/learn/book/core/expressions/tuples-and-unit",
		"aliases": [
			"/docs/book/core/expressions/tuples-and-unit",
			"/learn/book/core/expressions/tuples-and-unit",
			"/docs/book/core/tuples-and-unit",
			"/learn/book/core/tuples-and-unit",
			"/docs/language/core/tuples-and-unit",
			"/learn/language/core/tuples-and-unit"
		],
		"questions": [],
		"title": "Tuples and Unit",
		"description": "Group values by position and recognize the empty value.",
		"group": "Core Syntax",
		"section": "Core Syntax",
		"order": 7,
		"slug": "tuples-and-unit",
		"summary": "Use tuple expressions for small positional groups and unit when no payload matters.",
		"descriptionHtml": "Group values by position and recognize the empty value.",
		"headings": [
			{
				"depth": 2,
				"id": "reading-model",
				"text": "Reading Model"
			},
			{
				"depth": 2,
				"id": "practical-rule",
				"text": "Practical Rule"
			}
		],
		"html": "<p>Tuples group values by position. Unit <code>()</code> carries no information and marks completion as a value. Use records when names would prevent mistakes.</p>\n<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> status </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> (</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">8080</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">, </span><span style=\"color:#032563;--shiki-dark:#9ECBFF\">\"ready\"</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">);</span></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> empty </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> ();</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">status;</span></span></code></pre></section></div><h2 id=\"reading-model\"><a href=\"#reading-model\">Reading Model</a></h2><p>Read the example from top to bottom. The first visible name gives the reader a handle, the following expressions show how values move, and the final expression shows what leaves the example.</p>\n<h2 id=\"practical-rule\"><a href=\"#practical-rule\">Practical Rule</a></h2><p>Use this form when it makes value movement clearer than copying habits from another language. Prefer the smallest form that still tells the reader where names, types, effects, and boundaries live.</p>\n<p>Continue to <a href=\"/learn/book/core/expressions/operators\">Operators</a>.</p>\n",
		"summaryHtml": "Use tuple expressions for small positional groups and unit when no payload matters."
	},
	{
		"locale": "en",
		"id": "operators",
		"kind": "chapter",
		"parentId": "core-expressions",
		"depth": 2,
		"treePath": [
			"core",
			"core-expressions",
			"operators"
		],
		"childIds": [],
		"partId": "core",
		"partTitle": "Core Syntax",
		"sectionId": "core-expressions",
		"sectionTitle": "Expressions",
		"path": "/learn/book/core/expressions/operators",
		"canonicalPath": "/learn/book/core/expressions/operators",
		"aliases": [
			"/docs/book/core/expressions/operators",
			"/learn/book/core/expressions/operators",
			"/docs/book/core/operators",
			"/learn/book/core/operators",
			"/docs/language/core/operators",
			"/learn/language/core/operators"
		],
		"questions": [],
		"title": "Operators",
		"description": "Add operators after literals so expressions stay readable.",
		"group": "Core Syntax",
		"section": "Core Syntax",
		"order": 7,
		"slug": "operators",
		"summary": "Read arithmetic, comparison, and logic in ordinary expressions.",
		"descriptionHtml": "Add operators after literals so expressions stay readable.",
		"headings": [
			{
				"depth": 2,
				"id": "reading-model",
				"text": "Reading Model"
			},
			{
				"depth": 2,
				"id": "practical-rule",
				"text": "Practical Rule"
			}
		],
		"html": "<p>Operators are calls written in operator position. <code>=</code> compares; <code>:=</code> binds or assigns depending on surrounding form. Use names when punctuation would hide domain meaning.</p>\n<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> port </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 8080</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> next </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> port </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">+</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 1</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> same </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> next </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">=</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 8081</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> capped </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> next </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">&#x3C;=</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 9000</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span></code></pre></section></div><h2 id=\"reading-model\"><a href=\"#reading-model\">Reading Model</a></h2><p>Read the example from top to bottom. The first visible name gives the reader a handle, the following expressions show how values move, and the final expression shows what leaves the example.</p>\n<h2 id=\"practical-rule\"><a href=\"#practical-rule\">Practical Rule</a></h2><p>Use this form when it makes value movement clearer than copying habits from another language. Prefer the smallest form that still tells the reader where names, types, effects, and boundaries live.</p>\n<p>Continue to <a href=\"/learn/book/core/expressions/ranges\">Ranges</a>.</p>\n",
		"summaryHtml": "Read arithmetic, comparison, and logic in ordinary expressions."
	},
	{
		"locale": "en",
		"id": "ranges",
		"kind": "chapter",
		"parentId": "core-expressions",
		"depth": 2,
		"treePath": [
			"core",
			"core-expressions",
			"ranges"
		],
		"childIds": [],
		"partId": "core",
		"partTitle": "Core Syntax",
		"sectionId": "core-expressions",
		"sectionTitle": "Expressions",
		"path": "/learn/book/core/expressions/ranges",
		"canonicalPath": "/learn/book/core/expressions/ranges",
		"aliases": [
			"/docs/book/core/expressions/ranges",
			"/learn/book/core/expressions/ranges",
			"/docs/book/core/ranges",
			"/learn/book/core/ranges",
			"/docs/language/core/ranges",
			"/learn/language/core/ranges"
		],
		"questions": [],
		"title": "Ranges",
		"description": "Learn Musi's range operators in isolation before they appear inside larger code.",
		"group": "Core Syntax",
		"section": "Core Syntax",
		"order": 8,
		"slug": "ranges",
		"summary": "Read open, closed, and spread-like range forms without guessing.",
		"descriptionHtml": "Learn Musi&#39;s range operators in isolation before they appear inside larger code.",
		"headings": [
			{
				"depth": 2,
				"id": "reading-model",
				"text": "Reading Model"
			},
			{
				"depth": 2,
				"id": "practical-rule",
				"text": "Practical Rule"
			}
		],
		"html": "<p>Ranges describe ordered spans of values. Use them for boundaries, slices, repeated numeric spans, and membership checks.</p>\n<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> closed </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 0</span><span style=\"color:#A0111F;--shiki-dark:#F97583\">..</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">10</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> halfOpen </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 0</span><span style=\"color:#A0111F;--shiki-dark:#F97583\">..&#x3C;</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">10</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">halfOpen;</span></span></code></pre></section></div><h2 id=\"reading-model\"><a href=\"#reading-model\">Reading Model</a></h2><p>Read the example from top to bottom. The first visible name gives the reader a handle, the following expressions show how values move, and the final expression shows what leaves the example.</p>\n<h2 id=\"practical-rule\"><a href=\"#practical-rule\">Practical Rule</a></h2><p>Use this form when it makes value movement clearer than copying habits from another language. Prefer the smallest form that still tells the reader where names, types, effects, and boundaries live.</p>\n<p>Continue to <a href=\"/learn/book/core/functions-and-calls/functions\">Functions</a>.</p>\n",
		"summaryHtml": "Read open, closed, and spread-like range forms without guessing."
	},
	{
		"locale": "en",
		"id": "functions",
		"kind": "chapter",
		"parentId": "core-functions",
		"depth": 2,
		"treePath": [
			"core",
			"core-functions",
			"functions"
		],
		"childIds": [],
		"partId": "core",
		"partTitle": "Core Syntax",
		"sectionId": "core-functions",
		"sectionTitle": "Functions and Calls",
		"path": "/learn/book/core/functions-and-calls/functions",
		"canonicalPath": "/learn/book/core/functions-and-calls/functions",
		"aliases": [
			"/docs/book/core/functions-and-calls/functions",
			"/learn/book/core/functions-and-calls/functions",
			"/docs/book/core/functions",
			"/learn/book/core/functions",
			"/docs/language/core/functions",
			"/learn/language/core/functions"
		],
		"questions": [],
		"title": "Functions",
		"description": "Define reusable functions, learn named arguments, and understand why parameter names matter.",
		"group": "Core Syntax",
		"section": "Core Syntax",
		"order": 9,
		"slug": "functions",
		"summary": "Functions are ordinary `let` bindings with parameters, result types, and expression bodies.",
		"descriptionHtml": "Define reusable functions, learn named arguments, and understand why parameter names matter.",
		"headings": [
			{
				"depth": 2,
				"id": "reading-model",
				"text": "Reading Model"
			},
			{
				"depth": 2,
				"id": "practical-rule",
				"text": "Practical Rule"
			}
		],
		"html": "<p>A function is a named calculation. It receives parameters, uses them in a body, and produces the final expression. Named arguments keep calls readable when values could be confused.</p>\n<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> render</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> (port : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">, secure : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">Bool</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">) : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> :=</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> port;</span></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> positional </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> render</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">8080</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">, </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">0</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> =</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 0</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">);</span></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> labeled </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> render</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(secure </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 0</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> =</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 0</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">, port </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 8080</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">);</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">labeled;</span></span></code></pre></section></div><div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> render</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> (port : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">, secure : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">Bool</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">) : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> :=</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> port;</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> f </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> render;</span></span>\n<span class=\"line\"><span style=\"color:#622CBC;--shiki-dark:#B392F0\">f</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(secure </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 0</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> =</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 0</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">, port </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 8080</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">);</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> g : (</span><span style=\"color:#702C00;--shiki-dark:#B392F0\">Int</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">, </span><span style=\"color:#702C00;--shiki-dark:#B392F0\">Bool</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">) </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">-></span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> Int</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> :=</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> render;</span></span>\n<span class=\"line\"><span style=\"color:#622CBC;--shiki-dark:#B392F0\">g</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">8080</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">, </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">0</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> =</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 0</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">);</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> h : (host : </span><span style=\"color:#702C00;--shiki-dark:#B392F0\">Int</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">, tls : </span><span style=\"color:#702C00;--shiki-dark:#B392F0\">Bool</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">) </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">-></span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> Int</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> :=</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> render;</span></span>\n<span class=\"line\"><span style=\"color:#622CBC;--shiki-dark:#B392F0\">h</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(host </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 8080</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">, tls </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 0</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> =</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 0</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">);</span></span></code></pre></section></div><h2 id=\"reading-model\"><a href=\"#reading-model\">Reading Model</a></h2><p>Read the example from top to bottom. The first visible name gives the reader a handle, the following expressions show how values move, and the final expression shows what leaves the example.</p>\n<h2 id=\"practical-rule\"><a href=\"#practical-rule\">Practical Rule</a></h2><p>Use this form when it makes value movement clearer than copying habits from another language. Prefer the smallest form that still tells the reader where names, types, effects, and boundaries live.</p>\n<p>Continue to <a href=\"/learn/book/core/functions-and-calls/lambdas\">Lambdas</a>.</p>\n",
		"summaryHtml": "Functions are ordinary <code>let</code> bindings with parameters, result types, and expression bodies."
	},
	{
		"locale": "en",
		"id": "lambdas",
		"kind": "chapter",
		"parentId": "core-functions",
		"depth": 2,
		"treePath": [
			"core",
			"core-functions",
			"lambdas"
		],
		"childIds": [],
		"partId": "core",
		"partTitle": "Core Syntax",
		"sectionId": "core-functions",
		"sectionTitle": "Functions and Calls",
		"path": "/learn/book/core/functions-and-calls/lambdas",
		"canonicalPath": "/learn/book/core/functions-and-calls/lambdas",
		"aliases": [
			"/docs/book/core/functions-and-calls/lambdas",
			"/learn/book/core/functions-and-calls/lambdas",
			"/docs/book/core/lambdas",
			"/learn/book/core/lambdas",
			"/docs/language/core/lambdas",
			"/learn/language/core/lambdas"
		],
		"questions": [],
		"title": "Lambdas",
		"description": "Write a small function value directly inside an expression.",
		"group": "Core Syntax",
		"section": "Core Syntax",
		"order": 12,
		"slug": "lambdas",
		"summary": "Use lambda expressions when a short function value reads better in place.",
		"descriptionHtml": "Write a small function value directly inside an expression.",
		"headings": [
			{
				"depth": 2,
				"id": "reading-model",
				"text": "Reading Model"
			},
			{
				"depth": 2,
				"id": "practical-rule",
				"text": "Practical Rule"
			}
		],
		"html": "<p>A lambda is an unnamed function value. Use it when the calculation is small and travels directly to another value or helper.</p>\n<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> twice </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> \\(x : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">) : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> =></span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> x </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">+</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> x;</span></span>\n<span class=\"line\"><span style=\"color:#622CBC;--shiki-dark:#B392F0\">twice</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">21</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">);</span></span></code></pre></section></div><h2 id=\"reading-model\"><a href=\"#reading-model\">Reading Model</a></h2><p>Read the example from top to bottom. The first visible name gives the reader a handle, the following expressions show how values move, and the final expression shows what leaves the example.</p>\n<h2 id=\"practical-rule\"><a href=\"#practical-rule\">Practical Rule</a></h2><p>Use this form when it makes value movement clearer than copying habits from another language. Prefer the smallest form that still tells the reader where names, types, effects, and boundaries live.</p>\n<p>Continue to <a href=\"/learn/book/core/functions-and-calls/calls\">Calls</a>.</p>\n",
		"summaryHtml": "Use lambda expressions when a short function value reads better in place."
	},
	{
		"locale": "en",
		"id": "calls",
		"kind": "chapter",
		"parentId": "core-functions",
		"depth": 2,
		"treePath": [
			"core",
			"core-functions",
			"calls"
		],
		"childIds": [],
		"partId": "core",
		"partTitle": "Core Syntax",
		"sectionId": "core-functions",
		"sectionTitle": "Functions and Calls",
		"path": "/learn/book/core/functions-and-calls/calls",
		"canonicalPath": "/learn/book/core/functions-and-calls/calls",
		"aliases": [
			"/docs/book/core/functions-and-calls/calls",
			"/learn/book/core/functions-and-calls/calls",
			"/docs/book/core/calls",
			"/learn/book/core/calls",
			"/docs/language/core/calls",
			"/learn/language/core/calls"
		],
		"questions": [],
		"title": "Calls",
		"description": "Learn call syntax as its own reading skill before adding dot calls.",
		"group": "Core Syntax",
		"section": "Core Syntax",
		"order": 10,
		"slug": "calls",
		"summary": "Call functions directly and follow argument flow left to right.",
		"descriptionHtml": "Learn call syntax as its own reading skill before adding dot calls.",
		"headings": [
			{
				"depth": 2,
				"id": "reading-model",
				"text": "Reading Model"
			},
			{
				"depth": 2,
				"id": "practical-rule",
				"text": "Practical Rule"
			}
		],
		"html": "<p>A call applies a callable value to arguments. Positional calls are compact; named calls are clearer when labels carry meaning.</p>\n<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> greet</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> (name : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">String</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">) : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">String</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> :=</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> name;</span></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> message </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> greet</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(</span><span style=\"color:#032563;--shiki-dark:#9ECBFF\">\"Musi\"</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">);</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">message;</span></span></code></pre></section></div><div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> render</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> (port : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">, secure : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">Bool</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">) : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> :=</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> port;</span></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> positional </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> render</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">8080</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">, </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">0</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> =</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 0</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">);</span></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> labeled </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> render</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(secure </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 0</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> =</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 0</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">, port </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 8080</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">);</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">labeled;</span></span></code></pre></section></div><h2 id=\"reading-model\"><a href=\"#reading-model\">Reading Model</a></h2><p>Read the example from top to bottom. The first visible name gives the reader a handle, the following expressions show how values move, and the final expression shows what leaves the example.</p>\n<h2 id=\"practical-rule\"><a href=\"#practical-rule\">Practical Rule</a></h2><p>Use this form when it makes value movement clearer than copying habits from another language. Prefer the smallest form that still tells the reader where names, types, effects, and boundaries live.</p>\n<p>Continue to <a href=\"/learn/book/core/functions-and-calls/methods\">Methods</a>.</p>\n",
		"summaryHtml": "Call functions directly and follow argument flow left to right."
	},
	{
		"locale": "en",
		"id": "methods",
		"kind": "chapter",
		"parentId": "core-functions",
		"depth": 2,
		"treePath": [
			"core",
			"core-functions",
			"methods"
		],
		"childIds": [],
		"partId": "core",
		"partTitle": "Core Syntax",
		"sectionId": "core-functions",
		"sectionTitle": "Functions and Calls",
		"path": "/learn/book/core/functions-and-calls/methods",
		"canonicalPath": "/learn/book/core/functions-and-calls/methods",
		"aliases": [
			"/docs/book/core/functions-and-calls/methods",
			"/learn/book/core/functions-and-calls/methods",
			"/docs/book/core/methods",
			"/learn/book/core/methods",
			"/docs/language/core/methods",
			"/learn/language/core/methods"
		],
		"questions": [],
		"title": "Methods",
		"description": "Learn Musi's attached-method model after plain functions and calls.",
		"group": "Core Syntax",
		"section": "Core Syntax",
		"order": 11,
		"slug": "methods",
		"summary": "Use receiver-prefixed methods and dot calls without needing an impl block.",
		"descriptionHtml": "Learn Musi&#39;s attached-method model after plain functions and calls.",
		"headings": [
			{
				"depth": 2,
				"id": "reading-model",
				"text": "Reading Model"
			},
			{
				"depth": 2,
				"id": "practical-rule",
				"text": "Practical Rule"
			}
		],
		"html": "<p>Methods are functions attached to a receiver shape. The receiver comes before the dot, which makes the main subject visible first.</p>\n<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> (</span><span style=\"color:#702C00;--shiki-dark:#FFAB70\">self</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> : Int).</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\">abs</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> () : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> :=</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> self;</span></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> one </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 1</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">one.</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\">abs</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">();</span></span></code></pre></section></div><h2 id=\"reading-model\"><a href=\"#reading-model\">Reading Model</a></h2><p>Read the example from top to bottom. The first visible name gives the reader a handle, the following expressions show how values move, and the final expression shows what leaves the example.</p>\n<h2 id=\"practical-rule\"><a href=\"#practical-rule\">Practical Rule</a></h2><p>Use this form when it makes value movement clearer than copying habits from another language. Prefer the smallest form that still tells the reader where names, types, effects, and boundaries live.</p>\n<p>Continue to <a href=\"/learn/book/data/modeling/records\">Records</a>.</p>\n",
		"summaryHtml": "Use receiver-prefixed methods and dot calls without needing an impl block."
	},
	{
		"locale": "en",
		"id": "records",
		"kind": "chapter",
		"parentId": "data-modeling",
		"depth": 2,
		"treePath": [
			"data",
			"data-modeling",
			"records"
		],
		"childIds": [],
		"partId": "data",
		"partTitle": "Data",
		"sectionId": "data-modeling",
		"sectionTitle": "Data Modeling",
		"path": "/learn/book/data/modeling/records",
		"canonicalPath": "/learn/book/data/modeling/records",
		"aliases": [
			"/docs/book/data/modeling/records",
			"/learn/book/data/modeling/records",
			"/docs/book/data/records",
			"/learn/book/data/records",
			"/docs/language/data/records",
			"/learn/language/data/records"
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
				"id": "reading-model",
				"text": "Reading Model"
			},
			{
				"depth": 2,
				"id": "practical-rule",
				"text": "Practical Rule"
			}
		],
		"html": "<p>Records group named fields. Use them when names matter more than position. Spread updates start from an existing record and replace the named fields shown.</p>\n<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> point </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> { x </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 3</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">, y </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 4</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> };</span></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> moved </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> { </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">...</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">point, y </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 9</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> };</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">moved;</span></span></code></pre></section></div><h2 id=\"reading-model\"><a href=\"#reading-model\">Reading Model</a></h2><p>Read the example from top to bottom. The first visible name gives the reader a handle, the following expressions show how values move, and the final expression shows what leaves the example.</p>\n<h2 id=\"practical-rule\"><a href=\"#practical-rule\">Practical Rule</a></h2><p>Use this form when it makes value movement clearer than copying habits from another language. Prefer the smallest form that still tells the reader where names, types, effects, and boundaries live.</p>\n<p>Continue to <a href=\"/learn/book/data/modeling/indexing-and-fields\">Indexing and Fields</a>.</p>\n",
		"summaryHtml": "Build named-field values and access fields directly."
	},
	{
		"locale": "en",
		"id": "indexing-and-fields",
		"kind": "chapter",
		"parentId": "data-modeling",
		"depth": 2,
		"treePath": [
			"data",
			"data-modeling",
			"indexing-and-fields"
		],
		"childIds": [],
		"partId": "data",
		"partTitle": "Data",
		"sectionId": "data-modeling",
		"sectionTitle": "Data Modeling",
		"path": "/learn/book/data/modeling/indexing-and-fields",
		"canonicalPath": "/learn/book/data/modeling/indexing-and-fields",
		"aliases": [
			"/docs/book/data/modeling/indexing-and-fields",
			"/learn/book/data/modeling/indexing-and-fields",
			"/docs/book/data/indexing-and-fields",
			"/learn/book/data/indexing-and-fields",
			"/docs/language/data/indexing-and-fields",
			"/learn/language/data/indexing-and-fields"
		],
		"questions": [],
		"title": "Indexing and Fields",
		"description": "Read field access and indexed access without mixing up their roles.",
		"group": "Data",
		"section": "Data",
		"order": 14,
		"slug": "indexing-and-fields",
		"summary": "Use `.field` for named data and `.[index]` for positional access.",
		"descriptionHtml": "Read field access and indexed access without mixing up their roles.",
		"headings": [
			{
				"depth": 2,
				"id": "reading-model",
				"text": "Reading Model"
			},
			{
				"depth": 2,
				"id": "practical-rule",
				"text": "Practical Rule"
			}
		],
		"html": "<p>Field access reads named record data. Indexing reads position-based data. Use the form that matches how a reader should find the value.</p>\n<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> point </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> { x </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 3</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">, y </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 4</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> };</span></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> values </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> [</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">10</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">, </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">20</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">, </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">30</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">];</span></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> x </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> point.x;</span></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> first </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> values.[</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">0</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">];</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">x </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">+</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> first;</span></span></code></pre></section></div><h2 id=\"reading-model\"><a href=\"#reading-model\">Reading Model</a></h2><p>Read the example from top to bottom. The first visible name gives the reader a handle, the following expressions show how values move, and the final expression shows what leaves the example.</p>\n<h2 id=\"practical-rule\"><a href=\"#practical-rule\">Practical Rule</a></h2><p>Use this form when it makes value movement clearer than copying habits from another language. Prefer the smallest form that still tells the reader where names, types, effects, and boundaries live.</p>\n<p>Continue to <a href=\"/learn/book/data/modeling/data-definitions\">Data Definitions</a>.</p>\n",
		"summaryHtml": "Use <code>.field</code> for named data and <code>.[index]</code> for positional access."
	},
	{
		"locale": "en",
		"id": "data-definitions",
		"kind": "chapter",
		"parentId": "data-modeling",
		"depth": 2,
		"treePath": [
			"data",
			"data-modeling",
			"data-definitions"
		],
		"childIds": [],
		"partId": "data",
		"partTitle": "Data",
		"sectionId": "data-modeling",
		"sectionTitle": "Data Modeling",
		"path": "/learn/book/data/modeling/data-definitions",
		"canonicalPath": "/learn/book/data/modeling/data-definitions",
		"aliases": [
			"/docs/book/data/modeling/data-definitions",
			"/learn/book/data/modeling/data-definitions",
			"/docs/book/data/data-definitions",
			"/learn/book/data/data-definitions",
			"/docs/language/data/data-definitions",
			"/learn/language/data/data-definitions"
		],
		"questions": [],
		"title": "Data Definitions",
		"description": "Define record-shaped and variant-shaped data with payloads and defaults.",
		"group": "Data",
		"section": "Data",
		"order": 15,
		"slug": "data-definitions",
		"summary": "Use `data` for named shapes, variant choices, payload fields, and record defaults.",
		"descriptionHtml": "Define record-shaped and variant-shaped data with payloads and defaults.",
		"headings": [
			{
				"depth": 2,
				"id": "reading-model",
				"text": "Reading Model"
			},
			{
				"depth": 2,
				"id": "practical-rule",
				"text": "Practical Rule"
			}
		],
		"html": "<p><code>data</code> gives a shape a name. It can describe record-shaped data with fields or variant-shaped data with choices and named payloads.</p>\n<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> Port</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> :=</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> data</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> {</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">  | </span><span style=\"color:#702C00;--shiki-dark:#B392F0\">Configured</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(</span><span style=\"color:#702C00;--shiki-dark:#FFAB70\">port</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> : </span><span style=\"color:#702C00;--shiki-dark:#B392F0\">Int</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">, </span><span style=\"color:#702C00;--shiki-dark:#FFAB70\">secure</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> : </span><span style=\"color:#702C00;--shiki-dark:#B392F0\">Bool</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">)</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">  | </span><span style=\"color:#702C00;--shiki-dark:#B392F0\">Default</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">};</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> Settings</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> :=</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> data</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> {</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">  port : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> :=</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 3000</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">  label : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">String</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">};</span></span></code></pre></section></div><h2 id=\"reading-model\"><a href=\"#reading-model\">Reading Model</a></h2><p>Read the example from top to bottom. The first visible name gives the reader a handle, the following expressions show how values move, and the final expression shows what leaves the example.</p>\n<h2 id=\"practical-rule\"><a href=\"#practical-rule\">Practical Rule</a></h2><p>Use this form when it makes value movement clearer than copying habits from another language. Prefer the smallest form that still tells the reader where names, types, effects, and boundaries live.</p>\n<p>Continue to <a href=\"/learn/book/data/modeling/arrays-and-slices\">Arrays and Slices</a>.</p>\n",
		"summaryHtml": "Use <code>data</code> for named shapes, variant choices, payload fields, and record defaults."
	},
	{
		"locale": "en",
		"id": "arrays-and-slices",
		"kind": "chapter",
		"parentId": "data-modeling",
		"depth": 2,
		"treePath": [
			"data",
			"data-modeling",
			"arrays-and-slices"
		],
		"childIds": [],
		"partId": "data",
		"partTitle": "Data",
		"sectionId": "data-modeling",
		"sectionTitle": "Data Modeling",
		"path": "/learn/book/data/modeling/arrays-and-slices",
		"canonicalPath": "/learn/book/data/modeling/arrays-and-slices",
		"aliases": [
			"/docs/book/data/modeling/arrays-and-slices",
			"/learn/book/data/modeling/arrays-and-slices",
			"/docs/book/data/arrays-and-slices",
			"/learn/book/data/arrays-and-slices",
			"/docs/language/data/arrays-and-slices",
			"/learn/language/data/arrays-and-slices"
		],
		"questions": [],
		"title": "Arrays and Slices",
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
				"id": "reading-model",
				"text": "Reading Model"
			},
			{
				"depth": 2,
				"id": "practical-rule",
				"text": "Practical Rule"
			}
		],
		"html": "<p>Arrays and slices hold ordered values. Use them when position or sequence matters more than a field name.</p>\n<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> Slice</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> :=</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> import</span><span style=\"color:#032563;--shiki-dark:#9ECBFF\"> \"@std/slice\"</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> values </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> [</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">1</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">, </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">2</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">, </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">3</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">];</span></span>\n<span class=\"line\"><span style=\"color:#023B95;--shiki-dark:#79B8FF\">Slice</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">.</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\">concat</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#702C00;--shiki-dark:#B392F0\">Int</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">](values, [</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">4</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">]);</span></span></code></pre></section></div><h2 id=\"reading-model\"><a href=\"#reading-model\">Reading Model</a></h2><p>Read the example from top to bottom. The first visible name gives the reader a handle, the following expressions show how values move, and the final expression shows what leaves the example.</p>\n<h2 id=\"practical-rule\"><a href=\"#practical-rule\">Practical Rule</a></h2><p>Use this form when it makes value movement clearer than copying habits from another language. Prefer the smallest form that still tells the reader where names, types, effects, and boundaries live.</p>\n<p>Continue to <a href=\"/learn/book/data/modeling/patterns\">Patterns</a>.</p>\n",
		"summaryHtml": "Store ordered values and learn where slices fit."
	},
	{
		"locale": "en",
		"id": "patterns",
		"kind": "chapter",
		"parentId": "data-modeling",
		"depth": 2,
		"treePath": [
			"data",
			"data-modeling",
			"patterns"
		],
		"childIds": [],
		"partId": "data",
		"partTitle": "Data",
		"sectionId": "data-modeling",
		"sectionTitle": "Data Modeling",
		"path": "/learn/book/data/modeling/patterns",
		"canonicalPath": "/learn/book/data/modeling/patterns",
		"aliases": [
			"/docs/book/data/modeling/patterns",
			"/learn/book/data/modeling/patterns",
			"/docs/book/data/patterns",
			"/learn/book/data/patterns",
			"/docs/language/data/patterns",
			"/learn/language/data/patterns"
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
				"id": "reading-model",
				"text": "Reading Model"
			},
			{
				"depth": 2,
				"id": "practical-rule",
				"text": "Practical Rule"
			}
		],
		"html": "<p>Patterns read data by shape. They are the other half of data definitions: define a shape first, then match on that shape later.</p>\n<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> Port</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> :=</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> data</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> {</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">  | </span><span style=\"color:#702C00;--shiki-dark:#B392F0\">Configured</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(</span><span style=\"color:#702C00;--shiki-dark:#FFAB70\">port</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> : </span><span style=\"color:#702C00;--shiki-dark:#B392F0\">Int</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">)</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">  | </span><span style=\"color:#702C00;--shiki-dark:#B392F0\">Default</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">};</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> port : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">Port</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> :=</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> .Configured</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(port </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 8080</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">);</span></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">match</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> port</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> (</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">| </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">.Configured</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(value) </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">=></span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> value</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">| </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">.Default</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> =></span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 3000</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">);</span></span></code></pre></section></div><h2 id=\"reading-model\"><a href=\"#reading-model\">Reading Model</a></h2><p>Read the example from top to bottom. The first visible name gives the reader a handle, the following expressions show how values move, and the final expression shows what leaves the example.</p>\n<h2 id=\"practical-rule\"><a href=\"#practical-rule\">Practical Rule</a></h2><p>Use this form when it makes value movement clearer than copying habits from another language. Prefer the smallest form that still tells the reader where names, types, effects, and boundaries live.</p>\n<p>Continue to <a href=\"/learn/book/organization/modules/files\">Files</a>.</p>\n",
		"summaryHtml": "Use match and destructuring to branch on data shape."
	},
	{
		"locale": "en",
		"id": "files",
		"kind": "chapter",
		"parentId": "organization-modules",
		"depth": 2,
		"treePath": [
			"organization",
			"organization-modules",
			"files"
		],
		"childIds": [],
		"partId": "organization",
		"partTitle": "Code Organization",
		"sectionId": "organization-modules",
		"sectionTitle": "Files and Modules",
		"path": "/learn/book/organization/modules/files",
		"canonicalPath": "/learn/book/organization/modules/files",
		"aliases": [
			"/docs/book/organization/modules/files",
			"/learn/book/organization/modules/files",
			"/docs/book/organization/files",
			"/learn/book/organization/files",
			"/docs/language/organization/files",
			"/learn/language/organization/files"
		],
		"questions": [],
		"title": "Files",
		"description": "Separate file reading from package structure to reduce beginner overload.",
		"group": "Code Organization",
		"section": "Code Organization",
		"order": 15,
		"slug": "files",
		"summary": "Know what a single file means before building a package.",
		"descriptionHtml": "Separate file reading from package structure to reduce beginner overload.",
		"headings": [
			{
				"depth": 2,
				"id": "reading-model",
				"text": "Reading Model"
			},
			{
				"depth": 2,
				"id": "practical-rule",
				"text": "Practical Rule"
			}
		],
		"html": "<p>A file is the first module boundary most readers see. It gives names a place to live and gives imports something concrete to point at.</p>\n<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> answer </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 42</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">answer;</span></span></code></pre></section></div><h2 id=\"reading-model\"><a href=\"#reading-model\">Reading Model</a></h2><p>Read the example from top to bottom. The first visible name gives the reader a handle, the following expressions show how values move, and the final expression shows what leaves the example.</p>\n<h2 id=\"practical-rule\"><a href=\"#practical-rule\">Practical Rule</a></h2><p>Use this form when it makes value movement clearer than copying habits from another language. Prefer the smallest form that still tells the reader where names, types, effects, and boundaries live.</p>\n<p>Continue to <a href=\"/learn/book/organization/modules/packages\">Packages</a>.</p>\n",
		"summaryHtml": "Know what a single file means before building a package."
	},
	{
		"locale": "en",
		"id": "packages",
		"kind": "chapter",
		"parentId": "organization-modules",
		"depth": 2,
		"treePath": [
			"organization",
			"organization-modules",
			"packages"
		],
		"childIds": [],
		"partId": "organization",
		"partTitle": "Code Organization",
		"sectionId": "organization-modules",
		"sectionTitle": "Files and Modules",
		"path": "/learn/book/organization/modules/packages",
		"canonicalPath": "/learn/book/organization/modules/packages",
		"aliases": [
			"/docs/book/organization/modules/packages",
			"/learn/book/organization/modules/packages",
			"/docs/book/organization/packages",
			"/learn/book/organization/packages",
			"/docs/language/organization/packages",
			"/learn/language/organization/packages"
		],
		"questions": [],
		"title": "Packages",
		"description": "Learn package roots and entry files after single-file work makes sense.",
		"group": "Code Organization",
		"section": "Code Organization",
		"order": 16,
		"slug": "packages",
		"summary": "Move from one file to package-managed code without changing mental models.",
		"descriptionHtml": "Learn package roots and entry files after single-file work makes sense.",
		"headings": [
			{
				"depth": 2,
				"id": "reading-model",
				"text": "Reading Model"
			},
			{
				"depth": 2,
				"id": "practical-rule",
				"text": "Practical Rule"
			}
		],
		"html": "<p>A package is a project boundary. It gives source files a root, gives tools a manifest, and gives other packages a stable name to import.</p>\n<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#702C00;--shiki-dark:#B392F0\">musi</span><span style=\"color:#032563;--shiki-dark:#9ECBFF\"> new</span><span style=\"color:#032563;--shiki-dark:#9ECBFF\"> hello</span></span>\n<span class=\"line\"><span style=\"color:#023B95;--shiki-dark:#79B8FF\">cd</span><span style=\"color:#032563;--shiki-dark:#9ECBFF\"> hello</span></span>\n<span class=\"line\"><span style=\"color:#702C00;--shiki-dark:#B392F0\">musi</span><span style=\"color:#032563;--shiki-dark:#9ECBFF\"> run</span></span></code></pre></section></div><h2 id=\"reading-model\"><a href=\"#reading-model\">Reading Model</a></h2><p>Read the example from top to bottom. The first visible name gives the reader a handle, the following expressions show how values move, and the final expression shows what leaves the example.</p>\n<h2 id=\"practical-rule\"><a href=\"#practical-rule\">Practical Rule</a></h2><p>Use this form when it makes value movement clearer than copying habits from another language. Prefer the smallest form that still tells the reader where names, types, effects, and boundaries live.</p>\n<p>Continue to <a href=\"/learn/book/organization/modules/imports-and-exports\">Imports and Exports</a>.</p>\n",
		"summaryHtml": "Move from one file to package-managed code without changing mental models."
	},
	{
		"locale": "en",
		"id": "imports-and-exports",
		"kind": "chapter",
		"parentId": "organization-modules",
		"depth": 2,
		"treePath": [
			"organization",
			"organization-modules",
			"imports-and-exports"
		],
		"childIds": [],
		"partId": "organization",
		"partTitle": "Code Organization",
		"sectionId": "organization-modules",
		"sectionTitle": "Files and Modules",
		"path": "/learn/book/organization/modules/imports-and-exports",
		"canonicalPath": "/learn/book/organization/modules/imports-and-exports",
		"aliases": [
			"/docs/book/organization/modules/imports-and-exports",
			"/learn/book/organization/modules/imports-and-exports",
			"/docs/book/organization/imports-and-exports",
			"/learn/book/organization/imports-and-exports",
			"/docs/language/organization/imports-and-exports",
			"/learn/language/organization/imports-and-exports"
		],
		"questions": [],
		"title": "Imports and Exports",
		"description": "Use imports and exports after package shape is clear.",
		"group": "Code Organization",
		"section": "Code Organization",
		"order": 17,
		"slug": "imports-and-exports",
		"summary": "Bring code in explicitly and expose only what other files need.",
		"descriptionHtml": "Use imports and exports after package shape is clear.",
		"headings": [
			{
				"depth": 2,
				"id": "reading-model",
				"text": "Reading Model"
			},
			{
				"depth": 2,
				"id": "practical-rule",
				"text": "Practical Rule"
			}
		],
		"html": "<p>Imports bring another module exported names into reach. Exports decide which names leave the current module.</p>\n<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">export</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> answer </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 42</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> Option</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> :=</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> import</span><span style=\"color:#032563;--shiki-dark:#9ECBFF\"> \"@std/option\"</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> Local</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> :=</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> import</span><span style=\"color:#032563;--shiki-dark:#9ECBFF\"> \"./index.ms\"</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#023B95;--shiki-dark:#79B8FF\">Local</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">.answer;</span></span></code></pre></section></div><h2 id=\"reading-model\"><a href=\"#reading-model\">Reading Model</a></h2><p>Read the example from top to bottom. The first visible name gives the reader a handle, the following expressions show how values move, and the final expression shows what leaves the example.</p>\n<h2 id=\"practical-rule\"><a href=\"#practical-rule\">Practical Rule</a></h2><p>Use this form when it makes value movement clearer than copying habits from another language. Prefer the smallest form that still tells the reader where names, types, effects, and boundaries live.</p>\n<p>Continue to <a href=\"/learn/book/types/foundations/type-annotations\">Type Annotations</a>.</p>\n",
		"summaryHtml": "Bring code in explicitly and expose only what other files need."
	},
	{
		"locale": "en",
		"id": "type-annotations",
		"kind": "chapter",
		"parentId": "types-foundations",
		"depth": 2,
		"treePath": [
			"types",
			"types-foundations",
			"type-annotations"
		],
		"childIds": [],
		"partId": "types",
		"partTitle": "Types",
		"sectionId": "types-foundations",
		"sectionTitle": "Type Foundations",
		"path": "/learn/book/types/foundations/type-annotations",
		"canonicalPath": "/learn/book/types/foundations/type-annotations",
		"aliases": [
			"/docs/book/types/foundations/type-annotations",
			"/learn/book/types/foundations/type-annotations",
			"/docs/book/types/type-annotations",
			"/learn/book/types/type-annotations",
			"/docs/language/types/type-annotations",
			"/learn/language/types/type-annotations"
		],
		"questions": [],
		"title": "Type Annotations",
		"description": "Use `:` for annotations, `where` for constraints, and constructor-style payloads for variants without mixing them up.",
		"group": "Types",
		"section": "Types",
		"order": 17,
		"slug": "type-annotations",
		"summary": "Learn the visible boundary markers for values, constraints, callable types, and data variants.",
		"descriptionHtml": "Use <code>:</code> for annotations, <code>where</code> for constraints, and constructor-style payloads for variants without mixing them up.",
		"headings": [
			{
				"depth": 2,
				"id": "reading-model",
				"text": "Reading Model"
			},
			{
				"depth": 2,
				"id": "practical-rule",
				"text": "Practical Rule"
			}
		],
		"html": "<p><code>:</code> marks a type relationship, but the surrounding form tells you which relationship you are reading: value annotation, parameter annotation, constraint, or payload field.</p>\n<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> port : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> :=</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 8080</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> twice</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> (x : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">) : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> :=</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> x </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">+</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> x;</span></span>\n<span class=\"line\"><span style=\"color:#622CBC;--shiki-dark:#B392F0\">twice</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(port);</span></span></code></pre></section></div><h2 id=\"reading-model\"><a href=\"#reading-model\">Reading Model</a></h2><p>Read the example from top to bottom. The first visible name gives the reader a handle, the following expressions show how values move, and the final expression shows what leaves the example.</p>\n<h2 id=\"practical-rule\"><a href=\"#practical-rule\">Practical Rule</a></h2><p>Use this form when it makes value movement clearer than copying habits from another language. Prefer the smallest form that still tells the reader where names, types, effects, and boundaries live.</p>\n<p>Continue to <a href=\"/learn/book/types/foundations/type-inference\">Type Inference</a>.</p>\n",
		"summaryHtml": "Learn the visible boundary markers for values, constraints, callable types, and data variants."
	},
	{
		"locale": "en",
		"id": "callable-types",
		"kind": "chapter",
		"parentId": "types-foundations",
		"depth": 2,
		"treePath": [
			"types",
			"types-foundations",
			"callable-types"
		],
		"childIds": [],
		"partId": "types",
		"partTitle": "Types",
		"sectionId": "types-foundations",
		"sectionTitle": "Type Foundations",
		"path": "/learn/book/types/foundations/callable-types",
		"canonicalPath": "/learn/book/types/foundations/callable-types",
		"aliases": [
			"/docs/book/types/foundations/callable-types",
			"/learn/book/types/foundations/callable-types",
			"/docs/book/types/callable-types",
			"/learn/book/types/callable-types",
			"/docs/language/types/callable-types",
			"/learn/language/types/callable-types"
		],
		"questions": [],
		"title": "Callable Types",
		"description": "Read pure and effectful function types as part of ordinary type syntax.",
		"group": "Types",
		"section": "Types",
		"order": 18,
		"slug": "callable-types",
		"summary": "Use `T -> U` for pure callables and `T ~> U` for effectful callables.",
		"descriptionHtml": "Read pure and effectful function types as part of ordinary type syntax.",
		"headings": [
			{
				"depth": 2,
				"id": "reading-model",
				"text": "Reading Model"
			},
			{
				"depth": 2,
				"id": "practical-rule",
				"text": "Practical Rule"
			}
		],
		"html": "<p>Callable types describe functions as values. <code>Int -&gt; Int</code> is pure shape; <code>Int ~&gt; Int</code> is effectful callable shape.</p>\n<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> Pure</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> :=</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> Int</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> -></span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> Int</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> Effectful</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> :=</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> Int</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> ~></span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> Int</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#023B95;--shiki-dark:#79B8FF\">Pure</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span></code></pre></section></div><h2 id=\"reading-model\"><a href=\"#reading-model\">Reading Model</a></h2><p>Read the example from top to bottom. The first visible name gives the reader a handle, the following expressions show how values move, and the final expression shows what leaves the example.</p>\n<h2 id=\"practical-rule\"><a href=\"#practical-rule\">Practical Rule</a></h2><p>Use this form when it makes value movement clearer than copying habits from another language. Prefer the smallest form that still tells the reader where names, types, effects, and boundaries live.</p>\n<p>Continue to <a href=\"/learn/book/types/foundations/type-tests-and-casts\">Type Tests and Casts</a>.</p>\n",
		"summaryHtml": "Use <code>T -&gt; U</code> for pure callables and <code>T ~&gt; U</code> for effectful callables."
	},
	{
		"locale": "en",
		"id": "type-inference",
		"kind": "chapter",
		"parentId": "types-foundations",
		"depth": 2,
		"treePath": [
			"types",
			"types-foundations",
			"type-inference"
		],
		"childIds": [],
		"partId": "types",
		"partTitle": "Types",
		"sectionId": "types-foundations",
		"sectionTitle": "Type Foundations",
		"path": "/learn/book/types/foundations/type-inference",
		"canonicalPath": "/learn/book/types/foundations/type-inference",
		"aliases": [
			"/docs/book/types/foundations/type-inference",
			"/learn/book/types/foundations/type-inference",
			"/docs/book/types/type-inference",
			"/learn/book/types/type-inference",
			"/docs/language/types/type-inference",
			"/learn/language/types/type-inference"
		],
		"questions": [],
		"title": "Type Inference",
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
				"id": "reading-model",
				"text": "Reading Model"
			},
			{
				"depth": 2,
				"id": "practical-rule",
				"text": "Practical Rule"
			}
		],
		"html": "<p>Type inference lets Musi fill in types that are already clear from nearby code. Annotate public boundaries and surprising local values.</p>\n<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> port : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> :=</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 8080</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> next </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> port </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">+</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 1</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">next;</span></span></code></pre></section></div><h2 id=\"reading-model\"><a href=\"#reading-model\">Reading Model</a></h2><p>Read the example from top to bottom. The first visible name gives the reader a handle, the following expressions show how values move, and the final expression shows what leaves the example.</p>\n<h2 id=\"practical-rule\"><a href=\"#practical-rule\">Practical Rule</a></h2><p>Use this form when it makes value movement clearer than copying habits from another language. Prefer the smallest form that still tells the reader where names, types, effects, and boundaries live.</p>\n<p>Continue to <a href=\"/learn/book/types/foundations/generics\">Generics</a>.</p>\n",
		"summaryHtml": "See what Musi can infer so you know when to write less."
	},
	{
		"locale": "en",
		"id": "generics",
		"kind": "chapter",
		"parentId": "types-foundations",
		"depth": 2,
		"treePath": [
			"types",
			"types-foundations",
			"generics"
		],
		"childIds": [],
		"partId": "types",
		"partTitle": "Types",
		"sectionId": "types-foundations",
		"sectionTitle": "Type Foundations",
		"path": "/learn/book/types/foundations/generics",
		"canonicalPath": "/learn/book/types/foundations/generics",
		"aliases": [
			"/docs/book/types/foundations/generics",
			"/learn/book/types/foundations/generics",
			"/docs/book/types/generics",
			"/learn/book/types/generics",
			"/docs/language/types/generics",
			"/learn/language/types/generics"
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
		"headings": [],
		"html": "<p>Generics let one definition work for many types. Start with the direct case: <code>identityFn[T]</code> names a type parameter, and <code>identityFn[Int](8080)</code> chooses <code>Int</code> for that call.</p>\n<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> identityFn</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#702C00;--shiki-dark:#B392F0\">T</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">] (input : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">T</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">) : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">T</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> :=</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> input;</span></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> port </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> identityFn</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#702C00;--shiki-dark:#B392F0\">Int</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">](</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">8080</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">);</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> tools </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> {</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">  identity </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> identityFn</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">};</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> copiedPort </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> tools.</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\">identity</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#702C00;--shiki-dark:#B392F0\">Int</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">](port);</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> Box1</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#702C00;--shiki-dark:#B392F0\">T</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">] </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> data</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> {</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">  | </span><span style=\"color:#702C00;--shiki-dark:#B392F0\">Box1</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(</span><span style=\"color:#702C00;--shiki-dark:#FFAB70\">value</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> : </span><span style=\"color:#702C00;--shiki-dark:#B392F0\">T</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">)</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">};</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> Keeps</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#702C00;--shiki-dark:#B392F0\">F</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> : </span><span style=\"color:#702C00;--shiki-dark:#B392F0\">Type</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> -></span><span style=\"color:#702C00;--shiki-dark:#B392F0\"> Type</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">] </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> class</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> {</span></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">  let</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> keep</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(value : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">F</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#702C00;--shiki-dark:#B392F0\">Int</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">]) : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">F</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#702C00;--shiki-dark:#B392F0\">Int</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">];</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">};</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> boxKeeps </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> instance</span><span style=\"color:#702C00;--shiki-dark:#B392F0\"> Keeps</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">Box1</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">] {</span></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">  let</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> keep</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(value : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">Box1</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#702C00;--shiki-dark:#B392F0\">Int</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">]) : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">Box1</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#702C00;--shiki-dark:#B392F0\">Int</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">] </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> value;</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">};</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">copiedPort;</span></span></code></pre></section></div><p><code>tools</code> shows that a generic function can be stored in an ordinary record and called through a field. The type argument still belongs at the call site, so <code>tools.identity[Int](port)</code> reads the same way as the direct call.</p>\n<p><code>Box1</code> and <code>Keeps</code> show the larger shape. <code>Box1</code> is not a single finished type; it is a type constructor that becomes <code>Box1[Int]</code> when given <code>Int</code>. That is why <code>Keeps</code> accepts <code>F : Type -&gt; Type</code>: the class needs something that can build a concrete type from another type.</p>\n<p>Use generics when the operation is truly the same across several types. Keep the first examples concrete, then add type-constructor parameters only when the abstraction needs to talk about containers, wrappers, or other type families.</p>\n<p>Continue to <a href=\"/learn/book/types/foundations/callable-types\">Callable Types</a>.</p>\n",
		"summaryHtml": "Write reusable functions over many types without losing clarity."
	},
	{
		"locale": "en",
		"id": "type-tests-and-casts",
		"kind": "chapter",
		"parentId": "types-foundations",
		"depth": 2,
		"treePath": [
			"types",
			"types-foundations",
			"type-tests-and-casts"
		],
		"childIds": [],
		"partId": "types",
		"partTitle": "Types",
		"sectionId": "types-foundations",
		"sectionTitle": "Type Foundations",
		"path": "/learn/book/types/foundations/type-tests-and-casts",
		"canonicalPath": "/learn/book/types/foundations/type-tests-and-casts",
		"aliases": [
			"/docs/book/types/foundations/type-tests-and-casts",
			"/learn/book/types/foundations/type-tests-and-casts",
			"/docs/book/types/type-tests-and-casts",
			"/learn/book/types/type-tests-and-casts",
			"/docs/language/types/type-tests-and-casts",
			"/learn/language/types/type-tests-and-casts"
		],
		"questions": [],
		"title": "Type Tests and Casts",
		"description": "Check and narrow values with explicit type-facing expressions.",
		"group": "Types",
		"section": "Types",
		"order": 21,
		"slug": "type-tests-and-casts",
		"summary": "Use `:?` to test a value against a type and `:?>` for an explicit cast.",
		"descriptionHtml": "Check and narrow values with explicit type-facing expressions.",
		"headings": [
			{
				"depth": 2,
				"id": "reading-model",
				"text": "Reading Model"
			},
			{
				"depth": 2,
				"id": "practical-rule",
				"text": "Practical Rule"
			}
		],
		"html": "<p>Type tests ask whether a value has a shape. Casts state that code wants to treat a value as that shape and deserve attention near dynamic or native boundaries.</p>\n<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> value </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 42</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> isInt </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> value </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:?</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> Int</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> same </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> value </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:?></span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> Int</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">same;</span></span></code></pre></section></div><h2 id=\"reading-model\"><a href=\"#reading-model\">Reading Model</a></h2><p>Read the example from top to bottom. The first visible name gives the reader a handle, the following expressions show how values move, and the final expression shows what leaves the example.</p>\n<h2 id=\"practical-rule\"><a href=\"#practical-rule\">Practical Rule</a></h2><p>Use this form when it makes value movement clearer than copying habits from another language. Prefer the smallest form that still tells the reader where names, types, effects, and boundaries live.</p>\n<p>Continue to <a href=\"/learn/book/types/foundations/forall-types\">Forall Types</a>.</p>\n",
		"summaryHtml": "Use <code>:?</code> to test a value against a type and <code>:?&gt;</code> for an explicit cast."
	},
	{
		"locale": "en",
		"id": "forall-types",
		"kind": "chapter",
		"parentId": "types-foundations",
		"depth": 2,
		"treePath": [
			"types",
			"types-foundations",
			"forall-types"
		],
		"childIds": [],
		"partId": "types",
		"partTitle": "Types",
		"sectionId": "types-foundations",
		"sectionTitle": "Type Foundations",
		"path": "/learn/book/types/foundations/forall-types",
		"canonicalPath": "/learn/book/types/foundations/forall-types",
		"aliases": [
			"/docs/book/types/foundations/forall-types",
			"/learn/book/types/foundations/forall-types",
			"/docs/book/types/forall-types",
			"/learn/book/types/forall-types",
			"/docs/language/types/forall-types",
			"/learn/language/types/forall-types"
		],
		"questions": [],
		"title": "Forall Types",
		"description": "Read explicit universal type forms without turning them into everyday ceremony.",
		"group": "Types",
		"section": "Types",
		"order": 22,
		"slug": "forall-types",
		"summary": "Use `forall` when a type expression must bind a type variable explicitly.",
		"descriptionHtml": "Read explicit universal type forms without turning them into everyday ceremony.",
		"headings": [
			{
				"depth": 2,
				"id": "reading-model",
				"text": "Reading Model"
			},
			{
				"depth": 2,
				"id": "practical-rule",
				"text": "Practical Rule"
			}
		],
		"html": "<p><code>forall</code> writes polymorphism as a type. Use it when the type itself must say that one callable works for every type parameter.</p>\n<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> identityFn</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#702C00;--shiki-dark:#B392F0\">T</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">] (input : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">T</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">) : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">T</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> :=</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> input;</span></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> identityType </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> forall</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">T</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">Type</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">) </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">-></span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> T</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> -></span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> T</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#622CBC;--shiki-dark:#B392F0\">identityFn</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#702C00;--shiki-dark:#B392F0\">Int</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">](</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">8080</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">);</span></span></code></pre></section></div><h2 id=\"reading-model\"><a href=\"#reading-model\">Reading Model</a></h2><p>Read the example from top to bottom. The first visible name gives the reader a handle, the following expressions show how values move, and the final expression shows what leaves the example.</p>\n<h2 id=\"practical-rule\"><a href=\"#practical-rule\">Practical Rule</a></h2><p>Use this form when it makes value movement clearer than copying habits from another language. Prefer the smallest form that still tells the reader where names, types, effects, and boundaries live.</p>\n<p>Continue to <a href=\"/learn/book/types/foundations/dependent-types\">Dependent Types</a>.</p>\n",
		"summaryHtml": "Use <code>forall</code> when a type expression must bind a type variable explicitly."
	},
	{
		"locale": "en",
		"id": "dependent-types",
		"kind": "chapter",
		"parentId": "types-foundations",
		"depth": 2,
		"treePath": [
			"types",
			"types-foundations",
			"dependent-types"
		],
		"childIds": [],
		"partId": "types",
		"partTitle": "Types",
		"sectionId": "types-foundations",
		"sectionTitle": "Type Foundations",
		"path": "/learn/book/types/foundations/dependent-types",
		"canonicalPath": "/learn/book/types/foundations/dependent-types",
		"aliases": [
			"/docs/book/types/foundations/dependent-types",
			"/learn/book/types/foundations/dependent-types",
			"/docs/book/types/dependent-types",
			"/learn/book/types/dependent-types",
			"/docs/language/types/dependent-types",
			"/learn/language/types/dependent-types"
		],
		"questions": [],
		"title": "Dependent Types",
		"description": "Use values in type positions when shape, size, or protocol state should be visible in the type.",
		"group": "Types",
		"section": "Types",
		"order": 23,
		"slug": "dependent-types",
		"summary": "Use value-indexed types, indexed data results, `partial`, and `~=` without turning Musi into a proof assistant.",
		"descriptionHtml": "Use values in type positions when shape, size, or protocol state should be visible in the type.",
		"headings": [
			{
				"depth": 2,
				"id": "reading-model",
				"text": "Reading Model"
			},
			{
				"depth": 2,
				"id": "practical-rule",
				"text": "Practical Rule"
			}
		],
		"html": "<p>Musi supports dependent-style surface forms where type parameters can mention values, such as a length in a vector type. <code>partial</code> marks definitions that may not be total at compile time.</p>\n<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> Vec</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#702C00;--shiki-dark:#B392F0\">T</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">, n : </span><span style=\"color:#702C00;--shiki-dark:#B392F0\">Nat</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">] </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> data</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> {</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">  | </span><span style=\"color:#702C00;--shiki-dark:#B392F0\">Nil</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">() </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">-></span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> Vec</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">T</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">, </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">0</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">]</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">  | </span><span style=\"color:#702C00;--shiki-dark:#B392F0\">Cons</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(</span><span style=\"color:#702C00;--shiki-dark:#FFAB70\">head</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> : </span><span style=\"color:#702C00;--shiki-dark:#B392F0\">T</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">, </span><span style=\"color:#702C00;--shiki-dark:#FFAB70\">tail</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> : </span><span style=\"color:#702C00;--shiki-dark:#B392F0\">Vec</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#702C00;--shiki-dark:#B392F0\">T</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">, </span><span style=\"color:#702C00;--shiki-dark:#B392F0\">n</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">]) </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">-></span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> Vec</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">T</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">, n </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">+</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 1</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">]</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">};</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">partial</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> let</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> parsePort</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(text : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">String</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">) : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> :=</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 0</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span></code></pre></section></div><h2 id=\"reading-model\"><a href=\"#reading-model\">Reading Model</a></h2><p>Read the example from top to bottom. The first visible name gives the reader a handle, the following expressions show how values move, and the final expression shows what leaves the example.</p>\n<h2 id=\"practical-rule\"><a href=\"#practical-rule\">Practical Rule</a></h2><p>Use this form when it makes value movement clearer than copying habits from another language. Prefer the smallest form that still tells the reader where names, types, effects, and boundaries live.</p>\n<p>Continue to <a href=\"/learn/book/abstractions/classes-instances-laws/classes\">Classes</a>.</p>\n",
		"summaryHtml": "Use value-indexed types, indexed data results, <code>partial</code>, and <code>~=</code> without turning Musi into a proof assistant."
	},
	{
		"locale": "en",
		"id": "classes",
		"kind": "chapter",
		"parentId": "abstractions-laws",
		"depth": 2,
		"treePath": [
			"abstractions",
			"abstractions-laws",
			"classes"
		],
		"childIds": [],
		"partId": "abstractions",
		"partTitle": "Abstractions",
		"sectionId": "abstractions-laws",
		"sectionTitle": "Classes, Instances, and Laws",
		"path": "/learn/book/abstractions/classes-instances-laws/classes",
		"canonicalPath": "/learn/book/abstractions/classes-instances-laws/classes",
		"aliases": [
			"/docs/book/abstractions/classes-instances-laws/classes",
			"/learn/book/abstractions/classes-instances-laws/classes",
			"/docs/book/abstractions/classes",
			"/learn/book/abstractions/classes",
			"/docs/language/abstractions/classes",
			"/learn/language/abstractions/classes"
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
		"headings": [],
		"html": "<p>A class names shared behavior. It says which operations a type must provide before generic code can rely on that behavior.</p>\n<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> Eq</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#702C00;--shiki-dark:#B392F0\">T</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">] </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> class</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> {</span></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">  let</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> (=)</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> (a : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">T</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">, b : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">T</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">) : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">Bool</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">  law</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> reflexive</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> (x : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">T</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">) </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> .True</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">};</span></span></code></pre></section></div><p>Read the example from top to bottom. The class names required behavior, and later instances supply behavior for concrete types.</p>\n<p>A class is like a license category. It does not say who the worker is; it says what a qualified worker must be able to do.</p>\n<p>Continue to <a href=\"/learn/book/abstractions/classes-instances-laws/instances\">Instances</a>.</p>\n",
		"summaryHtml": "Describe shared behavior with class declarations."
	},
	{
		"locale": "en",
		"id": "instances",
		"kind": "chapter",
		"parentId": "abstractions-laws",
		"depth": 2,
		"treePath": [
			"abstractions",
			"abstractions-laws",
			"instances"
		],
		"childIds": [],
		"partId": "abstractions",
		"partTitle": "Abstractions",
		"sectionId": "abstractions-laws",
		"sectionTitle": "Classes, Instances, and Laws",
		"path": "/learn/book/abstractions/classes-instances-laws/instances",
		"canonicalPath": "/learn/book/abstractions/classes-instances-laws/instances",
		"aliases": [
			"/docs/book/abstractions/classes-instances-laws/instances",
			"/learn/book/abstractions/classes-instances-laws/instances",
			"/docs/book/abstractions/instances",
			"/learn/book/abstractions/instances",
			"/docs/language/abstractions/instances",
			"/learn/language/abstractions/instances"
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
		"headings": [],
		"html": "<p>An instance supplies class behavior for one concrete type. It bridges a generic behavior contract and a specific implementation.</p>\n<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> eqInt </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> instance</span><span style=\"color:#702C00;--shiki-dark:#B392F0\"> Eq</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">] {</span></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">  let</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> (=)</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> (a : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">, b : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">) : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">Bool</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> :=</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> .True</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">};</span></span></code></pre></section></div><p>Read <code>instance Eq[Int]</code> as equality behavior for <code>Int</code>. The class is the rulebook; the instance is one team showing how it follows the rulebook.</p>\n<p>Keep instances predictable and close to the types or classes they explain.</p>\n<p>Continue to <a href=\"/learn/book/abstractions/classes-instances-laws/laws\">Laws</a>.</p>\n",
		"summaryHtml": "Attach concrete behavior to concrete types."
	},
	{
		"locale": "en",
		"id": "laws",
		"kind": "chapter",
		"parentId": "abstractions-laws",
		"depth": 2,
		"treePath": [
			"abstractions",
			"abstractions-laws",
			"laws"
		],
		"childIds": [],
		"partId": "abstractions",
		"partTitle": "Abstractions",
		"sectionId": "abstractions-laws",
		"sectionTitle": "Classes, Instances, and Laws",
		"path": "/learn/book/abstractions/classes-instances-laws/laws",
		"canonicalPath": "/learn/book/abstractions/classes-instances-laws/laws",
		"aliases": [
			"/docs/book/abstractions/classes-instances-laws/laws",
			"/learn/book/abstractions/classes-instances-laws/laws",
			"/docs/book/abstractions/laws",
			"/learn/book/abstractions/laws",
			"/docs/language/abstractions/laws",
			"/learn/language/abstractions/laws"
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
		"headings": [],
		"html": "<p>Laws document the meaning of a class, not just member names. They tell readers what a correct instance must keep true.</p>\n<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> Vehicle</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#702C00;--shiki-dark:#B392F0\">T</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">] </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> class</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> {</span></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">  let</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> wheels</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(self : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">T</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">) : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">  law</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> atLeastFourWheels</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(vehicle : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">T</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">) </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> vehicle.</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\">wheels</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">() </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">>=</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 4</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">};</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> Car</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> :=</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> data</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> {</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">  | </span><span style=\"color:#702C00;--shiki-dark:#B392F0\">Sport</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">  | </span><span style=\"color:#702C00;--shiki-dark:#B392F0\">Family</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">};</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> carLaw </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> instance</span><span style=\"color:#702C00;--shiki-dark:#B392F0\"> Vehicle</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">Car</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">] {</span></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">  let</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> wheels</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(self : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">Car</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">) : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> :=</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 4</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">};</span></span></code></pre></section></div><p>A car is a vehicle, but a law can still require at least four wheels before that car counts as valid in the model. The class names the family, the instance describes one member, and the law states the trust rule.</p>\n<p>Write a law when it helps separate a believable implementation from a suspicious one.</p>\n<p>Continue to <a href=\"/learn/book/effects-runtime/handling/effects\">Effects</a>.</p>\n",
		"summaryHtml": "Use laws to document the meaning of an abstraction, not just its shape."
	},
	{
		"locale": "en",
		"id": "effects",
		"kind": "chapter",
		"parentId": "effects-handling",
		"depth": 2,
		"treePath": [
			"effects-runtime",
			"effects-handling",
			"effects"
		],
		"childIds": [],
		"partId": "effects-runtime",
		"partTitle": "Effects and Runtime",
		"sectionId": "effects-handling",
		"sectionTitle": "Effect Handling",
		"path": "/learn/book/effects-runtime/handling/effects",
		"canonicalPath": "/learn/book/effects-runtime/handling/effects",
		"aliases": [
			"/docs/book/effects-runtime/handling/effects",
			"/learn/book/effects-runtime/handling/effects",
			"/docs/book/effects-runtime/effects",
			"/learn/book/effects-runtime/effects",
			"/docs/language/effects-runtime/effects",
			"/learn/language/effects-runtime/effects"
		],
		"questions": [],
		"title": "Effects",
		"description": "Introduce effect vocabulary before using clauses or handlers.",
		"group": "Effects and Runtime",
		"section": "Effects and Runtime",
		"order": 24,
		"slug": "effects",
		"summary": "Understand effects as requests for work, not immediate hidden side effects.",
		"descriptionHtml": "Introduce effect vocabulary before using clauses or handlers.",
		"headings": [
			{
				"depth": 2,
				"id": "request-model",
				"text": "Request Model"
			},
			{
				"depth": 2,
				"id": "service-boundary",
				"text": "Service Boundary"
			}
		],
		"html": "<p>An effect describes operations code may request from an outside capability. A request is visible in source so the boundary stays clear.</p>\n<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> console </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> effect</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> {</span></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">  let</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> readLine</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> () : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">String</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">};</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">request</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> console.</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\">readLine</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">();</span></span></code></pre></section></div><h2 id=\"request-model\"><a href=\"#request-model\">Request Model</a></h2><p>Read the <code>effect</code> block as a menu of operations. Read <code>request console.readLine();</code> as code asking for one operation from that menu. Something else must eventually provide the answer.</p>\n<h2 id=\"service-boundary\"><a href=\"#service-boundary\">Service Boundary</a></h2><p>A request is like a service bell at a counter: code asks, handler or host answers.</p>\n<p>Continue to <a href=\"/learn/book/effects-runtime/handling/using\">Using</a>.</p>\n",
		"summaryHtml": "Understand effects as requests for work, not immediate hidden side effects."
	},
	{
		"locale": "en",
		"id": "using",
		"kind": "chapter",
		"parentId": "effects-handling",
		"depth": 2,
		"treePath": [
			"effects-runtime",
			"effects-handling",
			"using"
		],
		"childIds": [],
		"partId": "effects-runtime",
		"partTitle": "Effects and Runtime",
		"sectionId": "effects-handling",
		"sectionTitle": "Effect Handling",
		"path": "/learn/book/effects-runtime/handling/using",
		"canonicalPath": "/learn/book/effects-runtime/handling/using",
		"aliases": [
			"/docs/book/effects-runtime/handling/using",
			"/learn/book/effects-runtime/handling/using",
			"/docs/book/effects-runtime/using",
			"/learn/book/effects-runtime/using",
			"/docs/language/effects-runtime/using",
			"/learn/language/effects-runtime/using"
		],
		"questions": [],
		"title": "Using",
		"description": "Track required effects with `using`, understand capability flow, and keep effectful code readable.",
		"group": "Effects and Runtime",
		"section": "Effects and Runtime",
		"order": 25,
		"slug": "using",
		"summary": "`using` tells readers and the compiler which effects a callable may request.",
		"descriptionHtml": "Track required effects with <code>using</code>, understand capability flow, and keep effectful code readable.",
		"headings": [],
		"html": "<p><code>using</code> lists the capabilities a function may request. It belongs in the function surface so callers can see the boundary before reading the body.</p>\n<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> readClosed</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> () : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">String</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> using</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> { </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">Console</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> } </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">  request</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> Console</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">.</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\">readLine</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">();</span></span></code></pre></section></div><p>Read <code>using { Console }</code> as a permit list. The body requests console work, and the signature tells that story before the body starts.</p>\n<p>Callable types show the same idea: <code>T -&gt; U</code> is pure shape, while <code>T ~&gt; U</code> can require effects. Musi does not let effectful work blend into ordinary pure code without a marker.</p>\n<p>Continue to <a href=\"/learn/book/effects-runtime/handling/handlers\">Handlers</a>.</p>\n",
		"summaryHtml": "<code>using</code> tells readers and the compiler which effects a callable may request."
	},
	{
		"locale": "en",
		"id": "handlers",
		"kind": "chapter",
		"parentId": "effects-handling",
		"depth": 2,
		"treePath": [
			"effects-runtime",
			"effects-handling",
			"handlers"
		],
		"childIds": [],
		"partId": "effects-runtime",
		"partTitle": "Effects and Runtime",
		"sectionId": "effects-handling",
		"sectionTitle": "Effect Handling",
		"path": "/learn/book/effects-runtime/handling/handlers",
		"canonicalPath": "/learn/book/effects-runtime/handling/handlers",
		"aliases": [
			"/docs/book/effects-runtime/handling/handlers",
			"/learn/book/effects-runtime/handling/handlers",
			"/docs/book/effects-runtime/handlers",
			"/learn/book/effects-runtime/handlers",
			"/docs/language/effects-runtime/handlers",
			"/learn/language/effects-runtime/handlers"
		],
		"questions": [],
		"title": "Handlers",
		"description": "Handle effects after the effect and using model are already clear.",
		"group": "Effects and Runtime",
		"section": "Effects and Runtime",
		"order": 26,
		"slug": "handlers",
		"summary": "Resolve requested effects at the boundary where policy belongs.",
		"descriptionHtml": "Handle effects after the effect and using model are already clear.",
		"headings": [],
		"html": "<p>A handler interprets requested operations. It decides what a request means at a particular boundary.</p>\n<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">handle</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> console.</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\">readLine</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">() </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">using</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> console {</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">  value </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">=></span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> value;</span></span>\n<span class=\"line\"><span style=\"color:#622CBC;--shiki-dark:#B392F0\">  readLine</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(k) </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">=></span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> resume</span><span style=\"color:#032563;--shiki-dark:#9ECBFF\"> \"ok\"</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">};</span></span></code></pre></section></div><p>Read <code>handle console.readLine() using console { ... }</code> as running an expression while interpreting console requests with this handler. <code>value =&gt; value;</code> covers normal completion. <code>readLine(k) =&gt; resume &quot;ok&quot;;</code> answers one requested operation and continues the suspended computation.</p>\n<p>A handler is a local policy desk: request asks for help; handler says what help means here.</p>\n<p>Continue to <a href=\"/learn/book/effects-runtime/runtime-model/foundation\">Foundation</a>.</p>\n",
		"summaryHtml": "Resolve requested effects at the boundary where policy belongs."
	},
	{
		"locale": "en",
		"id": "foundation",
		"kind": "chapter",
		"parentId": "effects-runtime-model",
		"depth": 2,
		"treePath": [
			"effects-runtime",
			"effects-runtime-model",
			"foundation"
		],
		"childIds": [],
		"partId": "effects-runtime",
		"partTitle": "Effects and Runtime",
		"sectionId": "effects-runtime-model",
		"sectionTitle": "Runtime Model",
		"path": "/learn/book/effects-runtime/runtime-model/foundation",
		"canonicalPath": "/learn/book/effects-runtime/runtime-model/foundation",
		"aliases": [
			"/docs/book/effects-runtime/runtime-model/foundation",
			"/learn/book/effects-runtime/runtime-model/foundation",
			"/docs/book/effects-runtime/foundation",
			"/learn/book/effects-runtime/foundation",
			"/docs/language/effects-runtime/foundation",
			"/learn/language/effects-runtime/foundation"
		],
		"questions": [],
		"title": "Foundation",
		"description": "Separate language foundation from runtime and stdlib layers.",
		"group": "Effects and Runtime",
		"section": "Effects and Runtime",
		"order": 27,
		"slug": "foundation",
		"summary": "Understand what belongs to musi:core before reaching for stdlib modules.",
		"descriptionHtml": "Separate language foundation from runtime and stdlib layers.",
		"headings": [],
		"html": "<p>Foundation modules provide compiler-known language roots. They are the bridge between surface syntax and names the compiler or runtime must recognize.</p>\n<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> Core</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> :=</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> import</span><span style=\"color:#032563;--shiki-dark:#9ECBFF\"> \"musi:core\"</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#023B95;--shiki-dark:#79B8FF\">Core</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span></code></pre></section></div><p>Read <code>import &quot;musi:core&quot;</code> as importing a reserved foundation module. It is not a normal package dependency; it names part of the language foundation.</p>\n<p>Most user code should start with ordinary package imports. Reach for foundation modules only when the docs or standard library say a language-level root is needed.</p>\n<p>Continue to <a href=\"/learn/book/effects-runtime/runtime-model/runtime\">Runtime</a>.</p>\n",
		"summaryHtml": "Understand what belongs to musi:core before reaching for stdlib modules."
	},
	{
		"locale": "en",
		"id": "runtime",
		"kind": "chapter",
		"parentId": "effects-runtime-model",
		"depth": 2,
		"treePath": [
			"effects-runtime",
			"effects-runtime-model",
			"runtime"
		],
		"childIds": [],
		"partId": "effects-runtime",
		"partTitle": "Effects and Runtime",
		"sectionId": "effects-runtime-model",
		"sectionTitle": "Runtime Model",
		"path": "/learn/book/effects-runtime/runtime-model/runtime",
		"canonicalPath": "/learn/book/effects-runtime/runtime-model/runtime",
		"aliases": [
			"/docs/book/effects-runtime/runtime-model/runtime",
			"/learn/book/effects-runtime/runtime-model/runtime",
			"/docs/book/effects-runtime/runtime",
			"/learn/book/effects-runtime/runtime",
			"/docs/language/effects-runtime/runtime",
			"/learn/language/effects-runtime/runtime"
		],
		"questions": [],
		"title": "Runtime",
		"description": "Learn what runtime-backed imports are for and why they are separate from stdlib helpers.",
		"group": "Effects and Runtime",
		"section": "Effects and Runtime",
		"order": 28,
		"slug": "runtime",
		"summary": "Use musi:runtime for runtime-backed capabilities and host services.",
		"descriptionHtml": "Learn what runtime-backed imports are for and why they are separate from stdlib helpers.",
		"headings": [],
		"html": "<p>Runtime modules provide host-backed services such as environment, process, filesystem, time, random, logging, and dynamic module loading.</p>\n<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> Runtime</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> :=</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> import</span><span style=\"color:#032563;--shiki-dark:#9ECBFF\"> \"musi:runtime\"</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#023B95;--shiki-dark:#79B8FF\">Runtime</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">.</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\">envGet</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(</span><span style=\"color:#032563;--shiki-dark:#9ECBFF\">\"HOME\"</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">);</span></span></code></pre></section></div><p>Read <code>import &quot;musi:runtime&quot;</code> as opening the runtime service surface. Runtime operations may depend on the host process, filesystem, clock, or environment.</p>\n<p>Prefer standard-library wrappers when they exist. They give runtime operations stable names and keep host details out of ordinary modules.</p>\n<p>Continue to <a href=\"/learn/book/effects-runtime/runtime-model/stdlib\">Stdlib</a>.</p>\n",
		"summaryHtml": "Use musi:runtime for runtime-backed capabilities and host services."
	},
	{
		"locale": "en",
		"id": "stdlib",
		"kind": "chapter",
		"parentId": "effects-runtime-model",
		"depth": 2,
		"treePath": [
			"effects-runtime",
			"effects-runtime-model",
			"stdlib"
		],
		"childIds": [],
		"partId": "effects-runtime",
		"partTitle": "Effects and Runtime",
		"sectionId": "effects-runtime-model",
		"sectionTitle": "Runtime Model",
		"path": "/learn/book/effects-runtime/runtime-model/stdlib",
		"canonicalPath": "/learn/book/effects-runtime/runtime-model/stdlib",
		"aliases": [
			"/docs/book/effects-runtime/runtime-model/stdlib",
			"/learn/book/effects-runtime/runtime-model/stdlib",
			"/docs/book/effects-runtime/stdlib",
			"/learn/book/effects-runtime/stdlib",
			"/docs/language/effects-runtime/stdlib",
			"/learn/language/effects-runtime/stdlib"
		],
		"questions": [],
		"title": "Stdlib",
		"description": "Place the standard library on top of foundation and runtime so the layering stays clear.",
		"group": "Effects and Runtime",
		"section": "Effects and Runtime",
		"order": 29,
		"slug": "stdlib",
		"summary": "Reach for @std modules first in ordinary application code.",
		"descriptionHtml": "Place the standard library on top of foundation and runtime so the layering stays clear.",
		"headings": [],
		"html": "<p>The standard library packages common language patterns behind stable imports. It gives everyday code names for options, results, slices, testing, environment access, FFI helpers, and more.</p>\n<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> Option</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> :=</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> import</span><span style=\"color:#032563;--shiki-dark:#9ECBFF\"> \"@std/option\"</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#023B95;--shiki-dark:#79B8FF\">Option</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">.</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\">some</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#702C00;--shiki-dark:#B392F0\">Int</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">](</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">8080</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">)</span></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">  |></span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> Option</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">.</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\">unwrapOr</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#702C00;--shiki-dark:#B392F0\">Int</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">](</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">3000</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">);</span></span></code></pre></section></div><p>Read <code>import &quot;@std/option&quot;</code> as a package import. Generic helpers such as <code>Option.unwrapOr[Int]</code> take type arguments when the operation needs to know the contained value type.</p>\n<p>Pipeline-friendly helpers make data movement easier to read: start with a value, then apply transformations in order.</p>\n<p>Continue to <a href=\"/learn/book/advanced/interop/attributes\">Attributes</a>.</p>\n",
		"summaryHtml": "Reach for @std modules first in ordinary application code."
	},
	{
		"locale": "en",
		"id": "attributes",
		"kind": "chapter",
		"parentId": "advanced-interop",
		"depth": 2,
		"treePath": [
			"advanced",
			"advanced-interop",
			"attributes"
		],
		"childIds": [],
		"partId": "advanced",
		"partTitle": "Advanced and Tooling",
		"sectionId": "advanced-interop",
		"sectionTitle": "Interop and Safety",
		"path": "/learn/book/advanced/interop/attributes",
		"canonicalPath": "/learn/book/advanced/interop/attributes",
		"aliases": [
			"/docs/book/advanced/interop/attributes",
			"/learn/book/advanced/interop/attributes",
			"/docs/book/advanced/attributes",
			"/learn/book/advanced/attributes",
			"/docs/language/advanced/attributes",
			"/learn/language/advanced/attributes"
		],
		"questions": [],
		"title": "Attributes",
		"description": "Use attributes to attach metadata for compiler-known items, layout, foreign links, diagnostics, and lifecycle information.",
		"group": "Advanced and Tooling",
		"section": "Advanced and Tooling",
		"order": 30,
		"slug": "attributes",
		"summary": "Attributes describe metadata, boundaries, and build-time intent without changing Musi into a macro language.",
		"descriptionHtml": "Use attributes to attach metadata for compiler-known items, layout, foreign links, diagnostics, and lifecycle information.",
		"headings": [
			{
				"depth": 2,
				"id": "boundary-tool",
				"text": "Boundary Tool"
			},
			{
				"depth": 2,
				"id": "when-to-reach-for-it",
				"text": "When to Reach for It"
			},
			{
				"depth": 2,
				"id": "read-the-boundary",
				"text": "Read the Boundary"
			},
			{
				"depth": 2,
				"id": "what-musi-does-not-do-here",
				"text": "What Musi does not do here"
			},
			{
				"depth": 2,
				"id": "small-exercise",
				"text": "Small Exercise"
			},
			{
				"depth": 2,
				"id": "mistake-to-avoid",
				"text": "Mistake to Avoid"
			},
			{
				"depth": 2,
				"id": "next-page",
				"text": "Next Page"
			}
		],
		"html": "<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">@</span><span style=\"color:#023B95;--shiki-dark:#B392F0\">known</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(name </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#032563;--shiki-dark:#9ECBFF\"> \"Bool\"</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">)</span></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">export</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> let</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> Bool</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> :=</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> Bool</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">@</span><span style=\"color:#023B95;--shiki-dark:#B392F0\">link</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(name </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#032563;--shiki-dark:#9ECBFF\"> \"c\"</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">)</span></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">foreign</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> \"c\" </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> puts</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> (msg : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">CString</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">) : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">@</span><span style=\"color:#023B95;--shiki-dark:#B392F0\">when</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(os </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#032563;--shiki-dark:#9ECBFF\"> \"linux\"</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">)</span></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">foreign</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> let</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> clock_gettime</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> (id : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">, out : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">CPtr</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">) : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span></code></pre></section></div><p>Attributes let you attach structured metadata to declarations.\nThey are not a replacement for ordinary language design, and they are not a free-form escape hatch for every feature.\nThey exist to carry information that matters at compile time, runtime boundaries, layout, tooling, or documentation.</p>\n<h2 id=\"boundary-tool\"><a href=\"#boundary-tool\">Boundary Tool</a></h2><p>The built-in attribute families you will see most often are:</p>\n<ul>\n<li>compiler and foundation identity: <code>@known</code>, <code>@intrinsic</code></li>\n<li>foreign boundary: <code>@link</code>, <code>@when</code></li>\n<li>data layout and freezing: <code>@repr</code>, <code>@layout</code>, <code>@frozen</code></li>\n<li>hotness and optimization hints: <code>@hot</code>, <code>@cold</code></li>\n<li>lifecycle metadata: <code>@deprecated</code>, <code>@since</code></li>\n</ul>\n<p>There can also be non-reserved metadata attributes that survive as inert data for tooling or documentation.</p>\n<h2 id=\"when-to-reach-for-it\"><a href=\"#when-to-reach-for-it\">When to Reach for It</a></h2><p>If the docs only say &quot;attributes exist&quot;, users still do not know which ones are ordinary metadata, which ones affect code generation, and which ones are only valid in special places.\nThis chapter should answer three practical questions:</p>\n<ol>\n<li>what family is this attribute in?</li>\n<li>what kind of declaration can it attach to?</li>\n<li>what does the compiler or runtime do with it?</li>\n</ol>\n<h2 id=\"read-the-boundary\"><a href=\"#read-the-boundary\">Read the Boundary</a></h2><p>Read attributes from the outside in:</p>\n<ul>\n<li>the path, such as <code>@link</code> or <code>@layout</code></li>\n<li>the named arguments, such as <code>name := &quot;c&quot;</code></li>\n<li>the declaration the attribute is attached to</li>\n</ul>\n<p>A short catalog of common meanings:</p>\n<ul>\n<li><code>@known(name := &quot;Bool&quot;)</code>: this exported item is one canonical built-in surface</li>\n<li><code>@intrinsic(name := &quot;ptr.load&quot;)</code>: implementation comes from compiler/runtime intrinsic machinery</li>\n<li><code>@link(name := &quot;c&quot;)</code>: foreign declaration links against host symbol provider</li>\n<li><code>@when(...)</code>: gate declaration by target or environment facts</li>\n<li><code>@repr(...)</code>, <code>@layout(...)</code>: influence data representation details</li>\n<li><code>@frozen</code>: exported data layout should not drift casually</li>\n<li><code>@hot</code>, <code>@cold</code>: codegen-facing temperature hint</li>\n<li><code>@deprecated</code>, <code>@since</code>: consumer-facing lifecycle metadata</li>\n</ul>\n<h2 id=\"what-musi-does-not-do-here\"><a href=\"#what-musi-does-not-do-here\">What Musi does not do here</a></h2><p>Musi attributes are not a full macro system.\nThey do not replace normal functions, data definitions, or effects.\nIf you need ordinary behavior, write ordinary Musi code first.\nReach for attributes when the information really is metadata.</p>\n<h2 id=\"small-exercise\"><a href=\"#small-exercise\">Small Exercise</a></h2><ul>\n<li>Read one <code>@link</code> declaration and identify every named argument.</li>\n<li>Compare one layout-related attribute with one lifecycle attribute.</li>\n<li>Ask whether the information belongs in ordinary code or in metadata.</li>\n</ul>\n<h2 id=\"mistake-to-avoid\"><a href=\"#mistake-to-avoid\">Mistake to Avoid</a></h2><p>Do not treat attributes as a generic place to hide behavior.\nIf a concept changes how code runs, it usually deserves a language or library construct first.</p>\n<h2 id=\"next-page\"><a href=\"#next-page\">Next Page</a></h2><p>Continue to <a href=\"/learn/book/advanced/interop/foreign\">Foreign</a> to see how the FFI-related attributes fit into real declarations.</p>\n",
		"summaryHtml": "Attributes describe metadata, boundaries, and build-time intent without changing Musi into a macro language."
	},
	{
		"locale": "en",
		"id": "foreign",
		"kind": "chapter",
		"parentId": "advanced-interop",
		"depth": 2,
		"treePath": [
			"advanced",
			"advanced-interop",
			"foreign"
		],
		"childIds": [],
		"partId": "advanced",
		"partTitle": "Advanced and Tooling",
		"sectionId": "advanced-interop",
		"sectionTitle": "Interop and Safety",
		"path": "/learn/book/advanced/interop/foreign",
		"canonicalPath": "/learn/book/advanced/interop/foreign",
		"aliases": [
			"/docs/book/advanced/interop/foreign",
			"/learn/book/advanced/interop/foreign",
			"/docs/book/advanced/foreign",
			"/learn/book/advanced/foreign",
			"/docs/language/advanced/foreign",
			"/learn/language/advanced/foreign"
		],
		"questions": [],
		"title": "Foreign",
		"description": "Keep foreign declarations separate from general attributes so boundary thinking stays clear.",
		"group": "Advanced and Tooling",
		"section": "Advanced and Tooling",
		"order": 31,
		"slug": "foreign",
		"summary": "Declare foreign bindings at the runtime boundary, not inside ordinary domain code.",
		"descriptionHtml": "Keep foreign declarations separate from general attributes so boundary thinking stays clear.",
		"headings": [
			{
				"depth": 2,
				"id": "boundary-tool",
				"text": "Boundary Tool"
			},
			{
				"depth": 2,
				"id": "when-to-reach-for-it",
				"text": "When to Reach for It"
			},
			{
				"depth": 2,
				"id": "read-the-boundary",
				"text": "Read the Boundary"
			},
			{
				"depth": 2,
				"id": "small-exercise",
				"text": "Small Exercise"
			},
			{
				"depth": 2,
				"id": "mistake-to-avoid",
				"text": "Mistake to Avoid"
			},
			{
				"depth": 2,
				"id": "next-page",
				"text": "Next Page"
			}
		],
		"html": "<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">foreign</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> \"c\" </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> puts</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> (msg : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">CString</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">) : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span></code></pre></section></div><h2 id=\"boundary-tool\"><a href=\"#boundary-tool\">Boundary Tool</a></h2><p>Foreign declarations describe bindings implemented outside Musi.\nThe example names a C function and its Musi-facing type so code on Musi side can call across boundary with explicit contract.\nThis is advanced because it is about integration, not about core language flow.</p>\n<h2 id=\"when-to-reach-for-it\"><a href=\"#when-to-reach-for-it\">When to Reach for It</a></h2><p>Users working near system boundaries need to know how Musi reaches native code without pretending that boundary is ordinary function definition.\nIf docs bury foreign declarations under attribute notes or runtime pages, the integration story stays fuzzy.\nA dedicated page keeps the riskier cross-language surface explicit.</p>\n<h2 id=\"read-the-boundary\"><a href=\"#read-the-boundary\">Read the Boundary</a></h2><p>Read <code>foreign &quot;c&quot;</code> as declaration of external implementation source.\nThen read remainder of line as ordinary Musi-facing name and type surface that callers will see on Musi side.\nWhen adding foreign bindings, keep signatures minimal, verify types carefully, and isolate these declarations near integration boundaries instead of scattering them through domain code.\nCalls to foreign bindings belong inside <code>unsafe { ... }</code> because Musi cannot prove what native code does with raw pointers, strings, global state, or process state.</p>\n<p>Wrap a foreign call in a small <code>unsafe</code> block, then expose a safe wrapper when ordinary callers should not see the native boundary.</p>\n<h2 id=\"small-exercise\"><a href=\"#small-exercise\">Small Exercise</a></h2><ul>\n<li>Declare one foreign binding.</li>\n<li>Identify language/runtime boundary it crosses.</li>\n<li>Wrap the call in a small <code>unsafe</code> block.</li>\n<li>Explain what Musi side promises about arguments and result.</li>\n</ul>\n<h2 id=\"mistake-to-avoid\"><a href=\"#mistake-to-avoid\">Mistake to Avoid</a></h2><p>Do not treat foreign declarations as casual shortcut for code that could stay inside normal Musi modules.</p>\n<h2 id=\"next-page\"><a href=\"#next-page\">Next Page</a></h2><p>Continue to <a href=\"/learn/book/advanced/interop/unsafe-and-ffi\">Unsafe and FFI</a> for raw pointer and native-call boundaries.</p>\n",
		"summaryHtml": "Declare foreign bindings at the runtime boundary, not inside ordinary domain code."
	},
	{
		"locale": "en",
		"id": "unsafe-and-ffi",
		"kind": "chapter",
		"parentId": "advanced-interop",
		"depth": 2,
		"treePath": [
			"advanced",
			"advanced-interop",
			"unsafe-and-ffi"
		],
		"childIds": [],
		"partId": "advanced",
		"partTitle": "Advanced and Tooling",
		"sectionId": "advanced-interop",
		"sectionTitle": "Interop and Safety",
		"path": "/learn/book/advanced/interop/unsafe-and-ffi",
		"canonicalPath": "/learn/book/advanced/interop/unsafe-and-ffi",
		"aliases": [
			"/docs/book/advanced/interop/unsafe-and-ffi",
			"/learn/book/advanced/interop/unsafe-and-ffi",
			"/docs/book/advanced/unsafe-and-ffi",
			"/learn/book/advanced/unsafe-and-ffi",
			"/docs/language/advanced/unsafe-and-ffi",
			"/learn/language/advanced/unsafe-and-ffi"
		],
		"questions": [],
		"title": "Unsafe and FFI",
		"description": "Use unsafe blocks and @std/ffi when Musi code crosses raw native boundaries.",
		"group": "Advanced and Tooling",
		"section": "Advanced and Tooling",
		"order": 32,
		"slug": "unsafe-and-ffi",
		"summary": "Keep raw pointer and native-call work behind a visible unsafe boundary.",
		"descriptionHtml": "Use unsafe blocks and @std/ffi when Musi code crosses raw native boundaries.",
		"headings": [
			{
				"depth": 2,
				"id": "c-abi-types",
				"text": "C ABI Types"
			},
			{
				"depth": 2,
				"id": "wrapper-shape",
				"text": "Wrapper Shape"
			}
		],
		"html": "<p>Native code can do things the Musi type system cannot prove safe. Musi marks that boundary with <code>unsafe { ... }</code>.</p>\n<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> Ffi</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> :=</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> import</span><span style=\"color:#032563;--shiki-dark:#9ECBFF\"> \"@std/ffi\"</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">foreign</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> \"c\" </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> get_counter</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> () : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">CPtr</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> counter </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> unsafe </span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">{</span></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">  let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> raw </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> get_counter</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">();</span></span>\n<span class=\"line\"><span style=\"color:#023B95;--shiki-dark:#79B8FF\">  Ffi</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">.ptr.</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\">cast</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#702C00;--shiki-dark:#B392F0\">Int</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">](raw);</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">};</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> next </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> unsafe </span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">{</span></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">  let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> offset </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> Ffi</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">.ptr.offset;</span></span>\n<span class=\"line\"><span style=\"color:#622CBC;--shiki-dark:#B392F0\">  offset</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#702C00;--shiki-dark:#B392F0\">Int</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">](counter, </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">1</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">);</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">};</span></span></code></pre></section></div><p><code>unsafe</code> is a block because it is a structural scope. Everything inside the braces is checked normally, but calls marked unsafe are only allowed while the checker is inside that scope. The last expression of the block is still the block result.</p>\n<h2 id=\"c-abi-types\"><a href=\"#c-abi-types\">C ABI Types</a></h2><p>Use <code>CString</code> for null-terminated C strings and <code>CPtr</code> for raw C pointer values in native signatures.</p>\n<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">foreign</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> \"c\" </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> puts</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> (message : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">CString</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">) : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">foreign</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> \"c\" </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> memset</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> (dst : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">CPtr</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">, byte : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">, count : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">) : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">CPtr</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span></code></pre></section></div><p><code>CPtr</code> means &quot;this value has C pointer ABI shape.&quot; It is the right type at the native boundary. <code>@std/ffi</code> adds typed pointer views for Musi code that wants to name what a raw pointer points at after the boundary has already been crossed.</p>\n<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> Ffi</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> :=</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> import</span><span style=\"color:#032563;--shiki-dark:#9ECBFF\"> \"@std/ffi\"</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">foreign</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> \"c\" </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> get_counter</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> () : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">CPtr</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> counter </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> unsafe </span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">{</span></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">  let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> raw </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> get_counter</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">();</span></span>\n<span class=\"line\"><span style=\"color:#023B95;--shiki-dark:#79B8FF\">  Ffi</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">.ptr.</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\">cast</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#702C00;--shiki-dark:#B392F0\">Int</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">](raw);</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">};</span></span></code></pre></section></div><p>A <code>Ptr[T]</code> is a typed view over a <code>CPtr</code>. It does not replace <code>CPtr</code> in native declarations. Use <code>CPtr</code> at the C boundary, then use <code>Ptr[T]</code> when Musi code needs a clearer static name for the pointed-at data.</p>\n<p><code>Ffi.ptr.null[T]()</code> creates a typed null pointer. <code>Ffi.ptr.isNull[T](pointer)</code> checks it before dereference.</p>\n<p><code>Ffi.ptr.offset[T](pointer, count)</code> moves by elements, not bytes. For <code>Ptr[CInt]</code>, <code>count := 2</code> means two C ints.</p>\n<p><code>Ffi.ptr.read[T](pointer)</code> and <code>Ffi.ptr.write[T](pointer, value)</code> dereference process memory. Calls to <code>offset</code>, <code>read</code>, and <code>write</code> belong inside <code>unsafe { ... }</code> because Musi cannot prove the address is valid.</p>\n<p>Use <code>CPtr</code> when declaring native calls. Use <code>Ptr[T]</code> after the call when Musi code needs to state the pointed-at shape.</p>\n<p><code>Ffi.ptr</code> is an ordinary record of values. The pointer operations are first-class polymorphic fields, so an alias keeps the type parameter.</p>\n<p>The unsafe rule follows the value. Calling <code>offset[Int]</code> through an alias still needs <code>unsafe { ... }</code>.</p>\n<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">foreign</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> \"c\" </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> clock</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> () : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">export</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> let</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> currentTicks</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> () : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> :=</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> unsafe </span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">{</span></span>\n<span class=\"line\"><span style=\"color:#622CBC;--shiki-dark:#B392F0\">  clock</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">();</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">};</span></span></code></pre></section></div><h2 id=\"wrapper-shape\"><a href=\"#wrapper-shape\">Wrapper Shape</a></h2><p>Raw native work should stay explicit through <code>unsafe { ... }</code>, <code>CPtr</code>, and the <code>@std/ffi</code> helpers. Musi also keeps the final expression as the value inside unsafe blocks, just like any other block.</p>\n<p>Do not hide a native call in ordinary code and expect the reader to notice it from the function name. Put the call inside a small unsafe block, then expose a safe wrapper when the rest of the module should not care about native details.</p>\n<p>Continue to <a href=\"/learn/book/advanced/meta-tooling/operator-forms\">Operator Forms</a>.</p>\n",
		"summaryHtml": "Keep raw pointer and native-call work behind a visible unsafe boundary."
	},
	{
		"locale": "en",
		"id": "operator-forms",
		"kind": "chapter",
		"parentId": "advanced-meta-tooling",
		"depth": 2,
		"treePath": [
			"advanced",
			"advanced-meta-tooling",
			"operator-forms"
		],
		"childIds": [],
		"partId": "advanced",
		"partTitle": "Advanced and Tooling",
		"sectionId": "advanced-meta-tooling",
		"sectionTitle": "Meta and Tooling",
		"path": "/learn/book/advanced/meta-tooling/operator-forms",
		"canonicalPath": "/learn/book/advanced/meta-tooling/operator-forms",
		"aliases": [
			"/docs/book/advanced/meta-tooling/operator-forms",
			"/learn/book/advanced/meta-tooling/operator-forms",
			"/docs/book/advanced/operator-forms",
			"/learn/book/advanced/operator-forms",
			"/docs/language/advanced/operator-forms",
			"/learn/language/advanced/operator-forms"
		],
		"questions": [],
		"title": "Operator Forms",
		"description": "Name operators, set fixity, and call an operator when that reads better.",
		"group": "Advanced and Tooling",
		"section": "Advanced and Tooling",
		"order": 32,
		"slug": "operator-forms",
		"summary": "Use fixity declarations and parenthesized operator names for advanced operator-heavy code.",
		"descriptionHtml": "Name operators, set fixity, and call an operator when that reads better.",
		"headings": [
			{
				"depth": 2,
				"id": "boundary-tool",
				"text": "Boundary Tool"
			},
			{
				"depth": 2,
				"id": "when-to-reach-for-it",
				"text": "When to Reach for It"
			},
			{
				"depth": 2,
				"id": "read-the-boundary",
				"text": "Read the Boundary"
			},
			{
				"depth": 2,
				"id": "small-exercise",
				"text": "Small Exercise"
			},
			{
				"depth": 2,
				"id": "mistake-to-avoid",
				"text": "Mistake to Avoid"
			},
			{
				"depth": 2,
				"id": "next-page",
				"text": "Next Page"
			}
		],
		"html": "<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">infixl</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 6</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> (+)</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> add </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> (+)</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> total </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> add</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">1</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">, </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">2</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">);</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">total;</span></span></code></pre></section></div><h2 id=\"boundary-tool\"><a href=\"#boundary-tool\">Boundary Tool</a></h2><p>Operators can appear as ordinary infix syntax, and parenthesized operator names can be used as values.\nFixity declarations such as <code>infixl 6 (+);</code> describe how an operator groups with neighboring expressions.\nMost code does not need custom fixity, but operator-heavy libraries do.</p>\n<h2 id=\"when-to-reach-for-it\"><a href=\"#when-to-reach-for-it\">When to Reach for It</a></h2><p>A small amount of operator syntax can make numeric or parser-like code clearer.\nToo much can make code unreadable.\nNaming the operator with <code>(+)</code> and declaring fixity explicitly keeps the unusual parts visible.</p>\n<h2 id=\"read-the-boundary\"><a href=\"#read-the-boundary\">Read the Boundary</a></h2><p>Read <code>infixl 6 (+);</code> as a declaration about grouping.\nRead <code>let add := (+);</code> as binding the operator itself to a name.\nRead <code>add(1, 2)</code> as an ordinary call through that binding.</p>\n<h2 id=\"small-exercise\"><a href=\"#small-exercise\">Small Exercise</a></h2><ul>\n<li>Bind one operator with its parenthesized name.</li>\n<li>Call it like a function.</li>\n<li>Avoid custom fixity unless repeated infix use actually becomes clearer.</li>\n</ul>\n<h2 id=\"mistake-to-avoid\"><a href=\"#mistake-to-avoid\">Mistake to Avoid</a></h2><p>Do not use custom operators where a named function would explain the domain better.</p>\n<h2 id=\"next-page\"><a href=\"#next-page\">Next Page</a></h2><p>Continue to <a href=\"/learn/book/advanced/meta-tooling/quote-and-syntax\">Quote and syntax</a> for code-as-data tools.</p>\n",
		"summaryHtml": "Use fixity declarations and parenthesized operator names for advanced operator-heavy code."
	},
	{
		"locale": "en",
		"id": "quote-and-syntax",
		"kind": "chapter",
		"parentId": "advanced-meta-tooling",
		"depth": 2,
		"treePath": [
			"advanced",
			"advanced-meta-tooling",
			"quote-and-syntax"
		],
		"childIds": [],
		"partId": "advanced",
		"partTitle": "Advanced and Tooling",
		"sectionId": "advanced-meta-tooling",
		"sectionTitle": "Meta and Tooling",
		"path": "/learn/book/advanced/meta-tooling/quote-and-syntax",
		"canonicalPath": "/learn/book/advanced/meta-tooling/quote-and-syntax",
		"aliases": [
			"/docs/book/advanced/meta-tooling/quote-and-syntax",
			"/learn/book/advanced/meta-tooling/quote-and-syntax",
			"/docs/book/advanced/quote-and-syntax",
			"/learn/book/advanced/quote-and-syntax",
			"/docs/language/advanced/quote-and-syntax",
			"/learn/language/advanced/quote-and-syntax"
		],
		"questions": [],
		"title": "Quote and Syntax",
		"description": "Introduce quote and syntax work late so beginners are not overloaded too early.",
		"group": "Advanced and Tooling",
		"section": "Advanced and Tooling",
		"order": 32,
		"slug": "quote-and-syntax",
		"summary": "Treat code as data only after ordinary code reading feels natural.",
		"descriptionHtml": "Introduce quote and syntax work late so beginners are not overloaded too early.",
		"headings": [
			{
				"depth": 2,
				"id": "boundary-tool",
				"text": "Boundary Tool"
			},
			{
				"depth": 2,
				"id": "when-to-reach-for-it",
				"text": "When to Reach for It"
			},
			{
				"depth": 2,
				"id": "read-the-boundary",
				"text": "Read the Boundary"
			},
			{
				"depth": 2,
				"id": "small-exercise",
				"text": "Small Exercise"
			},
			{
				"depth": 2,
				"id": "mistake-to-avoid",
				"text": "Mistake to Avoid"
			},
			{
				"depth": 2,
				"id": "next-page",
				"text": "Next Page"
			}
		],
		"html": "<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> addTemplate </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> quote </span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(x </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">+</span><span style=\"color:#A0111F;--shiki-dark:#E1E4E8\"> #(</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">delta</span><span style=\"color:#A0111F;--shiki-dark:#E1E4E8\">)</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">);</span></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> addOneSyntax </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> quote </span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(</span><span style=\"color:#A0111F;--shiki-dark:#E1E4E8\">#(</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">x</span><span style=\"color:#A0111F;--shiki-dark:#E1E4E8\">)</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> +</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 1</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">);</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">addOneSyntax;</span></span></code></pre></section></div><h2 id=\"boundary-tool\"><a href=\"#boundary-tool\">Boundary Tool</a></h2><p><code>quote</code> turns code shape into syntax data you can inspect, build, or reuse.\nThe first snippet shows simplest quoted expression, and the second shows interpolation with <code>#(...)</code> inside quoted form.\nThis chapter belongs late because it asks you to reason about code as data rather than just running code.</p>\n<h2 id=\"when-to-reach-for-it\"><a href=\"#when-to-reach-for-it\">When to Reach for It</a></h2><p>Metaprogramming questions show up after ordinary code already feels familiar.\nAt that point users need examples that explain both power and boundary: quoting is useful, but it is not default way to write everyday logic.\nA focused page keeps this tool available without overwhelming readers who are still stabilizing basic syntax.</p>\n<h2 id=\"read-the-boundary\"><a href=\"#read-the-boundary\">Read the Boundary</a></h2><p>Read <code>quote (x + 1);</code> as syntax value representing expression shape.\nThen read <code>#(delta)</code> or <code>#(x)</code> inside quoted form as splice points where surrounding values contribute pieces to generated syntax.\nWhen experimenting, start with very small quoted expressions and ask what syntax object each quote should represent before building larger templates.</p>\n<h2 id=\"small-exercise\"><a href=\"#small-exercise\">Small Exercise</a></h2><ul>\n<li>Quote one simple expression.</li>\n<li>Add one splice inside a quoted template.</li>\n<li>Compare quoted template with handwritten equivalent shape.</li>\n</ul>\n<h2 id=\"mistake-to-avoid\"><a href=\"#mistake-to-avoid\">Mistake to Avoid</a></h2><p>Do not reach for quote when an ordinary function or data value already solves the problem more directly.</p>\n<h2 id=\"next-page\"><a href=\"#next-page\">Next Page</a></h2><p>Continue to <a href=\"/learn/book/advanced/meta-tooling/templates-and-splices\">Templates and splices</a> to separate text interpolation from syntax splicing.</p>\n",
		"summaryHtml": "Treat code as data only after ordinary code reading feels natural."
	},
	{
		"locale": "en",
		"id": "templates-and-splices",
		"kind": "chapter",
		"parentId": "advanced-meta-tooling",
		"depth": 2,
		"treePath": [
			"advanced",
			"advanced-meta-tooling",
			"templates-and-splices"
		],
		"childIds": [],
		"partId": "advanced",
		"partTitle": "Advanced and Tooling",
		"sectionId": "advanced-meta-tooling",
		"sectionTitle": "Meta and Tooling",
		"path": "/learn/book/advanced/meta-tooling/templates-and-splices",
		"canonicalPath": "/learn/book/advanced/meta-tooling/templates-and-splices",
		"aliases": [
			"/docs/book/advanced/meta-tooling/templates-and-splices",
			"/learn/book/advanced/meta-tooling/templates-and-splices",
			"/docs/book/advanced/templates-and-splices",
			"/learn/book/advanced/templates-and-splices",
			"/docs/language/advanced/templates-and-splices",
			"/learn/language/advanced/templates-and-splices"
		],
		"questions": [],
		"title": "Templates and Splices",
		"description": "Build text with interpolation and understand splice syntax near quote forms.",
		"group": "Advanced and Tooling",
		"section": "Advanced and Tooling",
		"order": 34,
		"slug": "templates-and-splices",
		"summary": "Use template literals for interpolated text and splice forms when building syntax.",
		"descriptionHtml": "Build text with interpolation and understand splice syntax near quote forms.",
		"headings": [
			{
				"depth": 2,
				"id": "boundary-tool",
				"text": "Boundary Tool"
			},
			{
				"depth": 2,
				"id": "when-to-reach-for-it",
				"text": "When to Reach for It"
			},
			{
				"depth": 2,
				"id": "read-the-boundary",
				"text": "Read the Boundary"
			},
			{
				"depth": 2,
				"id": "small-exercise",
				"text": "Small Exercise"
			},
			{
				"depth": 2,
				"id": "mistake-to-avoid",
				"text": "Mistake to Avoid"
			},
			{
				"depth": 2,
				"id": "next-page",
				"text": "Next Page"
			}
		],
		"html": "<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> port </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 8080</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> label </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#032563;--shiki-dark:#9ECBFF\"> `port </span><span style=\"color:#A0111F;--shiki-dark:#9ECBFF\">${</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">port</span><span style=\"color:#A0111F;--shiki-dark:#9ECBFF\">}</span><span style=\"color:#032563;--shiki-dark:#9ECBFF\">`</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">label;</span></span></code></pre></section></div><h2 id=\"boundary-tool\"><a href=\"#boundary-tool\">Boundary Tool</a></h2><p>Template literals use backticks and <code>${...}</code> interpolation.\nThey are useful when surrounding text and computed values belong together.\nSplice forms such as <code>#x</code>, <code>#(expr)</code>, and <code>#[items]</code> appear in syntax-building contexts where existing values contribute pieces to quoted code.</p>\n<h2 id=\"when-to-reach-for-it\"><a href=\"#when-to-reach-for-it\">When to Reach for It</a></h2><p>String assembly and syntax assembly look similar from far away, but they answer different questions.\nTemplates produce text-like values.\nSplices feed existing values into quoted syntax.\nKeeping both forms named helps readers avoid treating every interpolation as a macro.</p>\n<h2 id=\"read-the-boundary\"><a href=\"#read-the-boundary\">Read the Boundary</a></h2><p>Read <code>`port ${port}`</code> as text with one embedded expression.\nRead <code>#(delta)</code> inside a quote as a syntax splice, not as string interpolation.\nUse templates for user-facing text and splices for code-as-data work.</p>\n<h2 id=\"small-exercise\"><a href=\"#small-exercise\">Small Exercise</a></h2><ul>\n<li>Build one template value with a named binding inside it.</li>\n<li>Compare it with a quoted expression that uses <code>#(...)</code>.</li>\n<li>Explain which one produces text and which one produces syntax.</li>\n</ul>\n<h2 id=\"mistake-to-avoid\"><a href=\"#mistake-to-avoid\">Mistake to Avoid</a></h2><p>Do not use syntax splices to build ordinary strings. Use templates for text.</p>\n<h2 id=\"next-page\"><a href=\"#next-page\">Next Page</a></h2><p>Continue to <a href=\"/learn/book/advanced/meta-tooling/testing\">Testing</a> to return to everyday project workflow.</p>\n",
		"summaryHtml": "Use template literals for interpolated text and splice forms when building syntax."
	},
	{
		"locale": "en",
		"id": "testing",
		"kind": "chapter",
		"parentId": "advanced-meta-tooling",
		"depth": 2,
		"treePath": [
			"advanced",
			"advanced-meta-tooling",
			"testing"
		],
		"childIds": [],
		"partId": "advanced",
		"partTitle": "Advanced and Tooling",
		"sectionId": "advanced-meta-tooling",
		"sectionTitle": "Meta and Tooling",
		"path": "/learn/book/advanced/meta-tooling/testing",
		"canonicalPath": "/learn/book/advanced/meta-tooling/testing",
		"aliases": [
			"/docs/book/advanced/meta-tooling/testing",
			"/learn/book/advanced/meta-tooling/testing",
			"/docs/book/advanced/testing",
			"/learn/book/advanced/testing",
			"/docs/language/advanced/testing",
			"/learn/language/advanced/testing"
		],
		"questions": [],
		"title": "Testing",
		"description": "Teach tests before the wider command surface so the workflow stays concrete.",
		"group": "Advanced and Tooling",
		"section": "Advanced and Tooling",
		"order": 33,
		"slug": "testing",
		"summary": "Write small package tests that read like ordinary code.",
		"descriptionHtml": "Teach tests before the wider command surface so the workflow stays concrete.",
		"headings": [
			{
				"depth": 2,
				"id": "boundary-tool",
				"text": "Boundary Tool"
			},
			{
				"depth": 2,
				"id": "when-to-reach-for-it",
				"text": "When to Reach for It"
			},
			{
				"depth": 2,
				"id": "read-the-boundary",
				"text": "Read the Boundary"
			},
			{
				"depth": 2,
				"id": "small-exercise",
				"text": "Small Exercise"
			},
			{
				"depth": 2,
				"id": "mistake-to-avoid",
				"text": "Mistake to Avoid"
			},
			{
				"depth": 2,
				"id": "next-page",
				"text": "Next Page"
			}
		],
		"html": "<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> Testing</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> :=</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> import</span><span style=\"color:#032563;--shiki-dark:#9ECBFF\"> \"@std/testing\"</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">export</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> let</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> test</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> () </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span></span>\n<span class=\"line\"><span style=\"color:#023B95;--shiki-dark:#79B8FF\">  Testing</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">.</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\">it</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(</span><span style=\"color:#032563;--shiki-dark:#9ECBFF\">\"adds values\"</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">, </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">Testing</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">.</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\">toBe</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">1</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> +</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 2</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">, </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">3</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">));</span></span></code></pre></section></div><h2 id=\"boundary-tool\"><a href=\"#boundary-tool\">Boundary Tool</a></h2><p>Musi tests are ordinary code organized for discovery and execution by tooling.\nThis example keeps that promise visible: import testing helpers, export a <code>test</code> binding, and express one expectation in same language surface you already know.\nTesting becomes easier to adopt when it does not require a second mini-language.</p>\n<h2 id=\"when-to-reach-for-it\"><a href=\"#when-to-reach-for-it\">When to Reach for It</a></h2><p>Users need confidence loop, not just syntax reference.\nIf the docs explain features but never show how to check them, learners still ask how to verify a package change or protect against regressions.\nA tiny test example gives them a habit they can keep using as code grows.</p>\n<h2 id=\"read-the-boundary\"><a href=\"#read-the-boundary\">Read the Boundary</a></h2><p>Read <code>let Testing := import &quot;@std/testing&quot;;</code> as setup of helpers, then read exported <code>test</code> binding as entry point tooling will discover.\nThe assertion itself is ordinary function-style code, which means testing builds on same import, call, and expression patterns from earlier chapters.\nWhen writing first tests, keep each one tiny and named around one behavior you want confidence in.</p>\n<h2 id=\"small-exercise\"><a href=\"#small-exercise\">Small Exercise</a></h2><ul>\n<li>Create one <code>*.test.ms</code> file.</li>\n<li>Export one <code>test</code> binding.</li>\n<li>Check one small behavior with stdlib testing helper.</li>\n</ul>\n<h2 id=\"mistake-to-avoid\"><a href=\"#mistake-to-avoid\">Mistake to Avoid</a></h2><p>Do not wait for a large project before learning the test shape; tiny examples benefit from it too.</p>\n<h2 id=\"next-page\"><a href=\"#next-page\">Next Page</a></h2><p>Continue to <a href=\"/learn/book/advanced/meta-tooling/running-and-tooling\">Running and tooling</a> to tie learning back to everyday commands.</p>\n",
		"summaryHtml": "Write small package tests that read like ordinary code."
	},
	{
		"locale": "en",
		"id": "running-and-tooling",
		"kind": "chapter",
		"parentId": "advanced-meta-tooling",
		"depth": 2,
		"treePath": [
			"advanced",
			"advanced-meta-tooling",
			"running-and-tooling"
		],
		"childIds": [],
		"partId": "advanced",
		"partTitle": "Advanced and Tooling",
		"sectionId": "advanced-meta-tooling",
		"sectionTitle": "Meta and Tooling",
		"path": "/learn/book/advanced/meta-tooling/running-and-tooling",
		"canonicalPath": "/learn/book/advanced/meta-tooling/running-and-tooling",
		"aliases": [
			"/docs/book/advanced/meta-tooling/running-and-tooling",
			"/learn/book/advanced/meta-tooling/running-and-tooling",
			"/docs/book/advanced/running-and-tooling",
			"/learn/book/advanced/running-and-tooling",
			"/docs/language/advanced/running-and-tooling",
			"/learn/language/advanced/running-and-tooling"
		],
		"questions": [],
		"title": "Running and Tooling",
		"description": "Bring the learning path back to commands and workflow once language basics are in place.",
		"group": "Advanced and Tooling",
		"section": "Advanced and Tooling",
		"order": 34,
		"slug": "running-and-tooling",
		"summary": "Finish with the everyday command flow for checking, running, and building code.",
		"descriptionHtml": "Bring the learning path back to commands and workflow once language basics are in place.",
		"headings": [
			{
				"depth": 2,
				"id": "boundary-tool",
				"text": "Boundary Tool"
			},
			{
				"depth": 2,
				"id": "when-to-reach-for-it",
				"text": "When to Reach for It"
			},
			{
				"depth": 2,
				"id": "read-the-boundary",
				"text": "Read the Boundary"
			},
			{
				"depth": 2,
				"id": "small-exercise",
				"text": "Small Exercise"
			},
			{
				"depth": 2,
				"id": "mistake-to-avoid",
				"text": "Mistake to Avoid"
			},
			{
				"depth": 2,
				"id": "next-page",
				"text": "Next Page"
			}
		],
		"html": "<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#702C00;--shiki-dark:#B392F0\">music</span><span style=\"color:#032563;--shiki-dark:#9ECBFF\"> check</span><span style=\"color:#032563;--shiki-dark:#9ECBFF\"> index.ms</span></span>\n<span class=\"line\"><span style=\"color:#702C00;--shiki-dark:#B392F0\">musi</span><span style=\"color:#032563;--shiki-dark:#9ECBFF\"> run</span></span>\n<span class=\"line\"><span style=\"color:#702C00;--shiki-dark:#B392F0\">musi</span><span style=\"color:#032563;--shiki-dark:#9ECBFF\"> test</span></span></code></pre></section></div><h2 id=\"boundary-tool\"><a href=\"#boundary-tool\">Boundary Tool</a></h2><p>Musi has everyday commands for package workflow and direct file workflow.\nThis page brings them together at end of language path so readers can connect all earlier examples to routine habits: check code, run packages, build outputs, and run tests.\nThe command surface is small enough to learn, but distinct enough to deserve a final summary.</p>\n<h2 id=\"when-to-reach-for-it\"><a href=\"#when-to-reach-for-it\">When to Reach for It</a></h2><p>After learning syntax, users still need operational confidence.\nThey want to know which command to run when checking a file, when to use package commands, and how testing fits into normal iteration.\nA workflow chapter turns scattered command knowledge into repeatable practice.</p>\n<h2 id=\"read-the-boundary\"><a href=\"#read-the-boundary\">Read the Boundary</a></h2><p>Read <code>musi run</code>, <code>musi check</code>, <code>musi build</code>, and <code>musi test</code> as package-root commands for project lifecycle.\nRead <code>music check index.ms</code>, <code>music build index.ms</code>, and <code>music run index.seam</code> as direct lane for single-file or lower-level work.\nWhen in doubt, ask first whether you are inside a package or handling one file directly; that decision usually picks the right command family immediately.</p>\n<h2 id=\"small-exercise\"><a href=\"#small-exercise\">Small Exercise</a></h2><ul>\n<li>Run one direct <code>music check</code> on a scratch file.</li>\n<li>Run one package command inside generated project.</li>\n<li>Use <code>musi test</code> after adding one tiny test.</li>\n</ul>\n<h2 id=\"mistake-to-avoid\"><a href=\"#mistake-to-avoid\">Mistake to Avoid</a></h2><p>Do not memorize commands as flat list; group them by direct-file lane versus package lane.</p>\n<h2 id=\"next-page\"><a href=\"#next-page\">Next Page</a></h2><p>Continue back through any chapter you need, now that you have both language model and workflow model tied together.</p>\n",
		"summaryHtml": "Finish with the everyday command flow for checking, running, and building code."
	},
	{
		"locale": "en",
		"id": "musi-for-python-developers",
		"kind": "chapter",
		"parentId": "developers-guides",
		"depth": 2,
		"treePath": [
			"developers",
			"developers-guides",
			"musi-for-python-developers"
		],
		"childIds": [],
		"partId": "developers",
		"partTitle": "Musi for Developers",
		"sectionId": "developers-guides",
		"sectionTitle": "Language Guides",
		"path": "/learn/book/developers/guides/python",
		"canonicalPath": "/learn/book/developers/guides/python",
		"aliases": [
			"/docs/book/developers/guides/python",
			"/learn/book/developers/guides/python",
			"/docs/book/developers/python",
			"/learn/book/developers/python",
			"/docs/language/developers/python",
			"/learn/language/developers/python"
		],
		"questions": [],
		"title": "Musi for Python Developers",
		"description": "Learn how Python habits translate into Musi code.",
		"group": "Musi for Developers",
		"section": "Musi for Developers",
		"order": 1,
		"slug": "musi-for-python-developers",
		"summary": "Start from Python habits and write the same ideas as Musi expressions.",
		"descriptionHtml": "Learn how Python habits translate into Musi code.",
		"headings": [
			{
				"depth": 2,
				"id": "first-translation",
				"text": "First Translation"
			},
			{
				"depth": 2,
				"id": "habits-that-transfer",
				"text": "Habits That Transfer"
			},
			{
				"depth": 2,
				"id": "habits-to-relearn",
				"text": "Habits to Relearn"
			},
			{
				"depth": 2,
				"id": "where-to-go-next",
				"text": "Where to Go Next"
			}
		],
		"html": "<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> User</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> :=</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> data</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> {</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">  name : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">String</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">  visits : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> :=</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 0</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">};</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> greet</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> (user : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">User</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">) : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">String</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> :=</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> user.name;</span></span></code></pre></section></div><p>Python developers often bring dynamic scripts, dictionaries, list work, and exceptions.\nMusi keeps the useful part of that experience, but the code shape changes: Use records for small named shapes, data definitions for variants, and effects for boundary work.</p>\n<h2 id=\"first-translation\"><a href=\"#first-translation\">First Translation</a></h2><p>Read the example as one small program.\nA <code>let</code> binding names a value or function.\nA block ends with the value it produces.\nA <code>match</code> expression returns a value from the selected arm.</p>\n<h2 id=\"habits-that-transfer\"><a href=\"#habits-that-transfer\">Habits That Transfer</a></h2><ul>\n<li>Name data by domain, not by container.</li>\n<li>Keep side effects at boundaries.</li>\n<li>Prefer small functions with explicit inputs.</li>\n<li>Put package boundaries where readers need names to stop leaking.</li>\n</ul>\n<h2 id=\"habits-to-relearn\"><a href=\"#habits-to-relearn\">Habits to Relearn</a></h2><ul>\n<li>Do not write a <code>return</code> keyword for the final value.</li>\n<li>Do not model every concept as an object.</li>\n<li>Do not hide boundary work; use <code>request</code>, <code>using</code>, handlers, or <code>unsafe</code> where the operation crosses a line.</li>\n</ul>\n<h2 id=\"where-to-go-next\"><a href=\"#where-to-go-next\">Where to Go Next</a></h2><ul>\n<li><a href=\"/learn/book/start/foundations/values-and-let\">Values and Let</a> for binding rules.</li>\n<li><a href=\"/learn/book/core/functions-and-calls/functions\">Functions</a> for named parameters and calls.</li>\n<li><a href=\"/learn/book/data/modeling/data-definitions\">Data Definitions</a> for variant payloads.</li>\n<li><a href=\"/learn/book/effects-runtime/handling/effects\">Effects</a> for requested work.</li>\n</ul>\n",
		"summaryHtml": "Start from Python habits and write the same ideas as Musi expressions."
	},
	{
		"locale": "en",
		"id": "musi-for-java-developers",
		"kind": "chapter",
		"parentId": "developers-guides",
		"depth": 2,
		"treePath": [
			"developers",
			"developers-guides",
			"musi-for-java-developers"
		],
		"childIds": [],
		"partId": "developers",
		"partTitle": "Musi for Developers",
		"sectionId": "developers-guides",
		"sectionTitle": "Language Guides",
		"path": "/learn/book/developers/guides/java",
		"canonicalPath": "/learn/book/developers/guides/java",
		"aliases": [
			"/docs/book/developers/guides/java",
			"/learn/book/developers/guides/java",
			"/docs/book/developers/java",
			"/learn/book/developers/java",
			"/docs/language/developers/java",
			"/learn/language/developers/java"
		],
		"questions": [],
		"title": "Musi for Java Developers",
		"description": "Learn how Java habits translate into Musi code.",
		"group": "Musi for Developers",
		"section": "Musi for Developers",
		"order": 1,
		"slug": "musi-for-java-developers",
		"summary": "Start from Java habits and write the same ideas as Musi expressions.",
		"descriptionHtml": "Learn how Java habits translate into Musi code.",
		"headings": [
			{
				"depth": 2,
				"id": "first-translation",
				"text": "First Translation"
			},
			{
				"depth": 2,
				"id": "habits-that-transfer",
				"text": "Habits That Transfer"
			},
			{
				"depth": 2,
				"id": "habits-to-relearn",
				"text": "Habits to Relearn"
			},
			{
				"depth": 2,
				"id": "where-to-go-next",
				"text": "Where to Go Next"
			}
		],
		"html": "<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> Show</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#702C00;--shiki-dark:#B392F0\">T</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">] </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> class</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> {</span></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">  let</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> show</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(value : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">T</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">) : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">String</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">};</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> showInt </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> instance</span><span style=\"color:#702C00;--shiki-dark:#B392F0\"> Show</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">] {</span></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">  let</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> show</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(value : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">) : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">String</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> :=</span><span style=\"color:#032563;--shiki-dark:#9ECBFF\"> \"int\"</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">};</span></span></code></pre></section></div><p>Java developers often bring classes, methods, interfaces, and checked boundaries.\nMusi keeps the useful part of that experience, but the code shape changes: Use data for shape, classes for behavior contracts, instances for implementations, and effects for requested work.</p>\n<h2 id=\"first-translation\"><a href=\"#first-translation\">First Translation</a></h2><p>Read the example as one small program.\nA <code>let</code> binding names a value or function.\nA block ends with the value it produces.\nA <code>match</code> expression returns a value from the selected arm.</p>\n<h2 id=\"habits-that-transfer\"><a href=\"#habits-that-transfer\">Habits That Transfer</a></h2><ul>\n<li>Name data by domain, not by container.</li>\n<li>Keep side effects at boundaries.</li>\n<li>Prefer small functions with explicit inputs.</li>\n<li>Put package boundaries where readers need names to stop leaking.</li>\n</ul>\n<h2 id=\"habits-to-relearn\"><a href=\"#habits-to-relearn\">Habits to Relearn</a></h2><ul>\n<li>Do not write a <code>return</code> keyword for the final value.</li>\n<li>Do not model every concept as an object.</li>\n<li>Do not hide boundary work; use <code>request</code>, <code>using</code>, handlers, or <code>unsafe</code> where the operation crosses a line.</li>\n</ul>\n<h2 id=\"where-to-go-next\"><a href=\"#where-to-go-next\">Where to Go Next</a></h2><ul>\n<li><a href=\"/learn/book/start/foundations/values-and-let\">Values and Let</a> for binding rules.</li>\n<li><a href=\"/learn/book/core/functions-and-calls/functions\">Functions</a> for named parameters and calls.</li>\n<li><a href=\"/learn/book/data/modeling/data-definitions\">Data Definitions</a> for variant payloads.</li>\n<li><a href=\"/learn/book/effects-runtime/handling/effects\">Effects</a> for requested work.</li>\n</ul>\n",
		"summaryHtml": "Start from Java habits and write the same ideas as Musi expressions."
	},
	{
		"locale": "en",
		"id": "musi-for-javascript-developers",
		"kind": "chapter",
		"parentId": "developers-guides",
		"depth": 2,
		"treePath": [
			"developers",
			"developers-guides",
			"musi-for-javascript-developers"
		],
		"childIds": [],
		"partId": "developers",
		"partTitle": "Musi for Developers",
		"sectionId": "developers-guides",
		"sectionTitle": "Language Guides",
		"path": "/learn/book/developers/guides/javascript",
		"canonicalPath": "/learn/book/developers/guides/javascript",
		"aliases": [
			"/docs/book/developers/guides/javascript",
			"/learn/book/developers/guides/javascript",
			"/docs/book/developers/javascript",
			"/learn/book/developers/javascript",
			"/docs/language/developers/javascript",
			"/learn/language/developers/javascript"
		],
		"questions": [],
		"title": "Musi for JavaScript Developers",
		"description": "Learn how JavaScript habits translate into Musi code.",
		"group": "Musi for Developers",
		"section": "Musi for Developers",
		"order": 1,
		"slug": "musi-for-javascript-developers",
		"summary": "Start from JavaScript habits and write the same ideas as Musi expressions.",
		"descriptionHtml": "Learn how JavaScript habits translate into Musi code.",
		"headings": [
			{
				"depth": 2,
				"id": "first-translation",
				"text": "First Translation"
			},
			{
				"depth": 2,
				"id": "habits-that-transfer",
				"text": "Habits That Transfer"
			},
			{
				"depth": 2,
				"id": "habits-to-relearn",
				"text": "Habits to Relearn"
			},
			{
				"depth": 2,
				"id": "where-to-go-next",
				"text": "Where to Go Next"
			}
		],
		"html": "<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> makeUser</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> (name : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">String</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">) </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> {</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">  name </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> name,</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">  active </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> .True</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">,</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">};</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#622CBC;--shiki-dark:#B392F0\">makeUser</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(</span><span style=\"color:#032563;--shiki-dark:#9ECBFF\">\"Musi\"</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">);</span></span></code></pre></section></div><p>JavaScript developers often bring objects, functions, promises, and module imports.\nMusi keeps the useful part of that experience, but the code shape changes: Use records for plain object shape, functions as values, packages for modules, and effects for async-like boundary work.</p>\n<h2 id=\"first-translation\"><a href=\"#first-translation\">First Translation</a></h2><p>Read the example as one small program.\nA <code>let</code> binding names a value or function.\nA block ends with the value it produces.\nA <code>match</code> expression returns a value from the selected arm.</p>\n<h2 id=\"habits-that-transfer\"><a href=\"#habits-that-transfer\">Habits That Transfer</a></h2><ul>\n<li>Name data by domain, not by container.</li>\n<li>Keep side effects at boundaries.</li>\n<li>Prefer small functions with explicit inputs.</li>\n<li>Put package boundaries where readers need names to stop leaking.</li>\n</ul>\n<h2 id=\"habits-to-relearn\"><a href=\"#habits-to-relearn\">Habits to Relearn</a></h2><ul>\n<li>Do not write a <code>return</code> keyword for the final value.</li>\n<li>Do not model every concept as an object.</li>\n<li>Do not hide boundary work; use <code>request</code>, <code>using</code>, handlers, or <code>unsafe</code> where the operation crosses a line.</li>\n</ul>\n<h2 id=\"where-to-go-next\"><a href=\"#where-to-go-next\">Where to Go Next</a></h2><ul>\n<li><a href=\"/learn/book/start/foundations/values-and-let\">Values and Let</a> for binding rules.</li>\n<li><a href=\"/learn/book/core/functions-and-calls/functions\">Functions</a> for named parameters and calls.</li>\n<li><a href=\"/learn/book/data/modeling/data-definitions\">Data Definitions</a> for variant payloads.</li>\n<li><a href=\"/learn/book/effects-runtime/handling/effects\">Effects</a> for requested work.</li>\n</ul>\n",
		"summaryHtml": "Start from JavaScript habits and write the same ideas as Musi expressions."
	},
	{
		"locale": "en",
		"id": "musi-for-typescript-developers",
		"kind": "chapter",
		"parentId": "developers-guides",
		"depth": 2,
		"treePath": [
			"developers",
			"developers-guides",
			"musi-for-typescript-developers"
		],
		"childIds": [],
		"partId": "developers",
		"partTitle": "Musi for Developers",
		"sectionId": "developers-guides",
		"sectionTitle": "Language Guides",
		"path": "/learn/book/developers/guides/typescript",
		"canonicalPath": "/learn/book/developers/guides/typescript",
		"aliases": [
			"/docs/book/developers/guides/typescript",
			"/learn/book/developers/guides/typescript",
			"/docs/book/developers/typescript",
			"/learn/book/developers/typescript",
			"/docs/language/developers/typescript",
			"/learn/language/developers/typescript"
		],
		"questions": [],
		"title": "Musi for TypeScript Developers",
		"description": "Learn how TypeScript habits translate into Musi code.",
		"group": "Musi for Developers",
		"section": "Musi for Developers",
		"order": 1,
		"slug": "musi-for-typescript-developers",
		"summary": "Start from TypeScript habits and write the same ideas as Musi expressions.",
		"descriptionHtml": "Learn how TypeScript habits translate into Musi code.",
		"headings": [
			{
				"depth": 2,
				"id": "first-translation",
				"text": "First Translation"
			},
			{
				"depth": 2,
				"id": "habits-that-transfer",
				"text": "Habits That Transfer"
			},
			{
				"depth": 2,
				"id": "habits-to-relearn",
				"text": "Habits to Relearn"
			},
			{
				"depth": 2,
				"id": "where-to-go-next",
				"text": "Where to Go Next"
			}
		],
		"html": "<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> LoadResult</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#702C00;--shiki-dark:#B392F0\">T</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">] </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> data</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> {</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">  | </span><span style=\"color:#702C00;--shiki-dark:#B392F0\">Loaded</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(</span><span style=\"color:#702C00;--shiki-dark:#FFAB70\">value</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> : </span><span style=\"color:#702C00;--shiki-dark:#B392F0\">T</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">)</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">  | </span><span style=\"color:#702C00;--shiki-dark:#B392F0\">Missing</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(</span><span style=\"color:#702C00;--shiki-dark:#FFAB70\">reason</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> : </span><span style=\"color:#702C00;--shiki-dark:#B392F0\">String</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">)</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">};</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> port : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">LoadResult</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#702C00;--shiki-dark:#B392F0\">Int</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">] </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> .Loaded</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(value </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 8080</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">);</span></span></code></pre></section></div><p>TypeScript developers often bring structural types, generics, union modeling, and typed call sites.\nMusi keeps the useful part of that experience, but the code shape changes: Use annotations at boundaries, named payloads for variants, and generics when one definition works across many types.</p>\n<h2 id=\"first-translation\"><a href=\"#first-translation\">First Translation</a></h2><p>Read the example as one small program.\nA <code>let</code> binding names a value or function.\nA block ends with the value it produces.\nA <code>match</code> expression returns a value from the selected arm.</p>\n<h2 id=\"habits-that-transfer\"><a href=\"#habits-that-transfer\">Habits That Transfer</a></h2><ul>\n<li>Name data by domain, not by container.</li>\n<li>Keep side effects at boundaries.</li>\n<li>Prefer small functions with explicit inputs.</li>\n<li>Put package boundaries where readers need names to stop leaking.</li>\n</ul>\n<h2 id=\"habits-to-relearn\"><a href=\"#habits-to-relearn\">Habits to Relearn</a></h2><ul>\n<li>Do not write a <code>return</code> keyword for the final value.</li>\n<li>Do not model every concept as an object.</li>\n<li>Do not hide boundary work; use <code>request</code>, <code>using</code>, handlers, or <code>unsafe</code> where the operation crosses a line.</li>\n</ul>\n<h2 id=\"where-to-go-next\"><a href=\"#where-to-go-next\">Where to Go Next</a></h2><ul>\n<li><a href=\"/learn/book/start/foundations/values-and-let\">Values and Let</a> for binding rules.</li>\n<li><a href=\"/learn/book/core/functions-and-calls/functions\">Functions</a> for named parameters and calls.</li>\n<li><a href=\"/learn/book/data/modeling/data-definitions\">Data Definitions</a> for variant payloads.</li>\n<li><a href=\"/learn/book/effects-runtime/handling/effects\">Effects</a> for requested work.</li>\n</ul>\n",
		"summaryHtml": "Start from TypeScript habits and write the same ideas as Musi expressions."
	},
	{
		"locale": "en",
		"id": "musi-for-c-developers",
		"kind": "chapter",
		"parentId": "developers-guides",
		"depth": 2,
		"treePath": [
			"developers",
			"developers-guides",
			"musi-for-c-developers"
		],
		"childIds": [],
		"partId": "developers",
		"partTitle": "Musi for Developers",
		"sectionId": "developers-guides",
		"sectionTitle": "Language Guides",
		"path": "/learn/book/developers/guides/c",
		"canonicalPath": "/learn/book/developers/guides/c",
		"aliases": [
			"/docs/book/developers/guides/c",
			"/learn/book/developers/guides/c",
			"/docs/book/developers/c",
			"/learn/book/developers/c",
			"/docs/language/developers/c",
			"/learn/language/developers/c"
		],
		"questions": [],
		"title": "Musi for C Developers",
		"description": "Learn how C habits translate into Musi code.",
		"group": "Musi for Developers",
		"section": "Musi for Developers",
		"order": 1,
		"slug": "musi-for-c-developers",
		"summary": "Start from C habits and write the same ideas as Musi expressions.",
		"descriptionHtml": "Learn how C habits translate into Musi code.",
		"headings": [
			{
				"depth": 2,
				"id": "first-translation",
				"text": "First Translation"
			},
			{
				"depth": 2,
				"id": "habits-that-transfer",
				"text": "Habits That Transfer"
			},
			{
				"depth": 2,
				"id": "habits-to-relearn",
				"text": "Habits to Relearn"
			},
			{
				"depth": 2,
				"id": "where-to-go-next",
				"text": "Where to Go Next"
			}
		],
		"html": "<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> Ffi</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> :=</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> import</span><span style=\"color:#032563;--shiki-dark:#9ECBFF\"> \"@std/ffi\"</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">foreign</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> \"c\" </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> get_counter</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> () : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">CPtr</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> counter </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> unsafe </span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">{</span></span>\n<span class=\"line\"><span style=\"color:#023B95;--shiki-dark:#79B8FF\">  Ffi</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">.ptr.</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\">cast</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#702C00;--shiki-dark:#B392F0\">Int</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">](</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\">get_counter</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">());</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">};</span></span></code></pre></section></div><p>C developers often bring plain functions, structs, pointers, headers, and ABI boundaries.\nMusi keeps the useful part of that experience, but the code shape changes: Use safe Musi values first, then cross C with <code>foreign</code>, <code>unsafe</code>, and <code>@std/ffi</code> pointer views.</p>\n<h2 id=\"first-translation\"><a href=\"#first-translation\">First Translation</a></h2><p>Read the example as one small program.\nA <code>let</code> binding names a value or function.\nA block ends with the value it produces.\nA <code>match</code> expression returns a value from the selected arm.</p>\n<h2 id=\"habits-that-transfer\"><a href=\"#habits-that-transfer\">Habits That Transfer</a></h2><ul>\n<li>Name data by domain, not by container.</li>\n<li>Keep side effects at boundaries.</li>\n<li>Prefer small functions with explicit inputs.</li>\n<li>Put package boundaries where readers need names to stop leaking.</li>\n</ul>\n<h2 id=\"habits-to-relearn\"><a href=\"#habits-to-relearn\">Habits to Relearn</a></h2><ul>\n<li>Do not write a <code>return</code> keyword for the final value.</li>\n<li>Do not model every concept as an object.</li>\n<li>Do not hide boundary work; use <code>request</code>, <code>using</code>, handlers, or <code>unsafe</code> where the operation crosses a line.</li>\n</ul>\n<h2 id=\"where-to-go-next\"><a href=\"#where-to-go-next\">Where to Go Next</a></h2><ul>\n<li><a href=\"/learn/book/start/foundations/values-and-let\">Values and Let</a> for binding rules.</li>\n<li><a href=\"/learn/book/core/functions-and-calls/functions\">Functions</a> for named parameters and calls.</li>\n<li><a href=\"/learn/book/data/modeling/data-definitions\">Data Definitions</a> for variant payloads.</li>\n<li><a href=\"/learn/book/effects-runtime/handling/effects\">Effects</a> for requested work.</li>\n</ul>\n",
		"summaryHtml": "Start from C habits and write the same ideas as Musi expressions."
	},
	{
		"locale": "en",
		"id": "musi-for-cpp-developers",
		"kind": "chapter",
		"parentId": "developers-guides",
		"depth": 2,
		"treePath": [
			"developers",
			"developers-guides",
			"musi-for-cpp-developers"
		],
		"childIds": [],
		"partId": "developers",
		"partTitle": "Musi for Developers",
		"sectionId": "developers-guides",
		"sectionTitle": "Language Guides",
		"path": "/learn/book/developers/guides/cpp",
		"canonicalPath": "/learn/book/developers/guides/cpp",
		"aliases": [
			"/docs/book/developers/guides/cpp",
			"/learn/book/developers/guides/cpp",
			"/docs/book/developers/cpp",
			"/learn/book/developers/cpp",
			"/docs/language/developers/cpp",
			"/learn/language/developers/cpp"
		],
		"questions": [],
		"title": "Musi for C++ Developers",
		"description": "Learn how C++ habits translate into Musi code.",
		"group": "Musi for Developers",
		"section": "Musi for Developers",
		"order": 1,
		"slug": "musi-for-cpp-developers",
		"summary": "Start from C++ habits and write the same ideas as Musi expressions.",
		"descriptionHtml": "Learn how C++ habits translate into Musi code.",
		"headings": [
			{
				"depth": 2,
				"id": "first-translation",
				"text": "First Translation"
			},
			{
				"depth": 2,
				"id": "habits-that-transfer",
				"text": "Habits That Transfer"
			},
			{
				"depth": 2,
				"id": "habits-to-relearn",
				"text": "Habits to Relearn"
			},
			{
				"depth": 2,
				"id": "where-to-go-next",
				"text": "Where to Go Next"
			}
		],
		"html": "<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> identityFn</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#702C00;--shiki-dark:#B392F0\">T</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">] (input : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">T</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">) : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">T</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> :=</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> input;</span></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> port </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> identityFn</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#702C00;--shiki-dark:#B392F0\">Int</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">](</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">8080</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">);</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">port;</span></span></code></pre></section></div><p>C++ developers often bring RAII habits, templates, overloaded operators, and native interop.\nMusi keeps the useful part of that experience, but the code shape changes: Use explicit data shape, generic functions, classes/instances for behavior, and narrow unsafe FFI blocks.</p>\n<h2 id=\"first-translation\"><a href=\"#first-translation\">First Translation</a></h2><p>Read the example as one small program.\nA <code>let</code> binding names a value or function.\nA block ends with the value it produces.\nA <code>match</code> expression returns a value from the selected arm.</p>\n<h2 id=\"habits-that-transfer\"><a href=\"#habits-that-transfer\">Habits That Transfer</a></h2><ul>\n<li>Name data by domain, not by container.</li>\n<li>Keep side effects at boundaries.</li>\n<li>Prefer small functions with explicit inputs.</li>\n<li>Put package boundaries where readers need names to stop leaking.</li>\n</ul>\n<h2 id=\"habits-to-relearn\"><a href=\"#habits-to-relearn\">Habits to Relearn</a></h2><ul>\n<li>Do not write a <code>return</code> keyword for the final value.</li>\n<li>Do not model every concept as an object.</li>\n<li>Do not hide boundary work; use <code>request</code>, <code>using</code>, handlers, or <code>unsafe</code> where the operation crosses a line.</li>\n</ul>\n<h2 id=\"where-to-go-next\"><a href=\"#where-to-go-next\">Where to Go Next</a></h2><ul>\n<li><a href=\"/learn/book/start/foundations/values-and-let\">Values and Let</a> for binding rules.</li>\n<li><a href=\"/learn/book/core/functions-and-calls/functions\">Functions</a> for named parameters and calls.</li>\n<li><a href=\"/learn/book/data/modeling/data-definitions\">Data Definitions</a> for variant payloads.</li>\n<li><a href=\"/learn/book/effects-runtime/handling/effects\">Effects</a> for requested work.</li>\n</ul>\n",
		"summaryHtml": "Start from C++ habits and write the same ideas as Musi expressions."
	},
	{
		"locale": "en",
		"id": "musi-for-csharp-developers",
		"kind": "chapter",
		"parentId": "developers-guides",
		"depth": 2,
		"treePath": [
			"developers",
			"developers-guides",
			"musi-for-csharp-developers"
		],
		"childIds": [],
		"partId": "developers",
		"partTitle": "Musi for Developers",
		"sectionId": "developers-guides",
		"sectionTitle": "Language Guides",
		"path": "/learn/book/developers/guides/csharp",
		"canonicalPath": "/learn/book/developers/guides/csharp",
		"aliases": [
			"/docs/book/developers/guides/csharp",
			"/learn/book/developers/guides/csharp",
			"/docs/book/developers/csharp",
			"/learn/book/developers/csharp",
			"/docs/language/developers/csharp",
			"/learn/language/developers/csharp"
		],
		"questions": [],
		"title": "Musi for C# Developers",
		"description": "Learn how C# habits translate into Musi code.",
		"group": "Musi for Developers",
		"section": "Musi for Developers",
		"order": 1,
		"slug": "musi-for-csharp-developers",
		"summary": "Start from C# habits and write the same ideas as Musi expressions.",
		"descriptionHtml": "Learn how C# habits translate into Musi code.",
		"headings": [
			{
				"depth": 2,
				"id": "first-translation",
				"text": "First Translation"
			},
			{
				"depth": 2,
				"id": "habits-that-transfer",
				"text": "Habits That Transfer"
			},
			{
				"depth": 2,
				"id": "habits-to-relearn",
				"text": "Habits to Relearn"
			},
			{
				"depth": 2,
				"id": "where-to-go-next",
				"text": "Where to Go Next"
			}
		],
		"html": "<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> Pipeline</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> :=</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> import</span><span style=\"color:#032563;--shiki-dark:#9ECBFF\"> \"@std/iter\"</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> values </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> [</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">1</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">, </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">2</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">, </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">3</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">];</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">values;</span></span></code></pre></section></div><p>C# developers often bring properties, LINQ-style flow, generics, and async boundaries.\nMusi keeps the useful part of that experience, but the code shape changes: Use records for property-like shape, pipelines for data flow, generics for reusable functions, and effects for boundary calls.</p>\n<h2 id=\"first-translation\"><a href=\"#first-translation\">First Translation</a></h2><p>Read the example as one small program.\nA <code>let</code> binding names a value or function.\nA block ends with the value it produces.\nA <code>match</code> expression returns a value from the selected arm.</p>\n<h2 id=\"habits-that-transfer\"><a href=\"#habits-that-transfer\">Habits That Transfer</a></h2><ul>\n<li>Name data by domain, not by container.</li>\n<li>Keep side effects at boundaries.</li>\n<li>Prefer small functions with explicit inputs.</li>\n<li>Put package boundaries where readers need names to stop leaking.</li>\n</ul>\n<h2 id=\"habits-to-relearn\"><a href=\"#habits-to-relearn\">Habits to Relearn</a></h2><ul>\n<li>Do not write a <code>return</code> keyword for the final value.</li>\n<li>Do not model every concept as an object.</li>\n<li>Do not hide boundary work; use <code>request</code>, <code>using</code>, handlers, or <code>unsafe</code> where the operation crosses a line.</li>\n</ul>\n<h2 id=\"where-to-go-next\"><a href=\"#where-to-go-next\">Where to Go Next</a></h2><ul>\n<li><a href=\"/learn/book/start/foundations/values-and-let\">Values and Let</a> for binding rules.</li>\n<li><a href=\"/learn/book/core/functions-and-calls/functions\">Functions</a> for named parameters and calls.</li>\n<li><a href=\"/learn/book/data/modeling/data-definitions\">Data Definitions</a> for variant payloads.</li>\n<li><a href=\"/learn/book/effects-runtime/handling/effects\">Effects</a> for requested work.</li>\n</ul>\n",
		"summaryHtml": "Start from C# habits and write the same ideas as Musi expressions."
	},
	{
		"locale": "en",
		"id": "musi-for-go-developers",
		"kind": "chapter",
		"parentId": "developers-guides",
		"depth": 2,
		"treePath": [
			"developers",
			"developers-guides",
			"musi-for-go-developers"
		],
		"childIds": [],
		"partId": "developers",
		"partTitle": "Musi for Developers",
		"sectionId": "developers-guides",
		"sectionTitle": "Language Guides",
		"path": "/learn/book/developers/guides/go",
		"canonicalPath": "/learn/book/developers/guides/go",
		"aliases": [
			"/docs/book/developers/guides/go",
			"/learn/book/developers/guides/go",
			"/docs/book/developers/go",
			"/learn/book/developers/go",
			"/docs/language/developers/go",
			"/learn/language/developers/go"
		],
		"questions": [],
		"title": "Musi for Go Developers",
		"description": "Learn how Go habits translate into Musi code.",
		"group": "Musi for Developers",
		"section": "Musi for Developers",
		"order": 1,
		"slug": "musi-for-go-developers",
		"summary": "Start from Go habits and write the same ideas as Musi expressions.",
		"descriptionHtml": "Learn how Go habits translate into Musi code.",
		"headings": [
			{
				"depth": 2,
				"id": "first-translation",
				"text": "First Translation"
			},
			{
				"depth": 2,
				"id": "habits-that-transfer",
				"text": "Habits That Transfer"
			},
			{
				"depth": 2,
				"id": "habits-to-relearn",
				"text": "Habits to Relearn"
			},
			{
				"depth": 2,
				"id": "where-to-go-next",
				"text": "Where to Go Next"
			}
		],
		"html": "<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> ParseResult</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> :=</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> data</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> {</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">  | </span><span style=\"color:#702C00;--shiki-dark:#B392F0\">Ok</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(</span><span style=\"color:#702C00;--shiki-dark:#FFAB70\">port</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> : </span><span style=\"color:#702C00;--shiki-dark:#B392F0\">Int</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">)</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">  | </span><span style=\"color:#702C00;--shiki-dark:#B392F0\">Error</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(</span><span style=\"color:#702C00;--shiki-dark:#FFAB70\">message</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> : </span><span style=\"color:#702C00;--shiki-dark:#B392F0\">String</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">)</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">};</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> parsed </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> .Ok</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(port </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 8080</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">);</span></span></code></pre></section></div><p>Go developers often bring small packages, explicit errors, structs, and interfaces.\nMusi keeps the useful part of that experience, but the code shape changes: Use package imports, result-like data, records, and classes/instances where behavior needs a named contract.</p>\n<h2 id=\"first-translation\"><a href=\"#first-translation\">First Translation</a></h2><p>Read the example as one small program.\nA <code>let</code> binding names a value or function.\nA block ends with the value it produces.\nA <code>match</code> expression returns a value from the selected arm.</p>\n<h2 id=\"habits-that-transfer\"><a href=\"#habits-that-transfer\">Habits That Transfer</a></h2><ul>\n<li>Name data by domain, not by container.</li>\n<li>Keep side effects at boundaries.</li>\n<li>Prefer small functions with explicit inputs.</li>\n<li>Put package boundaries where readers need names to stop leaking.</li>\n</ul>\n<h2 id=\"habits-to-relearn\"><a href=\"#habits-to-relearn\">Habits to Relearn</a></h2><ul>\n<li>Do not write a <code>return</code> keyword for the final value.</li>\n<li>Do not model every concept as an object.</li>\n<li>Do not hide boundary work; use <code>request</code>, <code>using</code>, handlers, or <code>unsafe</code> where the operation crosses a line.</li>\n</ul>\n<h2 id=\"where-to-go-next\"><a href=\"#where-to-go-next\">Where to Go Next</a></h2><ul>\n<li><a href=\"/learn/book/start/foundations/values-and-let\">Values and Let</a> for binding rules.</li>\n<li><a href=\"/learn/book/core/functions-and-calls/functions\">Functions</a> for named parameters and calls.</li>\n<li><a href=\"/learn/book/data/modeling/data-definitions\">Data Definitions</a> for variant payloads.</li>\n<li><a href=\"/learn/book/effects-runtime/handling/effects\">Effects</a> for requested work.</li>\n</ul>\n",
		"summaryHtml": "Start from Go habits and write the same ideas as Musi expressions."
	},
	{
		"locale": "en",
		"id": "musi-for-rust-developers",
		"kind": "chapter",
		"parentId": "developers-rust",
		"depth": 3,
		"treePath": [
			"developers",
			"developers-guides",
			"developers-rust",
			"musi-for-rust-developers"
		],
		"childIds": [],
		"partId": "developers",
		"partTitle": "Musi for Developers",
		"sectionId": "developers-rust",
		"sectionTitle": "Musi for Rust Developers",
		"path": "/learn/book/developers/guides/rust/overview",
		"canonicalPath": "/learn/book/developers/guides/rust/overview",
		"aliases": [
			"/docs/book/developers/guides/rust/overview",
			"/learn/book/developers/guides/rust/overview",
			"/docs/book/developers/overview",
			"/learn/book/developers/overview",
			"/docs/book/developers/rust/overview",
			"/learn/book/developers/rust/overview",
			"/docs/language/developers/rust",
			"/learn/language/developers/rust"
		],
		"questions": [],
		"title": "Overview",
		"description": "Translate Rust 1.87.0 habits into Musi code with side-by-side examples.",
		"group": "Musi for Developers",
		"section": "Rust Developers",
		"order": 1,
		"slug": "overview",
		"summary": "Start from Rust habits, then read the equivalent Musi expression, data, abstraction, effect, and FFI shapes.",
		"descriptionHtml": "Translate Rust 1.87.0 habits into Musi code with side-by-side examples.",
		"headings": [
			{
				"depth": 2,
				"id": "first-translation",
				"text": "First translation"
			},
			{
				"depth": 2,
				"id": "reading-path",
				"text": "Reading path"
			},
			{
				"depth": 2,
				"id": "rust-habits-that-transfer",
				"text": "Rust habits that transfer"
			},
			{
				"depth": 2,
				"id": "rust-habits-to-translate",
				"text": "Rust habits to translate"
			}
		],
		"html": "<p>Rust 1.87.0 is the comparison point for this guide. Each page starts with Rust code, then shows the Musi equivalent with the same names and the same job.</p>\n<p>A Rust reader should not have to skim a generic Musi example and guess the translation. The Musi block below each Rust block is the translation.</p>\n<h2 id=\"first-translation\"><a href=\"#first-translation\">First translation</a></h2><p>Rust often starts with a small function and a binding for the result:</p>\n<pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">fn</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> total</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(base</span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:</span><span style=\"color:#702C00;--shiki-dark:#B392F0\"> i32</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">, fee</span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:</span><span style=\"color:#702C00;--shiki-dark:#B392F0\"> i32</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">) </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">-></span><span style=\"color:#702C00;--shiki-dark:#B392F0\"> i32</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> {</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">    base </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">+</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> fee</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">}</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> answer </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">=</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> total</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">1200</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">, </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">45</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">);</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">answer</span></span></code></pre><p>Musi writes the same calculation as a <code>let</code> function. The final expression leaves the value.</p>\n<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> total</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> (base : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">, fee : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">) : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> :=</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> base </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">+</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> fee;</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> answer </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> total</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">1200</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">, </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">45</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">);</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">answer;</span></span></code></pre></section></div><h2 id=\"reading-path\"><a href=\"#reading-path\">Reading path</a></h2><p>Read these pages in order if you are moving a Rust habit into Musi code:</p>\n<ul>\n<li><a href=\"/learn/book/developers/guides/rust/values-functions\">Values, Functions, and Final Expressions</a></li>\n<li><a href=\"/learn/book/developers/guides/rust/mutation\">Mutation</a></li>\n<li><a href=\"/learn/book/developers/guides/rust/records-structs\">Records, Structs, and Field Updates</a></li>\n<li><a href=\"/learn/book/developers/guides/rust/enums-data\">Enums, Data, and Pattern Matching</a></li>\n<li><a href=\"/learn/book/developers/guides/rust/traits-classes-laws\">Traits, Classes, Instances, and Laws</a></li>\n<li><a href=\"/learn/book/developers/guides/rust/generics\">Generics and Type Constructors</a></li>\n<li><a href=\"/learn/book/developers/guides/rust/results-effects\">Results, Requests, and Effects</a></li>\n<li><a href=\"/learn/book/developers/guides/rust/modules-packages\">Modules, Packages, and Visibility</a></li>\n<li><a href=\"/learn/book/developers/guides/rust/unsafe-ffi\">Unsafe and FFI</a></li>\n<li><a href=\"/learn/book/developers/guides/rust/testing-tooling\">Testing and Tooling</a></li>\n</ul>\n<h2 id=\"rust-habits-that-transfer\"><a href=\"#rust-habits-that-transfer\">Rust habits that transfer</a></h2><ul>\n<li>name data by domain</li>\n<li>keep functions small</li>\n<li>make boundary work visible</li>\n<li>use types to explain what values can be</li>\n<li>test behavior close to the code that owns it</li>\n</ul>\n<h2 id=\"rust-habits-to-translate\"><a href=\"#rust-habits-to-translate\">Rust habits to translate</a></h2><ul>\n<li><code>let mut name</code> becomes a binding to a mutable value</li>\n<li><code>return</code> is not used for ordinary final values</li>\n<li><code>enum</code> variants become <code>data</code> variants</li>\n<li><code>trait</code> and <code>impl</code> become <code>class</code>, <code>instance</code>, and laws when rules matter</li>\n<li><code>Result</code> handles recoverable data failure; effects handle requested outside work</li>\n<li>raw native work stays behind <code>foreign</code> and <code>unsafe</code></li>\n</ul>\n",
		"summaryHtml": "Start from Rust habits, then read the equivalent Musi expression, data, abstraction, effect, and FFI shapes."
	},
	{
		"locale": "en",
		"id": "rust-values-functions",
		"kind": "chapter",
		"parentId": "developers-rust",
		"depth": 3,
		"treePath": [
			"developers",
			"developers-guides",
			"developers-rust",
			"rust-values-functions"
		],
		"childIds": [],
		"partId": "developers",
		"partTitle": "Musi for Developers",
		"sectionId": "developers-rust",
		"sectionTitle": "Musi for Rust Developers",
		"path": "/learn/book/developers/guides/rust/values-functions",
		"canonicalPath": "/learn/book/developers/guides/rust/values-functions",
		"aliases": [
			"/docs/book/developers/guides/rust/values-functions",
			"/learn/book/developers/guides/rust/values-functions",
			"/docs/book/developers/values-functions",
			"/learn/book/developers/values-functions",
			"/docs/book/developers/rust/values-functions",
			"/learn/book/developers/rust/values-functions",
			"/docs/language/developers/rust/values-functions",
			"/learn/language/developers/rust/values-functions"
		],
		"questions": [],
		"title": "Values, Functions, and Final Expressions",
		"description": "Translate Rust bindings, functions, and final expressions into Musi.",
		"group": "Musi for Developers",
		"section": "Rust Developers",
		"order": 2,
		"slug": "values-functions",
		"summary": "Rust `let`, `fn`, and final-expression habits map to Musi `let` functions and expression results.",
		"descriptionHtml": "Translate Rust bindings, functions, and final expressions into Musi.",
		"headings": [
			{
				"depth": 2,
				"id": "named-arguments",
				"text": "Named arguments"
			}
		],
		"html": "<p>Rust functions commonly end with an expression instead of an explicit <code>return</code>:</p>\n<pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">fn</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> total</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(base</span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:</span><span style=\"color:#702C00;--shiki-dark:#B392F0\"> i32</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">, fee</span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:</span><span style=\"color:#702C00;--shiki-dark:#B392F0\"> i32</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">) </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">-></span><span style=\"color:#702C00;--shiki-dark:#B392F0\"> i32</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> {</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">    base </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">+</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> fee</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">}</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> answer </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">=</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> total</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">1200</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">, </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">45</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">);</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">answer</span></span></code></pre><p>Musi uses the same expression-first idea, but function definitions are <code>let</code> bindings with parameters.</p>\n<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> total</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> (base : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">, fee : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">) : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> :=</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> base </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">+</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> fee;</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> answer </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> total</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">1200</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">, </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">45</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">);</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">answer;</span></span></code></pre></section></div><p><code>total</code> is a name bound to a function. <code>answer</code> is a name bound to the call result. The last expression is the value of the block or file.</p>\n<h2 id=\"named-arguments\"><a href=\"#named-arguments\">Named arguments</a></h2><p>Rust readers often use struct fields or builder methods when positional calls become unclear. Musi can name call arguments directly:</p>\n<pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">fn</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> render</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(port</span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:</span><span style=\"color:#702C00;--shiki-dark:#B392F0\"> u16</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">, secure</span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:</span><span style=\"color:#702C00;--shiki-dark:#B392F0\"> bool</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">) </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">-></span><span style=\"color:#702C00;--shiki-dark:#B392F0\"> u16</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> {</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">    port</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">}</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> selected </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">=</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> render</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">8080</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">, </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">true</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">);</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">selected</span></span></code></pre><p>The Musi equivalent can keep positional order or spell labels at the call site.</p>\n<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> render</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> (port : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">, secure : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">Bool</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">) : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> :=</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> port;</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> selected </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> render</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(port </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 8080</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">, secure </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 0</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> =</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 0</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">);</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">selected;</span></span></code></pre></section></div><p>Use named arguments when two values have the same type or when swapped order would be easy to miss.</p>\n",
		"summaryHtml": "Rust <code>let</code>, <code>fn</code>, and final-expression habits map to Musi <code>let</code> functions and expression results."
	},
	{
		"locale": "en",
		"id": "rust-mutation",
		"kind": "chapter",
		"parentId": "developers-rust",
		"depth": 3,
		"treePath": [
			"developers",
			"developers-guides",
			"developers-rust",
			"rust-mutation"
		],
		"childIds": [],
		"partId": "developers",
		"partTitle": "Musi for Developers",
		"sectionId": "developers-rust",
		"sectionTitle": "Musi for Rust Developers",
		"path": "/learn/book/developers/guides/rust/mutation",
		"canonicalPath": "/learn/book/developers/guides/rust/mutation",
		"aliases": [
			"/docs/book/developers/guides/rust/mutation",
			"/learn/book/developers/guides/rust/mutation",
			"/docs/book/developers/mutation",
			"/learn/book/developers/mutation",
			"/docs/book/developers/rust/mutation",
			"/learn/book/developers/rust/mutation",
			"/docs/language/developers/rust/mutation",
			"/learn/language/developers/rust/mutation"
		],
		"questions": [],
		"title": "Mutation",
		"description": "Translate Rust 1.87.0 mutation habits into Musi's value-based mutation model.",
		"group": "Musi for Developers",
		"section": "Rust Developers",
		"order": 3,
		"slug": "mutation",
		"summary": "Read Musi mutation as a mutable value binding, not as a variable-mode prefix.",
		"descriptionHtml": "Translate Rust 1.87.0 mutation habits into Musi&#39;s value-based mutation model.",
		"headings": [
			{
				"depth": 2,
				"id": "when-to-translate-let-mut",
				"text": "When to translate let mut"
			}
		],
		"html": "<p>Rust marks the binding mutable:</p>\n<pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> mut</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> visits </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">=</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 0</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">visits </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">+=</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 1</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">visits</span></span></code></pre><p>Musi keeps mutability on the value owned by the binding.</p>\n<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> visits </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> mut</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 0</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">visits </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> visits </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">+</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 1</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">visits;</span></span></code></pre></section></div><p>Read <code>let visits := mut 0;</code> as a normal name bound to a value that may change. The update writes the next value back into that mutable value.</p>\n<h2 id=\"when-to-translate-let-mut\"><a href=\"#when-to-translate-let-mut\">When to translate <code>let mut</code></a></h2><p>Use Musi mutation for Rust code where the same local state changes over time:</p>\n<ul>\n<li>counters</li>\n<li>parser cursors</li>\n<li>reusable buffers</li>\n<li>local totals built over several steps</li>\n</ul>\n<p>If Rust uses <code>mut</code> only to stage a clearer next value, Musi often reads better with a new <code>let</code> name.</p>\n<pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> base </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">=</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 1200</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> total </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">=</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> base </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">+</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 45</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">total</span></span></code></pre><div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> base </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 1200</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> total </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> base </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">+</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 45</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">total;</span></span></code></pre></section></div>\n",
		"summaryHtml": "Read Musi mutation as a mutable value binding, not as a variable-mode prefix."
	},
	{
		"locale": "en",
		"id": "rust-records-structs",
		"kind": "chapter",
		"parentId": "developers-rust",
		"depth": 3,
		"treePath": [
			"developers",
			"developers-guides",
			"developers-rust",
			"rust-records-structs"
		],
		"childIds": [],
		"partId": "developers",
		"partTitle": "Musi for Developers",
		"sectionId": "developers-rust",
		"sectionTitle": "Musi for Rust Developers",
		"path": "/learn/book/developers/guides/rust/records-structs",
		"canonicalPath": "/learn/book/developers/guides/rust/records-structs",
		"aliases": [
			"/docs/book/developers/guides/rust/records-structs",
			"/learn/book/developers/guides/rust/records-structs",
			"/docs/book/developers/records-structs",
			"/learn/book/developers/records-structs",
			"/docs/book/developers/rust/records-structs",
			"/learn/book/developers/rust/records-structs",
			"/docs/language/developers/rust/records-structs",
			"/learn/language/developers/rust/records-structs"
		],
		"questions": [],
		"title": "Records, Structs, and Field Updates",
		"description": "Translate Rust structs and update syntax into Musi records.",
		"group": "Musi for Developers",
		"section": "Rust Developers",
		"order": 4,
		"slug": "records-structs",
		"summary": "Rust structs map to named Musi record shapes and spread updates.",
		"descriptionHtml": "Translate Rust structs and update syntax into Musi records.",
		"headings": [],
		"html": "<p>Rust structs group named fields:</p>\n<pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">struct</span><span style=\"color:#702C00;--shiki-dark:#B392F0\"> Endpoint</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> {</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">    host</span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:</span><span style=\"color:#702C00;--shiki-dark:#B392F0\"> String</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">,</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">    port</span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:</span><span style=\"color:#702C00;--shiki-dark:#B392F0\"> u16</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">,</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">    secure</span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:</span><span style=\"color:#702C00;--shiki-dark:#B392F0\"> bool</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">,</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">}</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> local </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">=</span><span style=\"color:#702C00;--shiki-dark:#B392F0\"> Endpoint</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> {</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">    host</span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:</span><span style=\"color:#702C00;--shiki-dark:#B392F0\"> String</span><span style=\"color:#A0111F;--shiki-dark:#F97583\">::</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\">from</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(</span><span style=\"color:#032563;--shiki-dark:#9ECBFF\">\"localhost\"</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">),</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">    port</span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 8080</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">,</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">    secure</span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> false</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">,</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">};</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> secure </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">=</span><span style=\"color:#702C00;--shiki-dark:#B392F0\"> Endpoint</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> {</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">    secure</span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> true</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">,</span></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">    ..</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">local</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">};</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">secure</span><span style=\"color:#A0111F;--shiki-dark:#F97583\">.</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">port</span></span></code></pre><p>Musi uses record-shaped data for the field contract and record literals for values.</p>\n<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> Endpoint</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> :=</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> data</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> {</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">  host : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">String</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">  port : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">  secure : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">Bool</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">};</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> local </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> {</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">  host </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#032563;--shiki-dark:#9ECBFF\"> \"localhost\"</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">,</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">  port </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 8080</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">,</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">  secure </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 0</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> =</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 1</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">};</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> secure </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> { </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">...</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">local, secure </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 0</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> =</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 0</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> };</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">secure.port;</span></span></code></pre></section></div><p>The spread update starts from <code>local</code>, then replaces only <code>secure</code>. Field access stays direct: <code>secure.port</code>.</p>\n",
		"summaryHtml": "Rust structs map to named Musi record shapes and spread updates."
	},
	{
		"locale": "en",
		"id": "rust-enums-data",
		"kind": "chapter",
		"parentId": "developers-rust",
		"depth": 3,
		"treePath": [
			"developers",
			"developers-guides",
			"developers-rust",
			"rust-enums-data"
		],
		"childIds": [],
		"partId": "developers",
		"partTitle": "Musi for Developers",
		"sectionId": "developers-rust",
		"sectionTitle": "Musi for Rust Developers",
		"path": "/learn/book/developers/guides/rust/enums-data",
		"canonicalPath": "/learn/book/developers/guides/rust/enums-data",
		"aliases": [
			"/docs/book/developers/guides/rust/enums-data",
			"/learn/book/developers/guides/rust/enums-data",
			"/docs/book/developers/enums-data",
			"/learn/book/developers/enums-data",
			"/docs/book/developers/rust/enums-data",
			"/learn/book/developers/rust/enums-data",
			"/docs/language/developers/rust/enums-data",
			"/learn/language/developers/rust/enums-data"
		],
		"questions": [],
		"title": "Enums, Data, and Pattern Matching",
		"description": "Translate Rust enums with payloads into Musi data variants and matches.",
		"group": "Musi for Developers",
		"section": "Rust Developers",
		"order": 5,
		"slug": "enums-data",
		"summary": "Rust enum variants map to Musi `data` variants with named payloads and `match` arms.",
		"descriptionHtml": "Translate Rust enums with payloads into Musi data variants and matches.",
		"headings": [],
		"html": "<p>Rust enum variants can carry named payloads:</p>\n<pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">enum</span><span style=\"color:#702C00;--shiki-dark:#B392F0\"> Port</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> {</span></span>\n<span class=\"line\"><span style=\"color:#702C00;--shiki-dark:#B392F0\">    Configured</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> { port</span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:</span><span style=\"color:#702C00;--shiki-dark:#B392F0\"> u16</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> },</span></span>\n<span class=\"line\"><span style=\"color:#702C00;--shiki-dark:#B392F0\">    Default</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">,</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">}</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> selected </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">=</span><span style=\"color:#702C00;--shiki-dark:#B392F0\"> Port</span><span style=\"color:#A0111F;--shiki-dark:#F97583\">::</span><span style=\"color:#702C00;--shiki-dark:#B392F0\">Configured</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> { port</span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 8080</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> };</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">match</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> selected {</span></span>\n<span class=\"line\"><span style=\"color:#702C00;--shiki-dark:#B392F0\">    Port</span><span style=\"color:#A0111F;--shiki-dark:#F97583\">::</span><span style=\"color:#702C00;--shiki-dark:#B392F0\">Configured</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> { port } </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">=></span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> port,</span></span>\n<span class=\"line\"><span style=\"color:#702C00;--shiki-dark:#B392F0\">    Port</span><span style=\"color:#A0111F;--shiki-dark:#F97583\">::</span><span style=\"color:#702C00;--shiki-dark:#B392F0\">Default</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> =></span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 3000</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">,</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">}</span></span></code></pre><p>Musi puts payload labels inside constructor-style variant declarations.</p>\n<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> Port</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> :=</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> data</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> {</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">  | </span><span style=\"color:#702C00;--shiki-dark:#B392F0\">Configured</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(</span><span style=\"color:#702C00;--shiki-dark:#FFAB70\">port</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> : </span><span style=\"color:#702C00;--shiki-dark:#B392F0\">Int</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">)</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">  | </span><span style=\"color:#702C00;--shiki-dark:#B392F0\">Default</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">};</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> selected : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">Port</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> :=</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> .Configured</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(port </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 8080</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">);</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">match</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> selected</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> (</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">| </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">.Configured</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(port) </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">=></span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> port</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">| </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">.Default</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> =></span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 3000</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">);</span></span></code></pre></section></div><p>Constructor calls use named payloads. Match arms read the value back through the variant shape.</p>\n",
		"summaryHtml": "Rust enum variants map to Musi <code>data</code> variants with named payloads and <code>match</code> arms."
	},
	{
		"locale": "en",
		"id": "rust-traits-classes-laws",
		"kind": "chapter",
		"parentId": "developers-rust",
		"depth": 3,
		"treePath": [
			"developers",
			"developers-guides",
			"developers-rust",
			"rust-traits-classes-laws"
		],
		"childIds": [],
		"partId": "developers",
		"partTitle": "Musi for Developers",
		"sectionId": "developers-rust",
		"sectionTitle": "Musi for Rust Developers",
		"path": "/learn/book/developers/guides/rust/traits-classes-laws",
		"canonicalPath": "/learn/book/developers/guides/rust/traits-classes-laws",
		"aliases": [
			"/docs/book/developers/guides/rust/traits-classes-laws",
			"/learn/book/developers/guides/rust/traits-classes-laws",
			"/docs/book/developers/traits-classes-laws",
			"/learn/book/developers/traits-classes-laws",
			"/docs/book/developers/rust/traits-classes-laws",
			"/learn/book/developers/rust/traits-classes-laws",
			"/docs/language/developers/rust/traits-classes-laws",
			"/learn/language/developers/rust/traits-classes-laws"
		],
		"questions": [],
		"title": "Traits, Classes, Instances, and Laws",
		"description": "Translate Rust traits and impls into Musi classes, instances, and laws.",
		"group": "Musi for Developers",
		"section": "Rust Developers",
		"order": 6,
		"slug": "traits-classes-laws",
		"summary": "Rust trait contracts map to Musi classes, instances, and explicit laws when behavior has rules.",
		"descriptionHtml": "Translate Rust traits and impls into Musi classes, instances, and laws.",
		"headings": [],
		"html": "<p>Rust traits name shared behavior. Implementations attach that behavior to a type:</p>\n<pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">trait</span><span style=\"color:#702C00;--shiki-dark:#B392F0\"> Vehicle</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> {</span></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">    fn</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> wheels</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(</span><span style=\"color:#A0111F;--shiki-dark:#F97583\">&#x26;</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">self</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">) </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">-></span><span style=\"color:#702C00;--shiki-dark:#B392F0\"> usize</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">    fn</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> at_least_four_wheels</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(</span><span style=\"color:#A0111F;--shiki-dark:#F97583\">&#x26;</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">self</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">) </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">-></span><span style=\"color:#702C00;--shiki-dark:#B392F0\"> bool</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> {</span></span>\n<span class=\"line\"><span style=\"color:#023B95;--shiki-dark:#79B8FF\">        self</span><span style=\"color:#A0111F;--shiki-dark:#F97583\">.</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\">wheels</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">() </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">>=</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 4</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">    }</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">}</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">struct</span><span style=\"color:#702C00;--shiki-dark:#B392F0\"> Car</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">impl</span><span style=\"color:#702C00;--shiki-dark:#B392F0\"> Vehicle</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> for</span><span style=\"color:#702C00;--shiki-dark:#B392F0\"> Car</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> {</span></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">    fn</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> wheels</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(</span><span style=\"color:#A0111F;--shiki-dark:#F97583\">&#x26;</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">self</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">) </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">-></span><span style=\"color:#702C00;--shiki-dark:#B392F0\"> usize</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> {</span></span>\n<span class=\"line\"><span style=\"color:#023B95;--shiki-dark:#79B8FF\">        4</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">    }</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">}</span></span></code></pre><p>Musi separates the shape from the instance and writes the rule as a law.</p>\n<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> Vehicle</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#702C00;--shiki-dark:#B392F0\">T</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">] </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> class</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> {</span></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">  let</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> wheels</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(self : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">T</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">) : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">  law</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> atLeastFourWheels</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(vehicle : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">T</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">) </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> vehicle.</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\">wheels</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">() </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">>=</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 4</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">};</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> Car</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> :=</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> data</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> {</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">  | </span><span style=\"color:#702C00;--shiki-dark:#B392F0\">Car</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">};</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> carVehicle </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> instance</span><span style=\"color:#702C00;--shiki-dark:#B392F0\"> Vehicle</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">Car</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">] {</span></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">  let</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> wheels</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(self : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">Car</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">) : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> :=</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 4</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">};</span></span></code></pre></section></div><p>A car is a vehicle, but the law says which vehicle instances count as valid in this model. The class names behavior. The instance provides behavior. The law names the expectation readers can rely on.</p>\n",
		"summaryHtml": "Rust trait contracts map to Musi classes, instances, and explicit laws when behavior has rules."
	},
	{
		"locale": "en",
		"id": "rust-generics",
		"kind": "chapter",
		"parentId": "developers-rust",
		"depth": 3,
		"treePath": [
			"developers",
			"developers-guides",
			"developers-rust",
			"rust-generics"
		],
		"childIds": [],
		"partId": "developers",
		"partTitle": "Musi for Developers",
		"sectionId": "developers-rust",
		"sectionTitle": "Musi for Rust Developers",
		"path": "/learn/book/developers/guides/rust/generics",
		"canonicalPath": "/learn/book/developers/guides/rust/generics",
		"aliases": [
			"/docs/book/developers/guides/rust/generics",
			"/learn/book/developers/guides/rust/generics",
			"/docs/book/developers/generics",
			"/learn/book/developers/generics",
			"/docs/book/developers/rust/generics",
			"/learn/book/developers/rust/generics",
			"/docs/language/developers/rust/generics",
			"/learn/language/developers/rust/generics"
		],
		"questions": [],
		"title": "Generics and Type Constructors",
		"description": "Translate Rust generic functions and generic data into Musi type parameters.",
		"group": "Musi for Developers",
		"section": "Rust Developers",
		"order": 7,
		"slug": "generics",
		"summary": "Rust generic functions and structs map to Musi bracketed type parameters and constructor-style data.",
		"descriptionHtml": "Translate Rust generic functions and generic data into Musi type parameters.",
		"headings": [],
		"html": "<p>Rust generic functions name the type parameter before the function parameters:</p>\n<pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">fn</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> identity</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">&#x3C;</span><span style=\"color:#702C00;--shiki-dark:#B392F0\">T</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">>(input</span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:</span><span style=\"color:#702C00;--shiki-dark:#B392F0\"> T</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">) </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">-></span><span style=\"color:#702C00;--shiki-dark:#B392F0\"> T</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> {</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">    input</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">}</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> port </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">=</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> identity</span><span style=\"color:#A0111F;--shiki-dark:#F97583\">::</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">&#x3C;</span><span style=\"color:#702C00;--shiki-dark:#B392F0\">u16</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">>(</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">8080</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">);</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">port</span></span></code></pre><p>Musi uses bracketed type parameters on the <code>let</code> function.</p>\n<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> identity</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#702C00;--shiki-dark:#B392F0\">T</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">] (input : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">T</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">) : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">T</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> :=</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> input;</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> port </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> identity</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#702C00;--shiki-dark:#B392F0\">Int</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">](</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">8080</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">);</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">port;</span></span></code></pre></section></div><p>Generic data follows the same bracketed shape:</p>\n<pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">struct</span><span style=\"color:#702C00;--shiki-dark:#B392F0\"> Box1</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">&#x3C;</span><span style=\"color:#702C00;--shiki-dark:#B392F0\">T</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">> {</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">    value</span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:</span><span style=\"color:#702C00;--shiki-dark:#B392F0\"> T</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">,</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">}</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> boxed </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">=</span><span style=\"color:#702C00;--shiki-dark:#B392F0\"> Box1</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> { value</span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 8080</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> };</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">boxed</span><span style=\"color:#A0111F;--shiki-dark:#F97583\">.</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">value</span></span></code></pre><div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> Box1</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#702C00;--shiki-dark:#B392F0\">T</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">] </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> data</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> {</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">  | </span><span style=\"color:#702C00;--shiki-dark:#B392F0\">Box1</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(</span><span style=\"color:#702C00;--shiki-dark:#FFAB70\">value</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> : </span><span style=\"color:#702C00;--shiki-dark:#B392F0\">T</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">)</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">};</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> boxed </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> .Box1</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(value </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 8080</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">);</span></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">match</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> boxed</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> (</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">| </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">.Box1</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(value) </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">=></span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> value</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">);</span></span></code></pre></section></div><p>The Musi match reads the payload back out through the constructor shape.</p>\n<p>When a class receives a type constructor, Musi can name that constructor in the type parameter list.</p>\n<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> Box1</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#702C00;--shiki-dark:#B392F0\">T</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">] </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> data</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> {</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">  | </span><span style=\"color:#702C00;--shiki-dark:#B392F0\">Box1</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(</span><span style=\"color:#702C00;--shiki-dark:#FFAB70\">value</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> : </span><span style=\"color:#702C00;--shiki-dark:#B392F0\">T</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">)</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">};</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> Keeps</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#702C00;--shiki-dark:#B392F0\">F</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> : </span><span style=\"color:#702C00;--shiki-dark:#B392F0\">Type</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> -></span><span style=\"color:#702C00;--shiki-dark:#B392F0\"> Type</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">] </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> class</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> {</span></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">  let</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> keep</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(value : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">F</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#702C00;--shiki-dark:#B392F0\">Int</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">]) : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">F</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#702C00;--shiki-dark:#B392F0\">Int</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">];</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">};</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> boxKeeps </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> instance</span><span style=\"color:#702C00;--shiki-dark:#B392F0\"> Keeps</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">Box1</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">] {</span></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">  let</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> keep</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(value : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">Box1</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#702C00;--shiki-dark:#B392F0\">Int</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">]) : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">Box1</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#702C00;--shiki-dark:#B392F0\">Int</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">] </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> value;</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">};</span></span></code></pre></section></div>\n",
		"summaryHtml": "Rust generic functions and structs map to Musi bracketed type parameters and constructor-style data."
	},
	{
		"locale": "en",
		"id": "rust-results-effects",
		"kind": "chapter",
		"parentId": "developers-rust",
		"depth": 3,
		"treePath": [
			"developers",
			"developers-guides",
			"developers-rust",
			"rust-results-effects"
		],
		"childIds": [],
		"partId": "developers",
		"partTitle": "Musi for Developers",
		"sectionId": "developers-rust",
		"sectionTitle": "Musi for Rust Developers",
		"path": "/learn/book/developers/guides/rust/results-effects",
		"canonicalPath": "/learn/book/developers/guides/rust/results-effects",
		"aliases": [
			"/docs/book/developers/guides/rust/results-effects",
			"/learn/book/developers/guides/rust/results-effects",
			"/docs/book/developers/results-effects",
			"/learn/book/developers/results-effects",
			"/docs/book/developers/rust/results-effects",
			"/learn/book/developers/rust/results-effects",
			"/docs/language/developers/rust/results-effects",
			"/learn/language/developers/rust/results-effects"
		],
		"questions": [],
		"title": "Results, Requests, and Effects",
		"description": "Translate Rust `Result` and I/O boundary habits into Musi data and effects.",
		"group": "Musi for Developers",
		"section": "Rust Developers",
		"order": 8,
		"slug": "results-effects",
		"summary": "Rust `Result` maps to data for recoverable failure; outside work maps to effect requests.",
		"descriptionHtml": "Translate Rust <code>Result</code> and I/O boundary habits into Musi data and effects.",
		"headings": [],
		"html": "<p>Rust often uses <code>Result</code> for recoverable failure:</p>\n<pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">fn</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> read_port</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(text</span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> &#x26;</span><span style=\"color:#702C00;--shiki-dark:#B392F0\">str</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">) </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">-></span><span style=\"color:#702C00;--shiki-dark:#B392F0\"> Result</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">&#x3C;</span><span style=\"color:#702C00;--shiki-dark:#B392F0\">u16</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">, </span><span style=\"color:#702C00;--shiki-dark:#B392F0\">String</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">> {</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">    text</span><span style=\"color:#A0111F;--shiki-dark:#F97583\">.</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\">parse</span><span style=\"color:#A0111F;--shiki-dark:#F97583\">::</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">&#x3C;</span><span style=\"color:#702C00;--shiki-dark:#B392F0\">u16</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">>()</span><span style=\"color:#A0111F;--shiki-dark:#F97583\">.</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\">map_err</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(</span><span style=\"color:#A0111F;--shiki-dark:#F97583\">|</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">_</span><span style=\"color:#A0111F;--shiki-dark:#F97583\">|</span><span style=\"color:#702C00;--shiki-dark:#B392F0\"> String</span><span style=\"color:#A0111F;--shiki-dark:#F97583\">::</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\">from</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(</span><span style=\"color:#032563;--shiki-dark:#9ECBFF\">\"bad port\"</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">))</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">}</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> parsed </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">=</span><span style=\"color:#702C00;--shiki-dark:#B392F0\"> Ok</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">8080</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">);</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">parsed</span></span></code></pre><p>Musi can model that same recoverable shape as data.</p>\n<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> RustResult</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#702C00;--shiki-dark:#B392F0\">T</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">, </span><span style=\"color:#702C00;--shiki-dark:#B392F0\">E</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">] </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> data</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> {</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">  | </span><span style=\"color:#702C00;--shiki-dark:#B392F0\">Ok</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(</span><span style=\"color:#702C00;--shiki-dark:#FFAB70\">value</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> : </span><span style=\"color:#702C00;--shiki-dark:#B392F0\">T</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">)</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">  | </span><span style=\"color:#702C00;--shiki-dark:#B392F0\">Error</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(</span><span style=\"color:#702C00;--shiki-dark:#FFAB70\">error</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> : </span><span style=\"color:#702C00;--shiki-dark:#B392F0\">E</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">)</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">};</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> parsed </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> .Ok</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(value </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 8080</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">);</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">parsed;</span></span></code></pre></section></div><p>That is ordinary data: a value is either <code>Ok</code> or <code>Error</code>.</p>\n<p>Rust also uses <code>Result</code> for outside work such as reading from standard input:</p>\n<pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">fn</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> read_line</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">() </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">-></span><span style=\"color:#702C00;--shiki-dark:#B392F0\"> std</span><span style=\"color:#A0111F;--shiki-dark:#F97583\">::</span><span style=\"color:#702C00;--shiki-dark:#B392F0\">result</span><span style=\"color:#A0111F;--shiki-dark:#F97583\">::</span><span style=\"color:#702C00;--shiki-dark:#B392F0\">Result</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">&#x3C;</span><span style=\"color:#702C00;--shiki-dark:#B392F0\">String</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">, std</span><span style=\"color:#A0111F;--shiki-dark:#F97583\">::</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">io</span><span style=\"color:#A0111F;--shiki-dark:#F97583\">::</span><span style=\"color:#702C00;--shiki-dark:#B392F0\">Error</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">> {</span></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">    let</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> mut</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> line </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">=</span><span style=\"color:#702C00;--shiki-dark:#B392F0\"> String</span><span style=\"color:#A0111F;--shiki-dark:#F97583\">::</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\">new</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">();</span></span>\n<span class=\"line\"><span style=\"color:#702C00;--shiki-dark:#B392F0\">    std</span><span style=\"color:#A0111F;--shiki-dark:#F97583\">::</span><span style=\"color:#702C00;--shiki-dark:#B392F0\">io</span><span style=\"color:#A0111F;--shiki-dark:#F97583\">::</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\">stdin</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">()</span><span style=\"color:#A0111F;--shiki-dark:#F97583\">.</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\">read_line</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(</span><span style=\"color:#A0111F;--shiki-dark:#F97583\">&#x26;mut</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> line)</span><span style=\"color:#A0111F;--shiki-dark:#F97583\">?</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#702C00;--shiki-dark:#B392F0\">    Ok</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(line)</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">}</span></span></code></pre><p>Musi writes outside work as a request to an effect.</p>\n<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> Console</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> :=</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> effect</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> {</span></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">  let</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> readLine</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> () : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">String</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">};</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> readLine</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> () : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">String</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> using</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> { </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">Console</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> } </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">  request</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> Console</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">.</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\">readLine</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">();</span></span></code></pre></section></div><p>Use data results when the value itself can be one of several outcomes. Use effects when the computation asks an outside interpreter to do work.</p>\n",
		"summaryHtml": "Rust <code>Result</code> maps to data for recoverable failure; outside work maps to effect requests."
	},
	{
		"locale": "en",
		"id": "rust-modules-packages",
		"kind": "chapter",
		"parentId": "developers-rust",
		"depth": 3,
		"treePath": [
			"developers",
			"developers-guides",
			"developers-rust",
			"rust-modules-packages"
		],
		"childIds": [],
		"partId": "developers",
		"partTitle": "Musi for Developers",
		"sectionId": "developers-rust",
		"sectionTitle": "Musi for Rust Developers",
		"path": "/learn/book/developers/guides/rust/modules-packages",
		"canonicalPath": "/learn/book/developers/guides/rust/modules-packages",
		"aliases": [
			"/docs/book/developers/guides/rust/modules-packages",
			"/learn/book/developers/guides/rust/modules-packages",
			"/docs/book/developers/modules-packages",
			"/learn/book/developers/modules-packages",
			"/docs/book/developers/rust/modules-packages",
			"/learn/book/developers/rust/modules-packages",
			"/docs/language/developers/rust/modules-packages",
			"/learn/language/developers/rust/modules-packages"
		],
		"questions": [],
		"title": "Modules, Packages, and Visibility",
		"description": "Translate Rust module exports and package calls into Musi imports and exports.",
		"group": "Musi for Developers",
		"section": "Rust Developers",
		"order": 9,
		"slug": "modules-packages",
		"summary": "Rust `pub mod` habits map to Musi exported values and imported packages.",
		"descriptionHtml": "Translate Rust module exports and package calls into Musi imports and exports.",
		"headings": [],
		"html": "<p>Rust module code often exposes a small public function:</p>\n<pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">pub</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> mod</span><span style=\"color:#702C00;--shiki-dark:#B392F0\"> ports</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> {</span></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">    pub</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> fn</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> default_port</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">() </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">-></span><span style=\"color:#702C00;--shiki-dark:#B392F0\"> u16</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> {</span></span>\n<span class=\"line\"><span style=\"color:#023B95;--shiki-dark:#79B8FF\">        8080</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">    }</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">}</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> port </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">=</span><span style=\"color:#702C00;--shiki-dark:#B392F0\"> ports</span><span style=\"color:#A0111F;--shiki-dark:#F97583\">::</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\">default_port</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">();</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">port</span></span></code></pre><p>Musi exports names from a file or package and imports that package where the names are needed.</p>\n<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">export</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> let</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> defaultPort</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> () : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> :=</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 8080</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span></code></pre></section></div><p>A consumer reads the exported name through the imported package value.</p>\n<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> Ports</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> :=</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> import</span><span style=\"color:#032563;--shiki-dark:#9ECBFF\"> \"./ports\"</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> port </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> Ports</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">.</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\">defaultPort</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">();</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">port;</span></span></code></pre></section></div><p>Keep exports small. Export names a reader should depend on; leave local helpers unexported.</p>\n",
		"summaryHtml": "Rust <code>pub mod</code> habits map to Musi exported values and imported packages."
	},
	{
		"locale": "en",
		"id": "rust-unsafe-ffi",
		"kind": "chapter",
		"parentId": "developers-rust",
		"depth": 3,
		"treePath": [
			"developers",
			"developers-guides",
			"developers-rust",
			"rust-unsafe-ffi"
		],
		"childIds": [],
		"partId": "developers",
		"partTitle": "Musi for Developers",
		"sectionId": "developers-rust",
		"sectionTitle": "Musi for Rust Developers",
		"path": "/learn/book/developers/guides/rust/unsafe-ffi",
		"canonicalPath": "/learn/book/developers/guides/rust/unsafe-ffi",
		"aliases": [
			"/docs/book/developers/guides/rust/unsafe-ffi",
			"/learn/book/developers/guides/rust/unsafe-ffi",
			"/docs/book/developers/unsafe-ffi",
			"/learn/book/developers/unsafe-ffi",
			"/docs/book/developers/rust/unsafe-ffi",
			"/learn/book/developers/rust/unsafe-ffi",
			"/docs/language/developers/rust/unsafe-ffi",
			"/learn/language/developers/rust/unsafe-ffi"
		],
		"questions": [],
		"title": "Unsafe and FFI",
		"description": "Translate Rust unsafe and extern C habits into Musi foreign declarations and unsafe blocks.",
		"group": "Musi for Developers",
		"section": "Rust Developers",
		"order": 10,
		"slug": "unsafe-ffi",
		"summary": "Rust `extern` and raw pointer work maps to Musi `foreign`, `CPtr`, `@std/ffi`, and `unsafe`.",
		"descriptionHtml": "Translate Rust unsafe and extern C habits into Musi foreign declarations and unsafe blocks.",
		"headings": [],
		"html": "<p>Rust marks C ABI declarations and raw pointer calls explicitly:</p>\n<pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">unsafe</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> extern</span><span style=\"color:#032563;--shiki-dark:#9ECBFF\"> \"C\"</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> {</span></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">    fn</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> get_counter</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">() </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">-></span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> *mut</span><span style=\"color:#702C00;--shiki-dark:#B392F0\"> i32</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">}</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> counter </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">=</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> unsafe</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> { </span><span style=\"color:#622CBC;--shiki-dark:#B392F0\">get_counter</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">() };</span></span></code></pre><p>Musi keeps the same boundary visible with <code>foreign</code> and <code>unsafe</code>.</p>\n<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> Ffi</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> :=</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> import</span><span style=\"color:#032563;--shiki-dark:#9ECBFF\"> \"@std/ffi\"</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">foreign</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> \"c\" </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> get_counter</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> () : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">CPtr</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> counter </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> unsafe </span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">{</span></span>\n<span class=\"line\"><span style=\"color:#023B95;--shiki-dark:#79B8FF\">  Ffi</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">.ptr.</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\">cast</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#702C00;--shiki-dark:#B392F0\">Int</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">](</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\">get_counter</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">());</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">};</span></span></code></pre></section></div><p>Use <code>foreign</code> for the ABI declaration. Use <code>unsafe</code> for the block where raw native assumptions matter. Use <code>@std/ffi</code> helpers when pointer views need typed reads, writes, offsets, or casts.</p>\n",
		"summaryHtml": "Rust <code>extern</code> and raw pointer work maps to Musi <code>foreign</code>, <code>CPtr</code>, <code>@std/ffi</code>, and <code>unsafe</code>."
	},
	{
		"locale": "en",
		"id": "rust-testing-tooling",
		"kind": "chapter",
		"parentId": "developers-rust",
		"depth": 3,
		"treePath": [
			"developers",
			"developers-guides",
			"developers-rust",
			"rust-testing-tooling"
		],
		"childIds": [],
		"partId": "developers",
		"partTitle": "Musi for Developers",
		"sectionId": "developers-rust",
		"sectionTitle": "Musi for Rust Developers",
		"path": "/learn/book/developers/guides/rust/testing-tooling",
		"canonicalPath": "/learn/book/developers/guides/rust/testing-tooling",
		"aliases": [
			"/docs/book/developers/guides/rust/testing-tooling",
			"/learn/book/developers/guides/rust/testing-tooling",
			"/docs/book/developers/testing-tooling",
			"/learn/book/developers/testing-tooling",
			"/docs/book/developers/rust/testing-tooling",
			"/learn/book/developers/rust/testing-tooling",
			"/docs/language/developers/rust/testing-tooling",
			"/learn/language/developers/rust/testing-tooling"
		],
		"questions": [],
		"title": "Testing and Tooling",
		"description": "Translate Rust unit-test habits into Musi package tests.",
		"group": "Musi for Developers",
		"section": "Rust Developers",
		"order": 11,
		"slug": "testing-tooling",
		"summary": "Rust `#[test]` habits map to Musi package tests with `@std/testing`.",
		"descriptionHtml": "Translate Rust unit-test habits into Musi package tests.",
		"headings": [],
		"html": "<p>Rust usually keeps a small test beside the code that owns the behavior:</p>\n<pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">fn</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> default_port</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">() </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">-></span><span style=\"color:#702C00;--shiki-dark:#B392F0\"> u16</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> {</span></span>\n<span class=\"line\"><span style=\"color:#023B95;--shiki-dark:#79B8FF\">    8080</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">}</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">#[test]</span></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">fn</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> default_port_is_http_alt</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">() {</span></span>\n<span class=\"line\"><span style=\"color:#622CBC;--shiki-dark:#B392F0\">    assert_eq!</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\">default_port</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(), </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">8080</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">);</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">}</span></span></code></pre><p>Musi writes a test function and uses <code>@std/testing</code> assertions.</p>\n<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light-high-contrast github-dark\" style=\"background-color:#ffffff;--shiki-dark-bg:#24292e;color:#0e1116;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> Testing</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> :=</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> import</span><span style=\"color:#032563;--shiki-dark:#9ECBFF\"> \"@std/testing\"</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">let</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> defaultPort</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> () : </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> :=</span><span style=\"color:#023B95;--shiki-dark:#79B8FF\"> 8080</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#A0111F;--shiki-dark:#F97583\">export</span><span style=\"color:#A0111F;--shiki-dark:#F97583\"> let</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\"> test</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\"> () </span><span style=\"color:#A0111F;--shiki-dark:#F97583\">:=</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">  (</span></span>\n<span class=\"line\"><span style=\"color:#023B95;--shiki-dark:#79B8FF\">    Testing</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">.</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\">describe</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(</span><span style=\"color:#032563;--shiki-dark:#9ECBFF\">\"ports\"</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">);</span></span>\n<span class=\"line\"><span style=\"color:#023B95;--shiki-dark:#79B8FF\">    Testing</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">.</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\">it</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(</span><span style=\"color:#032563;--shiki-dark:#9ECBFF\">\"default port is http alt\"</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">, </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">Testing</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">.</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\">toBe</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\">defaultPort</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">(), </span><span style=\"color:#023B95;--shiki-dark:#79B8FF\">8080</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">));</span></span>\n<span class=\"line\"><span style=\"color:#023B95;--shiki-dark:#79B8FF\">    Testing</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">.</span><span style=\"color:#622CBC;--shiki-dark:#B392F0\">endDescribe</span><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">()</span></span>\n<span class=\"line\"><span style=\"color:#0E1116;--shiki-dark:#E1E4E8\">  );</span></span></code></pre></section></div><p>A package test should name behavior, call the function under test, and return the test runner&#39;s result expression.</p>\n",
		"summaryHtml": "Rust <code>#[test]</code> habits map to Musi package tests with <code>@std/testing</code>."
	}
] satisfies GeneratedDoc[];

