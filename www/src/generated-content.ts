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
	"homeSampleHtml": "<div class=\"code-tabs\" data-example-id=\"home-intro\"><div class=\"code-tabs-meta\"><p class=\"code-tabs-caption\">Start with one value, one function, and one final result. This matches Musi&#39;s beginner path.</p><p class=\"code-tabs-note\">Musi reads top to bottom. Bind values with <code>let</code>, define small functions the same way, then end with the result you want.</p></div><section role=\"tabpanel\" class=\"code-panel\"><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> base </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 21</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> twice</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> (x : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">) : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> x </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">+</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> x;</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> answer </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> twice</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(base);</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">answer;</span></span></code></pre></section></div>",
	"installCurlHtml": "<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">curl</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> -fsSL</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> https://raw.githubusercontent.com/musi-lang/musi/main/install.sh</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> |</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> sh</span></span></code></pre></section></div>",
	"installPowershellHtml": "<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span>powershell -NoProfile -ExecutionPolicy Bypass -Command \"irm https://raw.githubusercontent.com/musi-lang/musi/main/install.ps1 | iex\"</span></span></code></pre></section></div>",
	"installCargoHtml": "<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">git</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> clone</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> https://github.com/musi-lang/musi.git</span></span>\n<span class=\"line\"><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">cd</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> musi</span></span>\n<span class=\"line\"><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">cargo</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> install</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> --locked</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> --force</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> --path</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> crates/music</span></span>\n<span class=\"line\"><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">cargo</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> install</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> --locked</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> --force</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> --path</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> crates/musi</span></span></code></pre></section></div>",
	"quickstartHtml": "<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">musi</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> new</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> hello</span></span>\n<span class=\"line\"><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">cd</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> hello</span></span>\n<span class=\"line\"><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">musi</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> run</span></span></code></pre></section></div>"
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
				"id": "in-this-chapter",
				"text": "In this chapter"
			},
			{
				"depth": 2,
				"id": "why-it-matters",
				"text": "Why it matters"
			},
			{
				"depth": 2,
				"id": "walk-through-it",
				"text": "Walk through it"
			}
		],
		"html": "<p>Start here if Musi is new, but expect more than a checklist.\nThis part gives you first working model of the language: files read top to bottom, <code>let</code> names values, blocks produce results, and mutation is explicit instead of ambient.\nThe goal is quick traction without burying you under package structure or advanced abstractions.</p>\n<h2 id=\"in-this-chapter\"><a href=\"#in-this-chapter\">In this chapter</a></h2><p>This part teaches first Musi habits you will reuse everywhere: install the toolchain, write one file, bind values, read expressions, and recognize when state changes are explicit.\nEach chapter stays narrow, but each one should answer a real beginner question instead of only naming a topic.</p>\n<h2 id=\"why-it-matters\"><a href=\"#why-it-matters\">Why it matters</a></h2><p>Too little detail creates immediate &quot;how do I actually do this?&quot; gaps.\nToo much detail creates wall-of-text fatigue before the core reading model even lands.\nThis section aims for middle ground: enough examples to make first files feel practical, but small enough that every page still unlocks one new idea at a time.</p>\n<h2 id=\"walk-through-it\"><a href=\"#walk-through-it\">Walk through it</a></h2><p>Read these chapters in order.\nTry each example in a scratch file, keep names concrete, and stop to restate what value a file or block produces before moving on.\nIf one page still feels fuzzy, repeat its tiny example before piling on the next concept.</p>\n",
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
				"id": "in-this-chapter",
				"text": "In this chapter"
			},
			{
				"depth": 2,
				"id": "why-it-matters",
				"text": "Why it matters"
			},
			{
				"depth": 2,
				"id": "walk-through-it",
				"text": "Walk through it"
			}
		],
		"html": "<p>This part turns first-file syntax into everyday working code.\nYou will move from direct values into operators, ranges, functions, calls, and methods, always with examples that look like real Musi rather than token drills.\nBy the end, small expression-oriented code should feel normal instead of novel.</p>\n<h2 id=\"in-this-chapter\"><a href=\"#in-this-chapter\">In this chapter</a></h2><p>This section teaches core surface you read all the time: direct values, value transformations, reusable functions, ordinary calls, and receiver-led methods.\nThese are not isolated grammar facts; they are building blocks for nearly every later chapter.</p>\n<h2 id=\"why-it-matters\"><a href=\"#why-it-matters\">Why it matters</a></h2><p>Readers need enough substance here to stop asking basic &quot;how do I compute this?&quot; or &quot;how do I call that?&quot; questions later.\nAt same time, this section should not become a full reference manual before users can even read a tiny file comfortably.\nThe right balance is concrete examples with one clear lesson per chapter.</p>\n<h2 id=\"walk-through-it\"><a href=\"#walk-through-it\">Walk through it</a></h2><p>Work through chapters in order and keep rewriting examples in your own words.\nName intermediate results, trace values left to right through calls, and compare plain functions with methods so you can choose the clearer style in real code.\nIf one form still feels mechanical, keep it small until the reading pattern becomes boring.</p>\n",
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
				"id": "in-this-chapter",
				"text": "In this chapter"
			},
			{
				"depth": 2,
				"id": "why-it-matters",
				"text": "Why it matters"
			},
			{
				"depth": 2,
				"id": "walk-through-it",
				"text": "Walk through it"
			}
		],
		"html": "<p>This part introduces values with shape.\nRecords, arrays, slices, and patterns all answer different kinds of data questions, and the examples should help you feel when to reach for each one.\nThe section is about practical structure, not abstract taxonomy.</p>\n<h2 id=\"in-this-chapter\"><a href=\"#in-this-chapter\">In this chapter</a></h2><p>This section teaches labeled data, ordered data, and branching on data shape.\nYou will construct values, update them, and then decide what to do when shape changes the result.</p>\n<h2 id=\"why-it-matters\"><a href=\"#why-it-matters\">Why it matters</a></h2><p>Once values stop being just numbers or strings, users need examples that answer &quot;how should I model this?&quot; not only &quot;what punctuation exists?&quot;\nIf the docs stay too thin here, beginners end up asking how to update one field, how to represent a list, or how to branch on constructor cases.\nThis section should answer those questions with compact but meaningful examples.</p>\n<h2 id=\"walk-through-it\"><a href=\"#walk-through-it\">Walk through it</a></h2><p>Start with records and arrays before pattern matching.\nGet comfortable recognizing named fields versus ordered positions, then move into <code>match</code> once those shapes already mean something to you.\nWhen examples grow, keep asking what data shape each construct is making visible.</p>\n",
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
				"id": "in-this-chapter",
				"text": "In this chapter"
			},
			{
				"depth": 2,
				"id": "why-it-matters",
				"text": "Why it matters"
			},
			{
				"depth": 2,
				"id": "walk-through-it",
				"text": "Walk through it"
			}
		],
		"html": "<p>This part explains how Musi code grows past one scratch file.\nFiles stay important, but packages, imports, and exports become necessary once code needs boundaries, reuse, and repeatable commands.\nThe goal is to grow code organization without breaking the simple mental model from start chapters.</p>\n<h2 id=\"in-this-chapter\"><a href=\"#in-this-chapter\">In this chapter</a></h2><p>This section teaches file-level thinking, package structure, and explicit module boundaries.\nYou will see where code lives, how projects are created, and how names move across file edges.</p>\n<h2 id=\"why-it-matters\"><a href=\"#why-it-matters\">Why it matters</a></h2><p>Many guides overload readers by mixing package ceremony into first syntax lesson.\nThat is wrong direction.\nUsers should first understand one file well, then learn how to organize many files without losing track of what each boundary is for.</p>\n<h2 id=\"walk-through-it\"><a href=\"#walk-through-it\">Walk through it</a></h2><p>Read these pages as scaling story.\nFirst stabilize one-file flow, then move to package commands, then add imports and exports where code genuinely needs boundaries.\nAt each step, ask whether the new structure solves a real organization problem instead of adding ceremony for its own sake.</p>\n",
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
				"id": "in-this-chapter",
				"text": "In this chapter"
			},
			{
				"depth": 2,
				"id": "why-it-matters",
				"text": "Why it matters"
			},
			{
				"depth": 2,
				"id": "walk-through-it",
				"text": "Walk through it"
			}
		],
		"html": "<p>This part adds type information gradually.\nYou will start with explicit annotations, then see where inference reduces repetition, then use generics to reuse one definition across many types.\nThe emphasis stays on readable, practical code rather than abstract type-system tour.</p>\n<h2 id=\"in-this-chapter\"><a href=\"#in-this-chapter\">In this chapter</a></h2><p>This section teaches three related tools: annotations for explicit boundaries, inference for obvious cases, and generics for reusable typed definitions.\nIt introduces callable type spelling such as <code>T -&gt; U</code> and <code>T ~&gt; U</code>, so readers can connect type chapters back to functions and effects.\nIt also makes the <code>:</code> story explicit: annotations use <code>:</code>, constraints use <code>where</code>, and named variant payloads use constructor-style declarations such as <code>| Configured(port : Int)</code>.</p>\n<h2 id=\"why-it-matters\"><a href=\"#why-it-matters\">Why it matters</a></h2><p>Users need enough type detail to write and read real code, but too much theory too early causes drop-off.\nThis section aims for decision-making help: when should I annotate, when can I omit, when is a generic definition worth it, and how do callable types fit into effectful code?\nThat is more useful than a giant catalog of type features.</p>\n<h2 id=\"walk-through-it\"><a href=\"#walk-through-it\">Walk through it</a></h2><p>Learn one move at a time.\nAdd types where clarity rises, remove only what surrounding code makes obvious, and introduce generic parameters only after you can already read annotated functions comfortably.\nKeep examples small enough that every type choice still has an obvious reason.</p>\n<p>After <code>forall</code>, the section introduces practical dependent types: value parameters in type lists, indexed variant results, <code>partial</code> for runtime-only definitions, and <code>~=</code> for type equality.</p>\n",
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
				"id": "in-this-chapter",
				"text": "In this chapter"
			},
			{
				"depth": 2,
				"id": "why-it-matters",
				"text": "Why it matters"
			},
			{
				"depth": 2,
				"id": "walk-through-it",
				"text": "Walk through it"
			}
		],
		"html": "<p>This part moves from plain functions into reusable behavior contracts.\nClasses describe shared behavior, instances provide concrete implementations, and laws explain what those abstractions are supposed to mean.\nThe section works only if each layer stays distinct.</p>\n<h2 id=\"in-this-chapter\"><a href=\"#in-this-chapter\">In this chapter</a></h2><p>This section teaches three abstraction layers: contract, implementation, and semantic expectation.\nThose layers are connected, but they are not interchangeable.\nUnderstanding the separation is more important than memorizing every keyword.</p>\n<h2 id=\"why-it-matters\"><a href=\"#why-it-matters\">Why it matters</a></h2><p>Abstraction chapters become overwhelming when readers cannot tell whether they are looking at behavior shape, specific implementation, or mathematical expectation.\nToo little explanation here leads straight to &quot;what is class for?&quot; or &quot;where does real behavior live?&quot;\nThis section should answer those questions with small examples before complexity grows.</p>\n<h2 id=\"walk-through-it\"><a href=\"#walk-through-it\">Walk through it</a></h2><p>Read class page first, then instance page, then law page.\nFor each example, ask what stays generic, what becomes concrete, and what property should hold across all correct implementations.\nIf that separation is blurry, stay with the small <code>Eq</code>-style examples until it is not.</p>\n",
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
				"id": "in-this-chapter",
				"text": "In this chapter"
			},
			{
				"depth": 2,
				"id": "why-it-matters",
				"text": "Why it matters"
			},
			{
				"depth": 2,
				"id": "walk-through-it",
				"text": "Walk through it"
			}
		],
		"html": "<p>This part explains explicit capability flow and the layers built around it.\nEffects describe requested work, <code>using</code> surfaces required capabilities, handlers resolve requests, and foundation/runtime/stdlib pages place imports in their proper layer.\nThe goal is to make boundary thinking readable instead of mystical.</p>\n<h2 id=\"in-this-chapter\"><a href=\"#in-this-chapter\">In this chapter</a></h2><p>This section teaches effect requests, capability requirements, request handling, and module layering from core to runtime to stdlib.\nIt is one of richest parts of the book, but every page should still answer one practical question.</p>\n<h2 id=\"why-it-matters\"><a href=\"#why-it-matters\">Why it matters</a></h2><p>Effects and runtime topics become intimidating when all boundaries are introduced at once.\nUsers then ask whether something is built in, imported, handled, runtime-backed, or just standard library code.\nThis section prevents that pile-up by separating each concern while keeping one coherent model of explicit capability flow.</p>\n<h2 id=\"walk-through-it\"><a href=\"#walk-through-it\">Walk through it</a></h2><p>Follow the order.\nLearn effect requests before handlers, understand <code>using</code> before resolving capabilities, and keep module layers distinct when reading imports.\nWhenever a page feels abstract, come back to concrete question: what work is being requested, and who is responsible for providing it?</p>\n",
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
				"id": "in-this-chapter",
				"text": "In this chapter"
			},
			{
				"depth": 2,
				"id": "why-it-matters",
				"text": "Why it matters"
			},
			{
				"depth": 2,
				"id": "walk-through-it",
				"text": "Walk through it"
			}
		],
		"html": "<p>This part finishes the learning path with boundary and workflow tools.\nAttributes, foreign declarations, unsafe FFI, quote, testing, and command workflow all matter, but they matter most after ordinary Musi code already feels stable.\nThese pages should feel like practical extensions, not sudden genre change.</p>\n<h2 id=\"in-this-chapter\"><a href=\"#in-this-chapter\">In this chapter</a></h2><p>This section teaches metadata, host-boundary declarations, unsafe FFI boundaries, code-as-data tools, testing surface, and everyday command workflow.\nIt distinguishes attribute families explicitly, so advanced pages answer more than &quot;yes, attributes exist&quot;.</p>\n<h2 id=\"why-it-matters\"><a href=\"#why-it-matters\">Why it matters</a></h2><p>If advanced topics appear too early, beginners get overwhelmed and leave.\nIf they never appear with concrete examples, later-stage users still have to ask how Musi handles metadata, native integration, generated syntax, verification, or repeatable workflow.\nThis section answers those needs without turning into a wall of detail.</p>\n<h2 id=\"walk-through-it\"><a href=\"#walk-through-it\">Walk through it</a></h2><p>Treat these chapters as boundary tools.\nReach for them when ordinary code is already clear and you need metadata, external integration, generated syntax, verification, or repeatable workflow.\nFollow visible example sources when one chapter shows boundary syntax you want to trace back into repo code.</p>\n",
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
				"id": "in-this-chapter",
				"text": "In this chapter"
			},
			{
				"depth": 2,
				"id": "why-it-matters",
				"text": "Why it matters"
			},
			{
				"depth": 2,
				"id": "walk-through-it",
				"text": "Walk through it"
			},
			{
				"depth": 2,
				"id": "try-it-next",
				"text": "Try it next"
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
		"html": "<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">curl</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> -fsSL</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> https://raw.githubusercontent.com/musi-lang/musi/main/install.sh</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> |</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> sh</span></span>\n<span class=\"line\"><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">music</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> check</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> index.ms</span></span>\n<span class=\"line\"><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">musi</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> new</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> hello</span></span></code></pre></section></div><h2 id=\"in-this-chapter\"><a href=\"#in-this-chapter\">In this chapter</a></h2><p>Getting started in Musi means setting up one toolchain, then learning that there are two command lanes.\nUse <code>music</code> when you are working directly with one file. Use <code>musi</code> when you are working inside a package.\nThat split matters early because it tells you whether you are experimenting with syntax or managing a project.</p>\n<h2 id=\"why-it-matters\"><a href=\"#why-it-matters\">Why it matters</a></h2><p>Many &quot;how do I run this?&quot; questions come from not knowing which command lane owns which job.\nIf the install page only says &quot;install Musi,&quot; beginners still do not know whether to reach for <code>music check index.ms</code> or <code>musi run</code>.\nThis page should make the mental model concrete: one binary for direct file work, one binary for package workflow.</p>\n<h2 id=\"walk-through-it\"><a href=\"#walk-through-it\">Walk through it</a></h2><p>Read the first command block as fastest path to a working install, then keep the Cargo path as fallback when you want a local clone or explicit build.\nAfter installation, verify <code>music</code> first with a single-file check command.\nThen create one package and run <code>musi</code> inside it so the two lanes become separate muscle memory instead of one blurred command surface.</p>\n<h2 id=\"try-it-next\"><a href=\"#try-it-next\">Try it next</a></h2><ul>\n<li>Install Musi with one command path.</li>\n<li>Run <code>music check index.ms</code> on a scratch file.</li>\n<li>Create one package with <code>musi new hello</code> and run it.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not treat <code>music</code> and <code>musi</code> as duplicate names for same workflow.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/start/first-program\">First program</a> to use the direct file lane on the smallest possible Musi file.</p>\n",
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
				"id": "in-this-chapter",
				"text": "In this chapter"
			},
			{
				"depth": 2,
				"id": "why-it-matters",
				"text": "Why it matters"
			},
			{
				"depth": 2,
				"id": "walk-through-it",
				"text": "Walk through it"
			},
			{
				"depth": 2,
				"id": "try-it-next",
				"text": "Try it next"
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
		"html": "<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> answer </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 42</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">answer;</span></span></code></pre></section></div><h2 id=\"in-this-chapter\"><a href=\"#in-this-chapter\">In this chapter</a></h2><p>A first Musi program can be only a binding and a final expression.\nThere is no extra ceremony here: no wrapper function, no package manifest, and no boilerplate runtime setup.\nThe file reads top to bottom, and the last expression is the result you are asking Musi to evaluate.</p>\n<h2 id=\"why-it-matters\"><a href=\"#why-it-matters\">Why it matters</a></h2><p>This is first meaningful win for new readers.\nIf the first page immediately adds packages, imports, or larger syntax, beginners stop learning the language and start fighting setup detail.\nA two-line program proves the core reading model first: bind a value, then use it.</p>\n<h2 id=\"walk-through-it\"><a href=\"#walk-through-it\">Walk through it</a></h2><p>Read <code>let answer := 42;</code> as &quot;introduce a name for a value I care about.&quot;\nRead the final <code>answer;</code> line as &quot;this is the result of the file.&quot;\nOnce that shape feels normal, you can change the value, add one more binding above it, or swap the final line for a larger expression without changing the mental model.</p>\n<h2 id=\"try-it-next\"><a href=\"#try-it-next\">Try it next</a></h2><ul>\n<li>Create <code>index.ms</code> with one <code>let</code> binding.</li>\n<li>End the file with the bound name.</li>\n<li>Run <code>music check index.ms</code> to confirm the file shape.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not go hunting for a mandatory <code>main</code> just because other languages require one.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/start/values-and-let\">Values and let</a> to make that first binding pattern do real work.</p>\n",
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
				"id": "in-this-chapter",
				"text": "In this chapter"
			},
			{
				"depth": 2,
				"id": "why-it-matters",
				"text": "Why it matters"
			},
			{
				"depth": 2,
				"id": "walk-through-it",
				"text": "Walk through it"
			},
			{
				"depth": 2,
				"id": "try-it-next",
				"text": "Try it next"
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
		"html": "<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> port </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 8080</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> nextPort </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> port </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">+</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 1</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">nextPort;</span></span></code></pre></section></div><p><code>let</code> is one of the most important words in Musi.\nIt introduces a name, and that same surface grows with the language: plain values, functions, methods, and recursive definitions all start from <code>let</code>.</p>\n<h2 id=\"in-this-chapter\"><a href=\"#in-this-chapter\">In this chapter</a></h2><p>Read <code>let</code> as &quot;bind this name to this value or definition.&quot;\nThat is true whether the right-hand side is:</p>\n<ul>\n<li>a number</li>\n<li>a string</li>\n<li>a block</li>\n<li>a function body</li>\n<li>a data definition</li>\n</ul>\n<p>This is why Musi feels uniform.\nYou learn one binding form, then keep extending it instead of switching to a new top-level declaration family every few pages.</p>\n<p>You also meet <code>let rec</code> here.\nUse <code>let rec</code> when a binding must refer to itself while it is being defined.\nThat is the ordinary way to write recursive functions and other self-referential definitions.</p>\n<h2 id=\"why-it-matters\"><a href=\"#why-it-matters\">Why it matters</a></h2><p>Readers from Python or JavaScript often expect assignment-looking code to mean mutating state.\nThat is not what plain <code>let</code> means in Musi.\nPlain <code>let</code> introduces a binding.\nIf you want later mutation, that is a separate choice with <code>mut</code>.</p>\n<p>Readers from C-like languages also often expect loops to be the default way to repeat work.\nMusi does not start there.\nSimple repetition often begins with recursion, which is why <code>let rec</code> belongs early in the book.</p>\n<h2 id=\"walk-through-it\"><a href=\"#walk-through-it\">Walk through it</a></h2><p>Read the example in order:</p>\n<ol>\n<li>a name is introduced with <code>let</code></li>\n<li>the right-hand side determines what that name means</li>\n<li>later expressions use that name directly</li>\n</ol>\n<p>When you see <code>let rec</code>, stop and ask one question: &quot;does this definition need to see itself?&quot;\nIf yes, <code>rec</code> is appropriate.\nIf not, stay with plain <code>let</code>.</p>\n<p>A useful comparison for C-style readers is:</p>\n<ul>\n<li>C-like thinking: define control flow first, then push values through it</li>\n<li>Musi thinking: define values and transformations first, then let recursion or data shape drive control flow</li>\n</ul>\n<h2 id=\"try-it-next\"><a href=\"#try-it-next\">Try it next</a></h2><ul>\n<li>Write one plain <code>let</code> binding.</li>\n<li>Turn one helper into <code>let rec</code> and make it call itself.</li>\n<li>Explain why that recursive binding needs <code>rec</code>.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not treat <code>let</code> as hidden mutation.\nIt introduces a binding.\nMutation only starts when you opt into mutable state.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/start/blocks-and-expressions\">Blocks and expressions</a> to see how several local steps still collapse into one final result.</p>\n",
		"summaryHtml": "Use <code>let</code> to name values, define callables, and understand when recursion needs <code>let rec</code>."
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
				"id": "in-this-chapter",
				"text": "In this chapter"
			},
			{
				"depth": 2,
				"id": "why-it-matters",
				"text": "Why it matters"
			},
			{
				"depth": 2,
				"id": "walk-through-it",
				"text": "Walk through it"
			},
			{
				"depth": 2,
				"id": "try-it-next",
				"text": "Try it next"
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
		"html": "<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">  let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> base </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 8000</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">  let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> offset </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 80</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">  base </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">+</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> offset</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">);</span></span></code></pre></section></div><p>A Musi block groups several steps, but it still behaves like one expression.\nThat one idea explains a lot of the language.\nIt is why blocks, functions, and match arms all feel related instead of like disconnected grammar features.</p>\n<h2 id=\"in-this-chapter\"><a href=\"#in-this-chapter\">In this chapter</a></h2><p>Inside a block, earlier lines can prepare values and later lines can use them.\nThe last expression becomes the value of the whole block.</p>\n<p>That means this block model already answers questions many users bring from other languages:</p>\n<ul>\n<li>Where does the result come from? The last expression.</li>\n<li>Where is <code>return</code>? Usually nowhere. It is not needed.</li>\n<li>How do I do several steps? Put them in a block and end with the value you want.</li>\n</ul>\n<h2 id=\"why-it-matters\"><a href=\"#why-it-matters\">Why it matters</a></h2><p>If you keep expecting statement braces from C, JavaScript, or Java, Musi blocks will feel strange.\nIf you accept that blocks are expressions, many later features suddenly become easier:</p>\n<ul>\n<li><code>match</code> arms return values naturally</li>\n<li>handler clauses return values naturally</li>\n<li>helper blocks inside functions stay readable</li>\n<li>recursion becomes a more natural replacement for loop statements</li>\n</ul>\n<p>Musi does not have <code>for</code>, <code>while</code>, <code>break</code>, or <code>continue</code> statements.\nInstead, cycling work usually comes from recursion, ranges, or higher-order helpers from the standard library.\nThat only feels reasonable once expression flow is clear.</p>\n<h2 id=\"walk-through-it\"><a href=\"#walk-through-it\">Walk through it</a></h2><p>Read the example line by line.\nAsk what each line introduces, then ask what the final line computes from those earlier names.</p>\n<p>A good habit is to paraphrase a block like this:</p>\n<ul>\n<li>first, bind <code>base</code></li>\n<li>then, derive another value from it</li>\n<li>finally, produce the result</li>\n</ul>\n<p>That paraphrase scales from tiny arithmetic blocks to real code.</p>\n<h2 id=\"try-it-next\"><a href=\"#try-it-next\">Try it next</a></h2><ul>\n<li>Write one block with two helper bindings and one final expression.</li>\n<li>Replace the last line and predict the new result before running it.</li>\n<li>Rewrite a tiny loop-shaped idea as a recursive helper plus a block result.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not assume only a keyword such as <code>return</code> can make a block meaningful.\nIn Musi, the final expression already does that job.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/start/mutation\">Mutation</a> to see what changes when state is meant to vary instead of being rebound once.</p>\n",
		"summaryHtml": "Treat a block as one expression with setup at the top and the result at the bottom."
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
				"id": "in-this-chapter",
				"text": "In this chapter"
			},
			{
				"depth": 2,
				"id": "why-it-matters",
				"text": "Why it matters"
			},
			{
				"depth": 2,
				"id": "walk-through-it",
				"text": "Walk through it"
			},
			{
				"depth": 2,
				"id": "try-it-next",
				"text": "Try it next"
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
		"html": "<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> counter </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> mut</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 1</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">counter </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> counter </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">+</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 1</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">counter;</span></span></code></pre></section></div><h2 id=\"in-this-chapter\"><a href=\"#in-this-chapter\">In this chapter</a></h2><p>Mutation in Musi is explicit.\nA value becomes mutable only when you mark it with <code>mut</code>, and reassignment uses same <code>:=</code> surface in a clearly state-changing position.\nThat keeps changing state visible instead of quietly blending it into ordinary bindings.</p>\n<h2 id=\"why-it-matters\"><a href=\"#why-it-matters\">Why it matters</a></h2><p>New users need to know both that mutation exists and that Musi does not want it everywhere.\nIf docs only show immutable examples, people ask how to update counters or accumulators.\nIf docs present mutation as default, readers miss one of the language&#39;s clarity wins: stable values stay stable unless you opt into change.</p>\n<h2 id=\"walk-through-it\"><a href=\"#walk-through-it\">Walk through it</a></h2><p>Read <code>let counter := mut 1;</code> as creation of one mutable cell with initial value <code>1</code>.\nRead <code>counter := 2;</code> as reassignment of existing mutable value, not creation of a second binding.\nWhen writing real code, start by asking whether a new immutable value would read better; choose <code>mut</code> when step-by-step updates make the intent clearer.</p>\n<h2 id=\"try-it-next\"><a href=\"#try-it-next\">Try it next</a></h2><ul>\n<li>Create one mutable counter.</li>\n<li>Reassign it once.</li>\n<li>Rewrite same tiny task with immutable bindings and compare readability.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not add <code>mut</code> automatically just because value changes in other languages.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/core/literals\">Literals</a> to build up the everyday values you will bind, transform, and compare.</p>\n",
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
		"summary": "Start with numbers, strings, booleans, runes, and template text.",
		"descriptionHtml": "Meet Musi&#39;s everyday literal values before mixing them with operators.",
		"headings": [
			{
				"depth": 2,
				"id": "in-this-chapter",
				"text": "In this chapter"
			},
			{
				"depth": 2,
				"id": "why-it-matters",
				"text": "Why it matters"
			},
			{
				"depth": 2,
				"id": "walk-through-it",
				"text": "Walk through it"
			},
			{
				"depth": 2,
				"id": "try-it-next",
				"text": "Try it next"
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
		"html": "<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> port </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 8080</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> label </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> \"ready\"</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> enabled </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> .True</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">label;</span></span></code></pre></section></div><h2 id=\"in-this-chapter\"><a href=\"#in-this-chapter\">In this chapter</a></h2><p>Literals are values written directly in source: integers, floats, strings, runes, booleans, and template text you can read without another definition step.\nThis example mixes a few literal kinds with nearby derived bindings so you can see what is written directly and what is computed from it.\nThe language stays readable when you can spot that difference quickly.</p>\n<h2 id=\"why-it-matters\"><a href=\"#why-it-matters\">Why it matters</a></h2><p>Users rarely ask for &quot;literal theory.&quot; They ask how to write a port number, a label, or a comparison flag in working code.\nA useful chapter connects literal syntax to ordinary tasks instead of listing token categories.\nThat lowers early friction before operators, ranges, and structured data show up.</p>\n<h2 id=\"walk-through-it\"><a href=\"#walk-through-it\">Walk through it</a></h2><p>Read <code>8080</code>, <code>0xff</code>, <code>3.14</code>, <code>&quot;ready&quot;</code>, and rune values as direct source values.\nThen notice how nearby bindings such as <code>next</code>, <code>same</code>, and <code>capped</code> are built from those literals rather than introducing another category to memorize.\nWhen learning, start with small direct values, then derive one or two computed bindings so you can tell where literal writing stops and expression building begins.</p>\n<h2 id=\"try-it-next\"><a href=\"#try-it-next\">Try it next</a></h2><ul>\n<li>Bind one integer and one string.</li>\n<li>Add one boolean comparison from them or nearby values.</li>\n<li>Rename bindings so result reads like small real code instead of token practice.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not try to learn literals, operators, and data shapes as one giant syntax dump.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/core/tuples-and-unit\">Tuples and unit</a> to group direct values by position.</p>\n",
		"summaryHtml": "Start with numbers, strings, booleans, runes, and template text."
	},
	{
		"locale": "en",
		"id": "tuples-and-unit",
		"kind": "chapter",
		"partId": "core",
		"partTitle": "Core syntax",
		"path": "/learn/language/core/tuples-and-unit",
		"canonicalPath": "/learn/language/core/tuples-and-unit",
		"aliases": [
			"/docs/language/core/tuples-and-unit"
		],
		"questions": [],
		"title": "Tuples and unit",
		"description": "Group values by position and recognize the empty value.",
		"group": "Core syntax",
		"section": "Core syntax",
		"order": 7,
		"slug": "tuples-and-unit",
		"summary": "Use tuple expressions for small positional groups and unit when no payload matters.",
		"descriptionHtml": "Group values by position and recognize the empty value.",
		"headings": [
			{
				"depth": 2,
				"id": "in-this-chapter",
				"text": "In this chapter"
			},
			{
				"depth": 2,
				"id": "why-it-matters",
				"text": "Why it matters"
			},
			{
				"depth": 2,
				"id": "walk-through-it",
				"text": "Walk through it"
			},
			{
				"depth": 2,
				"id": "try-it-next",
				"text": "Try it next"
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
		"html": "<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> status </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> (</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">8080</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">, </span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\">\"ready\"</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">);</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> empty </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> ();</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">status;</span></span></code></pre></section></div><h2 id=\"in-this-chapter\"><a href=\"#in-this-chapter\">In this chapter</a></h2><p>Tuples group a few values by position. Use them when names would add noise and the order already carries the meaning.\nThe empty tuple <code>()</code> is unit. It is useful when a value must exist syntactically but no information needs to travel.</p>\n<h2 id=\"why-it-matters\"><a href=\"#why-it-matters\">Why it matters</a></h2><p>Many readers expect every grouped value to be a record or object.\nMusi gives you both choices: records for named fields, tuples for short positional data.\nKnowing that split keeps small return values readable without inventing temporary record names everywhere.</p>\n<h2 id=\"walk-through-it\"><a href=\"#walk-through-it\">Walk through it</a></h2><p>Read <code>(8080, &quot;ready&quot;)</code> as two values moving together.\nRead <code>()</code> as &quot;nothing to carry&quot; rather than as a missing value.\nWhen data grows past two or three positions, prefer a record so readers do not have to remember what each slot means.</p>\n<h2 id=\"try-it-next\"><a href=\"#try-it-next\">Try it next</a></h2><ul>\n<li>Return two related values from one expression.</li>\n<li>Replace an unclear tuple with a record and compare readability.</li>\n<li>Use <code>()</code> when a branch or handler result only needs to signal completion.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not use tuples as anonymous records when field names would explain the code better.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/core/operators\">Operators</a> to combine values into larger expressions.</p>\n",
		"summaryHtml": "Use tuple expressions for small positional groups and unit when no payload matters."
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
				"id": "in-this-chapter",
				"text": "In this chapter"
			},
			{
				"depth": 2,
				"id": "why-it-matters",
				"text": "Why it matters"
			},
			{
				"depth": 2,
				"id": "walk-through-it",
				"text": "Walk through it"
			},
			{
				"depth": 2,
				"id": "try-it-next",
				"text": "Try it next"
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
		"html": "<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> port </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 8080</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> next </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> port </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">+</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 1</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> same </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> next </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 8081</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> capped </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> next </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">&#x3C;=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 9000</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span></code></pre></section></div><h2 id=\"in-this-chapter\"><a href=\"#in-this-chapter\">In this chapter</a></h2><p>Operators let you combine existing values into new results.\nIn this example, arithmetic, equality, ordering, and bit-shift forms all stay inside ordinary <code>let</code> bindings so the code still reads like plain Musi, not like a separate operator sublanguage.\nThe point is not memorizing every symbol at once; it is seeing operators as part of normal expression flow.</p>\n<h2 id=\"why-it-matters\"><a href=\"#why-it-matters\">Why it matters</a></h2><p>Most &quot;how do I compute this?&quot; questions start here.\nUsers want to add one, compare two values, or build a flag, and they need examples that look like real code rather than isolated operator tables.\nShowing operators next to named bindings answers both syntax question and readability question.</p>\n<h2 id=\"walk-through-it\"><a href=\"#walk-through-it\">Walk through it</a></h2><p>Read <code>port + 1</code> as value transformation, <code>next = port + 1</code> as equality check, and <code>port &lt;= 9000</code> as a guard-like comparison that still returns a value.\nThe <code>shl</code> example shows that named operator forms follow same expression pattern.\nWhen writing your own code, start with named inputs, then build one operator expression per binding so the meaning stays obvious.</p>\n<h2 id=\"try-it-next\"><a href=\"#try-it-next\">Try it next</a></h2><ul>\n<li>Start from one numeric binding.</li>\n<li>Add one arithmetic result and one comparison result.</li>\n<li>Read both results aloud as values, not hidden control flow.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not pack several unrelated operators into one line before each result has a clear name.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/core/ranges\">Ranges</a> to see how Musi writes &quot;from here to there&quot; without guessing at endpoint meaning.</p>\n",
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
				"id": "in-this-chapter",
				"text": "In this chapter"
			},
			{
				"depth": 2,
				"id": "why-it-matters",
				"text": "Why it matters"
			},
			{
				"depth": 2,
				"id": "walk-through-it",
				"text": "Walk through it"
			},
			{
				"depth": 2,
				"id": "try-it-next",
				"text": "Try it next"
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
		"html": "<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> closed </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 0</span><span style=\"color:#D73A49;--shiki-dark:#F97583\">..</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">10</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> halfOpen </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 0</span><span style=\"color:#D73A49;--shiki-dark:#F97583\">..&#x3C;</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">10</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">halfOpen;</span></span></code></pre></section></div><h2 id=\"in-this-chapter\"><a href=\"#in-this-chapter\">In this chapter</a></h2><p>Ranges are compact value forms for spans such as &quot;zero through ten&quot; or &quot;zero up to but not including ten.&quot;\nMusi makes the endpoint choice visible in the operator itself, which is why this topic deserves its own page instead of being buried inside larger examples.\nThe syntax is small, but the meaning matters.</p>\n<h2 id=\"why-it-matters\"><a href=\"#why-it-matters\">Why it matters</a></h2><p>Readers often trip on off-by-one errors long before they trip on advanced language features.\nIf docs rush past range syntax, users still have to ask whether end value is included, excluded, or context dependent.\nThis chapter should make those decisions visible early so later APIs that consume ranges feel predictable.</p>\n<h2 id=\"walk-through-it\"><a href=\"#walk-through-it\">Walk through it</a></h2><p>Read <code>0..10</code> as closed range and <code>0..&lt;10</code> as half-open range.\nThe example ends with <code>closed;</code> so you can see range syntax is still ordinary expression-producing code.\nWhen choosing between forms, decide first whether end value belongs inside result, then pick operator that states that choice directly.</p>\n<h2 id=\"try-it-next\"><a href=\"#try-it-next\">Try it next</a></h2><ul>\n<li>Create one closed range.</li>\n<li>Create one half-open range with same endpoints.</li>\n<li>Write down which values differ between them.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not assume every range form means same endpoint behavior with different punctuation.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/core/functions\">Functions</a> to package repeated expression logic behind a reusable name.</p>\n",
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
		"description": "Define reusable functions, learn named arguments, and understand why parameter names matter.",
		"group": "Core syntax",
		"section": "Core syntax",
		"order": 9,
		"slug": "functions",
		"summary": "Functions are ordinary `let` bindings with parameters, result types, and expression bodies.",
		"descriptionHtml": "Define reusable functions, learn named arguments, and understand why parameter names matter.",
		"headings": [
			{
				"depth": 2,
				"id": "in-this-chapter",
				"text": "In this chapter"
			},
			{
				"depth": 2,
				"id": "why-it-matters",
				"text": "Why it matters"
			},
			{
				"depth": 2,
				"id": "walk-through-it",
				"text": "Walk through it"
			},
			{
				"depth": 2,
				"id": "what-musi-does-not-have",
				"text": "What Musi does not have"
			},
			{
				"depth": 2,
				"id": "try-it-next",
				"text": "Try it next"
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
		"html": "<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> render</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> (port : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">, secure : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Bool</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">) : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> port;</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> positional </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> render</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">8080</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">, </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">0</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> =</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 0</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">);</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> labeled </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> render</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(secure </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 0</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> =</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 0</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">, port </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 8080</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">);</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">labeled;</span></span></code></pre></section></div><p>A Musi function is still a <code>let</code> binding.\nThe difference is that the bound value accepts parameters and produces a result.\nThat keeps the language surface small while still letting you write real reusable code.</p>\n<h2 id=\"in-this-chapter\"><a href=\"#in-this-chapter\">In this chapter</a></h2><p>Read a function definition in pieces:</p>\n<ul>\n<li>function name</li>\n<li>parameter list</li>\n<li>optional result type annotation</li>\n<li>body expression</li>\n</ul>\n<p>Musi supports named arguments at ordinary call sites.\nThat means parameter names are not just internal decoration.\nThey are part of the public call surface when you choose to call with labels.</p>\n<p>For example, if a function is defined with parameters like <code>port</code> and <code>secure</code>, callers can write the positional form or the labeled form:</p>\n<ul>\n<li><code>render(8080, .True)</code></li>\n<li><code>render(port := 8080, secure := .True)</code></li>\n</ul>\n<h2 id=\"why-it-matters\"><a href=\"#why-it-matters\">Why it matters</a></h2><p>Named arguments help most when a call has several parameters of similar shape.\nThey make call sites read like intent instead of like memory work.</p>\n<p>They also make one thing important: renaming a public parameter name changes call sites that use named arguments.</p>\n<h2 id=\"walk-through-it\"><a href=\"#walk-through-it\">Walk through it</a></h2><p>Start with the plain definition.\nThen read the call site.\nIf the call uses names, match each label back to the parameter list.</p>\n<p>Musi keeps one simple mixing rule:</p>\n<ul>\n<li>positional arguments first</li>\n<li>named arguments after</li>\n</ul>\n<p>That means code like <code>f(1, mode := &quot;fast&quot;)</code> is fine, but once named arguments begin, later positional arguments are not.</p>\n<p>Pipelines still work with this model.\nA piped value becomes the first positional argument, and any later named arguments stay named.</p>\n<p>Function values keep the labels from the callable surface you give them. An unlabeled callable annotation gives callers only positional access. A labeled callable annotation gives callers those labels.</p>\n<p>The chapter example above shows the same function called positionally and with labels.</p>\n<h2 id=\"what-musi-does-not-have\"><a href=\"#what-musi-does-not-have\">What Musi does not have</a></h2><p>Musi does not use a separate external-label system like some languages do.\nThe parameter names themselves are the labels.\nThat keeps the model smaller.</p>\n<h2 id=\"try-it-next\"><a href=\"#try-it-next\">Try it next</a></h2><ul>\n<li>Define one two-argument function.</li>\n<li>Call it positionally once.</li>\n<li>Call it again with named arguments in a different order.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not think of parameter names as throwaway implementation detail if other modules call the function with labels.\nWhen that happens, the names become part of the function&#39;s user-facing shape.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/core/lambdas\">Lambdas</a> to write small function values inside expressions.</p>\n",
		"summaryHtml": "Functions are ordinary <code>let</code> bindings with parameters, result types, and expression bodies."
	},
	{
		"locale": "en",
		"id": "lambdas",
		"kind": "chapter",
		"partId": "core",
		"partTitle": "Core syntax",
		"path": "/learn/language/core/lambdas",
		"canonicalPath": "/learn/language/core/lambdas",
		"aliases": [
			"/docs/language/core/lambdas"
		],
		"questions": [],
		"title": "Lambdas",
		"description": "Write a small function value directly inside an expression.",
		"group": "Core syntax",
		"section": "Core syntax",
		"order": 12,
		"slug": "lambdas",
		"summary": "Use lambda expressions when a short function value reads better in place.",
		"descriptionHtml": "Write a small function value directly inside an expression.",
		"headings": [
			{
				"depth": 2,
				"id": "in-this-chapter",
				"text": "In this chapter"
			},
			{
				"depth": 2,
				"id": "why-it-matters",
				"text": "Why it matters"
			},
			{
				"depth": 2,
				"id": "walk-through-it",
				"text": "Walk through it"
			},
			{
				"depth": 2,
				"id": "try-it-next",
				"text": "Try it next"
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
		"html": "<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> twice </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> \\(x : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">) : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> =></span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> x </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">+</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> x;</span></span>\n<span class=\"line\"><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">twice</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">21</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">);</span></span></code></pre></section></div><h2 id=\"in-this-chapter\"><a href=\"#in-this-chapter\">In this chapter</a></h2><p>A lambda is an unnamed function value.\nIt uses <code>\\</code> before the parameter list and <code>=&gt;</code> before the body.\nThat makes it useful where a helper is small enough that naming it separately would interrupt the reading flow.</p>\n<h2 id=\"why-it-matters\"><a href=\"#why-it-matters\">Why it matters</a></h2><p>Higher-order helpers, callbacks, adapters, and small transformations all need function values.\nA named <code>let</code> function is clearer for reused behavior.\nA lambda is clearer when the behavior belongs right where it is passed or bound.</p>\n<h2 id=\"walk-through-it\"><a href=\"#walk-through-it\">Walk through it</a></h2><p>Read <code>\\(x : Int) : Int =&gt; x + x</code> as &quot;given <code>x</code>, produce <code>x + x</code>.&quot;\nThe optional result annotation works the same way as a named function result annotation.\nIf the body grows beyond one direct idea, give the function a name.</p>\n<h2 id=\"try-it-next\"><a href=\"#try-it-next\">Try it next</a></h2><ul>\n<li>Bind one lambda to a name and call it.</li>\n<li>Rewrite the same code as a named function.</li>\n<li>Choose the version where the reader does less jumping around.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not hide multi-step behavior inside a lambda when a named helper would explain the intent.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/core/calls\">Calls</a> to follow arguments through functions.</p>\n",
		"summaryHtml": "Use lambda expressions when a short function value reads better in place."
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
				"id": "in-this-chapter",
				"text": "In this chapter"
			},
			{
				"depth": 2,
				"id": "why-it-matters",
				"text": "Why it matters"
			},
			{
				"depth": 2,
				"id": "walk-through-it",
				"text": "Walk through it"
			},
			{
				"depth": 2,
				"id": "try-it-next",
				"text": "Try it next"
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
		"html": "<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> greet</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> (name : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">String</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">) : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">String</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> name;</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> message </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> greet</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\">\"Musi\"</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">);</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">message;</span></span></code></pre></section></div><h2 id=\"in-this-chapter\"><a href=\"#in-this-chapter\">In this chapter</a></h2><p>A call applies a function to arguments.\nThis page reuses a tiny function example because the point is not more syntax surface; the point is learning to read <code>twice(21)</code> as value flow from argument into function and back out as result.\nCalls show up everywhere, so this reading habit must become boring fast.</p>\n<h2 id=\"why-it-matters\"><a href=\"#why-it-matters\">Why it matters</a></h2><p>Many beginner questions are really call-reading questions: &quot;where does this value go?&quot;, &quot;what is input here?&quot;, or &quot;why are parentheses here but not there?&quot;\nIf call syntax is only mentioned in passing, those questions keep interrupting later chapters.\nA short focused chapter pays off because functions, constructors, stdlib helpers, and methods all build on same habit of tracking inputs and outputs.</p>\n<h2 id=\"walk-through-it\"><a href=\"#walk-through-it\">Walk through it</a></h2><p>Look at definition first, then read call left to right.\n<code>twice</code> names the function, <code>(21)</code> supplies one argument, and the whole expression evaluates to returned result.\nWhen calls get larger, keep naming intermediate values so you are still reading one input step at a time rather than decoding a pile of nested punctuation.</p>\n<h2 id=\"try-it-next\"><a href=\"#try-it-next\">Try it next</a></h2><ul>\n<li>Write one small function.</li>\n<li>Call it with one literal argument.</li>\n<li>Bind call result to a name before doing anything larger.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not confuse function definition syntax with function call syntax just because both sit near same name.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/core/methods\">Methods</a> to see how Musi attaches behavior to a receiver and calls it with dot syntax.</p>\n",
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
				"id": "in-this-chapter",
				"text": "In this chapter"
			},
			{
				"depth": 2,
				"id": "why-it-matters",
				"text": "Why it matters"
			},
			{
				"depth": 2,
				"id": "walk-through-it",
				"text": "Walk through it"
			},
			{
				"depth": 2,
				"id": "try-it-next",
				"text": "Try it next"
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
		"html": "<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> (</span><span style=\"color:#E36209;--shiki-dark:#FFAB70\">self</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> : Int).</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">abs</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> () : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> self;</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> one </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 1</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">one.</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">abs</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">();</span></span></code></pre></section></div><h2 id=\"in-this-chapter\"><a href=\"#in-this-chapter\">In this chapter</a></h2><p>Methods in Musi are receiver-prefixed function definitions.\nInstead of inventing a separate <code>impl</code> block or class body, the receiver appears right in the definition, and the call site uses dot syntax on the value.\nThat keeps method behavior close to ordinary function behavior while making receiver visible.</p>\n<h2 id=\"why-it-matters\"><a href=\"#why-it-matters\">Why it matters</a></h2><p>Users coming from object-heavy languages expect one model; users coming from functional languages expect another.\nThis chapter should show that Musi&#39;s method surface is simpler than both expectations: define behavior with a receiver parameter, then call it from the value you already have.\nThat answers &quot;how do I write <code>x.abs()</code>?&quot; without dragging in more abstraction machinery.</p>\n<h2 id=\"walk-through-it\"><a href=\"#walk-through-it\">Walk through it</a></h2><p>Read <code>let (self : Int).abs () : Int := self;</code> as a function whose first visible role is receiver.\nA receiver can be marked <code>mut</code> when method body needs mutable receiver behavior.\nThen read <code>one.abs();</code> as method call on value <code>one</code>, not as magical property lookup.\nWhen deciding between plain function and method, prefer method when receiver-led reading is clearer at call site.</p>\n<h2 id=\"try-it-next\"><a href=\"#try-it-next\">Try it next</a></h2><ul>\n<li>Define one receiver method for a simple type.</li>\n<li>Call it from a named value.</li>\n<li>Compare that call with equivalent plain function style.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not assume methods require a separate container type declaration before they can exist.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/data/records\">Records</a> to move from scalar values into labeled data.</p>\n",
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
				"id": "in-this-chapter",
				"text": "In this chapter"
			},
			{
				"depth": 2,
				"id": "why-it-matters",
				"text": "Why it matters"
			},
			{
				"depth": 2,
				"id": "walk-through-it",
				"text": "Walk through it"
			},
			{
				"depth": 2,
				"id": "try-it-next",
				"text": "Try it next"
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
		"html": "<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> point </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> { x </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 3</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">, y </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 4</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> };</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> moved </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> { </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">...</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">point, y </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 9</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> };</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">moved;</span></span></code></pre></section></div><h2 id=\"in-this-chapter\"><a href=\"#in-this-chapter\">In this chapter</a></h2><p>Records let you store related values under field names.\nThe first snippet shows record construction with named fields, and the second shows how to build a new record from an existing one with spread update.\nTogether they cover the two questions beginners actually ask: how to make one, and how to change one without losing everything else.</p>\n<h2 id=\"why-it-matters\"><a href=\"#why-it-matters\">Why it matters</a></h2><p>Plain numbers and strings stop being enough as soon as data has roles such as <code>x</code>, <code>y</code>, <code>name</code>, or <code>port</code>.\nIf docs only mention record syntax once and move on, users still need to ask how to update one field while keeping rest intact.\nRecords are where labeled data starts feeling practical instead of theoretical.</p>\n<h2 id=\"walk-through-it\"><a href=\"#walk-through-it\">Walk through it</a></h2><p>Read <code>{ x := 3, y := 4 }</code> as one value with two named fields.\nThen read <code>{ ...point, z := 5 }</code> as &quot;copy existing record shape, then override or add selected fields.&quot;\nWhen writing your own records, choose field names that make access obvious and use spread when you want a new value that mostly keeps old data.</p>\n<h2 id=\"try-it-next\"><a href=\"#try-it-next\">Try it next</a></h2><ul>\n<li>Create one record with two named fields.</li>\n<li>Build a second record with spread update.</li>\n<li>Change one field name or value to reflect a real domain example.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not rebuild an entire record by hand when only one or two fields need to change.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/data/indexing-and-fields\">Indexing and fields</a> to read data back out of values.</p>\n",
		"summaryHtml": "Build named-field values and access fields directly."
	},
	{
		"locale": "en",
		"id": "indexing-and-fields",
		"kind": "chapter",
		"partId": "data",
		"partTitle": "Data",
		"path": "/learn/language/data/indexing-and-fields",
		"canonicalPath": "/learn/language/data/indexing-and-fields",
		"aliases": [
			"/docs/language/data/indexing-and-fields"
		],
		"questions": [],
		"title": "Indexing and fields",
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
				"id": "in-this-chapter",
				"text": "In this chapter"
			},
			{
				"depth": 2,
				"id": "why-it-matters",
				"text": "Why it matters"
			},
			{
				"depth": 2,
				"id": "walk-through-it",
				"text": "Walk through it"
			},
			{
				"depth": 2,
				"id": "try-it-next",
				"text": "Try it next"
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
		"html": "<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> point </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> { x </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 3</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">, y </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 4</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> };</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> values </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> [</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">10</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">, </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">20</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">, </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">30</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">];</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> x </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> point.x;</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> first </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> values.[</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">0</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">];</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">x </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">+</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> first;</span></span></code></pre></section></div><h2 id=\"in-this-chapter\"><a href=\"#in-this-chapter\">In this chapter</a></h2><p>Musi uses dot syntax for two common data reads.\nA named field uses <code>.field</code> after the value.\nIndexed access uses <code>.[...]</code> so it stays visually separate from field names and method calls.</p>\n<h2 id=\"why-it-matters\"><a href=\"#why-it-matters\">Why it matters</a></h2><p>Records and arrays answer different questions.\nA field says &quot;read this named part.&quot;\nAn index says &quot;read this position.&quot;\nKeeping the punctuation distinct helps readers understand whether the code depends on a name or on an order.</p>\n<h2 id=\"walk-through-it\"><a href=\"#walk-through-it\">Walk through it</a></h2><p>Read <code>point.x</code> as field access on a record-like value.\nRead <code>values.[0]</code> as indexed access into ordered data.\nIf code starts using many numeric indexes, consider naming intermediate values or changing the data shape.</p>\n<h2 id=\"try-it-next\"><a href=\"#try-it-next\">Try it next</a></h2><ul>\n<li>Build one record and read one field.</li>\n<li>Build one array and read one index.</li>\n<li>Replace a confusing index read with a named field when the domain has a clear label.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not use positional access when a field name would carry the meaning better.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/data/data-definitions\">Data definitions</a> to define reusable data shapes.</p>\n",
		"summaryHtml": "Use <code>.field</code> for named data and <code>.[index]</code> for positional access."
	},
	{
		"locale": "en",
		"id": "data-definitions",
		"kind": "chapter",
		"partId": "data",
		"partTitle": "Data",
		"path": "/learn/language/data/data-definitions",
		"canonicalPath": "/learn/language/data/data-definitions",
		"aliases": [
			"/docs/language/data/data-definitions"
		],
		"questions": [],
		"title": "Data definitions",
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
				"id": "in-this-chapter",
				"text": "In this chapter"
			},
			{
				"depth": 2,
				"id": "why-it-matters",
				"text": "Why it matters"
			},
			{
				"depth": 2,
				"id": "walk-through-it",
				"text": "Walk through it"
			},
			{
				"depth": 2,
				"id": "try-it-next",
				"text": "Try it next"
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
		"html": "<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> Port</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> data</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> {</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">  | </span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">Configured</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(</span><span style=\"color:#E36209;--shiki-dark:#FFAB70\">port</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> : </span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">Int</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">, </span><span style=\"color:#E36209;--shiki-dark:#FFAB70\">secure</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> : </span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">Bool</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">)</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">  | </span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">Default</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">};</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> Settings</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> data</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> {</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">  port : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 3000</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">  label : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">String</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">};</span></span></code></pre></section></div><h2 id=\"in-this-chapter\"><a href=\"#in-this-chapter\">In this chapter</a></h2><p>A <code>data</code> block describes a reusable data shape.\nVariant data uses <code>|</code> arms for choices.\nRecord-shaped data uses named fields separated by semicolons, and fields may have defaults.</p>\n<h2 id=\"why-it-matters\"><a href=\"#why-it-matters\">Why it matters</a></h2><p>Users need more than record literals and match examples.\nThey need to know where the shape itself lives.\n<code>data</code> gives that shape a name, whether the value is one of several variants or a record with fixed fields.</p>\n<h2 id=\"walk-through-it\"><a href=\"#walk-through-it\">Walk through it</a></h2><p>Read <code>| Configured(port : Int, secure : Bool)</code> as a constructor with named payload fields.\nConstruction then uses those names: <code>.Configured(port := 8080, secure := .True)</code>.\nRead <code>port : Int := 3000;</code> in record-shaped data as a field with a default value.</p>\n<h2 id=\"try-it-next\"><a href=\"#try-it-next\">Try it next</a></h2><ul>\n<li>Define one variant with a named payload.</li>\n<li>Construct it with named arguments.</li>\n<li>Define one record-shaped data type with one default field.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not treat variant payload fields as ordinary record fields. Variants describe choices; records describe a fixed shape.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/data/arrays-and-slices\">Arrays and slices</a> to compare named shapes with ordered sequence data.</p>\n",
		"summaryHtml": "Use <code>data</code> for named shapes, variant choices, payload fields, and record defaults."
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
				"id": "in-this-chapter",
				"text": "In this chapter"
			},
			{
				"depth": 2,
				"id": "why-it-matters",
				"text": "Why it matters"
			},
			{
				"depth": 2,
				"id": "walk-through-it",
				"text": "Walk through it"
			},
			{
				"depth": 2,
				"id": "try-it-next",
				"text": "Try it next"
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
		"html": "<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> Slice</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> import</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> \"@std/slice\"</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> values </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> [</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">1</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">, </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">2</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">, </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">3</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">];</span></span>\n<span class=\"line\"><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Slice</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">.</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">concat</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">Int</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">](values, [</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">4</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">]);</span></span></code></pre></section></div><h2 id=\"in-this-chapter\"><a href=\"#in-this-chapter\">In this chapter</a></h2><p>Arrays store ordered values, while slice helpers let you work with sequence-shaped data without inventing your own low-level operations each time.\nThis page pairs a tiny literal array with one stdlib slice example so you can see both local syntax and the broader sequence workflow.\nOrdered data is common enough that users need more than a token example.</p>\n<h2 id=\"why-it-matters\"><a href=\"#why-it-matters\">Why it matters</a></h2><p>Beginners quickly ask how to represent &quot;a list of things&quot; and how to combine or pass that list around.\nIf the docs only show <code>[1, 2, 3]</code>, they still do not know what happens next.\nShowing array creation next to <code>@std/slice</code> answers both shape question and &quot;what do I do with this afterward?&quot; question.</p>\n<h2 id=\"walk-through-it\"><a href=\"#walk-through-it\">Walk through it</a></h2><p>Read <code>[1, 2, 3]</code> as one ordered value where position matters more than field names.\nThen read <code>Slice.concat[Int]([1], [2, 3]);</code> as a normal library call over sequence values, not special built-in mutation.\nWhen writing real code, create arrays locally, then reach for slice helpers when you need to combine, traverse, or transform them in clearer steps.</p>\n<h2 id=\"try-it-next\"><a href=\"#try-it-next\">Try it next</a></h2><ul>\n<li>Bind one small array literal.</li>\n<li>Concatenate two arrays with slice helper.</li>\n<li>Rename arrays so their roles are obvious.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not use arrays when named fields would explain the data better than positions.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/data/patterns\">Patterns</a> to branch on data shape once records and sequences already feel familiar.</p>\n",
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
				"id": "in-this-chapter",
				"text": "In this chapter"
			},
			{
				"depth": 2,
				"id": "why-it-matters",
				"text": "Why it matters"
			},
			{
				"depth": 2,
				"id": "walk-through-it",
				"text": "Walk through it"
			},
			{
				"depth": 2,
				"id": "try-it-next",
				"text": "Try it next"
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
		"html": "<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> Port</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> data</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> {</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">  | </span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">Configured</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(</span><span style=\"color:#E36209;--shiki-dark:#FFAB70\">port</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> : </span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">Int</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">)</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">  | </span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">Default</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">};</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> port : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Port</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> .Configured</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(port </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 8080</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">);</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">match</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> port</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> (</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">| </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">.Configured</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(value) </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">=></span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> value</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">| </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">.Default</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> =></span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 3000</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">);</span></span></code></pre></section></div><h2 id=\"in-this-chapter\"><a href=\"#in-this-chapter\">In this chapter</a></h2><p>Patterns let code react to value shape instead of only to raw scalar comparisons.\nThis example defines a small <code>Port</code> data type, constructs one value, and then uses <code>match</code> to branch on whether a configured value exists.\nIt is first complete example where data definition, construction, and branching all line up around one real decision.</p>\n<h2 id=\"why-it-matters\"><a href=\"#why-it-matters\">Why it matters</a></h2><p>Users ask &quot;how do I handle different cases?&quot; as soon as data can vary.\nIf docs rush into nested destructuring or advanced matching too early, readers get lost before simple constructor matching is stable.\nThe core win: pattern matching keeps data-dependent branching explicit and readable.</p>\n<h2 id=\"walk-through-it\"><a href=\"#walk-through-it\">Walk through it</a></h2><p>Read <code>.Configured(port := 8080)</code> and <code>.Default</code> as two possible shapes of same type.\nThen read each <code>match</code> arm as answer for one shape, with extracted values such as <code>port</code> made available only in arm that matches.\nNamed payload variants let definitions explain intent directly:</p>\n<ul>\n<li>definition: <code>| Configured(port : Int, secure : Bool)</code></li>\n<li>construction: <code>.Configured(port := 8080, secure := .True)</code></li>\n<li>pattern shorthand: <code>.Configured(port, secure := enabled)</code></li>\n</ul>\n<p>Patterns also support guards, aliases, record patterns, array patterns, and alternatives.\nUse a guard when shape is not enough, use <code>as</code> when the arm needs both whole value and extracted parts, and use alternatives when several shapes share the same result.\nWhen writing your own patterns, start with two or three clear cases and ask what value each branch should produce.\nIf payload fields already have meaningful names, keep those names visible in the pattern instead of falling back to anonymous tuple-like slots.</p>\n<h2 id=\"try-it-next\"><a href=\"#try-it-next\">Try it next</a></h2><ul>\n<li>Define one data type with two cases.</li>\n<li>Construct one variant value.</li>\n<li>Write <code>match</code> expression that returns different result for each shape.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not begin with deeply nested pattern trees before constructor-level branching feels easy.\nAlso do not mix positional and named payload style inside one variant definition.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/organization/files\">Files</a> to see how these language forms live inside ordinary source files.</p>\n",
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
				"id": "in-this-chapter",
				"text": "In this chapter"
			},
			{
				"depth": 2,
				"id": "why-it-matters",
				"text": "Why it matters"
			},
			{
				"depth": 2,
				"id": "walk-through-it",
				"text": "Walk through it"
			},
			{
				"depth": 2,
				"id": "try-it-next",
				"text": "Try it next"
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
		"html": "<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> answer </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 42</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">answer;</span></span></code></pre></section></div><h2 id=\"in-this-chapter\"><a href=\"#in-this-chapter\">In this chapter</a></h2><p>A Musi file is direct unit of source code.\nThe smallest useful file can hold bindings and a final expression, and that same top-to-bottom reading model continues even after programs get larger.\nThis page exists so package structure never hides the simpler truth that code still starts in files.</p>\n<h2 id=\"why-it-matters\"><a href=\"#why-it-matters\">Why it matters</a></h2><p>People often jump into package layout too early and lose track of where language code actually lives.\nIf docs only explain package commands, beginners still ask what one file means, what belongs in it, and how much ceremony it needs.\nAnchoring on the file keeps later organization topics grounded.</p>\n<h2 id=\"walk-through-it\"><a href=\"#walk-through-it\">Walk through it</a></h2><p>Read the example as one self-contained source file.\nEverything important is visible in one place: one binding, one final result, one clear evaluation path.\nWhen learning, create scratch files freely, keep related code close together, and only reach for multi-file organization when the single-file story starts feeling cramped.</p>\n<h2 id=\"try-it-next\"><a href=\"#try-it-next\">Try it next</a></h2><ul>\n<li>Create one scratch Musi file.</li>\n<li>Put one binding and one final expression in it.</li>\n<li>Check it directly with <code>music check</code>.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not assume package structure replaces the need to understand one-file evaluation flow.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/organization/packages\">Packages</a> to see when a single file stops being enough.</p>\n",
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
				"id": "in-this-chapter",
				"text": "In this chapter"
			},
			{
				"depth": 2,
				"id": "why-it-matters",
				"text": "Why it matters"
			},
			{
				"depth": 2,
				"id": "walk-through-it",
				"text": "Walk through it"
			},
			{
				"depth": 2,
				"id": "try-it-next",
				"text": "Try it next"
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
		"html": "<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">musi</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> new</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> hello</span></span>\n<span class=\"line\"><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">cd</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> hello</span></span>\n<span class=\"line\"><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">musi</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> run</span></span></code></pre></section></div><h2 id=\"in-this-chapter\"><a href=\"#in-this-chapter\">In this chapter</a></h2><p>A package groups source files, manifest data, and package-level commands under one project root.\nThe <code>musi new hello</code> flow shows what package work looks like when you are no longer just checking one scratch file.\nThis is about project shape, not new language semantics.</p>\n<h2 id=\"why-it-matters\"><a href=\"#why-it-matters\">Why it matters</a></h2><p>Users need a clear moment where they switch from &quot;I am learning syntax in one file&quot; to &quot;I am building a project I will rerun, test, and grow.&quot;\nWithout that transition, <code>musi run</code>, <code>musi test</code>, and manifest concepts feel arbitrary.\nA practical package example gives those commands a home.</p>\n<h2 id=\"walk-through-it\"><a href=\"#walk-through-it\">Walk through it</a></h2><p>Read the command sequence as package lifecycle: create project, move into root, then run package entry point.\nAfter that, inspect generated structure and connect it back to earlier file model: package entry is still just Musi source, now managed by project tooling.\nUse package workflow when code needs multiple files, dependency tracking, or repeatable commands.</p>\n<h2 id=\"try-it-next\"><a href=\"#try-it-next\">Try it next</a></h2><ul>\n<li>Create one package with <code>musi new</code>.</li>\n<li>Open generated entry file.</li>\n<li>Run <code>musi run</code> from package root.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not assume package workflow makes direct <code>music</code> file work obsolete.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/organization/imports-and-exports\">Imports and exports</a> to connect files without turning everything public.</p>\n",
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
				"id": "in-this-chapter",
				"text": "In this chapter"
			},
			{
				"depth": 2,
				"id": "why-it-matters",
				"text": "Why it matters"
			},
			{
				"depth": 2,
				"id": "walk-through-it",
				"text": "Walk through it"
			},
			{
				"depth": 2,
				"id": "try-it-next",
				"text": "Try it next"
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
		"html": "<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">export</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> answer </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 42</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> Option</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> import</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> \"@std/option\"</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> Local</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> import</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> \"./index.ms\"</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Local</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">.answer;</span></span></code></pre></section></div><h2 id=\"in-this-chapter\"><a href=\"#in-this-chapter\">In this chapter</a></h2><p>Imports bring code or modules into scope, and exports decide which names other files may use.\nThis page pairs a standard-library import with a local export/import cycle so the boundary is visible from both directions.\nThat makes module flow concrete instead of abstract.</p>\n<h2 id=\"why-it-matters\"><a href=\"#why-it-matters\">Why it matters</a></h2><p>As soon as code crosses file boundaries, users ask two questions: &quot;how do I bring this in?&quot; and &quot;how do I expose that out?&quot;\nIf docs only answer one of them, people still end up guessing about module ownership.\nClear import/export examples help keep dependencies explicit and public surfaces small.</p>\n<h2 id=\"walk-through-it\"><a href=\"#walk-through-it\">Walk through it</a></h2><p>Read <code>let Option := import &quot;@std/option&quot;;</code> as binding imported module to a local name you can call through.\nThen read <code>export let answer := 42;</code> as deliberate publication of one binding, not automatic exposure of whole file.\nUse <code>export opaque</code> when callers should see the exported name but not rely on representation details.\nWhen organizing code, import only what you need, export only what other files truly depend on, and keep local helpers unexported by default.</p>\n<h2 id=\"try-it-next\"><a href=\"#try-it-next\">Try it next</a></h2><ul>\n<li>Import one <code>@std</code> module into a file.</li>\n<li>Export one helper from another file.</li>\n<li>Use exported name through local import.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not use imports and exports as substitute for deciding which names should stay local.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/types/type-annotations\">Type annotations</a> to make important value shapes explicit once code spans more than one toy file.</p>\n",
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
				"id": "in-this-chapter",
				"text": "In this chapter"
			},
			{
				"depth": 2,
				"id": "why-it-matters",
				"text": "Why it matters"
			},
			{
				"depth": 2,
				"id": "walk-through-it",
				"text": "Walk through it"
			},
			{
				"depth": 2,
				"id": "what-musi-does-instead",
				"text": "What Musi does instead"
			},
			{
				"depth": 2,
				"id": "try-it-next",
				"text": "Try it next"
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
		"html": "<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> port : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 8080</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> twice</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> (x : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">) : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> x </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">+</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> x;</span></span>\n<span class=\"line\"><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">twice</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(port);</span></span></code></pre></section></div><p>This chapter exists because several pieces of Musi use punctuation that looks related at first glance.\nThe good news is that the roles separate cleanly once you read them by context.</p>\n<h2 id=\"in-this-chapter\"><a href=\"#in-this-chapter\">In this chapter</a></h2><p>Here are the core forms to recognize:</p>\n<ul>\n<li>annotation: <code>value : T</code></li>\n<li>pure callable type: <code>T -&gt; U</code></li>\n<li>effectful callable type: <code>T ~&gt; U</code></li>\n<li>labeled callable type: <code>(port : Int, secure : Bool) -&gt; Response</code></li>\n<li>implements constraint: <code>where T : Eq</code></li>\n<li>subtype constraint: <code>where T &lt;: Number</code></li>\n<li>variant payload definition: <code>| Configured(port : Int)</code></li>\n</ul>\n<p>Variant payloads use constructor-style declarations such as <code>| Configured(port : Int)</code>.\nInside that payload list, <code>:</code> labels a payload field and its type.</p>\n<h2 id=\"why-it-matters\"><a href=\"#why-it-matters\">Why it matters</a></h2><p>Without this separation, readers ask reasonable questions such as:</p>\n<ul>\n<li>is <code>Configured : Int</code> an annotation?</li>\n<li>how is a variant payload different from <code>x : Int</code>?</li>\n<li>why does <code>where T : Eq</code> use <code>:</code> too?</li>\n</ul>\n<p>The answer is context.\nMusi keeps <code>:</code> for &quot;name relates to type-like thing&quot; contexts, but the surrounding form tells you which job it is doing.\n<code>where</code> marks constraints.\nConstructor-style payload syntax marks variants.\nPlain expression position marks ordinary annotations.</p>\n<h2 id=\"walk-through-it\"><a href=\"#walk-through-it\">Walk through it</a></h2><p>Read each form with its own sentence:</p>\n<ul>\n<li><code>x : Int</code> means x has type Int.</li>\n<li><code>(port : Int) -&gt; Response</code> means a callable accepts one Int parameter labelled <code>port</code>.</li>\n<li><code>where T : Eq</code> means T must implement Eq.</li>\n<li><code>where T &lt;: Number</code> means T must be below Number in subtype relation.</li>\n<li><code>| Configured(port : Int)</code> means this variant carries a payload field named <code>port</code> of type <code>Int</code>.</li>\n</ul>\n<p>A useful comparison for C-like readers:</p>\n<ul>\n<li>annotations feel closest to typed variable declarations</li>\n<li>constraints are more like generic bounds</li>\n<li>variant payloads are closer to constructor signatures</li>\n</ul>\n<h2 id=\"what-musi-does-instead\"><a href=\"#what-musi-does-instead\">What Musi does instead</a></h2><p>Musi keeps variant payloads close to constructor syntax.\nTuple types and tuple values both use parentheses and commas. Sum types use <code>+</code> where a type needs to describe a choice between alternatives.</p>\n<h2 id=\"try-it-next\"><a href=\"#try-it-next\">Try it next</a></h2><ul>\n<li>Annotate one binding and one function result.</li>\n<li>Write one generic helper with <code>where T : Eq</code>.</li>\n<li>Define one variant with a named payload field.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not read every <code>:</code> the same way without looking at the surrounding form.\nIn Musi, context is the real disambiguator.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/types/type-inference\">Type inference</a> to learn when Musi can fill in the obvious parts for you.</p>\n",
		"summaryHtml": "Learn the visible boundary markers for values, constraints, callable types, and data variants."
	},
	{
		"locale": "en",
		"id": "callable-types",
		"kind": "chapter",
		"partId": "types",
		"partTitle": "Types",
		"path": "/learn/language/types/callable-types",
		"canonicalPath": "/learn/language/types/callable-types",
		"aliases": [
			"/docs/language/types/callable-types"
		],
		"questions": [],
		"title": "Callable types",
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
				"id": "in-this-chapter",
				"text": "In this chapter"
			},
			{
				"depth": 2,
				"id": "why-it-matters",
				"text": "Why it matters"
			},
			{
				"depth": 2,
				"id": "walk-through-it",
				"text": "Walk through it"
			},
			{
				"depth": 2,
				"id": "try-it-next",
				"text": "Try it next"
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
		"html": "<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> Pure</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> Int</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> -></span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> Int</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> Effectful</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> Int</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> ~></span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> Int</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Pure</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span></code></pre></section></div><h2 id=\"in-this-chapter\"><a href=\"#in-this-chapter\">In this chapter</a></h2><p>Callable types describe values that can be called.\n<code>T -&gt; U</code> describes a callable from <code>T</code> to <code>U</code>.\n<code>T ~&gt; U</code> marks a callable whose evaluation may involve effects.</p>\n<h2 id=\"why-it-matters\"><a href=\"#why-it-matters\">Why it matters</a></h2><p>Function definitions show parameters and result types, but APIs often need to talk about function values directly.\nWhen a value accepts or returns a callable, the type spelling tells readers whether ordinary evaluation is enough or effect handling may matter.</p>\n<h2 id=\"walk-through-it\"><a href=\"#walk-through-it\">Walk through it</a></h2><p>Read <code>Int -&gt; Int</code> as a pure transformation from one integer to another.\nRead <code>Int ~&gt; Int</code> as the same input and output shape with effectful evaluation.\nThe tilde is the visible clue that capability flow can matter.</p>\n<h2 id=\"try-it-next\"><a href=\"#try-it-next\">Try it next</a></h2><ul>\n<li>Annotate one value with a pure callable type.</li>\n<li>Annotate one value with an effectful callable type.</li>\n<li>Explain which one can request effects while running.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not read <code>~&gt;</code> as a different argument shape. It marks effectful evaluation, not a different number of inputs.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/types/type-inference\">Type inference</a> to see when annotations can be omitted.</p>\n",
		"summaryHtml": "Use <code>T -&gt; U</code> for pure callables and <code>T ~&gt; U</code> for effectful callables."
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
				"id": "in-this-chapter",
				"text": "In this chapter"
			},
			{
				"depth": 2,
				"id": "why-it-matters",
				"text": "Why it matters"
			},
			{
				"depth": 2,
				"id": "walk-through-it",
				"text": "Walk through it"
			},
			{
				"depth": 2,
				"id": "try-it-next",
				"text": "Try it next"
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
		"html": "<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> port : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 8080</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> next </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> port </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">+</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 1</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">next;</span></span></code></pre></section></div><h2 id=\"in-this-chapter\"><a href=\"#in-this-chapter\">In this chapter</a></h2><p>Type inference lets Musi recover some type information from surrounding code so you do not have to repeat every obvious fact.\nThe example keeps one explicit <code>Int</code> annotation and then omits it on derived binding <code>next</code>.\nThat contrast is whole lesson: inference is convenience anchored by context, not magic guesswork.</p>\n<h2 id=\"why-it-matters\"><a href=\"#why-it-matters\">Why it matters</a></h2><p>Users want shorter code, but they also want to know when shorter code stops being clear.\nIf docs celebrate inference without boundaries, beginners start guessing what compiler can or cannot recover.\nA small example with one kept annotation and one omitted annotation teaches better instinct than a broad promise.</p>\n<h2 id=\"walk-through-it\"><a href=\"#walk-through-it\">Walk through it</a></h2><p>Read annotated <code>port</code> as source of type information.\nThen read <code>let next := port + 1;</code> as value whose type becomes obvious because operands already constrain it.\nWhen editing real code, remove only the annotations that feel redundant after you can still explain the type from nearby information.</p>\n<h2 id=\"try-it-next\"><a href=\"#try-it-next\">Try it next</a></h2><ul>\n<li>Start with one annotated value.</li>\n<li>Add derived binding without annotation.</li>\n<li>Put annotation back if meaning becomes harder to read.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not rely on inference in examples you cannot explain by tracing the surrounding code.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/types/generics\">Generics</a> to reuse one definition across many types once the annotation story is clear.</p>\n",
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
				"id": "in-this-chapter",
				"text": "In this chapter"
			},
			{
				"depth": 2,
				"id": "why-it-matters",
				"text": "Why it matters"
			},
			{
				"depth": 2,
				"id": "walk-through-it",
				"text": "Walk through it"
			},
			{
				"depth": 2,
				"id": "type-constructors-as-parameters",
				"text": "Type constructors as parameters"
			},
			{
				"depth": 2,
				"id": "value-indexed-parameters",
				"text": "Value-indexed parameters"
			},
			{
				"depth": 2,
				"id": "try-it-next",
				"text": "Try it next"
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
		"html": "<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> identityFn</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">T</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">] (input : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">T</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">) : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">T</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> input;</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> tools </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> { identity </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> identityFn };</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> Box1</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">T</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">] </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> data</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> {</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">  | </span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">Box1</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">T</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">)</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">};</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> Keeps</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">F</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Type</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> -></span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> Type</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">] </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> class</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> {</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">  let</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> keep</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(value : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">F</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">Int</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">]) : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">F</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">Int</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">];</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">};</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> boxKeeps </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> instance</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> Keeps</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Box1</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">] {</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">  let</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> keep</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(value : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Box1</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">Int</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">]) : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Box1</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">Int</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">] </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> value;</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">};</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">tools.</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">identity</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">Int</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">](</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">8080</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">);</span></span></code></pre></section></div><h2 id=\"in-this-chapter\"><a href=\"#in-this-chapter\">In this chapter</a></h2><p>Generics let one definition work across many concrete types by abstracting over a type parameter such as <code>T</code>.\nThis page keeps the example deliberately small: identity function in generic form, then one explicit application at <code>Int</code>.\nThat is enough to teach the core move without turning the chapter into a full type-system reference.</p>\n<h2 id=\"why-it-matters\"><a href=\"#why-it-matters\">Why it matters</a></h2><p>Sooner or later users ask how to avoid copy-pasting same function for several types.\nGenerics answer that need, but they become overwhelming when introduced before annotations and inference are stable.\nHere the goal is practical reuse: one function shape, many compatible inputs.</p>\n<h2 id=\"walk-through-it\"><a href=\"#walk-through-it\">Walk through it</a></h2><p>Read <code>[T]</code> as parameterization over a type, not over a runtime value.\nThen trace where <code>T</code> appears in input and output positions to understand what stays same across all uses.\nAt the call site, <code>identityFn[Int](port)</code> makes type application explicit; that can be useful whenever you want the chosen type to be obvious to reader.</p>\n<p>Generic functions are still values. You can put one in a record, pass it through another binding, then type-apply it later:</p>\n<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span>let identityFn[T] (input : T) : T := input;</span></span>\n<span class=\"line\"><span>let tools := { identity := identityFn };</span></span>\n<span class=\"line\"><span></span></span>\n<span class=\"line\"><span>tools.identity[Int](8080);</span></span></code></pre><p>That matters because Musi does not treat generic functions as a separate second-class feature. The name, the alias, and the record field all carry the same polymorphic value.</p>\n<h2 id=\"type-constructors-as-parameters\"><a href=\"#type-constructors-as-parameters\">Type constructors as parameters</a></h2><p>A normal type parameter stands for one concrete type:</p>\n<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span>let identityFn[T] (input : T) : T := input;</span></span></code></pre><p>A higher-kinded parameter stands for a type constructor. Write its kind with type arrows:</p>\n<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span>F : Type -> Type</span></span></code></pre><p>That says <code>F</code> needs one type argument before it becomes a concrete type. <code>Option</code> fits that shape because <code>Option[Int]</code> is concrete. A two-argument constructor can be partially applied from the left, so <code>Box2[String]</code> also fits <code>Type -&gt; Type</code>.</p>\n<p>Use this when an abstraction cares about the outer shape. <code>Functor[Option]</code> talks about mapping inside <code>Option</code>; <code>Functor[Box2[String]]</code> talks about mapping inside a two-argument constructor after the first argument has already been chosen.</p>\n<p>Musi uses <code>F : Type -&gt; Type</code> instead of placeholder syntax such as <code>F[_]</code>. The same arrow notation already appears in function types, so higher-kinded parameters reuse a spelling readers already know.</p>\n<h2 id=\"value-indexed-parameters\"><a href=\"#value-indexed-parameters\">Value-indexed parameters</a></h2><p>A bracket parameter can also name a compile-time value when it has a value kind such as <code>Nat</code>:</p>\n<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span>let Vec[T, n : Nat] := data {</span></span>\n<span class=\"line\"><span>  | Nil() -> Vec[T, 0]</span></span>\n<span class=\"line\"><span>  | Cons(head : T, tail : Vec[T, n]) -> Vec[T, n + 1]</span></span>\n<span class=\"line\"><span>};</span></span></code></pre><p>Use this when a type should remember shape information such as length or protocol state. Keep normal generics for ordinary reusable code; move to value-indexed parameters only when that extra information helps callers.</p>\n<h2 id=\"try-it-next\"><a href=\"#try-it-next\">Try it next</a></h2><ul>\n<li>Write one generic function with <code>[T]</code>.</li>\n<li>Call it once with <code>Int</code>.</li>\n<li>Store it in a record field and call that field with <code>Int</code>.</li>\n<li>Write one class that accepts <code>F : Type -&gt; Type</code> and instantiate it with <code>Option</code>.</li>\n<li>Partially apply a two-argument type constructor and pass it where <code>Type -&gt; Type</code> is expected.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not introduce several type parameters before one-parameter generic code feels easy to read.\nFor higher-kinded parameters, do not write <code>F[T]</code> in the parameter list. Write the kind once: <code>F : Type -&gt; Type</code>.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/types/type-tests-and-casts\">Type tests and casts</a> to handle explicit type-facing checks near boundaries.</p>\n",
		"summaryHtml": "Write reusable functions over many types without losing clarity."
	},
	{
		"locale": "en",
		"id": "type-tests-and-casts",
		"kind": "chapter",
		"partId": "types",
		"partTitle": "Types",
		"path": "/learn/language/types/type-tests-and-casts",
		"canonicalPath": "/learn/language/types/type-tests-and-casts",
		"aliases": [
			"/docs/language/types/type-tests-and-casts"
		],
		"questions": [],
		"title": "Type tests and casts",
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
				"id": "in-this-chapter",
				"text": "In this chapter"
			},
			{
				"depth": 2,
				"id": "why-it-matters",
				"text": "Why it matters"
			},
			{
				"depth": 2,
				"id": "walk-through-it",
				"text": "Walk through it"
			},
			{
				"depth": 2,
				"id": "try-it-next",
				"text": "Try it next"
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
		"html": "<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> value </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 42</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> isInt </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> value </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:?</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> Int</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> same </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> value </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:?></span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> Int</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">same;</span></span></code></pre></section></div><h2 id=\"in-this-chapter\"><a href=\"#in-this-chapter\">In this chapter</a></h2><p>Type-facing expressions keep runtime checks visible.\n<code>value :? Type</code> asks whether a value fits a type.\n<code>value :?&gt; Type</code> asks for an explicit cast to that type.</p>\n<h2 id=\"why-it-matters\"><a href=\"#why-it-matters\">Why it matters</a></h2><p>Dynamic boundaries, foreign data, and broad APIs sometimes need a visible check before code continues.\nHiding that check inside a helper name can make the risky part disappear.\nThe <code>:?</code> and <code>:?&gt;</code> forms keep the type question attached to the value being checked.</p>\n<h2 id=\"walk-through-it\"><a href=\"#walk-through-it\">Walk through it</a></h2><p>Read <code>value :? Int</code> as a question.\nRead <code>value :?&gt; Int</code> as a conversion request that should only appear where the cast is justified.\nWhen possible, prefer earlier precise types so casts stay rare.</p>\n<h2 id=\"try-it-next\"><a href=\"#try-it-next\">Try it next</a></h2><ul>\n<li>Write one type test for a value.</li>\n<li>Write one cast near a boundary.</li>\n<li>Move the cast closer to the boundary if it drifts into ordinary domain code.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not use casts to avoid modeling data precisely. Casts should explain a boundary, not erase one.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/types/forall-types\">Forall types</a> for explicit universal type forms.</p>\n",
		"summaryHtml": "Use <code>:?</code> to test a value against a type and <code>:?&gt;</code> for an explicit cast."
	},
	{
		"locale": "en",
		"id": "forall-types",
		"kind": "chapter",
		"partId": "types",
		"partTitle": "Types",
		"path": "/learn/language/types/forall-types",
		"canonicalPath": "/learn/language/types/forall-types",
		"aliases": [
			"/docs/language/types/forall-types"
		],
		"questions": [],
		"title": "Forall types",
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
				"id": "in-this-chapter",
				"text": "In this chapter"
			},
			{
				"depth": 2,
				"id": "why-it-matters",
				"text": "Why it matters"
			},
			{
				"depth": 2,
				"id": "walk-through-it",
				"text": "Walk through it"
			},
			{
				"depth": 2,
				"id": "try-it-next",
				"text": "Try it next"
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
		"html": "<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> identityFn</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">T</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">] (input : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">T</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">) : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">T</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> input;</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> identityType </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> forall</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">T</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Type</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">) </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">-></span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> T</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> -></span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> T</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">identityFn</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">Int</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">](</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">8080</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">);</span></span></code></pre></section></div><h2 id=\"in-this-chapter\"><a href=\"#in-this-chapter\">In this chapter</a></h2><p><code>forall</code> introduces a type variable inside a type expression.\nMost day-to-day generic functions use bracket parameters on <code>let</code> definitions.\n<code>forall</code> is the explicit form for type-level expressions that need to name the variable directly.</p>\n<h2 id=\"why-it-matters\"><a href=\"#why-it-matters\">Why it matters</a></h2><p>Some APIs talk about polymorphic values themselves, not just generic definitions.\nWhen that happens, the type needs a place to say &quot;this works for every <code>T</code>.&quot;\n<code>forall(T : Type) -&gt; T -&gt; T</code> makes that binding visible.\nHigher-kinded binders use the same form with an arrow kind: <code>forall(F : Type -&gt; Type) -&gt; F[Int] -&gt; F[Int]</code>.</p>\n<h2 id=\"walk-through-it\"><a href=\"#walk-through-it\">Walk through it</a></h2><p>Read <code>forall(T : Type) -&gt; ...</code> as &quot;for every type <code>T</code>, the rest of the type follows.&quot;\nRead <code>forall(F : Type -&gt; Type) -&gt; ...</code> as &quot;for every unary type constructor <code>F</code>, the rest of the type follows.&quot;\nThen read <code>T -&gt; T</code> as the callable shape under that binding.\nUse this form when the type is the subject of the code, not just a detail inferred from a generic function.</p>\n<p>The bracket form on a definition and the <code>forall</code> form in a type describe the same idea from two sides:</p>\n<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span>let identityFn[T] (input : T) : T := input;</span></span>\n<span class=\"line\"><span>let identityType := forall(T : Type) -> T -> T;</span></span></code></pre><p><code>identityFn</code> is a polymorphic value. If you move it through another value, the <code>forall</code> shape moves with it.</p>\n<h2 id=\"try-it-next\"><a href=\"#try-it-next\">Try it next</a></h2><ul>\n<li>Read one generic function signature.</li>\n<li>Write the matching callable type with <code>forall</code>.</li>\n<li>Store the generic function in a record and read the field type as a <code>forall</code> value.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not add <code>forall</code> to every generic function definition. Bracket parameters already cover the common case.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/abstractions/classes\">Classes</a> to move from type parameters into reusable behavior contracts.</p>\n",
		"summaryHtml": "Use <code>forall</code> when a type expression must bind a type variable explicitly."
	},
	{
		"locale": "en",
		"id": "dependent-types",
		"kind": "chapter",
		"partId": "types",
		"partTitle": "Types",
		"path": "/learn/language/types/dependent-types",
		"canonicalPath": "/learn/language/types/dependent-types",
		"aliases": [
			"/docs/language/types/dependent-types"
		],
		"questions": [],
		"title": "Dependent types",
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
				"id": "in-this-chapter",
				"text": "In this chapter"
			},
			{
				"depth": 2,
				"id": "value-parameters-in-type-parameter-lists",
				"text": "Value parameters in type parameter lists"
			},
			{
				"depth": 2,
				"id": "type-equality-constraints",
				"text": "Type equality constraints"
			},
			{
				"depth": 2,
				"id": "partial-marks-runtime-only-definitions",
				"text": "partial marks runtime-only definitions"
			},
			{
				"depth": 2,
				"id": "what-musi-does-not-ask-you-to-write",
				"text": "What Musi does not ask you to write"
			},
			{
				"depth": 2,
				"id": "try-it-next",
				"text": "Try it next"
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
		"html": "<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> Vec</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">T</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">, n : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Nat</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">] </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> data</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> {</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">  | </span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">Nil</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">() </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">-></span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> Vec</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">T</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">, </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">0</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">]</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">  | </span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">Cons</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(</span><span style=\"color:#E36209;--shiki-dark:#FFAB70\">head</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> : </span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">T</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">, </span><span style=\"color:#E36209;--shiki-dark:#FFAB70\">tail</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> : </span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">Vec</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">T</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">, </span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">n</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">]) </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">-></span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> Vec</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">T</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">, n </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">+</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 1</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">]</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">};</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">partial </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> parsePort</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(text : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">String</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">) : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 0</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span></code></pre></section></div><h2 id=\"in-this-chapter\"><a href=\"#in-this-chapter\">In this chapter</a></h2><p>Musi allows type parameters to be ordinary types or value-shaped indices. A type parameter such as <code>T</code> has kind <code>Type</code>. Universe names such as <code>Type0</code> are accepted when you want to make the level explicit. A value index such as <code>n : Nat</code> has kind <code>Nat</code>, so it can describe a length, state number, port number, protocol phase, or other compile-time value.</p>\n<p>This is dependent typing in the practical language sense: useful values can appear in types. It is not a proof-assistant surface. You write programs, data shapes, and constraints; you do not write proof terms just to make normal code compile.</p>\n<h2 id=\"value-parameters-in-type-parameter-lists\"><a href=\"#value-parameters-in-type-parameter-lists\">Value parameters in type parameter lists</a></h2><p>Use <code>name : Kind</code> in brackets when the parameter is not just another concrete type:</p>\n<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span>let Vec[T, n : Nat] := data {</span></span>\n<span class=\"line\"><span>  | Nil() -> Vec[T, 0]</span></span>\n<span class=\"line\"><span>  | Cons(head : T, tail : Vec[T, n]) -> Vec[T, n + 1]</span></span>\n<span class=\"line\"><span>};</span></span></code></pre><p>Read that as:</p>\n<ul>\n<li><code>T</code> is an item type.</li>\n<li><code>n : Nat</code> is a compile-time natural number.</li>\n<li><code>Nil</code> constructs a <code>Vec[T, 0]</code>.</li>\n<li><code>Cons</code> takes a tail of length <code>n</code> and returns length <code>n + 1</code>.</li>\n</ul>\n<p>The constructor result after <code>-&gt;</code> is part of the variant declaration. It lets a variant say which indexed instance of the data type it creates.</p>\n<h2 id=\"type-equality-constraints\"><a href=\"#type-equality-constraints\">Type equality constraints</a></h2><p>Use <code>~=</code> for solver-facing type equality. It reads as &quot;has the same type-level shape as&quot; and belongs in constraints and type reasoning, not in ordinary value comparison. Runtime equality stays <code>=</code>.</p>\n<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span>let choose[A, B] (left : A, right : B) : A</span></span>\n<span class=\"line\"><span>where A ~= B</span></span>\n<span class=\"line\"><span>:= left;</span></span></code></pre><p>Use this when two separately named type expressions must be known equal by the type checker.</p>\n<h2 id=\"partial-marks-runtime-only-definitions\"><a href=\"#partial-marks-runtime-only-definitions\"><code>partial</code> marks runtime-only definitions</a></h2><p>A definition referenced from a type must be usable by the type checker. Put <code>partial</code> on a <code>let</code> when the definition is intentionally runtime-only, may diverge, or depends on behavior that should not be evaluated during type checking.</p>\n<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span>partial let parsePort(text : String) : Int := 0;</span></span></code></pre><p><code>partial</code> is a modifier like <code>foreign</code>, but the two do not combine. A foreign binding is already runtime-bound through an ABI boundary.</p>\n<h2 id=\"what-musi-does-not-ask-you-to-write\"><a href=\"#what-musi-does-not-ask-you-to-write\">What Musi does not ask you to write</a></h2><p>Musi does not make everyday users write <code>refl</code> proofs, tactic scripts, or theorem-prover terms. If two indexed types are equal by the language rules and available total definitions, the solver should see that. If code is runtime-only, mark it <code>partial</code> instead of pretending it can run in the type checker.</p>\n<h2 id=\"try-it-next\"><a href=\"#try-it-next\">Try it next</a></h2><ul>\n<li>Write an indexed data type with one <code>Nat</code> parameter.</li>\n<li>Give each variant an explicit <code>-&gt;</code> result when it changes the index.</li>\n<li>Use <code>partial</code> on parsing, FFI wrappers, or non-total helpers that should stay out of type computation.</li>\n<li>Use <code>~=</code> only when you need type-level equality, not normal value equality.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not use <code>:</code> for constraints. <code>:</code> annotates a name with a type or kind. Constraints belong after <code>where</code>, and type equality uses <code>~=</code>.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/abstractions/classes\">Classes</a> to see how reusable behavior contracts sit on top of types.</p>\n",
		"summaryHtml": "Use value-indexed types, indexed data results, <code>partial</code>, and <code>~=</code> without turning Musi into a proof assistant."
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
				"id": "in-this-chapter",
				"text": "In this chapter"
			},
			{
				"depth": 2,
				"id": "why-it-matters",
				"text": "Why it matters"
			},
			{
				"depth": 2,
				"id": "walk-through-it",
				"text": "Walk through it"
			},
			{
				"depth": 2,
				"id": "try-it-next",
				"text": "Try it next"
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
		"html": "<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> Eq</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">T</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">] </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> class</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> {</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">  let</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> (=)</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> (a : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">T</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">, b : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">T</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">) : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Bool</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">  law</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> reflexive</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> (x : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">T</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">) </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> .True</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">};</span></span></code></pre></section></div><h2 id=\"in-this-chapter\"><a href=\"#in-this-chapter\">In this chapter</a></h2><p>A class describes a behavior surface for values of some type.\nIn this example, <code>Eq[T]</code> says that values of type <code>T</code> can be compared for equality, and the law names one semantic expectation that should hold.\nThe key idea is contract first: what operations and guarantees exist before any one concrete implementation appears.</p>\n<h2 id=\"why-it-matters\"><a href=\"#why-it-matters\">Why it matters</a></h2><p>Users coming from inheritance-heavy languages can misread classes immediately.\nThis page should prevent that by showing classes as behavior descriptions, not object hierarchies.\nOnce that distinction is clear, later instance and law pages feel like natural follow-ups instead of confusing add-ons.</p>\n<h2 id=\"walk-through-it\"><a href=\"#walk-through-it\">Walk through it</a></h2><p>Read <code>let Eq[T] := class { ... };</code> as definition of shared capability over some type parameter <code>T</code>.\nInside it, focus first on operation shape <code>let (=) ... : Bool;</code>, then on the law as statement about meaning rather than syntax decoration.\nWhen writing your own first class, keep member count tiny and choose behavior that several concrete types could plausibly share.</p>\n<h2 id=\"try-it-next\"><a href=\"#try-it-next\">Try it next</a></h2><ul>\n<li>Define one class with one operation.</li>\n<li>Name the behavior after what callers need.</li>\n<li>List one or two concrete types that should satisfy it.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not read Musi classes as inheritance trees with hidden fields or subclass state.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/abstractions/instances\">Instances</a> to see how one concrete type fulfills that behavior contract.</p>\n",
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
				"id": "in-this-chapter",
				"text": "In this chapter"
			},
			{
				"depth": 2,
				"id": "why-it-matters",
				"text": "Why it matters"
			},
			{
				"depth": 2,
				"id": "walk-through-it",
				"text": "Walk through it"
			},
			{
				"depth": 2,
				"id": "try-it-next",
				"text": "Try it next"
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
		"html": "<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> eqInt </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> instance</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> Eq</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">] {</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">  let</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> (=)</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> (a : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">, b : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">) : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Bool</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> .True</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">};</span></span></code></pre></section></div><h2 id=\"in-this-chapter\"><a href=\"#in-this-chapter\">In this chapter</a></h2><p>An instance says how one concrete type satisfies one class.\nWhere the class page defined behavior shape, this page fills in actual implementation for <code>Eq[Int]</code>.\nThat split is important because it keeps reusable abstraction separate from concrete decision.</p>\n<h2 id=\"why-it-matters\"><a href=\"#why-it-matters\">Why it matters</a></h2><p>If users only see class declarations, they still ask where the real behavior lives.\nIf they see classes and instances collapsed together too early, they lose the difference between contract and implementation.\nA tiny <code>Int</code> instance makes the handoff between those layers easy to follow.</p>\n<h2 id=\"walk-through-it\"><a href=\"#walk-through-it\">Walk through it</a></h2><p>Read <code>instance Eq[Int]</code> as commitment to implement <code>Eq</code> behavior for the specific type <code>Int</code>.\nInside the block, compare member names with class definition and notice that instance must satisfy required surface.\nWhen writing your own first instance, pick one small class and one obvious concrete type so the mapping from contract to implementation is immediate.</p>\n<h2 id=\"try-it-next\"><a href=\"#try-it-next\">Try it next</a></h2><ul>\n<li>Take one small class definition.</li>\n<li>Add one instance for <code>Int</code> or another simple type.</li>\n<li>Use same member names so relation stays obvious.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not define several classes and several instances at once before one mapping feels clear.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/abstractions/laws\">Laws</a> to capture what those abstractions are supposed to mean, not only how they are spelled.</p>\n",
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
				"id": "in-this-chapter",
				"text": "In this chapter"
			},
			{
				"depth": 2,
				"id": "why-it-matters",
				"text": "Why it matters"
			},
			{
				"depth": 2,
				"id": "walk-through-it",
				"text": "Walk through it"
			},
			{
				"depth": 2,
				"id": "try-it-next",
				"text": "Try it next"
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
		"html": "<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> Eq</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">T</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">] </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> class</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> {</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">  let</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> (=)</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> (a : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">T</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">, b : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">T</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">) : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Bool</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">  law</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> reflexive</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> (x : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">T</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">) </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> .True</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">};</span></span></code></pre></section></div><h2 id=\"in-this-chapter\"><a href=\"#in-this-chapter\">In this chapter</a></h2><p>Laws document semantic expectations of an abstraction.\nIn the <code>Eq</code> example, <code>reflexive</code> states that comparing a value with itself should succeed.\nThat means class definitions can communicate not only available operations, but also what correct behavior is supposed to preserve.</p>\n<h2 id=\"why-it-matters\"><a href=\"#why-it-matters\">Why it matters</a></h2><p>Without laws, abstractions can be technically implemented yet still behave in surprising or inconsistent ways.\nUsers need to know that Musi has a place to state these expectations close to the abstraction itself.\nThat keeps class design from becoming only a bag of function signatures.</p>\n<h2 id=\"walk-through-it\"><a href=\"#walk-through-it\">Walk through it</a></h2><p>Read a law as part of abstraction meaning.\nIt is not there to add another callable member; it tells implementers and readers what behavior should hold across instances.\nWhen adding laws, start with one obvious property that would help another reader tell correct implementation from suspicious one.</p>\n<h2 id=\"try-it-next\"><a href=\"#try-it-next\">Try it next</a></h2><ul>\n<li>Look at one class operation.</li>\n<li>Write one law that expresses expected behavior of that operation.</li>\n<li>Ask how an instance could violate it.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not treat laws as decorative comments that can say anything without relation to the operations they describe.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/effects-runtime/effects\">Effects</a> to shift from shared behavior contracts into explicit requests for external work.</p>\n",
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
				"id": "in-this-chapter",
				"text": "In this chapter"
			},
			{
				"depth": 2,
				"id": "why-it-matters",
				"text": "Why it matters"
			},
			{
				"depth": 2,
				"id": "walk-through-it",
				"text": "Walk through it"
			},
			{
				"depth": 2,
				"id": "try-it-next",
				"text": "Try it next"
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
		"html": "<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> console </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> effect</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> {</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">  let</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> readln</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> () : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">String</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">};</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">request</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> console.</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">readln</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">();</span></span></code></pre></section></div><h2 id=\"in-this-chapter\"><a href=\"#in-this-chapter\">In this chapter</a></h2><p>An effect describes operations code may request, and <code>request</code> issues one of those requests.\nThis pair of snippets keeps model intentionally small: first define console capability, then request one read operation from it.\nThat is core effect story before handlers, runtime imports, or stdlib layering enter scene.</p>\n<h2 id=\"why-it-matters\"><a href=\"#why-it-matters\">Why it matters</a></h2><p>Users often know hidden side effects from other languages, but Musi wants capability flow to stay visible.\nIf docs jump straight to handlers, the basic question &quot;what is an effect?&quot; never gets a clean answer.\nThis page should make one thing obvious: effectful code is asking for work that something else must eventually provide.</p>\n<h2 id=\"walk-through-it\"><a href=\"#walk-through-it\">Walk through it</a></h2><p>Read effect block as declaration of available operations, not as immediate implementation.\nThen read <code>request console.readln();</code> as explicit request made from code that depends on that capability.\nWhen writing your own examples, keep one effect and one operation at first so the request model stays sharper than the surrounding syntax.</p>\n<h2 id=\"try-it-next\"><a href=\"#try-it-next\">Try it next</a></h2><ul>\n<li>Define one effect with one operation.</li>\n<li>Request that operation once.</li>\n<li>Explain in words what still needs to handle the request.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not skip straight to full handlers before the &quot;request for work&quot; model feels stable.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/effects-runtime/using\">Using</a> to make required capabilities visible in function signatures.</p>\n",
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
		"description": "Track required effects with `using`, understand capability flow, and keep effectful code readable.",
		"group": "Effects and runtime",
		"section": "Effects and runtime",
		"order": 25,
		"slug": "using",
		"summary": "`using` tells readers and the compiler which effects a callable may request.",
		"descriptionHtml": "Track required effects with <code>using</code>, understand capability flow, and keep effectful code readable.",
		"headings": [
			{
				"depth": 2,
				"id": "in-this-chapter",
				"text": "In this chapter"
			},
			{
				"depth": 2,
				"id": "why-it-matters",
				"text": "Why it matters"
			},
			{
				"depth": 2,
				"id": "walk-through-it",
				"text": "Walk through it"
			},
			{
				"depth": 2,
				"id": "what-musi-does-not-have",
				"text": "What Musi does not have"
			},
			{
				"depth": 2,
				"id": "try-it-next",
				"text": "Try it next"
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
		"html": "<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> readClosed</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> () : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">String</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> using</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> { </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Console</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> } </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">  request</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> Console</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">.</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">readln</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">();</span></span></code></pre></section></div><p><code>using</code> is Musi&#39;s way of making effect requirements visible.\nIt answers a practical question: what capabilities may this code request while it runs?</p>\n<h2 id=\"in-this-chapter\"><a href=\"#in-this-chapter\">In this chapter</a></h2><p>Read <code>using { Console }</code> as part of the callable&#39;s public shape.\nEffect sets can include named effects and a rest entry such as <code>...Base</code> when a signature forwards an existing capability set.\nIt says this code may request operations from <code>Console</code>.\nThat is not a hidden implementation detail.\nIt is something callers, handlers, and readers should be able to see.</p>\n<p>A callable type can also show the same distinction:</p>\n<ul>\n<li><code>T -&gt; U</code> for pure callables</li>\n<li><code>T ~&gt; U</code> for effectful callables</li>\n</ul>\n<p>Those two spellings matter because Musi does not hide effectful work behind the same surface as pure transformation.</p>\n<h2 id=\"why-it-matters\"><a href=\"#why-it-matters\">Why it matters</a></h2><p>Users coming from mainstream languages often expect effects to be ambient.\nFile I/O, randomness, or console input might just happen inside the function body with no visible contract.\nMusi goes the other direction.\nIt makes effect use explicit so code review and composition stay honest.</p>\n<p>That visibility matters even more when you later reach handlers.\nIf a request appears, there should be a visible effect story around it.</p>\n<h2 id=\"walk-through-it\"><a href=\"#walk-through-it\">Walk through it</a></h2><p>Read a <code>using</code> example in three parts:</p>\n<ol>\n<li>which effect set appears in the signature?</li>\n<li>which <code>request</code> expressions appear in the body?</li>\n<li>who will eventually handle those requests?</li>\n</ol>\n<p>If the body requests an effect that is not present in the surrounding <code>using</code> set, that is a type error, not a hidden runtime surprise.</p>\n<h2 id=\"what-musi-does-not-have\"><a href=\"#what-musi-does-not-have\">What Musi does not have</a></h2><p>Musi does not let effectful work blend into ordinary pure code without a marker.\nInstead of hoping readers notice side effects by convention, the language makes capability requirements visible.</p>\n<h2 id=\"try-it-next\"><a href=\"#try-it-next\">Try it next</a></h2><ul>\n<li>Write one pure callable type with <code>-&gt;</code>.</li>\n<li>Write one effectful callable type with <code>~&gt;</code>.</li>\n<li>Add <code>using { Console }</code> to a callable that requests console input.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not read <code>using</code> as optional documentation.\nIt is part of the callable&#39;s real contract.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/effects-runtime/handlers\">Handlers</a> to see how requested work gets interpreted.</p>\n",
		"summaryHtml": "<code>using</code> tells readers and the compiler which effects a callable may request."
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
				"id": "in-this-chapter",
				"text": "In this chapter"
			},
			{
				"depth": 2,
				"id": "why-it-matters",
				"text": "Why it matters"
			},
			{
				"depth": 2,
				"id": "walk-through-it",
				"text": "Walk through it"
			},
			{
				"depth": 2,
				"id": "try-it-next",
				"text": "Try it next"
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
		"html": "<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">handle</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> request</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> console.</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">readln</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">() </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">using</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> console {</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">  value </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">=></span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> value;</span></span>\n<span class=\"line\"><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">  readln</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(k) </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">=></span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> resume</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> \"ok\"</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">};</span></span></code></pre></section></div><h2 id=\"in-this-chapter\"><a href=\"#in-this-chapter\">In this chapter</a></h2><p>A handler decides what to do with requested operations.\nThis example handles a console read request, gives a value path for normal completion, and uses <code>resume</code> to continue computation after operation match is handled.\nIt is first place where effect requests meet concrete policy.</p>\n<h2 id=\"why-it-matters\"><a href=\"#why-it-matters\">Why it matters</a></h2><p>Users ask &quot;where does the effect actually get answered?&quot; right after they see <code>request</code>.\nHandlers are that answer, but they are easier to learn once effect and <code>using</code> ideas are already clear.\nA small handler example keeps focus on resolution flow instead of broad control abstractions.</p>\n<h2 id=\"walk-through-it\"><a href=\"#walk-through-it\">Walk through it</a></h2><p>Read <code>handle ... using console</code> as boundary around effectful computation.\nInside the handler, <code>value =&gt; value;</code> covers normal completion, while <code>readln(k) =&gt; resume &quot;ok&quot;;</code> covers specific requested operation and chooses how computation continues.\nIf a handler arm does not resume, the captured continuation does not continue from that request.\nWhen writing your own handlers, start with one operation and one simple resume path so control flow stays easy to trace.</p>\n<h2 id=\"try-it-next\"><a href=\"#try-it-next\">Try it next</a></h2><ul>\n<li>Wrap one requested operation in a handler.</li>\n<li>Add one operation case.</li>\n<li>Resume computation with a concrete replacement value.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not mix several effects and several policies into first handler example.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/effects-runtime/foundation\">Foundation</a> to separate language-level core from runtime and stdlib layers built above it.</p>\n",
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
				"id": "in-this-chapter",
				"text": "In this chapter"
			},
			{
				"depth": 2,
				"id": "why-it-matters",
				"text": "Why it matters"
			},
			{
				"depth": 2,
				"id": "walk-through-it",
				"text": "Walk through it"
			},
			{
				"depth": 2,
				"id": "try-it-next",
				"text": "Try it next"
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
		"html": "<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> Core</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> import</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> \"musi:core\"</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Core</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span></code></pre></section></div><h2 id=\"in-this-chapter\"><a href=\"#in-this-chapter\">In this chapter</a></h2><p><code>musi:core</code> is language foundation layer.\nIt names the lowest-level built-in surface that exists before you start reaching for runtime-backed modules or standard-library conveniences.\nThis page matters because &quot;what is built in?&quot; and &quot;what comes from libraries?&quot; are different questions.</p>\n<h2 id=\"why-it-matters\"><a href=\"#why-it-matters\">Why it matters</a></h2><p>Users get overwhelmed when docs mention effects, runtime, and stdlib as if they are one blurred toolbox.\nA foundation page prevents that blur by giving core layer its own place in the model.\nOnce readers know what belongs to the base layer, later imports make more sense.</p>\n<h2 id=\"walk-through-it\"><a href=\"#walk-through-it\">Walk through it</a></h2><p>Read <code>let Core := import &quot;musi:core&quot;;</code> as explicit access to foundational language surface.\nThen ask what kind of code needs this layer directly: mostly infrastructure, lower-level libraries, or explanation of system boundaries rather than ordinary app code.\nWhen teaching or writing app code, prefer clearer higher-level modules unless you specifically need the foundational layer.</p>\n<h2 id=\"try-it-next\"><a href=\"#try-it-next\">Try it next</a></h2><ul>\n<li>Import <code>musi:core</code> once.</li>\n<li>Note what kind of code would reach for it directly.</li>\n<li>Compare that role with a higher-level stdlib import.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not assume foundational modules are where everyday application code should start by default.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/effects-runtime/runtime\">Runtime</a> to see where host-backed capabilities enter the picture.</p>\n",
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
				"id": "in-this-chapter",
				"text": "In this chapter"
			},
			{
				"depth": 2,
				"id": "why-it-matters",
				"text": "Why it matters"
			},
			{
				"depth": 2,
				"id": "walk-through-it",
				"text": "Walk through it"
			},
			{
				"depth": 2,
				"id": "try-it-next",
				"text": "Try it next"
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
		"html": "<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> Runtime</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> import</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> \"musi:runtime\"</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Runtime</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">.</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">envGet</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\">\"HOME\"</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">);</span></span></code></pre></section></div><h2 id=\"in-this-chapter\"><a href=\"#in-this-chapter\">In this chapter</a></h2><p><code>musi:runtime</code> exposes runtime-backed capabilities tied to the host environment.\nThe example uses <code>envGet(&quot;HOME&quot;)</code>, which is good because it looks like something ordinary code might want while still clearly depending on runtime presence.\nThis is where Musi crosses from pure language surface into host-connected services.</p>\n<h2 id=\"why-it-matters\"><a href=\"#why-it-matters\">Why it matters</a></h2><p>Users need to know why runtime imports are separate from both foundation and <code>@std</code> modules.\nIf docs flatten those layers together, it becomes hard to tell what is portable language code and what depends on runtime support.\nA concrete runtime import makes that boundary easier to reason about.</p>\n<h2 id=\"walk-through-it\"><a href=\"#walk-through-it\">Walk through it</a></h2><p>Read <code>import &quot;musi:runtime&quot;</code> as explicit opt-in to host-backed functionality.\nThen read <code>Runtime.envGet(&quot;HOME&quot;)</code> as normal call over imported module, with extra understanding that result depends on surrounding runtime environment.\nUse runtime modules when code truly needs host services, and keep that dependency visible instead of smuggling it in through unrelated helpers.</p>\n<h2 id=\"try-it-next\"><a href=\"#try-it-next\">Try it next</a></h2><ul>\n<li>Import <code>musi:runtime</code>.</li>\n<li>Call one runtime-backed function.</li>\n<li>Explain what part of result depends on host environment.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not present runtime-backed imports as interchangeable with pure stdlib helpers.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/effects-runtime/stdlib\">Stdlib</a> to see what ordinary application code usually reaches for first.</p>\n",
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
				"id": "in-this-chapter",
				"text": "In this chapter"
			},
			{
				"depth": 2,
				"id": "why-it-matters",
				"text": "Why it matters"
			},
			{
				"depth": 2,
				"id": "walk-through-it",
				"text": "Walk through it"
			},
			{
				"depth": 2,
				"id": "try-it-next",
				"text": "Try it next"
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
		"html": "<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> Option</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> import</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> \"@std/option\"</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Option</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">.</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">some</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">Int</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">](</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">8080</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">)</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">  |></span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> Option</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">.</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">unwrapOr</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">Int</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">](</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">3000</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">);</span></span></code></pre></section></div><h2 id=\"in-this-chapter\"><a href=\"#in-this-chapter\">In this chapter</a></h2><p>The standard library gives ordinary application code ready-made modules such as <code>@std/option</code> and <code>@std/testing</code>.\nThis page pairs a simple import example with a testing import to show stdlib as practical toolbox, not abstract layer diagram.\nFor most day-to-day code, this is friendliest layer to reach for first.</p>\n<h2 id=\"why-it-matters\"><a href=\"#why-it-matters\">Why it matters</a></h2><p>After foundation and runtime pages, users need a clear answer to &quot;what do I usually import in normal code?&quot;\nThat answer is often <code>@std</code>.\nPutting stdlib in its own chapter prevents the lower layers from looking like default entry point for common tasks.</p>\n<h2 id=\"walk-through-it\"><a href=\"#walk-through-it\">Walk through it</a></h2><p>Read <code>let Option := import &quot;@std/option&quot;;</code> as explicit acquisition of a higher-level module designed for ordinary code.\nThen notice that testing uses same import model: <code>@std/testing</code> is still just a module you bind and call through.\nWhen writing app code, start from <code>@std</code> modules, then move downward only when you truly need lower-level control.</p>\n<h2 id=\"try-it-next\"><a href=\"#try-it-next\">Try it next</a></h2><ul>\n<li>Import one <code>@std</code> module.</li>\n<li>Call one exported helper from it.</li>\n<li>Compare that import with one <code>musi:*</code> import and decide which belongs in app code.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not reach for foundation or runtime modules first when a clearer <code>@std</code> module already fits the job.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/advanced/attributes\">Attributes</a> to finish the learning path with boundary and tooling features.</p>\n",
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
		"description": "Use attributes to attach metadata for compiler-known items, layout, foreign links, diagnostics, and lifecycle information.",
		"group": "Advanced and tooling",
		"section": "Advanced and tooling",
		"order": 30,
		"slug": "attributes",
		"summary": "Attributes describe metadata, boundaries, and build-time intent without changing Musi into a macro language.",
		"descriptionHtml": "Use attributes to attach metadata for compiler-known items, layout, foreign links, diagnostics, and lifecycle information.",
		"headings": [
			{
				"depth": 2,
				"id": "in-this-chapter",
				"text": "In this chapter"
			},
			{
				"depth": 2,
				"id": "why-it-matters",
				"text": "Why it matters"
			},
			{
				"depth": 2,
				"id": "walk-through-it",
				"text": "Walk through it"
			},
			{
				"depth": 2,
				"id": "what-musi-does-not-do-here",
				"text": "What Musi does not do here"
			},
			{
				"depth": 2,
				"id": "try-it-next",
				"text": "Try it next"
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
		"html": "<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">@</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">known</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(name </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> \"Bool\"</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">)</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">export</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> let</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> Bool</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> Bool</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">@</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">link</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(name </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> \"c\"</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">)</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">foreign</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> \"c\" </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> puts</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> (msg : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">CString</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">) : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">@</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">when</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(os </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> \"linux\"</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">)</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">foreign</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> let</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> clock_gettime</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> (id : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">, out : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">CPtr</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">) : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span></code></pre></section></div><p>Attributes let you attach structured metadata to declarations.\nThey are not a replacement for ordinary language design, and they are not a free-form escape hatch for every feature.\nThey exist to carry information that matters at compile time, runtime boundaries, layout, tooling, or documentation.</p>\n<h2 id=\"in-this-chapter\"><a href=\"#in-this-chapter\">In this chapter</a></h2><p>The built-in attribute families you will see most often are:</p>\n<ul>\n<li>compiler and foundation identity: <code>@known</code>, <code>@intrinsic</code></li>\n<li>foreign boundary: <code>@link</code>, <code>@when</code></li>\n<li>data layout and freezing: <code>@repr</code>, <code>@layout</code>, <code>@frozen</code></li>\n<li>hotness and optimization hints: <code>@hot</code>, <code>@cold</code></li>\n<li>lifecycle metadata: <code>@deprecated</code>, <code>@since</code></li>\n</ul>\n<p>There can also be non-reserved metadata attributes that survive as inert data for tooling or documentation.</p>\n<h2 id=\"why-it-matters\"><a href=\"#why-it-matters\">Why it matters</a></h2><p>If the docs only say &quot;attributes exist&quot;, users still do not know which ones are ordinary metadata, which ones affect code generation, and which ones are only valid in special places.\nThis chapter should answer three practical questions:</p>\n<ol>\n<li>what family is this attribute in?</li>\n<li>what kind of declaration can it attach to?</li>\n<li>what does the compiler or runtime do with it?</li>\n</ol>\n<h2 id=\"walk-through-it\"><a href=\"#walk-through-it\">Walk through it</a></h2><p>Read attributes from the outside in:</p>\n<ul>\n<li>the path, such as <code>@link</code> or <code>@layout</code></li>\n<li>the named arguments, such as <code>name := &quot;c&quot;</code></li>\n<li>the declaration the attribute is attached to</li>\n</ul>\n<p>A short catalog of common meanings:</p>\n<ul>\n<li><code>@known(name := &quot;Bool&quot;)</code>: this exported item is one canonical built-in surface</li>\n<li><code>@intrinsic(name := &quot;ptr.load&quot;)</code>: implementation comes from compiler/runtime intrinsic machinery</li>\n<li><code>@link(name := &quot;c&quot;)</code>: foreign declaration links against host symbol provider</li>\n<li><code>@when(...)</code>: gate declaration by target or environment facts</li>\n<li><code>@repr(...)</code>, <code>@layout(...)</code>: influence data representation details</li>\n<li><code>@frozen</code>: exported data layout should not drift casually</li>\n<li><code>@hot</code>, <code>@cold</code>: codegen-facing temperature hint</li>\n<li><code>@deprecated</code>, <code>@since</code>: consumer-facing lifecycle metadata</li>\n</ul>\n<h2 id=\"what-musi-does-not-do-here\"><a href=\"#what-musi-does-not-do-here\">What Musi does not do here</a></h2><p>Musi attributes are not a full macro system.\nThey do not replace normal functions, data definitions, or effects.\nIf you need ordinary behavior, write ordinary Musi code first.\nReach for attributes when the information really is metadata.</p>\n<h2 id=\"try-it-next\"><a href=\"#try-it-next\">Try it next</a></h2><ul>\n<li>Read one <code>@link</code> declaration and identify every named argument.</li>\n<li>Compare one layout-related attribute with one lifecycle attribute.</li>\n<li>Ask whether the information belongs in ordinary code or in metadata.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not treat attributes as a generic place to hide behavior.\nIf a concept changes how code runs, it usually deserves a language or library construct first.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/advanced/foreign\">Foreign</a> to see how the FFI-related attributes fit into real declarations.</p>\n",
		"summaryHtml": "Attributes describe metadata, boundaries, and build-time intent without changing Musi into a macro language."
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
				"id": "in-this-chapter",
				"text": "In this chapter"
			},
			{
				"depth": 2,
				"id": "why-it-matters",
				"text": "Why it matters"
			},
			{
				"depth": 2,
				"id": "walk-through-it",
				"text": "Walk through it"
			},
			{
				"depth": 2,
				"id": "try-it-next",
				"text": "Try it next"
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
		"html": "<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">foreign</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> \"c\" </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> puts</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> (msg : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">CString</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">) : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Int</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span></code></pre></section></div><h2 id=\"in-this-chapter\"><a href=\"#in-this-chapter\">In this chapter</a></h2><p>Foreign declarations describe bindings implemented outside Musi.\nThe example names a C function and its Musi-facing type so code on Musi side can call across boundary with explicit contract.\nThis is advanced because it is about integration, not about core language flow.</p>\n<h2 id=\"why-it-matters\"><a href=\"#why-it-matters\">Why it matters</a></h2><p>Users working near system boundaries need to know how Musi reaches native code without pretending that boundary is ordinary function definition.\nIf docs bury foreign declarations under attribute notes or runtime pages, the integration story stays fuzzy.\nA dedicated page keeps the riskier cross-language surface explicit.</p>\n<h2 id=\"walk-through-it\"><a href=\"#walk-through-it\">Walk through it</a></h2><p>Read <code>foreign &quot;c&quot;</code> as declaration of external implementation source.\nThen read remainder of line as ordinary Musi-facing name and type surface that callers will see on Musi side.\nWhen adding foreign bindings, keep signatures minimal, verify types carefully, and isolate these declarations near integration boundaries instead of scattering them through domain code.\nCalls to foreign bindings belong inside <code>unsafe { ... }</code> because Musi cannot prove what native code does with raw pointers, strings, global state, or process state.</p>\n<p>Wrap a foreign call in a small <code>unsafe</code> block, then expose a safe wrapper when ordinary callers should not see the native boundary.</p>\n<h2 id=\"try-it-next\"><a href=\"#try-it-next\">Try it next</a></h2><ul>\n<li>Declare one foreign binding.</li>\n<li>Identify language/runtime boundary it crosses.</li>\n<li>Wrap the call in a small <code>unsafe</code> block.</li>\n<li>Explain what Musi side promises about arguments and result.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not treat foreign declarations as casual shortcut for code that could stay inside normal Musi modules.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/advanced/unsafe-and-ffi\">Unsafe and FFI</a> for raw pointer and native-call boundaries.</p>\n",
		"summaryHtml": "Declare foreign bindings at the runtime boundary, not inside ordinary domain code."
	},
	{
		"locale": "en",
		"id": "unsafe-and-ffi",
		"kind": "chapter",
		"partId": "advanced",
		"partTitle": "Advanced and tooling",
		"path": "/learn/language/advanced/unsafe-and-ffi",
		"canonicalPath": "/learn/language/advanced/unsafe-and-ffi",
		"aliases": [
			"/docs/language/advanced/unsafe-and-ffi"
		],
		"questions": [],
		"title": "Unsafe and FFI",
		"description": "Use unsafe blocks and @std/ffi when Musi code crosses raw native boundaries.",
		"group": "Advanced and tooling",
		"section": "Advanced and tooling",
		"order": 32,
		"slug": "unsafe-and-ffi",
		"summary": "Keep raw pointer and native-call work behind a visible unsafe boundary.",
		"descriptionHtml": "Use unsafe blocks and @std/ffi when Musi code crosses raw native boundaries.",
		"headings": [
			{
				"depth": 2,
				"id": "c-abi-types",
				"text": "C ABI types"
			},
			{
				"depth": 2,
				"id": "what-musi-does-not-have",
				"text": "What Musi does not have"
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
		"html": "<p>Native code can do things the Musi type system cannot prove safe. Musi marks that boundary with <code>unsafe { ... }</code>.</p>\n<p><code>unsafe</code> is a block because it is a structural scope. Everything inside the braces is checked normally, but calls marked unsafe are only allowed while the checker is inside that scope. The last expression of the block is still the block result.</p>\n<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> Ffi</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> import</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> \"@std/ffi\"</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">foreign</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> \"c\" </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> get_counter</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> () : </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">CPtr</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> counter </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> unsafe </span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">{</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">  let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> raw </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> get_counter</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">();</span></span>\n<span class=\"line\"><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">  Ffi</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">.ptr.</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">cast</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">Int</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">](raw);</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">};</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> next </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> unsafe </span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">{</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">  let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> offset </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> Ffi</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">.ptr.offset;</span></span>\n<span class=\"line\"><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">  offset</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">[</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">Int</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">](counter, </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">1</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">);</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">};</span></span></code></pre></section></div><h2 id=\"c-abi-types\"><a href=\"#c-abi-types\">C ABI types</a></h2><p>Use <code>CString</code> for null-terminated C strings and <code>CPtr</code> for raw C pointer values in native signatures.</p>\n<p>The same boundary model applies to declarations such as <code>puts</code> for <code>CString</code> and <code>memset</code> for <code>CPtr</code>.</p>\n<p><code>CPtr</code> means &quot;this value has C pointer ABI shape&quot;. It is the right type at the native boundary.</p>\n<p><code>@std/ffi</code> adds typed pointer views for Musi code that wants to name what a raw pointer points at after the boundary has already been crossed.</p>\n<p><code>@std/ffi</code> then lets Musi code cast a <code>CPtr</code> into a typed <code>Ptr[T]</code> view after the call has crossed the native boundary.</p>\n<p>A <code>Ptr[T]</code> is a typed view over a <code>CPtr</code>. It does not replace <code>CPtr</code> in native declarations. Use <code>CPtr</code> at the C boundary, then use <code>Ptr[T]</code> when Musi code needs a clearer static name for the pointed-at data.</p>\n<p><code>Ffi.ptr.null[T]()</code> creates a typed null pointer. <code>Ffi.ptr.isNull[T](pointer)</code> checks it before dereference.</p>\n<p><code>Ffi.ptr.offset[T](pointer, count)</code> moves by elements, not bytes. For <code>Ptr[CInt]</code>, <code>count := 2</code> means two C ints.</p>\n<p><code>Ffi.ptr.read[T](pointer)</code> and <code>Ffi.ptr.write[T](pointer, value)</code> dereference process memory. Calls to <code>offset</code>, <code>read</code>, and <code>write</code> belong inside <code>unsafe { ... }</code> because Musi cannot prove the address is valid.</p>\n<p>Use <code>CPtr</code> when declaring native calls. Use <code>Ptr[T]</code> after the call when Musi code needs to state the pointed-at shape.</p>\n<p><code>Ffi.ptr</code> is an ordinary record of values. The pointer operations are first-class polymorphic fields, so an alias keeps the type parameter:</p>\n<pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span>let offset := Ffi.ptr.offset;</span></span>\n<span class=\"line\"><span></span></span>\n<span class=\"line\"><span>let next := unsafe {</span></span>\n<span class=\"line\"><span>  offset[Int](counter, 1);</span></span>\n<span class=\"line\"><span>};</span></span></code></pre><p>The unsafe rule follows the value. Calling <code>offset[Int]</code> through an alias still needs <code>unsafe { ... }</code>.</p>\n<h2 id=\"what-musi-does-not-have\"><a href=\"#what-musi-does-not-have\">What Musi does not have</a></h2><p>Musi does not use a <code>borrow</code> keyword or pointer sigils for this model. Raw native work should stay explicit through <code>unsafe { ... }</code>, <code>CPtr</code>, and the <code>@std/ffi</code> helpers.</p>\n<p>Musi also does not use <code>return</code> inside unsafe blocks. The final expression is the value, just like any other block.</p>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not hide a native call in ordinary code and expect the reader to notice it from the function name. Put the call inside a small unsafe block, then expose a safe wrapper when the rest of the module should not care about native details.</p>\n<p>A wrapper can keep the unsafe block small while exposing an ordinary Musi function to the rest of the module.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/advanced/running-and-tooling\">Running and tooling</a> for command workflow and validation.</p>\n",
		"summaryHtml": "Keep raw pointer and native-call work behind a visible unsafe boundary."
	},
	{
		"locale": "en",
		"id": "operator-forms",
		"kind": "chapter",
		"partId": "advanced",
		"partTitle": "Advanced and tooling",
		"path": "/learn/language/advanced/operator-forms",
		"canonicalPath": "/learn/language/advanced/operator-forms",
		"aliases": [
			"/docs/language/advanced/operator-forms"
		],
		"questions": [],
		"title": "Operator forms",
		"description": "Name operators, set fixity, and call an operator when that reads better.",
		"group": "Advanced and tooling",
		"section": "Advanced and tooling",
		"order": 32,
		"slug": "operator-forms",
		"summary": "Use fixity declarations and parenthesized operator names for advanced operator-heavy code.",
		"descriptionHtml": "Name operators, set fixity, and call an operator when that reads better.",
		"headings": [
			{
				"depth": 2,
				"id": "in-this-chapter",
				"text": "In this chapter"
			},
			{
				"depth": 2,
				"id": "why-it-matters",
				"text": "Why it matters"
			},
			{
				"depth": 2,
				"id": "walk-through-it",
				"text": "Walk through it"
			},
			{
				"depth": 2,
				"id": "try-it-next",
				"text": "Try it next"
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
		"html": "<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">infixl</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 6</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> (+)</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> add </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> (+)</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> total </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> add</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">1</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">, </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">2</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">);</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">total;</span></span></code></pre></section></div><h2 id=\"in-this-chapter\"><a href=\"#in-this-chapter\">In this chapter</a></h2><p>Operators can appear as ordinary infix syntax, and parenthesized operator names can be used as values.\nFixity declarations such as <code>infixl 6 (+);</code> describe how an operator groups with neighboring expressions.\nMost code does not need custom fixity, but operator-heavy libraries do.</p>\n<h2 id=\"why-it-matters\"><a href=\"#why-it-matters\">Why it matters</a></h2><p>A small amount of operator syntax can make numeric or parser-like code clearer.\nToo much can make code unreadable.\nNaming the operator with <code>(+)</code> and declaring fixity explicitly keeps the unusual parts visible.</p>\n<h2 id=\"walk-through-it\"><a href=\"#walk-through-it\">Walk through it</a></h2><p>Read <code>infixl 6 (+);</code> as a declaration about grouping.\nRead <code>let add := (+);</code> as binding the operator itself to a name.\nRead <code>add(1, 2)</code> as an ordinary call through that binding.</p>\n<h2 id=\"try-it-next\"><a href=\"#try-it-next\">Try it next</a></h2><ul>\n<li>Bind one operator with its parenthesized name.</li>\n<li>Call it like a function.</li>\n<li>Avoid custom fixity unless repeated infix use actually becomes clearer.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not use custom operators where a named function would explain the domain better.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/advanced/quote-and-syntax\">Quote and syntax</a> for code-as-data tools.</p>\n",
		"summaryHtml": "Use fixity declarations and parenthesized operator names for advanced operator-heavy code."
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
				"id": "in-this-chapter",
				"text": "In this chapter"
			},
			{
				"depth": 2,
				"id": "why-it-matters",
				"text": "Why it matters"
			},
			{
				"depth": 2,
				"id": "walk-through-it",
				"text": "Walk through it"
			},
			{
				"depth": 2,
				"id": "try-it-next",
				"text": "Try it next"
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
		"html": "<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> addTemplate </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> quote </span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(x </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">+</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> #(delta));</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> addOneSyntax </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> quote </span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(#(x) </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">+</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 1</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">);</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">addOneSyntax;</span></span></code></pre></section></div><h2 id=\"in-this-chapter\"><a href=\"#in-this-chapter\">In this chapter</a></h2><p><code>quote</code> turns code shape into syntax data you can inspect, build, or reuse.\nThe first snippet shows simplest quoted expression, and the second shows interpolation with <code>#(...)</code> inside quoted form.\nThis chapter belongs late because it asks you to reason about code as data rather than just running code.</p>\n<h2 id=\"why-it-matters\"><a href=\"#why-it-matters\">Why it matters</a></h2><p>Metaprogramming questions show up after ordinary code already feels familiar.\nAt that point users need examples that explain both power and boundary: quoting is useful, but it is not default way to write everyday logic.\nA focused page keeps this tool available without overwhelming readers who are still stabilizing basic syntax.</p>\n<h2 id=\"walk-through-it\"><a href=\"#walk-through-it\">Walk through it</a></h2><p>Read <code>quote (x + 1);</code> as syntax value representing expression shape.\nThen read <code>#(delta)</code> or <code>#(x)</code> inside quoted form as splice points where surrounding values contribute pieces to generated syntax.\nWhen experimenting, start with very small quoted expressions and ask what syntax object each quote should represent before building larger templates.</p>\n<h2 id=\"try-it-next\"><a href=\"#try-it-next\">Try it next</a></h2><ul>\n<li>Quote one simple expression.</li>\n<li>Add one splice inside a quoted template.</li>\n<li>Compare quoted template with handwritten equivalent shape.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not reach for quote when an ordinary function or data value already solves the problem more directly.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/advanced/templates-and-splices\">Templates and splices</a> to separate text interpolation from syntax splicing.</p>\n",
		"summaryHtml": "Treat code as data only after ordinary code reading feels natural."
	},
	{
		"locale": "en",
		"id": "templates-and-splices",
		"kind": "chapter",
		"partId": "advanced",
		"partTitle": "Advanced and tooling",
		"path": "/learn/language/advanced/templates-and-splices",
		"canonicalPath": "/learn/language/advanced/templates-and-splices",
		"aliases": [
			"/docs/language/advanced/templates-and-splices"
		],
		"questions": [],
		"title": "Templates and splices",
		"description": "Build text with interpolation and understand splice syntax near quote forms.",
		"group": "Advanced and tooling",
		"section": "Advanced and tooling",
		"order": 34,
		"slug": "templates-and-splices",
		"summary": "Use template literals for interpolated text and splice forms when building syntax.",
		"descriptionHtml": "Build text with interpolation and understand splice syntax near quote forms.",
		"headings": [
			{
				"depth": 2,
				"id": "in-this-chapter",
				"text": "In this chapter"
			},
			{
				"depth": 2,
				"id": "why-it-matters",
				"text": "Why it matters"
			},
			{
				"depth": 2,
				"id": "walk-through-it",
				"text": "Walk through it"
			},
			{
				"depth": 2,
				"id": "try-it-next",
				"text": "Try it next"
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
		"html": "<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> port </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 8080</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> label </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> `port </span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\">${</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">port</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\">}</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\">`</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">label;</span></span></code></pre></section></div><h2 id=\"in-this-chapter\"><a href=\"#in-this-chapter\">In this chapter</a></h2><p>Template literals use backticks and <code>${...}</code> interpolation.\nThey are useful when surrounding text and computed values belong together.\nSplice forms such as <code>#x</code>, <code>#(expr)</code>, and <code>#[items]</code> appear in syntax-building contexts where existing values contribute pieces to quoted code.</p>\n<h2 id=\"why-it-matters\"><a href=\"#why-it-matters\">Why it matters</a></h2><p>String assembly and syntax assembly look similar from far away, but they answer different questions.\nTemplates produce text-like values.\nSplices feed existing values into quoted syntax.\nKeeping both forms named helps readers avoid treating every interpolation as a macro.</p>\n<h2 id=\"walk-through-it\"><a href=\"#walk-through-it\">Walk through it</a></h2><p>Read <code>`port ${port}`</code> as text with one embedded expression.\nRead <code>#(delta)</code> inside a quote as a syntax splice, not as string interpolation.\nUse templates for user-facing text and splices for code-as-data work.</p>\n<h2 id=\"try-it-next\"><a href=\"#try-it-next\">Try it next</a></h2><ul>\n<li>Build one template value with a named binding inside it.</li>\n<li>Compare it with a quoted expression that uses <code>#(...)</code>.</li>\n<li>Explain which one produces text and which one produces syntax.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not use syntax splices to build ordinary strings. Use templates for text.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/advanced/testing\">Testing</a> to return to everyday project workflow.</p>\n",
		"summaryHtml": "Use template literals for interpolated text and splice forms when building syntax."
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
				"id": "in-this-chapter",
				"text": "In this chapter"
			},
			{
				"depth": 2,
				"id": "why-it-matters",
				"text": "Why it matters"
			},
			{
				"depth": 2,
				"id": "walk-through-it",
				"text": "Walk through it"
			},
			{
				"depth": 2,
				"id": "try-it-next",
				"text": "Try it next"
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
		"html": "<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">let</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> Testing</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> :=</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> import</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> \"@std/testing\"</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">;</span></span>\n<span class=\"line\"></span>\n<span class=\"line\"><span style=\"color:#D73A49;--shiki-dark:#F97583\">export</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> let</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\"> test</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\"> () </span><span style=\"color:#D73A49;--shiki-dark:#F97583\">:=</span></span>\n<span class=\"line\"><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">  Testing</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">.</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">it</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\">\"adds values\"</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">, </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">Testing</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">.</span><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">toBe</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">(</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">1</span><span style=\"color:#D73A49;--shiki-dark:#F97583\"> +</span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\"> 2</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">, </span><span style=\"color:#005CC5;--shiki-dark:#79B8FF\">3</span><span style=\"color:#24292E;--shiki-dark:#E1E4E8\">));</span></span></code></pre></section></div><h2 id=\"in-this-chapter\"><a href=\"#in-this-chapter\">In this chapter</a></h2><p>Musi tests are ordinary code organized for discovery and execution by tooling.\nThis example keeps that promise visible: import testing helpers, export a <code>test</code> binding, and express one expectation in same language surface you already know.\nTesting becomes easier to adopt when it does not require a second mini-language.</p>\n<h2 id=\"why-it-matters\"><a href=\"#why-it-matters\">Why it matters</a></h2><p>Users need confidence loop, not just syntax reference.\nIf the docs explain features but never show how to check them, learners still ask how to verify a package change or protect against regressions.\nA tiny test example gives them a habit they can keep using as code grows.</p>\n<h2 id=\"walk-through-it\"><a href=\"#walk-through-it\">Walk through it</a></h2><p>Read <code>let Testing := import &quot;@std/testing&quot;;</code> as setup of helpers, then read exported <code>test</code> binding as entry point tooling will discover.\nThe assertion itself is ordinary function-style code, which means testing builds on same import, call, and expression patterns from earlier chapters.\nWhen writing first tests, keep each one tiny and named around one behavior you want confidence in.</p>\n<h2 id=\"try-it-next\"><a href=\"#try-it-next\">Try it next</a></h2><ul>\n<li>Create one <code>*.test.ms</code> file.</li>\n<li>Export one <code>test</code> binding.</li>\n<li>Check one small behavior with stdlib testing helper.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not wait for a large project before learning the test shape; tiny examples benefit from it too.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue to <a href=\"/docs/language/advanced/running-and-tooling\">Running and tooling</a> to tie learning back to everyday commands.</p>\n",
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
				"id": "in-this-chapter",
				"text": "In this chapter"
			},
			{
				"depth": 2,
				"id": "why-it-matters",
				"text": "Why it matters"
			},
			{
				"depth": 2,
				"id": "walk-through-it",
				"text": "Walk through it"
			},
			{
				"depth": 2,
				"id": "try-it-next",
				"text": "Try it next"
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
		"html": "<div class=\"snippet-block\"><section class=\"code-panel\"><pre class=\"shiki shiki-themes github-light github-dark\" style=\"background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8\" tabindex=\"0\"><code><span class=\"line\"><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">music</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> check</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> index.ms</span></span>\n<span class=\"line\"><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">musi</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> run</span></span>\n<span class=\"line\"><span style=\"color:#6F42C1;--shiki-dark:#B392F0\">musi</span><span style=\"color:#032F62;--shiki-dark:#9ECBFF\"> test</span></span></code></pre></section></div><h2 id=\"in-this-chapter\"><a href=\"#in-this-chapter\">In this chapter</a></h2><p>Musi has everyday commands for package workflow and direct file workflow.\nThis page brings them together at end of language path so readers can connect all earlier examples to routine habits: check code, run packages, build outputs, and run tests.\nThe command surface is small enough to learn, but distinct enough to deserve a final summary.</p>\n<h2 id=\"why-it-matters\"><a href=\"#why-it-matters\">Why it matters</a></h2><p>After learning syntax, users still need operational confidence.\nThey want to know which command to run when checking a file, when to use package commands, and how testing fits into normal iteration.\nA workflow chapter turns scattered command knowledge into repeatable practice.</p>\n<h2 id=\"walk-through-it\"><a href=\"#walk-through-it\">Walk through it</a></h2><p>Read <code>musi run</code>, <code>musi check</code>, <code>musi build</code>, and <code>musi test</code> as package-root commands for project lifecycle.\nRead <code>music check index.ms</code>, <code>music build index.ms</code>, and <code>music run index.seam</code> as direct lane for single-file or lower-level work.\nWhen in doubt, ask first whether you are inside a package or handling one file directly; that decision usually picks the right command family immediately.</p>\n<h2 id=\"try-it-next\"><a href=\"#try-it-next\">Try it next</a></h2><ul>\n<li>Run one direct <code>music check</code> on a scratch file.</li>\n<li>Run one package command inside generated project.</li>\n<li>Use <code>musi test</code> after adding one tiny test.</li>\n</ul>\n<h2 id=\"common-mistake\"><a href=\"#common-mistake\">Common mistake</a></h2><p>Do not memorize commands as flat list; group them by direct-file lane versus package lane.</p>\n<h2 id=\"next\"><a href=\"#next\">Next</a></h2><p>Continue back through any chapter you need, now that you have both language model and workflow model tied together.</p>\n",
		"summaryHtml": "Finish with the everyday command flow for checking, running, and building code."
	}
] satisfies GeneratedDoc[];

