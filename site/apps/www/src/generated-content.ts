export interface GeneratedHeading {
	depth: number;
	id: string;
	text: string;
}

export interface GeneratedDoc {
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
	homeSampleHtml:
		'<div class="code-tabs" data-code-tabs="1" data-example-id="option-fallback" data-default="musi" data-active-language="musi">\n<div class="code-tabs-meta">\n<p class="code-tabs-caption">One value may be present or missing. Musi does this with the real stdlib <code>Option</code> family.</p>\n<p class="code-tabs-note">Musi uses imported constructors and <code>case</code>, not method chaining or statement-style branching.</p>\n</div>\n<div class="code-tablist" role="tablist" aria-label="Fallback value from an optional result"><button type="button" role="tab" class="code-tab" data-language="java" aria-selected="false" tabindex="-1">Java</button><button type="button" role="tab" class="code-tab" data-language="musi" aria-selected="true" tabindex="0">Musi</button><button type="button" role="tab" class="code-tab" data-language="rust" aria-selected="false" tabindex="-1">Rust</button><button type="button" role="tab" class="code-tab" data-language="typescript" aria-selected="false" tabindex="-1">TypeScript</button></div>\n<section role="tabpanel" class="code-panel" data-language="java" data-active="false" hidden=""><pre class="shiki shiki-themes github-light github-dark" style="background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8" tabindex="0"><code><span class="line"><span style="color:#24292E;--shiki-dark:#E1E4E8">Optional&#x3C;</span><span style="color:#D73A49;--shiki-dark:#F97583">Integer</span><span style="color:#24292E;--shiki-dark:#E1E4E8">> configured </span><span style="color:#D73A49;--shiki-dark:#F97583">=</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> Optional.</span><span style="color:#6F42C1;--shiki-dark:#B392F0">of</span><span style="color:#24292E;--shiki-dark:#E1E4E8">(</span><span style="color:#005CC5;--shiki-dark:#79B8FF">8080</span><span style="color:#24292E;--shiki-dark:#E1E4E8">);</span></span>\n<span class="line"><span style="color:#D73A49;--shiki-dark:#F97583">int</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> port </span><span style="color:#D73A49;--shiki-dark:#F97583">=</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> configured.</span><span style="color:#6F42C1;--shiki-dark:#B392F0">orElse</span><span style="color:#24292E;--shiki-dark:#E1E4E8">(</span><span style="color:#005CC5;--shiki-dark:#79B8FF">3000</span><span style="color:#24292E;--shiki-dark:#E1E4E8">);</span></span></code></pre></section>\n<section role="tabpanel" class="code-panel" data-language="musi" data-active="true"><pre class="shiki shiki-themes github-light github-dark" style="background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8" tabindex="0"><code><span class="line"><span style="color:#D73A49;--shiki-dark:#F97583">let</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> configured </span><span style="color:#D73A49;--shiki-dark:#F97583">:=</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> Option.some[Int](</span><span style="color:#005CC5;--shiki-dark:#79B8FF">8080</span><span style="color:#24292E;--shiki-dark:#E1E4E8">);</span></span>\n<span class="line"><span style="color:#24292E;--shiki-dark:#E1E4E8">Option.unwrap_or[Int](configured, </span><span style="color:#005CC5;--shiki-dark:#79B8FF">3000</span><span style="color:#24292E;--shiki-dark:#E1E4E8">);</span></span></code></pre></section>\n<section role="tabpanel" class="code-panel" data-language="rust" data-active="false" hidden=""><pre class="shiki shiki-themes github-light github-dark" style="background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8" tabindex="0"><code><span class="line"><span style="color:#D73A49;--shiki-dark:#F97583">let</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> configured </span><span style="color:#D73A49;--shiki-dark:#F97583">=</span><span style="color:#6F42C1;--shiki-dark:#B392F0"> Some</span><span style="color:#24292E;--shiki-dark:#E1E4E8">(</span><span style="color:#005CC5;--shiki-dark:#79B8FF">8080</span><span style="color:#24292E;--shiki-dark:#E1E4E8">);</span></span>\n<span class="line"><span style="color:#D73A49;--shiki-dark:#F97583">let</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> port </span><span style="color:#D73A49;--shiki-dark:#F97583">=</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> configured</span><span style="color:#D73A49;--shiki-dark:#F97583">.</span><span style="color:#6F42C1;--shiki-dark:#B392F0">unwrap_or</span><span style="color:#24292E;--shiki-dark:#E1E4E8">(</span><span style="color:#005CC5;--shiki-dark:#79B8FF">3000</span><span style="color:#24292E;--shiki-dark:#E1E4E8">);</span></span></code></pre></section>\n<section role="tabpanel" class="code-panel" data-language="typescript" data-active="false" hidden=""><pre class="shiki shiki-themes github-light github-dark" style="background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8" tabindex="0"><code><span class="line"><span style="color:#D73A49;--shiki-dark:#F97583">const</span><span style="color:#005CC5;--shiki-dark:#79B8FF"> configured</span><span style="color:#D73A49;--shiki-dark:#F97583">:</span><span style="color:#005CC5;--shiki-dark:#79B8FF"> number</span><span style="color:#D73A49;--shiki-dark:#F97583"> |</span><span style="color:#005CC5;--shiki-dark:#79B8FF"> null</span><span style="color:#D73A49;--shiki-dark:#F97583"> =</span><span style="color:#005CC5;--shiki-dark:#79B8FF"> 8080</span><span style="color:#24292E;--shiki-dark:#E1E4E8">;</span></span>\n<span class="line"><span style="color:#D73A49;--shiki-dark:#F97583">const</span><span style="color:#005CC5;--shiki-dark:#79B8FF"> port</span><span style="color:#D73A49;--shiki-dark:#F97583"> =</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> configured </span><span style="color:#D73A49;--shiki-dark:#F97583">??</span><span style="color:#005CC5;--shiki-dark:#79B8FF"> 3000</span><span style="color:#24292E;--shiki-dark:#E1E4E8">;</span></span></code></pre></section>\n</div>',
	installSourceHtml:
		'<pre class="shiki shiki-themes github-light github-dark" style="background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8" tabindex="0"><code><span class="line"><span style="color:#6F42C1;--shiki-dark:#B392F0">git</span><span style="color:#032F62;--shiki-dark:#9ECBFF"> clone</span><span style="color:#032F62;--shiki-dark:#9ECBFF"> https://github.com/musi-lang/musi.git</span></span>\n<span class="line"><span style="color:#005CC5;--shiki-dark:#79B8FF">cd</span><span style="color:#032F62;--shiki-dark:#9ECBFF"> musi</span></span>\n<span class="line"><span style="color:#6F42C1;--shiki-dark:#B392F0">cargo</span><span style="color:#032F62;--shiki-dark:#9ECBFF"> build</span><span style="color:#005CC5;--shiki-dark:#79B8FF"> --release</span></span>\n<span class="line"><span style="color:#D73A49;--shiki-dark:#F97583">export</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> PATH</span><span style="color:#D73A49;--shiki-dark:#F97583">=</span><span style="color:#032F62;--shiki-dark:#9ECBFF">"/path/to/musi/target/release:</span><span style="color:#24292E;--shiki-dark:#E1E4E8">$PATH</span><span style="color:#032F62;--shiki-dark:#9ECBFF">"</span></span></code></pre>',
	quickstartHtml:
		'<pre class="shiki shiki-themes github-light github-dark" style="background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8" tabindex="0"><code><span class="line"><span style="color:#6F42C1;--shiki-dark:#B392F0">musi</span><span style="color:#032F62;--shiki-dark:#9ECBFF"> new</span><span style="color:#032F62;--shiki-dark:#9ECBFF"> hello</span></span>\n<span class="line"><span style="color:#005CC5;--shiki-dark:#79B8FF">cd</span><span style="color:#032F62;--shiki-dark:#9ECBFF"> hello</span></span>\n<span class="line"><span style="color:#6F42C1;--shiki-dark:#B392F0">musi</span><span style="color:#032F62;--shiki-dark:#9ECBFF"> run</span></span></code></pre>',
} as const;

export const renderedDocs = [
	{
		title: "Getting started",
		description:
			"Install the tools, know what they do, and start with the right command.",
		group: "Start",
		section: "Start",
		order: 1,
		slug: "getting-started",
		summary:
			"Install, PATH setup, and the difference between <code>musi</code> and <code>music</code>.",
		descriptionHtml:
			"Install the tools, know what they do, and start with the right command.",
		headings: [
			{
				depth: 2,
				id: "what",
				text: "What",
			},
			{
				depth: 2,
				id: "why",
				text: "Why",
			},
			{
				depth: 2,
				id: "how",
				text: "How",
			},
			{
				depth: 2,
				id: "when",
				text: "When",
			},
			{
				depth: 2,
				id: "analogy",
				text: "Analogy",
			},
			{
				depth: 2,
				id: "try-it",
				text: "Try it",
			},
		],
		html: '<p>This guide is for people who want to write Musi, run packages, and get useful behavior quickly.</p>\n<h2 id="what"><a href="#what">What</a></h2><p>You install the command-line tools once, then use Musi as your package and source language for small scripts, services, or experiments.</p>\n<h2 id="why"><a href="#why">Why</a></h2><p>Musi separates package workflows from direct file workflows.</p>\n<ul>\n<li><code>musi</code> handles package config, entry resolution, and project scripts.</li>\n<li><code>music</code> runs one source graph or artifact directly.</li>\n</ul>\n<h2 id="how"><a href="#how">How</a></h2><p>Start at the <a href="/install">install page</a>, then follow this order:</p>\n<ul>\n<li>install binaries and PATH entries</li>\n<li>create a package</li>\n<li>add first expressions</li>\n<li>run <code>musi check</code> and <code>musi run</code></li>\n</ul>\n<h2 id="when"><a href="#when">When</a></h2><p>Use this sequence when you have a new machine, a new clone, or you are onboarding from Python, JS, or TS and want the smallest working first step.</p>\n<h2 id="analogy"><a href="#analogy">Analogy</a></h2><p>Treat <code>musi</code> like <code>npm</code> or <code>cargo</code> for project lifecycle, and <code>music</code> like a direct <code>node</code>/<code>deno</code>-style file runner.</p>\n<h2 id="try-it"><a href="#try-it">Try it</a></h2><p>Open install notes on <a href="/install">Install</a>, then continue to <a href="/docs/first-program">First program</a>.</p>\n',
		summaryHtml:
			"Install, PATH setup, and the difference between <code>musi</code> and <code>music</code>.",
	},
	{
		title: "First program",
		description: "Write a small file and read it as expressions.",
		group: "Start",
		section: "Start",
		order: 2,
		slug: "first-program",
		summary: "A first Musi file without extra ceremony.",
		descriptionHtml: "Write a small file and read it as expressions.",
		headings: [
			{
				depth: 2,
				id: "what",
				text: "What",
			},
			{
				depth: 2,
				id: "why",
				text: "Why",
			},
			{
				depth: 2,
				id: "how",
				text: "How",
			},
			{
				depth: 2,
				id: "compare",
				text: "Compare",
			},
			{
				depth: 2,
				id: "when",
				text: "When",
			},
			{
				depth: 2,
				id: "analogy",
				text: "Analogy",
			},
			{
				depth: 2,
				id: "try-it",
				text: "Try it",
			},
		],
		html: '<p>Musi files are read as expressions. A file is executed from top to bottom as a sequence.</p>\n<h2 id="what"><a href="#what">What</a></h2><p>This page teaches the smallest runnable surface: values, bindings, and simple expressions.</p>\n<pre class="shiki shiki-themes github-light github-dark" style="background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8" tabindex="0"><code><span class="line"><span style="color:#D73A49;--shiki-dark:#F97583">let</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> answer </span><span style="color:#D73A49;--shiki-dark:#F97583">:=</span><span style="color:#005CC5;--shiki-dark:#79B8FF"> 42</span><span style="color:#24292E;--shiki-dark:#E1E4E8">;</span></span>\n<span class="line"><span style="color:#24292E;--shiki-dark:#E1E4E8">answer;</span></span></code></pre><h2 id="why"><a href="#why">Why</a></h2><p>Short files are the fastest way to learn language behavior without project scaffolding.\nYou can spot how names and results flow before layering packages, effects, or classes.</p>\n<h2 id="how"><a href="#how">How</a></h2><p>Use <code>let</code> for values and end each expression with <code>;</code>. After a value exists, the next expression can use it.</p>\n<h2 id="compare"><a href="#compare">Compare</a></h2><div class="code-tabs" data-code-tabs="1" data-example-id="double-function" data-default="musi" data-active-language="musi">\n<div class="code-tabs-meta">\n<p class="code-tabs-caption">Same small task across four languages. Musi keeps it as an expression-oriented <code>let</code> binding.</p>\n<p class="code-tabs-note">Musi functions are ordinary bindings, so the syntax stays close to other definitions.</p>\n</div>\n<div class="code-tablist" role="tablist" aria-label="Small function that doubles a number"><button type="button" role="tab" class="code-tab" data-language="java" aria-selected="false" tabindex="-1">Java</button><button type="button" role="tab" class="code-tab" data-language="musi" aria-selected="true" tabindex="0">Musi</button><button type="button" role="tab" class="code-tab" data-language="rust" aria-selected="false" tabindex="-1">Rust</button><button type="button" role="tab" class="code-tab" data-language="typescript" aria-selected="false" tabindex="-1">TypeScript</button></div>\n<section role="tabpanel" class="code-panel" data-language="java" data-active="false" hidden=""><pre class="shiki shiki-themes github-light github-dark" style="background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8" tabindex="0"><code><span class="line"><span style="color:#D73A49;--shiki-dark:#F97583">static</span><span style="color:#D73A49;--shiki-dark:#F97583"> int</span><span style="color:#6F42C1;--shiki-dark:#B392F0"> twice</span><span style="color:#24292E;--shiki-dark:#E1E4E8">(</span><span style="color:#D73A49;--shiki-dark:#F97583">int</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> x) {</span></span>\n<span class="line"><span style="color:#D73A49;--shiki-dark:#F97583">    return</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> x </span><span style="color:#D73A49;--shiki-dark:#F97583">+</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> x;</span></span>\n<span class="line"><span style="color:#24292E;--shiki-dark:#E1E4E8">}</span></span>\n<span class="line"></span>\n<span class="line"><span style="color:#D73A49;--shiki-dark:#F97583">int</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> answer </span><span style="color:#D73A49;--shiki-dark:#F97583">=</span><span style="color:#6F42C1;--shiki-dark:#B392F0"> twice</span><span style="color:#24292E;--shiki-dark:#E1E4E8">(</span><span style="color:#005CC5;--shiki-dark:#79B8FF">21</span><span style="color:#24292E;--shiki-dark:#E1E4E8">);</span></span></code></pre></section>\n<section role="tabpanel" class="code-panel" data-language="musi" data-active="true"><pre class="shiki shiki-themes github-light github-dark" style="background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8" tabindex="0"><code><span class="line"><span style="color:#D73A49;--shiki-dark:#F97583">let</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> twice (x : Int) : Int </span><span style="color:#D73A49;--shiki-dark:#F97583">:=</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> x </span><span style="color:#D73A49;--shiki-dark:#F97583">+</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> x;</span></span>\n<span class="line"></span>\n<span class="line"><span style="color:#24292E;--shiki-dark:#E1E4E8">twice(</span><span style="color:#005CC5;--shiki-dark:#79B8FF">21</span><span style="color:#24292E;--shiki-dark:#E1E4E8">);</span></span></code></pre></section>\n<section role="tabpanel" class="code-panel" data-language="rust" data-active="false" hidden=""><pre class="shiki shiki-themes github-light github-dark" style="background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8" tabindex="0"><code><span class="line"><span style="color:#D73A49;--shiki-dark:#F97583">fn</span><span style="color:#6F42C1;--shiki-dark:#B392F0"> twice</span><span style="color:#24292E;--shiki-dark:#E1E4E8">(x</span><span style="color:#D73A49;--shiki-dark:#F97583">:</span><span style="color:#6F42C1;--shiki-dark:#B392F0"> i32</span><span style="color:#24292E;--shiki-dark:#E1E4E8">) </span><span style="color:#D73A49;--shiki-dark:#F97583">-></span><span style="color:#6F42C1;--shiki-dark:#B392F0"> i32</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> {</span></span>\n<span class="line"><span style="color:#24292E;--shiki-dark:#E1E4E8">    x </span><span style="color:#D73A49;--shiki-dark:#F97583">+</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> x</span></span>\n<span class="line"><span style="color:#24292E;--shiki-dark:#E1E4E8">}</span></span>\n<span class="line"></span>\n<span class="line"><span style="color:#D73A49;--shiki-dark:#F97583">let</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> answer </span><span style="color:#D73A49;--shiki-dark:#F97583">=</span><span style="color:#6F42C1;--shiki-dark:#B392F0"> twice</span><span style="color:#24292E;--shiki-dark:#E1E4E8">(</span><span style="color:#005CC5;--shiki-dark:#79B8FF">21</span><span style="color:#24292E;--shiki-dark:#E1E4E8">);</span></span></code></pre></section>\n<section role="tabpanel" class="code-panel" data-language="typescript" data-active="false" hidden=""><pre class="shiki shiki-themes github-light github-dark" style="background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8" tabindex="0"><code><span class="line"><span style="color:#D73A49;--shiki-dark:#F97583">function</span><span style="color:#6F42C1;--shiki-dark:#B392F0"> twice</span><span style="color:#24292E;--shiki-dark:#E1E4E8">(</span><span style="color:#E36209;--shiki-dark:#FFAB70">x</span><span style="color:#D73A49;--shiki-dark:#F97583">:</span><span style="color:#005CC5;--shiki-dark:#79B8FF"> number</span><span style="color:#24292E;--shiki-dark:#E1E4E8">)</span><span style="color:#D73A49;--shiki-dark:#F97583">:</span><span style="color:#005CC5;--shiki-dark:#79B8FF"> number</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> {</span></span>\n<span class="line"><span style="color:#D73A49;--shiki-dark:#F97583">  return</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> x </span><span style="color:#D73A49;--shiki-dark:#F97583">+</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> x;</span></span>\n<span class="line"><span style="color:#24292E;--shiki-dark:#E1E4E8">}</span></span>\n<span class="line"></span>\n<span class="line"><span style="color:#D73A49;--shiki-dark:#F97583">const</span><span style="color:#005CC5;--shiki-dark:#79B8FF"> answer</span><span style="color:#D73A49;--shiki-dark:#F97583"> =</span><span style="color:#6F42C1;--shiki-dark:#B392F0"> twice</span><span style="color:#24292E;--shiki-dark:#E1E4E8">(</span><span style="color:#005CC5;--shiki-dark:#79B8FF">21</span><span style="color:#24292E;--shiki-dark:#E1E4E8">);</span></span></code></pre></section>\n</div><p>The next snippet introduces a reusable function and direct call style.</p>\n<h2 id="when"><a href="#when">When</a></h2><p>Use this pattern for notes, toy utilities, and onboarding examples before you add package config.</p>\n<h2 id="analogy"><a href="#analogy">Analogy</a></h2><p>Think like Python or JavaScript REPL cells: each statement creates a value, and later statements can use earlier results.</p>\n<h2 id="try-it"><a href="#try-it">Try it</a></h2><p>Define the two snippets above, evaluate with <code>music</code>, then move to <a href="/docs/files-packages-and-entry">Files, packages, and entry</a>.</p>\n',
		summaryHtml: "A first Musi file without extra ceremony.",
	},
	{
		title: "Files, packages, and entry",
		description:
			"Know what <code>musi new</code> creates and what <code>musi run</code> looks for.",
		group: "Start",
		section: "Start",
		order: 3,
		slug: "files-packages-and-entry",
		summary: "Packages, <code>musi.json</code>, and the resolved entry file.",
		descriptionHtml:
			"Know what <code>musi new</code> creates and what <code>musi run</code> looks for.",
		headings: [
			{
				depth: 2,
				id: "what",
				text: "What",
			},
			{
				depth: 2,
				id: "why",
				text: "Why",
			},
			{
				depth: 2,
				id: "how",
				text: "How",
			},
			{
				depth: 2,
				id: "when",
				text: "When",
			},
			{
				depth: 2,
				id: "analogy",
				text: "Analogy",
			},
			{
				depth: 2,
				id: "try-it",
				text: "Try it",
			},
		],
		html: '<p>Packages are the default shape for real projects. A package gives you an entry file and command entrypoints.</p>\n<h2 id="what"><a href="#what">What</a></h2><p>You use this when your project grows past one file: dependencies, scripts, and shared entry points stay in one place.</p>\n<pre class="shiki shiki-themes github-light github-dark" style="background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8" tabindex="0"><code><span class="line"><span style="color:#6F42C1;--shiki-dark:#B392F0">musi</span><span style="color:#032F62;--shiki-dark:#9ECBFF"> run</span></span>\n<span class="line"><span style="color:#6F42C1;--shiki-dark:#B392F0">musi</span><span style="color:#032F62;--shiki-dark:#9ECBFF"> check</span></span>\n<span class="line"><span style="color:#6F42C1;--shiki-dark:#B392F0">musi</span><span style="color:#032F62;--shiki-dark:#9ECBFF"> build</span></span>\n<span class="line"><span style="color:#6F42C1;--shiki-dark:#B392F0">musi</span><span style="color:#032F62;--shiki-dark:#9ECBFF"> test</span></span></code></pre><h2 id="why"><a href="#why">Why</a></h2><p><code>musi</code> saves you from hand-managing file paths every time you run, test, or build.</p>\n<ul>\n<li>entry resolution follows package config</li>\n<li>commands stay stable across environments</li>\n<li>team members use the same workflow</li>\n</ul>\n<h2 id="how"><a href="#how">How</a></h2><p>Keep the generated package shape and use package commands from the root:</p>\n<ul>\n<li><code>musi run</code></li>\n<li><code>musi check</code></li>\n<li><code>musi build</code></li>\n<li><code>musi test</code></li>\n</ul>\n<p>For direct one-off work, use a specific source file or artifact with <code>music</code>.</p>\n<pre class="shiki shiki-themes github-light github-dark" style="background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8" tabindex="0"><code><span class="line"><span style="color:#6F42C1;--shiki-dark:#B392F0">music</span><span style="color:#032F62;--shiki-dark:#9ECBFF"> check</span><span style="color:#032F62;--shiki-dark:#9ECBFF"> index.ms</span></span>\n<span class="line"><span style="color:#6F42C1;--shiki-dark:#B392F0">music</span><span style="color:#032F62;--shiki-dark:#9ECBFF"> build</span><span style="color:#032F62;--shiki-dark:#9ECBFF"> index.ms</span></span>\n<span class="line"><span style="color:#6F42C1;--shiki-dark:#B392F0">music</span><span style="color:#032F62;--shiki-dark:#9ECBFF"> run</span><span style="color:#032F62;--shiki-dark:#9ECBFF"> index.seam</span></span></code></pre><h2 id="when"><a href="#when">When</a></h2><p>Use package mode for project workflows and <code>music</code> for experimentation, quick checks, or artifact tests.</p>\n<h2 id="analogy"><a href="#analogy">Analogy</a></h2><p>Think <code>musi</code> as the CLI wrapper around your project folder and <code>music</code> as a direct file runner.</p>\n<h2 id="try-it"><a href="#try-it">Try it</a></h2><p>Create a package, confirm which entry file is used, then continue to <a href="/docs/expressions-and-bindings">Expressions and bindings</a>.</p>\n',
		summaryHtml:
			"Packages, <code>musi.json</code>, and the resolved entry file.",
	},
	{
		title: "Expressions and bindings",
		description:
			"Read Musi through <code>let</code>, sequences, and <code>case</code>.",
		group: "Core language",
		section: "Core language",
		order: 4,
		slug: "expressions-and-bindings",
		summary: "The base reading model for names, sequences, and branching.",
		descriptionHtml:
			"Read Musi through <code>let</code>, sequences, and <code>case</code>.",
		headings: [
			{
				depth: 2,
				id: "what",
				text: "What",
			},
			{
				depth: 2,
				id: "why",
				text: "Why",
			},
			{
				depth: 2,
				id: "how",
				text: "How",
			},
			{
				depth: 2,
				id: "when",
				text: "When",
			},
			{
				depth: 2,
				id: "analogy",
				text: "Analogy",
			},
			{
				depth: 2,
				id: "try-it",
				text: "Try it",
			},
		],
		html: '<h2 id="what"><a href="#what">What</a></h2><p>Expressions and bindings are the foundation of Musi reading.\nStart with a bound name, then read later expressions from the top of the file down.</p>\n<pre class="shiki shiki-themes github-light github-dark" style="background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8" tabindex="0"><code><span class="line"><span style="color:#D73A49;--shiki-dark:#F97583">let</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> port </span><span style="color:#D73A49;--shiki-dark:#F97583">:=</span><span style="color:#005CC5;--shiki-dark:#79B8FF"> 8080</span><span style="color:#24292E;--shiki-dark:#E1E4E8">;</span></span>\n<span class="line"><span style="color:#24292E;--shiki-dark:#E1E4E8">port;</span></span></code></pre><h2 id="why"><a href="#why">Why</a></h2><p>This model is predictable for Python/JS users: each line can introduce data, then later lines consume it.</p>\n<h2 id="how"><a href="#how">How</a></h2><p>Use <code>let</code> for names and <code>;</code> for expression boundaries.</p>\n<pre class="shiki shiki-themes github-light github-dark" style="background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8" tabindex="0"><code><span class="line"><span style="color:#24292E;--shiki-dark:#E1E4E8">(</span></span>\n<span class="line"><span style="color:#D73A49;--shiki-dark:#F97583">  let</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> base </span><span style="color:#D73A49;--shiki-dark:#F97583">:=</span><span style="color:#005CC5;--shiki-dark:#79B8FF"> 8000</span><span style="color:#24292E;--shiki-dark:#E1E4E8">;</span></span>\n<span class="line"><span style="color:#24292E;--shiki-dark:#E1E4E8">  base </span><span style="color:#D73A49;--shiki-dark:#F97583">+</span><span style="color:#005CC5;--shiki-dark:#79B8FF"> 80</span></span>\n<span class="line"><span style="color:#24292E;--shiki-dark:#E1E4E8">);</span></span></code></pre><p>Case expressions are the branching form in this surface.</p>\n<pre class="shiki shiki-themes github-light github-dark" style="background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8" tabindex="0"><code><span class="line"><span style="color:#D73A49;--shiki-dark:#F97583">let</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> Port </span><span style="color:#D73A49;--shiki-dark:#F97583">:=</span><span style="color:#D73A49;--shiki-dark:#F97583"> data</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> {</span></span>\n<span class="line"><span style="color:#24292E;--shiki-dark:#E1E4E8">  | Configured : Int</span></span>\n<span class="line"><span style="color:#24292E;--shiki-dark:#E1E4E8">  | Default</span></span>\n<span class="line"><span style="color:#24292E;--shiki-dark:#E1E4E8">};</span></span>\n<span class="line"></span>\n<span class="line"><span style="color:#D73A49;--shiki-dark:#F97583">let</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> port : Port </span><span style="color:#D73A49;--shiki-dark:#F97583">:=</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> .Configured(</span><span style="color:#005CC5;--shiki-dark:#79B8FF">8080</span><span style="color:#24292E;--shiki-dark:#E1E4E8">);</span></span>\n<span class="line"><span style="color:#D73A49;--shiki-dark:#F97583">case</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> port </span><span style="color:#D73A49;--shiki-dark:#F97583">of</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> (</span></span>\n<span class="line"><span style="color:#24292E;--shiki-dark:#E1E4E8">| .Configured(port) </span><span style="color:#D73A49;--shiki-dark:#F97583">=></span><span style="color:#24292E;--shiki-dark:#E1E4E8"> port</span></span>\n<span class="line"><span style="color:#24292E;--shiki-dark:#E1E4E8">| .Default </span><span style="color:#D73A49;--shiki-dark:#F97583">=></span><span style="color:#005CC5;--shiki-dark:#79B8FF"> 3000</span></span>\n<span class="line"><span style="color:#24292E;--shiki-dark:#E1E4E8">);</span></span></code></pre><h2 id="when"><a href="#when">When</a></h2><p>Use these patterns when you want readable scripts, data preparation, and deterministic command flow.</p>\n<h2 id="analogy"><a href="#analogy">Analogy</a></h2><p>Like a recipe card: bind each ingredient first, then assemble a final step.</p>\n<h2 id="try-it"><a href="#try-it">Try it</a></h2><p>Read the two snippets, then move to <a href="/docs/functions-and-calls">Functions and calls</a>.</p>\n',
		summaryHtml: "The base reading model for names, sequences, and branching.",
	},
	{
		title: "Functions and calls",
		description:
			"Define functions with <code>let</code>, call them normally, and use <code>let rec</code> for recursion.",
		group: "Core language",
		section: "Core language",
		order: 5,
		slug: "functions-and-calls",
		summary: "Functions, calls, and recursion without extra control syntax.",
		descriptionHtml:
			"Define functions with <code>let</code>, call them normally, and use <code>let rec</code> for recursion.",
		headings: [
			{
				depth: 2,
				id: "what",
				text: "What",
			},
			{
				depth: 2,
				id: "why",
				text: "Why",
			},
			{
				depth: 2,
				id: "how",
				text: "How",
			},
			{
				depth: 2,
				id: "compare",
				text: "Compare",
			},
			{
				depth: 2,
				id: "when",
				text: "When",
			},
			{
				depth: 2,
				id: "analogy",
				text: "Analogy",
			},
			{
				depth: 2,
				id: "try-it",
				text: "Try it",
			},
		],
		html: '<h2 id="what"><a href="#what">What</a></h2><p>Functions are values you can bind, pass, and call.</p>\n<h2 id="why"><a href="#why">Why</a></h2><p>This gives a direct way to organize repeated logic without relying on special syntax blocks.</p>\n<h2 id="how"><a href="#how">How</a></h2><p>Call named functions like usual value calls, then define recursion in the same surface with <code>let rec</code>.</p>\n<h2 id="compare"><a href="#compare">Compare</a></h2><div class="code-tabs" data-code-tabs="1" data-example-id="double-function" data-default="musi" data-active-language="musi">\n<div class="code-tabs-meta">\n<p class="code-tabs-caption">Same small task across four languages. Musi keeps it as an expression-oriented <code>let</code> binding.</p>\n<p class="code-tabs-note">Musi functions are ordinary bindings, so the syntax stays close to other definitions.</p>\n</div>\n<div class="code-tablist" role="tablist" aria-label="Small function that doubles a number"><button type="button" role="tab" class="code-tab" data-language="java" aria-selected="false" tabindex="-1">Java</button><button type="button" role="tab" class="code-tab" data-language="musi" aria-selected="true" tabindex="0">Musi</button><button type="button" role="tab" class="code-tab" data-language="rust" aria-selected="false" tabindex="-1">Rust</button><button type="button" role="tab" class="code-tab" data-language="typescript" aria-selected="false" tabindex="-1">TypeScript</button></div>\n<section role="tabpanel" class="code-panel" data-language="java" data-active="false" hidden=""><pre class="shiki shiki-themes github-light github-dark" style="background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8" tabindex="0"><code><span class="line"><span style="color:#D73A49;--shiki-dark:#F97583">static</span><span style="color:#D73A49;--shiki-dark:#F97583"> int</span><span style="color:#6F42C1;--shiki-dark:#B392F0"> twice</span><span style="color:#24292E;--shiki-dark:#E1E4E8">(</span><span style="color:#D73A49;--shiki-dark:#F97583">int</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> x) {</span></span>\n<span class="line"><span style="color:#D73A49;--shiki-dark:#F97583">    return</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> x </span><span style="color:#D73A49;--shiki-dark:#F97583">+</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> x;</span></span>\n<span class="line"><span style="color:#24292E;--shiki-dark:#E1E4E8">}</span></span>\n<span class="line"></span>\n<span class="line"><span style="color:#D73A49;--shiki-dark:#F97583">int</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> answer </span><span style="color:#D73A49;--shiki-dark:#F97583">=</span><span style="color:#6F42C1;--shiki-dark:#B392F0"> twice</span><span style="color:#24292E;--shiki-dark:#E1E4E8">(</span><span style="color:#005CC5;--shiki-dark:#79B8FF">21</span><span style="color:#24292E;--shiki-dark:#E1E4E8">);</span></span></code></pre></section>\n<section role="tabpanel" class="code-panel" data-language="musi" data-active="true"><pre class="shiki shiki-themes github-light github-dark" style="background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8" tabindex="0"><code><span class="line"><span style="color:#D73A49;--shiki-dark:#F97583">let</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> twice (x : Int) : Int </span><span style="color:#D73A49;--shiki-dark:#F97583">:=</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> x </span><span style="color:#D73A49;--shiki-dark:#F97583">+</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> x;</span></span>\n<span class="line"></span>\n<span class="line"><span style="color:#24292E;--shiki-dark:#E1E4E8">twice(</span><span style="color:#005CC5;--shiki-dark:#79B8FF">21</span><span style="color:#24292E;--shiki-dark:#E1E4E8">);</span></span></code></pre></section>\n<section role="tabpanel" class="code-panel" data-language="rust" data-active="false" hidden=""><pre class="shiki shiki-themes github-light github-dark" style="background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8" tabindex="0"><code><span class="line"><span style="color:#D73A49;--shiki-dark:#F97583">fn</span><span style="color:#6F42C1;--shiki-dark:#B392F0"> twice</span><span style="color:#24292E;--shiki-dark:#E1E4E8">(x</span><span style="color:#D73A49;--shiki-dark:#F97583">:</span><span style="color:#6F42C1;--shiki-dark:#B392F0"> i32</span><span style="color:#24292E;--shiki-dark:#E1E4E8">) </span><span style="color:#D73A49;--shiki-dark:#F97583">-></span><span style="color:#6F42C1;--shiki-dark:#B392F0"> i32</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> {</span></span>\n<span class="line"><span style="color:#24292E;--shiki-dark:#E1E4E8">    x </span><span style="color:#D73A49;--shiki-dark:#F97583">+</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> x</span></span>\n<span class="line"><span style="color:#24292E;--shiki-dark:#E1E4E8">}</span></span>\n<span class="line"></span>\n<span class="line"><span style="color:#D73A49;--shiki-dark:#F97583">let</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> answer </span><span style="color:#D73A49;--shiki-dark:#F97583">=</span><span style="color:#6F42C1;--shiki-dark:#B392F0"> twice</span><span style="color:#24292E;--shiki-dark:#E1E4E8">(</span><span style="color:#005CC5;--shiki-dark:#79B8FF">21</span><span style="color:#24292E;--shiki-dark:#E1E4E8">);</span></span></code></pre></section>\n<section role="tabpanel" class="code-panel" data-language="typescript" data-active="false" hidden=""><pre class="shiki shiki-themes github-light github-dark" style="background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8" tabindex="0"><code><span class="line"><span style="color:#D73A49;--shiki-dark:#F97583">function</span><span style="color:#6F42C1;--shiki-dark:#B392F0"> twice</span><span style="color:#24292E;--shiki-dark:#E1E4E8">(</span><span style="color:#E36209;--shiki-dark:#FFAB70">x</span><span style="color:#D73A49;--shiki-dark:#F97583">:</span><span style="color:#005CC5;--shiki-dark:#79B8FF"> number</span><span style="color:#24292E;--shiki-dark:#E1E4E8">)</span><span style="color:#D73A49;--shiki-dark:#F97583">:</span><span style="color:#005CC5;--shiki-dark:#79B8FF"> number</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> {</span></span>\n<span class="line"><span style="color:#D73A49;--shiki-dark:#F97583">  return</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> x </span><span style="color:#D73A49;--shiki-dark:#F97583">+</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> x;</span></span>\n<span class="line"><span style="color:#24292E;--shiki-dark:#E1E4E8">}</span></span>\n<span class="line"></span>\n<span class="line"><span style="color:#D73A49;--shiki-dark:#F97583">const</span><span style="color:#005CC5;--shiki-dark:#79B8FF"> answer</span><span style="color:#D73A49;--shiki-dark:#F97583"> =</span><span style="color:#6F42C1;--shiki-dark:#B392F0"> twice</span><span style="color:#24292E;--shiki-dark:#E1E4E8">(</span><span style="color:#005CC5;--shiki-dark:#79B8FF">21</span><span style="color:#24292E;--shiki-dark:#E1E4E8">);</span></span></code></pre></section>\n</div><pre class="shiki shiki-themes github-light github-dark" style="background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8" tabindex="0"><code><span class="line"><span style="color:#D73A49;--shiki-dark:#F97583">let</span><span style="color:#D73A49;--shiki-dark:#F97583"> rec</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> loop (x : Int) : Int </span><span style="color:#D73A49;--shiki-dark:#F97583">:=</span></span>\n<span class="line"><span style="color:#D73A49;--shiki-dark:#F97583">  case</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> x </span><span style="color:#D73A49;--shiki-dark:#F97583">of</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> (</span></span>\n<span class="line"><span style="color:#24292E;--shiki-dark:#E1E4E8">  | </span><span style="color:#005CC5;--shiki-dark:#79B8FF">0</span><span style="color:#D73A49;--shiki-dark:#F97583"> =></span><span style="color:#005CC5;--shiki-dark:#79B8FF"> 0</span></span>\n<span class="line"><span style="color:#24292E;--shiki-dark:#E1E4E8">  | </span><span style="color:#005CC5;--shiki-dark:#79B8FF">_</span><span style="color:#D73A49;--shiki-dark:#F97583"> =></span><span style="color:#24292E;--shiki-dark:#E1E4E8"> loop(x </span><span style="color:#D73A49;--shiki-dark:#F97583">-</span><span style="color:#005CC5;--shiki-dark:#79B8FF"> 1</span><span style="color:#24292E;--shiki-dark:#E1E4E8">)</span></span>\n<span class="line"><span style="color:#24292E;--shiki-dark:#E1E4E8">  );</span></span></code></pre><h2 id="when"><a href="#when">When</a></h2><p>Use recursion for traversals, accumulations, and simple parsers when a loop is not needed.</p>\n<h2 id="analogy"><a href="#analogy">Analogy</a></h2><p>Like defining a helper in Python and calling itself from its own body, but in one value-binding style.</p>\n<h2 id="try-it"><a href="#try-it">Try it</a></h2><p>Bind and call a function, then add the recursive form and continue to <a href="/docs/imports-and-packages">Imports and packages</a>.</p>\n',
		summaryHtml:
			"Functions, calls, and recursion without extra control syntax.",
	},
	{
		title: "Imports and packages",
		description: "Import modules and use the main namespace families.",
		group: "Core language",
		section: "Core language",
		order: 6,
		slug: "imports-and-packages",
		summary:
			"Import expressions, <code>@std</code>, and the <code>musi:*</code> foundation namespace.",
		descriptionHtml: "Import modules and use the main namespace families.",
		headings: [
			{
				depth: 2,
				id: "what",
				text: "What",
			},
			{
				depth: 2,
				id: "why",
				text: "Why",
			},
			{
				depth: 2,
				id: "how",
				text: "How",
			},
			{
				depth: 2,
				id: "compare",
				text: "Compare",
			},
			{
				depth: 2,
				id: "when",
				text: "When",
			},
			{
				depth: 2,
				id: "analogy",
				text: "Analogy",
			},
			{
				depth: 2,
				id: "try-it",
				text: "Try it",
			},
		],
		html: '<h2 id="what"><a href="#what">What</a></h2><p>Imports let you pull shared code into the current file: standard features from <code>@std</code> and lower-level building blocks from <code>musi:*</code> when needed.</p>\n<h2 id="why"><a href="#why">Why</a></h2><p>This keeps code short and reusable while avoiding copy-paste or duplicated helpers.</p>\n<h2 id="how"><a href="#how">How</a></h2><p>Add imports at the top of your file, then use imported names directly in regular expressions.</p>\n<h2 id="compare"><a href="#compare">Compare</a></h2><div class="code-tabs" data-code-tabs="1" data-example-id="import-stdlib" data-default="musi" data-active-language="musi">\n<div class="code-tabs-meta">\n<p class="code-tabs-caption">Import the standard library, then reach the family you need. Musi keeps stdlib access explicit through <code>@std</code>.</p>\n<p class="code-tabs-note">Musi package imports are values, so <code>@std</code> becomes a normal binding you can pass around.</p>\n</div>\n<div class="code-tablist" role="tablist" aria-label="Import a standard library module"><button type="button" role="tab" class="code-tab" data-language="java" aria-selected="false" tabindex="-1">Java</button><button type="button" role="tab" class="code-tab" data-language="musi" aria-selected="true" tabindex="0">Musi</button><button type="button" role="tab" class="code-tab" data-language="rust" aria-selected="false" tabindex="-1">Rust</button><button type="button" role="tab" class="code-tab" data-language="typescript" aria-selected="false" tabindex="-1">TypeScript</button></div>\n<section role="tabpanel" class="code-panel" data-language="java" data-active="false" hidden=""><pre class="shiki shiki-themes github-light github-dark" style="background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8" tabindex="0"><code><span class="line"><span style="color:#D73A49;--shiki-dark:#F97583">import</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> java.util.Optional;</span></span></code></pre></section>\n<section role="tabpanel" class="code-panel" data-language="musi" data-active="true"><pre class="shiki shiki-themes github-light github-dark" style="background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8" tabindex="0"><code><span class="line"><span style="color:#D73A49;--shiki-dark:#F97583">let</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> Std </span><span style="color:#D73A49;--shiki-dark:#F97583">:=</span><span style="color:#D73A49;--shiki-dark:#F97583"> import</span><span style="color:#032F62;--shiki-dark:#9ECBFF"> "@std"</span><span style="color:#24292E;--shiki-dark:#E1E4E8">;</span></span>\n<span class="line"><span style="color:#D73A49;--shiki-dark:#F97583">let</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> Option </span><span style="color:#D73A49;--shiki-dark:#F97583">:=</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> Std.Option;</span></span></code></pre></section>\n<section role="tabpanel" class="code-panel" data-language="rust" data-active="false" hidden=""><pre class="shiki shiki-themes github-light github-dark" style="background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8" tabindex="0"><code><span class="line"><span style="color:#D73A49;--shiki-dark:#F97583">use</span><span style="color:#6F42C1;--shiki-dark:#B392F0"> std</span><span style="color:#D73A49;--shiki-dark:#F97583">::</span><span style="color:#6F42C1;--shiki-dark:#B392F0">option</span><span style="color:#D73A49;--shiki-dark:#F97583">::</span><span style="color:#6F42C1;--shiki-dark:#B392F0">Option</span><span style="color:#24292E;--shiki-dark:#E1E4E8">;</span></span></code></pre></section>\n<section role="tabpanel" class="code-panel" data-language="typescript" data-active="false" hidden=""><pre class="shiki shiki-themes github-light github-dark" style="background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8" tabindex="0"><code><span class="line"><span style="color:#D73A49;--shiki-dark:#F97583">import</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> { readFileSync } </span><span style="color:#D73A49;--shiki-dark:#F97583">from</span><span style="color:#032F62;--shiki-dark:#9ECBFF"> "node:fs"</span><span style="color:#24292E;--shiki-dark:#E1E4E8">;</span></span></code></pre></section>\n</div><h2 id="when"><a href="#when">When</a></h2><p>Use imports in most new files.</p>\n<ul>\n<li>Start with <code>@std</code> for everyday tasks.</li>\n<li>Use <code>musi:*</code> for boundary-level interoperability and foundation-level work.</li>\n</ul>\n<h2 id="analogy"><a href="#analogy">Analogy</a></h2><p>Like Python imports or TypeScript module imports: one place for shared functionality, one place for lower-level tools.</p>\n<h2 id="try-it"><a href="#try-it">Try it</a></h2><p>Replace duplicated snippets with imported names, then continue to <a href="/docs/data-and-pattern-matching">Data and pattern matching</a>.</p>\n',
		summaryHtml:
			"Import expressions, <code>@std</code>, and the <code>musi:*</code> foundation namespace.",
	},
	{
		title: "Data and pattern matching",
		description:
			"Define sums with <code>data</code>, construct variants, and read them with <code>case</code>.",
		group: "Core language",
		section: "Core language",
		order: 7,
		slug: "data-and-pattern-matching",
		summary: "Data definitions, constructors, and pattern matching.",
		descriptionHtml:
			"Define sums with <code>data</code>, construct variants, and read them with <code>case</code>.",
		headings: [
			{
				depth: 2,
				id: "what",
				text: "What",
			},
			{
				depth: 2,
				id: "why",
				text: "Why",
			},
			{
				depth: 2,
				id: "how",
				text: "How",
			},
			{
				depth: 2,
				id: "when",
				text: "When",
			},
			{
				depth: 2,
				id: "analogy",
				text: "Analogy",
			},
			{
				depth: 2,
				id: "try-it",
				text: "Try it",
			},
		],
		html: '<h2 id="what"><a href="#what">What</a></h2><p>Use <code>data</code> to model bounded domains directly in code.\nThe <code>case</code> form reads shape by shape and keeps branching explicit.</p>\n<h2 id="why"><a href="#why">Why</a></h2><p>Named variants make domain rules obvious and easier to test.\nYou avoid stringly-typed flags and boolean ambiguity.</p>\n<h2 id="how"><a href="#how">How</a></h2><p>Define variants once, construct values from those constructors, then consume them with <code>case</code>.</p>\n<pre class="shiki shiki-themes github-light github-dark" style="background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8" tabindex="0"><code><span class="line"><span style="color:#D73A49;--shiki-dark:#F97583">let</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> Port </span><span style="color:#D73A49;--shiki-dark:#F97583">:=</span><span style="color:#D73A49;--shiki-dark:#F97583"> data</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> {</span></span>\n<span class="line"><span style="color:#24292E;--shiki-dark:#E1E4E8">  | Configured : Int</span></span>\n<span class="line"><span style="color:#24292E;--shiki-dark:#E1E4E8">  | Default</span></span>\n<span class="line"><span style="color:#24292E;--shiki-dark:#E1E4E8">};</span></span></code></pre><pre class="shiki shiki-themes github-light github-dark" style="background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8" tabindex="0"><code><span class="line"><span style="color:#D73A49;--shiki-dark:#F97583">let</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> port : Port </span><span style="color:#D73A49;--shiki-dark:#F97583">:=</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> .Configured(</span><span style="color:#005CC5;--shiki-dark:#79B8FF">8080</span><span style="color:#24292E;--shiki-dark:#E1E4E8">);</span></span></code></pre><h2 id="when"><a href="#when">When</a></h2><p>Reach for this chapter when values have distinct outcomes (task states, command result states, protocol messages).</p>\n<pre class="shiki shiki-themes github-light github-dark" style="background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8" tabindex="0"><code><span class="line"><span style="color:#D73A49;--shiki-dark:#F97583">case</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> port </span><span style="color:#D73A49;--shiki-dark:#F97583">of</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> (</span></span>\n<span class="line"><span style="color:#24292E;--shiki-dark:#E1E4E8">| .Configured(value) </span><span style="color:#D73A49;--shiki-dark:#F97583">=></span><span style="color:#24292E;--shiki-dark:#E1E4E8"> value</span></span>\n<span class="line"><span style="color:#24292E;--shiki-dark:#E1E4E8">| .Default </span><span style="color:#D73A49;--shiki-dark:#F97583">=></span><span style="color:#005CC5;--shiki-dark:#79B8FF"> 3000</span></span>\n<span class="line"><span style="color:#24292E;--shiki-dark:#E1E4E8">);</span></span></code></pre><h2 id="analogy"><a href="#analogy">Analogy</a></h2><p>Like a JavaScript union with explicit branches, but checked and navigated with one <code>case</code> expression.</p>\n<h2 id="try-it"><a href="#try-it">Try it</a></h2><p>Read the three snippets in order, then continue to <a href="/docs/records-arrays-and-mutation">Records and arrays</a>.</p>\n',
		summaryHtml: "Data definitions, constructors, and pattern matching.",
	},
	{
		title: "Records and arrays",
		description: "Use record literals, arrays, and explicit spread forms.",
		group: "Core language",
		section: "Core language",
		order: 8,
		slug: "records-arrays-and-mutation",
		summary: "Structured values and the current writeable-data surface.",
		descriptionHtml: "Use record literals, arrays, and explicit spread forms.",
		headings: [
			{
				depth: 2,
				id: "what",
				text: "What",
			},
			{
				depth: 2,
				id: "why",
				text: "Why",
			},
			{
				depth: 2,
				id: "how",
				text: "How",
			},
			{
				depth: 2,
				id: "when",
				text: "When",
			},
			{
				depth: 2,
				id: "analogy",
				text: "Analogy",
			},
			{
				depth: 2,
				id: "try-it",
				text: "Try it",
			},
		],
		html: '<h2 id="what"><a href="#what">What</a></h2><p>Records and arrays are ordinary values with predictable update patterns.</p>\n<pre class="shiki shiki-themes github-light github-dark" style="background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8" tabindex="0"><code><span class="line"><span style="color:#D73A49;--shiki-dark:#F97583">let</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> point </span><span style="color:#D73A49;--shiki-dark:#F97583">:=</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> { x </span><span style="color:#D73A49;--shiki-dark:#F97583">:=</span><span style="color:#005CC5;--shiki-dark:#79B8FF"> 3</span><span style="color:#24292E;--shiki-dark:#E1E4E8">, y </span><span style="color:#D73A49;--shiki-dark:#F97583">:=</span><span style="color:#005CC5;--shiki-dark:#79B8FF"> 4</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> };</span></span>\n<span class="line"><span style="color:#D73A49;--shiki-dark:#F97583">let</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> values </span><span style="color:#D73A49;--shiki-dark:#F97583">:=</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> [</span><span style="color:#005CC5;--shiki-dark:#79B8FF">1</span><span style="color:#24292E;--shiki-dark:#E1E4E8">, </span><span style="color:#005CC5;--shiki-dark:#79B8FF">2</span><span style="color:#24292E;--shiki-dark:#E1E4E8">, </span><span style="color:#005CC5;--shiki-dark:#79B8FF">3</span><span style="color:#24292E;--shiki-dark:#E1E4E8">];</span></span></code></pre><h2 id="why"><a href="#why">Why</a></h2><p>They keep data structured and avoid mixing unrelated values into one flat tuple.</p>\n<h2 id="how"><a href="#how">How</a></h2><p>Build values with literals, then use spread/update forms when you need a modified copy.</p>\n<pre class="shiki shiki-themes github-light github-dark" style="background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8" tabindex="0"><code><span class="line"><span style="color:#D73A49;--shiki-dark:#F97583">let</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> point3 </span><span style="color:#D73A49;--shiki-dark:#F97583">:=</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> { </span><span style="color:#D73A49;--shiki-dark:#F97583">...</span><span style="color:#24292E;--shiki-dark:#E1E4E8">point, z </span><span style="color:#D73A49;--shiki-dark:#F97583">:=</span><span style="color:#005CC5;--shiki-dark:#79B8FF"> 5</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> };</span></span>\n<span class="line"><span style="color:#D73A49;--shiki-dark:#F97583">let</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> extended </span><span style="color:#D73A49;--shiki-dark:#F97583">:=</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> [</span><span style="color:#005CC5;--shiki-dark:#79B8FF">0</span><span style="color:#24292E;--shiki-dark:#E1E4E8">, </span><span style="color:#D73A49;--shiki-dark:#F97583">...</span><span style="color:#24292E;--shiki-dark:#E1E4E8">values];</span></span></code></pre><h2 id="when"><a href="#when">When</a></h2><p>Use these forms for request payloads, config objects, and small in-memory collections.</p>\n<h2 id="analogy"><a href="#analogy">Analogy</a></h2><p>Like object/array literals in JS, with explicit update syntax.</p>\n<h2 id="try-it"><a href="#try-it">Try it</a></h2><p>Create a base value then build one spread-based variant, then continue to <a href="/docs/effects-and-handlers">Effects and handlers</a>.</p>\n',
		summaryHtml: "Structured values and the current writeable-data surface.",
	},
	{
		title: "Effects and handlers",
		description:
			"Use <code>effect</code>, <code>perform</code>, <code>handle</code>, and <code>resume</code> as part of normal Musi code.",
		group: "Effects",
		section: "Effects",
		order: 9,
		slug: "effects-and-handlers",
		summary: "The main Musi differentiator, shown with real syntax.",
		descriptionHtml:
			"Use <code>effect</code>, <code>perform</code>, <code>handle</code>, and <code>resume</code> as part of normal Musi code.",
		headings: [
			{
				depth: 2,
				id: "what",
				text: "What",
			},
			{
				depth: 2,
				id: "why",
				text: "Why",
			},
			{
				depth: 2,
				id: "how",
				text: "How",
			},
			{
				depth: 2,
				id: "when",
				text: "When",
			},
			{
				depth: 2,
				id: "analogy",
				text: "Analogy",
			},
			{
				depth: 2,
				id: "try-it",
				text: "Try it",
			},
		],
		html: '<h2 id="what"><a href="#what">What</a></h2><p>Effects are built into the language as part of normal flow control:\nyou define an effect, perform it, and handle it at a boundary.</p>\n<h2 id="why"><a href="#why">Why</a></h2><p>This model separates “what happened” from “how to handle it,” which keeps business logic clearer as projects grow.</p>\n<pre class="shiki shiki-themes github-light github-dark" style="background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8" tabindex="0"><code><span class="line"><span style="color:#D73A49;--shiki-dark:#F97583">let</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> Console </span><span style="color:#D73A49;--shiki-dark:#F97583">:=</span><span style="color:#D73A49;--shiki-dark:#F97583"> effect</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> {</span></span>\n<span class="line"><span style="color:#D73A49;--shiki-dark:#F97583">  let</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> readln () : String;</span></span>\n<span class="line"><span style="color:#24292E;--shiki-dark:#E1E4E8">};</span></span></code></pre><h2 id="how"><a href="#how">How</a></h2><p>Define one effect family, perform operations, and add handlers for policy (logging, fallback, default values, reporting).</p>\n<pre class="shiki shiki-themes github-light github-dark" style="background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8" tabindex="0"><code><span class="line"><span style="color:#D73A49;--shiki-dark:#F97583">perform</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> Console.readln();</span></span></code></pre><h2 id="when"><a href="#when">When</a></h2><p>Use effects for cross-cutting concerns:\nresource usage, command routing, telemetry, and deferred behavior.</p>\n<pre class="shiki shiki-themes github-light github-dark" style="background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8" tabindex="0"><code><span class="line"><span style="color:#D73A49;--shiki-dark:#F97583">handle</span><span style="color:#D73A49;--shiki-dark:#F97583"> perform</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> Console.readln() </span><span style="color:#D73A49;--shiki-dark:#F97583">with</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> Console </span><span style="color:#D73A49;--shiki-dark:#F97583">of</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> (</span></span>\n<span class="line"><span style="color:#24292E;--shiki-dark:#E1E4E8">| value </span><span style="color:#D73A49;--shiki-dark:#F97583">=></span><span style="color:#24292E;--shiki-dark:#E1E4E8"> value</span></span>\n<span class="line"><span style="color:#24292E;--shiki-dark:#E1E4E8">| readln(k) </span><span style="color:#D73A49;--shiki-dark:#F97583">=></span><span style="color:#D73A49;--shiki-dark:#F97583"> resume</span><span style="color:#032F62;--shiki-dark:#9ECBFF"> "ok"</span></span>\n<span class="line"><span style="color:#24292E;--shiki-dark:#E1E4E8">);</span></span></code></pre><h2 id="analogy"><a href="#analogy">Analogy</a></h2><p>Comparable to middleware stacks in web frameworks, but in expression-level form.</p>\n<h2 id="try-it"><a href="#try-it">Try it</a></h2><p>Read all three snippets end-to-end, then continue to <a href="/docs/types">Types and generics</a>.</p>\n<p>Continue to <a href="/docs/types">Types and generics</a>.</p>\n',
		summaryHtml: "The main Musi differentiator, shown with real syntax.",
	},
	{
		title: "Types and generics",
		description:
			"Read type annotations and generic parameters in the same surface as values.",
		group: "Types",
		section: "Types",
		order: 10,
		slug: "types",
		summary:
			"Type annotations, generic parameters, and direct type application.",
		descriptionHtml:
			"Read type annotations and generic parameters in the same surface as values.",
		headings: [
			{
				depth: 2,
				id: "what",
				text: "What",
			},
			{
				depth: 2,
				id: "why",
				text: "Why",
			},
			{
				depth: 2,
				id: "how",
				text: "How",
			},
			{
				depth: 2,
				id: "when",
				text: "When",
			},
			{
				depth: 2,
				id: "analogy",
				text: "Analogy",
			},
			{
				depth: 2,
				id: "try-it",
				text: "Try it",
			},
		],
		html: '<h2 id="what"><a href="#what">What</a></h2><p>Types in Musi appear near values and functions.\nYou can read types without switching to a separate declaration section.</p>\n<h2 id="why"><a href="#why">Why</a></h2><p>Type annotations make intent and errors clearer for mixed teams and longer files.</p>\n<pre class="shiki shiki-themes github-light github-dark" style="background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8" tabindex="0"><code><span class="line"><span style="color:#D73A49;--shiki-dark:#F97583">let</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> port : Int </span><span style="color:#D73A49;--shiki-dark:#F97583">:=</span><span style="color:#005CC5;--shiki-dark:#79B8FF"> 8080</span><span style="color:#24292E;--shiki-dark:#E1E4E8">;</span></span>\n<span class="line"><span style="color:#D73A49;--shiki-dark:#F97583">let</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> identity[T] (input : T) : T </span><span style="color:#D73A49;--shiki-dark:#F97583">:=</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> input;</span></span></code></pre><h2 id="how"><a href="#how">How</a></h2><p>Add annotations to values and functions, then apply generics where reusable behavior is needed.</p>\n<pre class="shiki shiki-themes github-light github-dark" style="background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8" tabindex="0"><code><span class="line"><span style="color:#24292E;--shiki-dark:#E1E4E8">identity[Int](port);</span></span></code></pre><h2 id="when"><a href="#when">When</a></h2><p>Use explicit typing when APIs are shared across modules or when signatures are not obvious.</p>\n<h2 id="analogy"><a href="#analogy">Analogy</a></h2><p>Similar to TypeScript annotations, but placed directly in the expression style used throughout Musi.</p>\n<h2 id="try-it"><a href="#try-it">Try it</a></h2><p>Try the two snippets before moving to <a href="/docs/classes-instances-and-laws">Classes and instances</a>.</p>\n',
		summaryHtml:
			"Type annotations, generic parameters, and direct type application.",
	},
	{
		title: "Classes and instances",
		description: "Read the class surface and define instances.",
		group: "Abstractions",
		section: "Abstractions",
		order: 11,
		slug: "classes-instances-and-laws",
		summary: "Classes, methods, and instance declarations.",
		descriptionHtml: "Read the class surface and define instances.",
		headings: [
			{
				depth: 2,
				id: "what",
				text: "What",
			},
			{
				depth: 2,
				id: "why",
				text: "Why",
			},
			{
				depth: 2,
				id: "how",
				text: "How",
			},
			{
				depth: 2,
				id: "when",
				text: "When",
			},
			{
				depth: 2,
				id: "analogy",
				text: "Analogy",
			},
			{
				depth: 2,
				id: "try-it",
				text: "Try it",
			},
		],
		html: '<h2 id="what"><a href="#what">What</a></h2><p>Classes define shared behavior names.\nInstances provide concrete implementations for those behavior names.</p>\n<h2 id="why"><a href="#why">Why</a></h2><p>This pattern keeps behavior contracts explicit and avoids repeating equivalent helper sets.</p>\n<h2 id="how"><a href="#how">How</a></h2><p>Define a class, then declare matching instances for concrete types.</p>\n<pre class="shiki shiki-themes github-light github-dark" style="background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8" tabindex="0"><code><span class="line"><span style="color:#D73A49;--shiki-dark:#F97583">let</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> Eq[T] </span><span style="color:#D73A49;--shiki-dark:#F97583">:=</span><span style="color:#D73A49;--shiki-dark:#F97583"> class</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> {</span></span>\n<span class="line"><span style="color:#D73A49;--shiki-dark:#F97583">  let</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> (</span><span style="color:#D73A49;--shiki-dark:#F97583">=</span><span style="color:#24292E;--shiki-dark:#E1E4E8">) (a : T, b : T) : Bool;</span></span>\n<span class="line"><span style="color:#D73A49;--shiki-dark:#F97583">  law</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> reflexive (x : T) </span><span style="color:#D73A49;--shiki-dark:#F97583">:=</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> true;</span></span>\n<span class="line"><span style="color:#24292E;--shiki-dark:#E1E4E8">};</span></span></code></pre><pre class="shiki shiki-themes github-light github-dark" style="background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8" tabindex="0"><code><span class="line"><span style="color:#D73A49;--shiki-dark:#F97583">let</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> eqInt </span><span style="color:#D73A49;--shiki-dark:#F97583">:=</span><span style="color:#D73A49;--shiki-dark:#F97583"> instance</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> Eq[Int] {</span></span>\n<span class="line"><span style="color:#D73A49;--shiki-dark:#F97583">  let</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> (</span><span style="color:#D73A49;--shiki-dark:#F97583">=</span><span style="color:#24292E;--shiki-dark:#E1E4E8">) (a : Int, b : Int) : Bool </span><span style="color:#D73A49;--shiki-dark:#F97583">:=</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> true;</span></span>\n<span class="line"><span style="color:#24292E;--shiki-dark:#E1E4E8">};</span></span></code></pre><h2 id="when"><a href="#when">When</a></h2><p>Use classes when you want one operation (for example equality or formatting) to work across multiple domains.</p>\n<h2 id="analogy"><a href="#analogy">Analogy</a></h2><p>Like interfaces in JavaScript/TypeScript, but with explicit instance attachment in the same language surface.</p>\n<h2 id="try-it"><a href="#try-it">Try it</a></h2><p>Review both snippets, then continue to <a href="/docs/attributes-and-foreign">Attributes and foreign declarations</a>.</p>\n',
		summaryHtml: "Classes, methods, and instance declarations.",
	},
	{
		title: "Attributes and foreign declarations",
		description: "Use public attributes and foreign declarations.",
		group: "Abstractions",
		section: "Abstractions",
		order: 12,
		slug: "attributes-and-foreign",
		summary: "Public attributes, foreign bindings, and symbol metadata.",
		descriptionHtml: "Use public attributes and foreign declarations.",
		headings: [
			{
				depth: 2,
				id: "what",
				text: "What",
			},
			{
				depth: 2,
				id: "why",
				text: "Why",
			},
			{
				depth: 2,
				id: "how",
				text: "How",
			},
			{
				depth: 2,
				id: "when",
				text: "When",
			},
			{
				depth: 2,
				id: "analogy",
				text: "Analogy",
			},
			{
				depth: 2,
				id: "try-it",
				text: "Try it",
			},
		],
		html: '<h2 id="what"><a href="#what">What</a></h2><p>Attributes and foreign declarations are explicit annotations on declarations.</p>\n<h2 id="why"><a href="#why">Why</a></h2><p>They connect Musi declarations to external systems and symbol semantics without hiding behavior in implicit magic.</p>\n<h2 id="how"><a href="#how">How</a></h2><p>Use <code>@link</code>, <code>@when</code>, <code>@repr</code>, and <code>@layout</code> where external binding or metadata is required.</p>\n<pre class="shiki shiki-themes github-light github-dark" style="background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8" tabindex="0"><code><span class="line"><span style="color:#24292E;--shiki-dark:#E1E4E8">@</span><span style="color:#6F42C1;--shiki-dark:#B392F0">link</span><span style="color:#24292E;--shiki-dark:#E1E4E8">(name </span><span style="color:#D73A49;--shiki-dark:#F97583">:=</span><span style="color:#032F62;--shiki-dark:#9ECBFF"> "c"</span><span style="color:#24292E;--shiki-dark:#E1E4E8">) </span><span style="color:#D73A49;--shiki-dark:#F97583">foreign</span><span style="color:#032F62;--shiki-dark:#9ECBFF"> "c"</span><span style="color:#D73A49;--shiki-dark:#F97583"> let</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> puts (msg : CString) : Int;</span></span></code></pre><pre class="shiki shiki-themes github-light github-dark" style="background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8" tabindex="0"><code><span class="line"><span style="color:#D73A49;--shiki-dark:#F97583">foreign</span><span style="color:#032F62;--shiki-dark:#9ECBFF"> "c"</span><span style="color:#D73A49;--shiki-dark:#F97583"> let</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> puts (msg : CString) : Int;</span></span></code></pre><h2 id="when"><a href="#when">When</a></h2><p>Use foreign declarations when you integrate existing runtime boundaries.</p>\n<h2 id="analogy"><a href="#analogy">Analogy</a></h2><p>Like adding attributes in C# or annotations in Java, but in a compact declaration style.</p>\n<h2 id="try-it"><a href="#try-it">Try it</a></h2><p>Review attributes first, then the foreign example, then continue to <a href="/docs/quote-and-syntax">Quote and syntax values</a>.</p>\n',
		summaryHtml: "Public attributes, foreign bindings, and symbol metadata.",
	},
	{
		title: "Quote and syntax values",
		description: "Work with quoted syntax and embedded expressions.",
		group: "Abstractions",
		section: "Abstractions",
		order: 13,
		slug: "quote-and-syntax",
		summary: "Quoted expressions, quoted blocks, and splice forms.",
		descriptionHtml: "Work with quoted syntax and embedded expressions.",
		headings: [
			{
				depth: 2,
				id: "what",
				text: "What",
			},
			{
				depth: 2,
				id: "why",
				text: "Why",
			},
			{
				depth: 2,
				id: "how",
				text: "How",
			},
			{
				depth: 2,
				id: "when",
				text: "When",
			},
			{
				depth: 2,
				id: "analogy",
				text: "Analogy",
			},
			{
				depth: 2,
				id: "try-it",
				text: "Try it",
			},
		],
		html: '<h2 id="what"><a href="#what">What</a></h2><p>Quote syntax lets you treat code as data and then selectively run it inside templates or macros.</p>\n<h2 id="why"><a href="#why">Why</a></h2><p>This is useful for metaprogramming, AST transforms, and delayed execution patterns.</p>\n<pre class="shiki shiki-themes github-light github-dark" style="background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8" tabindex="0"><code><span class="line"><span style="color:#D73A49;--shiki-dark:#F97583">quote </span><span style="color:#24292E;--shiki-dark:#E1E4E8">(x </span><span style="color:#D73A49;--shiki-dark:#F97583">+</span><span style="color:#005CC5;--shiki-dark:#79B8FF"> 1</span><span style="color:#24292E;--shiki-dark:#E1E4E8">);</span></span></code></pre><h2 id="how"><a href="#how">How</a></h2><p>Use quote for expressions, then splice as needed with the quoted splice forms.</p>\n<pre class="shiki shiki-themes github-light github-dark" style="background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8" tabindex="0"><code><span class="line"><span style="color:#D73A49;--shiki-dark:#F97583">quote </span><span style="color:#24292E;--shiki-dark:#E1E4E8">{</span></span>\n<span class="line"><span style="color:#24292E;--shiki-dark:#E1E4E8">  x;</span></span>\n<span class="line"><span style="color:#24292E;--shiki-dark:#E1E4E8">};</span></span></code></pre><h2 id="when"><a href="#when">When</a></h2><p>Use it when code shape matters at runtime or when you want reusable template-style fragments.</p>\n<h2 id="analogy"><a href="#analogy">Analogy</a></h2><p>Think of it like template literals plus parser-level code as data in one step.</p>\n<h2 id="try-it"><a href="#try-it">Try it</a></h2><p>Try both quote snippets, then continue to <a href="/docs/foundation-and-standard-library">Foundation and standard library</a>.</p>\n',
		summaryHtml: "Quoted expressions, quoted blocks, and splice forms.",
	},
	{
		title: "Foundation and standard library",
		description:
			"Know when to use <code>@std</code> and when you are looking at lower-level foundation names.",
		group: "Tooling",
		section: "Tooling",
		order: 14,
		slug: "foundation-and-standard-library",
		summary:
			"The standard library family and the lower-level foundation namespace.",
		descriptionHtml:
			"Know when to use <code>@std</code> and when you are looking at lower-level foundation names.",
		headings: [
			{
				depth: 2,
				id: "what",
				text: "What",
			},
			{
				depth: 2,
				id: "why",
				text: "Why",
			},
			{
				depth: 2,
				id: "how",
				text: "How",
			},
			{
				depth: 2,
				id: "compare",
				text: "Compare",
			},
			{
				depth: 2,
				id: "when",
				text: "When",
			},
			{
				depth: 2,
				id: "analogy",
				text: "Analogy",
			},
			{
				depth: 2,
				id: "try-it",
				text: "Try it",
			},
		],
		html: '<h2 id="what"><a href="#what">What</a></h2><p>Most user code starts in <code>@std</code>.\n<code>musi:*</code> is the lower-level family when you need foundation-level capabilities.</p>\n<h2 id="why"><a href="#why">Why</a></h2><p>Keeping both namespaces explicit makes the dependency model visible:</p>\n<ul>\n<li><code>@std</code> for everyday work</li>\n<li><code>musi:*</code> for core-level operations</li>\n</ul>\n<h2 id="how"><a href="#how">How</a></h2><p>Import from <code>@std</code> modules first, then layer <code>musi:*</code> only where the project surface needs it.</p>\n<pre class="shiki shiki-themes github-light github-dark" style="background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8" tabindex="0"><code><span class="line"><span style="color:#D73A49;--shiki-dark:#F97583">let</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> configured </span><span style="color:#D73A49;--shiki-dark:#F97583">:=</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> Option.some[Int](</span><span style="color:#005CC5;--shiki-dark:#79B8FF">8080</span><span style="color:#24292E;--shiki-dark:#E1E4E8">);</span></span>\n<span class="line"><span style="color:#24292E;--shiki-dark:#E1E4E8">Option.unwrap_or[Int](configured, </span><span style="color:#005CC5;--shiki-dark:#79B8FF">3000</span><span style="color:#24292E;--shiki-dark:#E1E4E8">);</span></span></code></pre><pre class="shiki shiki-themes github-light github-dark" style="background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8" tabindex="0"><code><span class="line"><span style="color:#D73A49;--shiki-dark:#F97583">let</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> parsed </span><span style="color:#D73A49;--shiki-dark:#F97583">:=</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> Result.ok[Int, String](</span><span style="color:#005CC5;--shiki-dark:#79B8FF">8080</span><span style="color:#24292E;--shiki-dark:#E1E4E8">);</span></span>\n<span class="line"><span style="color:#24292E;--shiki-dark:#E1E4E8">Result.unwrap_or[Int, String](parsed, </span><span style="color:#005CC5;--shiki-dark:#79B8FF">3000</span><span style="color:#24292E;--shiki-dark:#E1E4E8">);</span></span></code></pre><h2 id="compare"><a href="#compare">Compare</a></h2><div class="code-tabs" data-code-tabs="1" data-example-id="import-stdlib" data-default="musi" data-active-language="musi">\n<div class="code-tabs-meta">\n<p class="code-tabs-caption">Import the standard library, then reach the family you need. Musi keeps stdlib access explicit through <code>@std</code>.</p>\n<p class="code-tabs-note">Musi package imports are values, so <code>@std</code> becomes a normal binding you can pass around.</p>\n</div>\n<div class="code-tablist" role="tablist" aria-label="Import a standard library module"><button type="button" role="tab" class="code-tab" data-language="java" aria-selected="false" tabindex="-1">Java</button><button type="button" role="tab" class="code-tab" data-language="musi" aria-selected="true" tabindex="0">Musi</button><button type="button" role="tab" class="code-tab" data-language="rust" aria-selected="false" tabindex="-1">Rust</button><button type="button" role="tab" class="code-tab" data-language="typescript" aria-selected="false" tabindex="-1">TypeScript</button></div>\n<section role="tabpanel" class="code-panel" data-language="java" data-active="false" hidden=""><pre class="shiki shiki-themes github-light github-dark" style="background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8" tabindex="0"><code><span class="line"><span style="color:#D73A49;--shiki-dark:#F97583">import</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> java.util.Optional;</span></span></code></pre></section>\n<section role="tabpanel" class="code-panel" data-language="musi" data-active="true"><pre class="shiki shiki-themes github-light github-dark" style="background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8" tabindex="0"><code><span class="line"><span style="color:#D73A49;--shiki-dark:#F97583">let</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> Std </span><span style="color:#D73A49;--shiki-dark:#F97583">:=</span><span style="color:#D73A49;--shiki-dark:#F97583"> import</span><span style="color:#032F62;--shiki-dark:#9ECBFF"> "@std"</span><span style="color:#24292E;--shiki-dark:#E1E4E8">;</span></span>\n<span class="line"><span style="color:#D73A49;--shiki-dark:#F97583">let</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> Option </span><span style="color:#D73A49;--shiki-dark:#F97583">:=</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> Std.Option;</span></span></code></pre></section>\n<section role="tabpanel" class="code-panel" data-language="rust" data-active="false" hidden=""><pre class="shiki shiki-themes github-light github-dark" style="background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8" tabindex="0"><code><span class="line"><span style="color:#D73A49;--shiki-dark:#F97583">use</span><span style="color:#6F42C1;--shiki-dark:#B392F0"> std</span><span style="color:#D73A49;--shiki-dark:#F97583">::</span><span style="color:#6F42C1;--shiki-dark:#B392F0">option</span><span style="color:#D73A49;--shiki-dark:#F97583">::</span><span style="color:#6F42C1;--shiki-dark:#B392F0">Option</span><span style="color:#24292E;--shiki-dark:#E1E4E8">;</span></span></code></pre></section>\n<section role="tabpanel" class="code-panel" data-language="typescript" data-active="false" hidden=""><pre class="shiki shiki-themes github-light github-dark" style="background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8" tabindex="0"><code><span class="line"><span style="color:#D73A49;--shiki-dark:#F97583">import</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> { readFileSync } </span><span style="color:#D73A49;--shiki-dark:#F97583">from</span><span style="color:#032F62;--shiki-dark:#9ECBFF"> "node:fs"</span><span style="color:#24292E;--shiki-dark:#E1E4E8">;</span></span></code></pre></section>\n</div><h2 id="when"><a href="#when">When</a></h2><p>Use <code>@std</code> for normal application logic.\nUse <code>musi:*</code> for boundary-focused tooling and low-level integration tasks.</p>\n<h2 id="analogy"><a href="#analogy">Analogy</a></h2><p>Like choosing a standard library versus runtime SDK in other ecosystems.</p>\n<h2 id="try-it"><a href="#try-it">Try it</a></h2><p>Move one project import to <code>@std</code>, then continue to <a href="/docs/testing-and-running">Testing and running</a>.</p>\n',
		summaryHtml:
			"The standard library family and the lower-level foundation namespace.",
	},
	{
		title: "Testing and running",
		description:
			"Run a package, run tests, and use the direct CLI when needed.",
		group: "Tooling",
		section: "Tooling",
		order: 15,
		slug: "testing-and-running",
		summary: "The main commands for package work and direct file work.",
		descriptionHtml:
			"Run a package, run tests, and use the direct CLI when needed.",
		headings: [
			{
				depth: 2,
				id: "what",
				text: "What",
			},
			{
				depth: 2,
				id: "why",
				text: "Why",
			},
			{
				depth: 2,
				id: "how",
				text: "How",
			},
			{
				depth: 2,
				id: "compare",
				text: "Compare",
			},
			{
				depth: 2,
				id: "when",
				text: "When",
			},
			{
				depth: 2,
				id: "analogy",
				text: "Analogy",
			},
			{
				depth: 2,
				id: "try-it",
				text: "Try it",
			},
		],
		html: '<h2 id="what"><a href="#what">What</a></h2><p>Testing and execution are split by scope:</p>\n<ul>\n<li>package scope with <code>musi</code></li>\n<li>direct source/artifact scope with <code>music</code></li>\n</ul>\n<h2 id="why"><a href="#why">Why</a></h2><p>This split keeps quick one-off checks and team workflow checks both fast.</p>\n<h2 id="how"><a href="#how">How</a></h2><p>Use package commands in normal development:</p>\n<pre class="shiki shiki-themes github-light github-dark" style="background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8" tabindex="0"><code><span class="line"><span style="color:#6F42C1;--shiki-dark:#B392F0">musi</span><span style="color:#032F62;--shiki-dark:#9ECBFF"> run</span></span>\n<span class="line"><span style="color:#6F42C1;--shiki-dark:#B392F0">musi</span><span style="color:#032F62;--shiki-dark:#9ECBFF"> check</span></span>\n<span class="line"><span style="color:#6F42C1;--shiki-dark:#B392F0">musi</span><span style="color:#032F62;--shiki-dark:#9ECBFF"> build</span></span>\n<span class="line"><span style="color:#6F42C1;--shiki-dark:#B392F0">musi</span><span style="color:#032F62;--shiki-dark:#9ECBFF"> test</span></span></code></pre><h2 id="compare"><a href="#compare">Compare</a></h2><div class="code-tabs" data-code-tabs="1" data-example-id="testing-entry" data-default="musi" data-active-language="musi">\n<div class="code-tabs-meta">\n<p class="code-tabs-caption">A small test entry should read like ordinary code. Musi uses <code>export let test ()</code> inside <code>*.test.ms</code> files.</p>\n<p class="code-tabs-note">Musi package tests are discovered by file name, then run through <code>musi test</code>.</p>\n</div>\n<div class="code-tablist" role="tablist" aria-label="Single package test entry"><button type="button" role="tab" class="code-tab" data-language="java" aria-selected="false" tabindex="-1">Java</button><button type="button" role="tab" class="code-tab" data-language="musi" aria-selected="true" tabindex="0">Musi</button><button type="button" role="tab" class="code-tab" data-language="rust" aria-selected="false" tabindex="-1">Rust</button><button type="button" role="tab" class="code-tab" data-language="typescript" aria-selected="false" tabindex="-1">TypeScript</button></div>\n<section role="tabpanel" class="code-panel" data-language="java" data-active="false" hidden=""><pre class="shiki shiki-themes github-light github-dark" style="background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8" tabindex="0"><code><span class="line"><span style="color:#24292E;--shiki-dark:#E1E4E8">@</span><span style="color:#D73A49;--shiki-dark:#F97583">Test</span></span>\n<span class="line"><span style="color:#D73A49;--shiki-dark:#F97583">void</span><span style="color:#6F42C1;--shiki-dark:#B392F0"> sums_values</span><span style="color:#24292E;--shiki-dark:#E1E4E8">() {</span></span>\n<span class="line"><span style="color:#6F42C1;--shiki-dark:#B392F0">    assertEquals</span><span style="color:#24292E;--shiki-dark:#E1E4E8">(</span><span style="color:#005CC5;--shiki-dark:#79B8FF">3</span><span style="color:#24292E;--shiki-dark:#E1E4E8">, </span><span style="color:#005CC5;--shiki-dark:#79B8FF">1</span><span style="color:#D73A49;--shiki-dark:#F97583"> +</span><span style="color:#005CC5;--shiki-dark:#79B8FF"> 2</span><span style="color:#24292E;--shiki-dark:#E1E4E8">);</span></span>\n<span class="line"><span style="color:#24292E;--shiki-dark:#E1E4E8">}</span></span></code></pre></section>\n<section role="tabpanel" class="code-panel" data-language="musi" data-active="true"><pre class="shiki shiki-themes github-light github-dark" style="background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8" tabindex="0"><code><span class="line"><span style="color:#D73A49;--shiki-dark:#F97583">let</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> Testing </span><span style="color:#D73A49;--shiki-dark:#F97583">:=</span><span style="color:#D73A49;--shiki-dark:#F97583"> import</span><span style="color:#032F62;--shiki-dark:#9ECBFF"> "@std/testing"</span><span style="color:#24292E;--shiki-dark:#E1E4E8">;</span></span>\n<span class="line"></span>\n<span class="line"><span style="color:#D73A49;--shiki-dark:#F97583">export</span><span style="color:#D73A49;--shiki-dark:#F97583"> let</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> test () </span><span style="color:#D73A49;--shiki-dark:#F97583">:=</span></span>\n<span class="line"><span style="color:#24292E;--shiki-dark:#E1E4E8">  Testing.it(</span><span style="color:#032F62;--shiki-dark:#9ECBFF">"adds values"</span><span style="color:#24292E;--shiki-dark:#E1E4E8">, Testing.to_be(</span><span style="color:#005CC5;--shiki-dark:#79B8FF">1</span><span style="color:#D73A49;--shiki-dark:#F97583"> +</span><span style="color:#005CC5;--shiki-dark:#79B8FF"> 2</span><span style="color:#24292E;--shiki-dark:#E1E4E8">, </span><span style="color:#005CC5;--shiki-dark:#79B8FF">3</span><span style="color:#24292E;--shiki-dark:#E1E4E8">));</span></span></code></pre></section>\n<section role="tabpanel" class="code-panel" data-language="rust" data-active="false" hidden=""><pre class="shiki shiki-themes github-light github-dark" style="background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8" tabindex="0"><code><span class="line"><span style="color:#24292E;--shiki-dark:#E1E4E8">#[test]</span></span>\n<span class="line"><span style="color:#D73A49;--shiki-dark:#F97583">fn</span><span style="color:#6F42C1;--shiki-dark:#B392F0"> sums_values</span><span style="color:#24292E;--shiki-dark:#E1E4E8">() {</span></span>\n<span class="line"><span style="color:#6F42C1;--shiki-dark:#B392F0">    assert_eq!</span><span style="color:#24292E;--shiki-dark:#E1E4E8">(</span><span style="color:#005CC5;--shiki-dark:#79B8FF">1</span><span style="color:#D73A49;--shiki-dark:#F97583"> +</span><span style="color:#005CC5;--shiki-dark:#79B8FF"> 2</span><span style="color:#24292E;--shiki-dark:#E1E4E8">, </span><span style="color:#005CC5;--shiki-dark:#79B8FF">3</span><span style="color:#24292E;--shiki-dark:#E1E4E8">);</span></span>\n<span class="line"><span style="color:#24292E;--shiki-dark:#E1E4E8">}</span></span></code></pre></section>\n<section role="tabpanel" class="code-panel" data-language="typescript" data-active="false" hidden=""><pre class="shiki shiki-themes github-light github-dark" style="background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8" tabindex="0"><code><span class="line"><span style="color:#6F42C1;--shiki-dark:#B392F0">test</span><span style="color:#24292E;--shiki-dark:#E1E4E8">(</span><span style="color:#032F62;--shiki-dark:#9ECBFF">"adds values"</span><span style="color:#24292E;--shiki-dark:#E1E4E8">, () </span><span style="color:#D73A49;--shiki-dark:#F97583">=></span><span style="color:#24292E;--shiki-dark:#E1E4E8"> {</span></span>\n<span class="line"><span style="color:#6F42C1;--shiki-dark:#B392F0">  expect</span><span style="color:#24292E;--shiki-dark:#E1E4E8">(</span><span style="color:#005CC5;--shiki-dark:#79B8FF">1</span><span style="color:#D73A49;--shiki-dark:#F97583"> +</span><span style="color:#005CC5;--shiki-dark:#79B8FF"> 2</span><span style="color:#24292E;--shiki-dark:#E1E4E8">).</span><span style="color:#6F42C1;--shiki-dark:#B392F0">toBe</span><span style="color:#24292E;--shiki-dark:#E1E4E8">(</span><span style="color:#005CC5;--shiki-dark:#79B8FF">3</span><span style="color:#24292E;--shiki-dark:#E1E4E8">);</span></span>\n<span class="line"><span style="color:#24292E;--shiki-dark:#E1E4E8">});</span></span></code></pre></section>\n</div><h2 id="when"><a href="#when">When</a></h2><p>Use tests for routine verification.</p>\n<ul>\n<li>keep tests in <code>*.test.ms</code></li>\n<li>expose each test with exported <code>test</code></li>\n</ul>\n<pre class="shiki shiki-themes github-light github-dark" style="background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8" tabindex="0"><code><span class="line"><span style="color:#D73A49;--shiki-dark:#F97583">let</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> Testing </span><span style="color:#D73A49;--shiki-dark:#F97583">:=</span><span style="color:#D73A49;--shiki-dark:#F97583"> import</span><span style="color:#032F62;--shiki-dark:#9ECBFF"> "@std/testing"</span><span style="color:#24292E;--shiki-dark:#E1E4E8">;</span></span>\n<span class="line"></span>\n<span class="line"><span style="color:#D73A49;--shiki-dark:#F97583">export</span><span style="color:#D73A49;--shiki-dark:#F97583"> let</span><span style="color:#24292E;--shiki-dark:#E1E4E8"> test () </span><span style="color:#D73A49;--shiki-dark:#F97583">:=</span></span>\n<span class="line"><span style="color:#24292E;--shiki-dark:#E1E4E8">  Testing.it(</span><span style="color:#032F62;--shiki-dark:#9ECBFF">"adds values"</span><span style="color:#24292E;--shiki-dark:#E1E4E8">, Testing.to_be(</span><span style="color:#005CC5;--shiki-dark:#79B8FF">1</span><span style="color:#D73A49;--shiki-dark:#F97583"> +</span><span style="color:#005CC5;--shiki-dark:#79B8FF"> 2</span><span style="color:#24292E;--shiki-dark:#E1E4E8">, </span><span style="color:#005CC5;--shiki-dark:#79B8FF">3</span><span style="color:#24292E;--shiki-dark:#E1E4E8">));</span></span></code></pre><pre class="shiki shiki-themes github-light github-dark" style="background-color:#fff;--shiki-dark-bg:#24292e;color:#24292e;--shiki-dark:#e1e4e8" tabindex="0"><code><span class="line"><span style="color:#6F42C1;--shiki-dark:#B392F0">music</span><span style="color:#032F62;--shiki-dark:#9ECBFF"> check</span><span style="color:#032F62;--shiki-dark:#9ECBFF"> index.ms</span></span>\n<span class="line"><span style="color:#6F42C1;--shiki-dark:#B392F0">music</span><span style="color:#032F62;--shiki-dark:#9ECBFF"> build</span><span style="color:#032F62;--shiki-dark:#9ECBFF"> index.ms</span></span>\n<span class="line"><span style="color:#6F42C1;--shiki-dark:#B392F0">music</span><span style="color:#032F62;--shiki-dark:#9ECBFF"> run</span><span style="color:#032F62;--shiki-dark:#9ECBFF"> index.seam</span></span></code></pre><h2 id="analogy"><a href="#analogy">Analogy</a></h2><p>Like project commands in a framework plus one-off script execution when needed.</p>\n<h2 id="try-it"><a href="#try-it">Try it</a></h2><p>Run one package command and one direct command, then revisit any chapter where behavior is unclear.</p>\n<p>See <a href="/reference">Reference</a> for source, grammar, extension, and issue links.</p>\n',
		summaryHtml: "The main commands for package work and direct file work.",
	},
] satisfies GeneratedDoc[];
