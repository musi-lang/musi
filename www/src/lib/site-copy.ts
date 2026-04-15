export type Locale = "en";

export interface SiteCopy {
	skipToContent: string;
	menu: string;
	nav: {
		learn: string;
		install: string;
		playground: string;
		community: string;
	};
	utilityLabels: {
		github: string;
		themeLight: string;
		themeDark: string;
		themeSystem: string;
	};
	ui: {
		sample: string;
		open: string;
		openSection: string;
		primaryPaths: string;
		docsEntryPoints: string;
		questions: string;
		openFirstChapter: string;
		openQuestions: string;
		learnSection: string;
		chapters: string;
		onThisPage: string;
		chapterNavigation: string;
		previousChapter: string;
		nextChapter: string;
		communityLinks: string;
		installCommands: string;
		commandReference: string;
		lane: string;
		command: string;
		description: string;
	};
	home: {
		eyebrow: string;
		title: string;
		description: string;
		sectionsTitle: string;
		summary: string[];
		primaryCta: string;
		secondaryCta: string;
		tertiaryCta: string;
		paths: Array<{
			label: string;
			title: string;
			copy: string;
			href: string;
			actionLabel: string;
		}>;
		sections: Array<{ title: string; copy: string }>;
	};
	install: {
		eyebrow: string;
		title: string;
		description: string;
		prerequisitesLabel: string;
		installScriptsLabel: string;
		cargoInstallLabel: string;
		quickStartLabel: string;
		commandMapLabel: string;
		commandMapTitle: string;
		commandMapCopy: string;
	};
	playground: {
		eyebrow: string;
		title: string;
		copy: string;
		statusTitle: string;
		statusCopy: string;
	};
	community: {
		eyebrow: string;
		title: string;
		description: string;
		sections: Array<{
			title: string;
			copy: string;
			href: string;
			label: string;
		}>;
	};
	learn: {
		eyebrow: string;
		title: string;
		description: string;
		startTitle: string;
		questionsTitle: string;
		partsTitle: string;
	};
}

export const siteCopy = {
	skipToContent: "Skip to main content",
	menu: "Menu",
	nav: {
		learn: "Learn",
		install: "Install",
		playground: "Playground",
		community: "Community",
	},
	utilityLabels: {
		github: "GitHub repository",
		themeLight: "Switch from light theme to dark theme",
		themeDark: "Switch from dark theme to system theme",
		themeSystem: "Switch from system theme to light theme",
	},
	ui: {
		sample: "Sample",
		open: "Open",
		openSection: "Open section",
		primaryPaths: "Primary paths",
		docsEntryPoints: "Docs Entry Points",
		questions: "Questions",
		openFirstChapter: "Open First Chapter",
		openQuestions: "Open questions",
		learnSection: "Learn",
		chapters: "Chapters",
		onThisPage: "On this page",
		chapterNavigation: "Chapter Navigation",
		previousChapter: "Previous Chapter",
		nextChapter: "Next Chapter",
		communityLinks: "Community Links",
		installCommands: "Install Commands",
		commandReference: "Musi command reference",
		lane: "Lane",
		command: "Command",
		description: "Description",
	},
	home: {
		eyebrow: "Home",
		title: "The Musi Programming Language",
		description:
			"Musi is an expression-first programming language with pattern matching, effects, and a small command surface. Learn the language, install it from source, check playground status, and find community links from one site.",
		sectionsTitle: "Why Musi",
		summary: [
			"Expression-first flow with clear data movement.",
			"Source-first setup with package and direct command lanes.",
			"Learning, setup, playground status, and community each have their own path.",
		],
		primaryCta: "Start Learning",
		secondaryCta: "Install Musi",
		tertiaryCta: "See Community",
		paths: [
			{
				label: "Learn",
				title: "Read Musi Book",
				copy: "Start with setup, then move through syntax, types, effects, and tooling.",
				href: "/learn/book",
				actionLabel: "Read Musi Book",
			},
			{
				label: "Install",
				title: "Build from source",
				copy: "Review prerequisites, build steps, PATH setup, and command lanes.",
				href: "/install",
				actionLabel: "Open install guide",
			},
			{
				label: "Community",
				title: "Join the project",
				copy: "Find contribution notes, issue tracking, and discussion links.",
				href: "/community",
				actionLabel: "Open community links",
			},
		],
		sections: [
			{
				title: "Expression-first",
				copy: "Read files top to bottom with let, match, and ordinary expressions instead of hidden control machinery.",
			},
			{
				title: "Effects stay readable",
				copy: "Use effects and handlers without pushing ordinary code into framework ceremony.",
			},
			{
				title: "Two command lanes",
				copy: "Use musi for package work and music for direct source or artifact work.",
			},
			{
				title: "Clear public structure",
				copy: "Learn, Install, Playground, and Community each own one public job.",
			},
		],
	},
	install: {
		eyebrow: "Install",
		title: "Install Musi with Script or Cargo",
		description:
			"Install Rust and libffi first, then choose a script bootstrap or a local cargo install. Both paths install music and musi into Cargo's bin directory.",
		prerequisitesLabel: "Prerequisites",
		installScriptsLabel: "Install Script",
		cargoInstallLabel: "Cargo Install",
		quickStartLabel: "Quick Start",
		commandMapLabel: "Command Map",
		commandMapTitle: "Current Commands",
		commandMapCopy:
			"Use package commands most of the time. Use direct commands when you want one file or one artifact.",
	},
	playground: {
		eyebrow: "Playground",
		title: "Playground Status",
		copy: "Browser execution support is planned but not public yet.",
		statusTitle: "WASM Playground Is Not Public Yet",
		statusCopy:
			"This page shows browser tooling status and points back to install and community work.",
	},
	community: {
		eyebrow: "Community",
		title: "Join the Musi Project",
		description:
			"Community keeps contribution, discussion, issue tracking, and repository links in one place.",
		sections: [
			{
				title: "Repository",
				copy: "Browse source, tags, issues, and release history.",
				href: "https://github.com/musi-lang/musi",
				label: "Open GitHub",
			},
			{
				title: "Contributing",
				copy: "Read contribution expectations, workflow notes, and project standards.",
				href: "https://github.com/musi-lang/musi/blob/main/CONTRIBUTING.md",
				label: "Read contributing guide",
			},
			{
				title: "Issue Tracking",
				copy: "See active bugs, language work, and open tasks.",
				href: "https://github.com/musi-lang/musi/issues",
				label: "Open issues",
			},
		],
	},
	learn: {
		eyebrow: "Learn",
		title: "Musi Book",
		description:
			"Read Musi in chapter order through small beginner-first language chapters. Learning pages stay close to install steps and current commands.",
		startTitle: "Start at the Beginning",
		questionsTitle: "Follow the Chapter Sequence",
		partsTitle: "Parts and Chapters",
	},
} satisfies SiteCopy;
