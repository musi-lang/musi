import type { ReactNode } from "react";
import { renderedSnippets } from "./generated-content";

interface CopyBlock {
	title: string;
	copy: ReactNode;
}

interface StartLink extends CopyBlock {
	href: string;
}

interface InstallPrerequisite extends CopyBlock {
	value: ReactNode;
}

interface CommandRow {
	command: string;
	description: ReactNode;
}

interface ReferenceLink {
	label: string;
	href: string;
	copy: ReactNode;
}

interface ReferenceGroup {
	title: string;
	links: ReferenceLink[];
}

function inlineCode(text: string) {
	return <code className="inline-code">{text}</code>;
}

export const siteOrigin = "https://musi-lang.com";

export const homeDescriptor = (
	<>Language-first learning and practical command flow in one place.</>
);

export const homeSections: CopyBlock[] = [
	{
		title: "Effects",
		copy: (
			<>
				Use {inlineCode("effect")}, {inlineCode("perform")},{" "}
				{inlineCode("handle")}, and {inlineCode("resume")} directly in code.
			</>
		),
	},
	{
		title: "Packages",
		copy: (
			<>
				Use {inlineCode("musi")} for package workflows and {inlineCode("music")}{" "}
				for direct source or artifact execution.
			</>
		),
	},
	{
		title: "Surface",
		copy: <>Docs focus on what, why, how, and when for each pattern.</>,
	},
];

export const startHereLinks: StartLink[] = [
	{
		title: "Read the docs",
		href: "/docs",
		copy: <>Start with Start, continue through Core language, then Tooling.</>,
	},
	{
		title: "Install Musi",
		href: "/install",
		copy: <>Build, add to PATH, run your first check.</>,
	},
	{
		title: "Reference",
		href: "/reference",
		copy: <>Repository, grammar, extension, and issue links.</>,
	},
];

export const installPrerequisites: InstallPrerequisite[] = [
	{
		title: "Rust",
		value: <>Rust 1.87 or newer</>,
		copy: <>Builds both package and direct-mode tooling.</>,
	},
	{
		title: "libffi",
		value: <>System libffi development package</>,
		copy: <>Required for current foreign-function flow.</>,
	},
	{
		title: "Git",
		value: <>Any recent Git install</>,
		copy: <>Retrieves and updates the project source.</>,
	},
];

export const commandRows: CommandRow[] = [
	{
		command: "musi check",
		description: <>Check the current package.</>,
	},
	{
		command: "musi build",
		description: <>Build the resolved package entry.</>,
	},
	{
		command: "musi run",
		description: <>Run the package entry.</>,
	},
	{
		command: "musi test",
		description: <>Discover and run {inlineCode("*.test.ms")} files.</>,
	},
	{
		command: "music check index.ms",
		description: <>Check one direct source graph.</>,
	},
	{
		command: "music build index.ms",
		description: <>Emit one direct {inlineCode(".seam")} artifact.</>,
	},
	{
		command: "music run index.seam",
		description: <>Run compiled bytecode directly.</>,
	},
];

export const referenceGroups: ReferenceGroup[] = [
	{
		title: "Project",
		links: [
			{
				label: "GitHub repository",
				href: "https://github.com/musi-lang/musi",
				copy: <>Source, tags, and issues.</>,
			},
			{
				label: "README",
				href: "https://github.com/musi-lang/musi/blob/main/README.md",
				copy: <>Install path and command quick start.</>,
			},
			{
				label: "VS Code extension",
				href: "https://github.com/musi-lang/musi/tree/main/vscode-ext",
				copy: <>Syntax support and editor integration.</>,
			},
		],
	},
	{
		title: "Language",
		links: [
			{
				label: "Syntax",
				href: "https://github.com/musi-lang/musi/blob/main/docs/what/language/syntax.md",
				copy: <>Language shape overview.</>,
			},
			{
				label: "Grammar",
				href: "https://github.com/musi-lang/musi/blob/main/grammar/Musi.g4",
				copy: <>Canonical grammar source.</>,
			},
			{
				label: "Effect system",
				href: "https://github.com/musi-lang/musi/blob/main/docs/what/language/effect-system.md",
				copy: (
					<>
						{inlineCode("effect")}, {inlineCode("perform")},{" "}
						{inlineCode("handle")}, {inlineCode("resume")}.
					</>
				),
			},
		],
	},
	{
		title: "Tracking",
		links: [
			{
				label: "Issues",
				href: "https://github.com/musi-lang/musi/issues",
				copy: <>Active bugs and open work.</>,
			},
			{
				label: "Releases",
				href: "https://github.com/musi-lang/musi/releases",
				copy: <>Tags and release history.</>,
			},
			{
				label: "SEAM bytecode",
				href: "https://github.com/musi-lang/musi/blob/main/docs/what/runtime/seam-bytecode.md",
				copy: <>Runtime bytecode notes.</>,
			},
		],
	},
];

export const homeSampleHtml = renderedSnippets.homeSampleHtml;
export const installSourceHtml = renderedSnippets.installSourceHtml;
export const quickstartHtml = renderedSnippets.quickstartHtml;
