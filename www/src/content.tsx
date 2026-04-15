import { renderedSnippets } from "./generated-content";

export interface InstallPrerequisite {
	title: string;
	value: string;
	copy: string;
}

export interface CommandRow {
	command: string;
	description: string;
	audience: string;
}

export interface InstallCommandTab {
	id: string;
	label: string;
	html: string;
}

export interface InstallCommandGroup {
	id: string;
	title: string;
	copy: string;
	html?: string;
	tabs?: InstallCommandTab[];
}

const installPrerequisitesData: InstallPrerequisite[] = [
	{
		title: "Rust",
		value: "Rust 1.87 or newer",
		copy: "Builds both package and direct-mode tooling.",
	},
	{
		title: "libffi",
		value: "System development package",
		copy: "Required for current foreign-function flow.",
	},
	{
		title: "Git",
		value: "Any recent Git install",
		copy: "Retrieves and updates project source.",
	},
];

const commandRowsData: CommandRow[] = [
	{
		command: "musi check",
		audience: "Package",
		description: "Check the current package.",
	},
	{
		command: "musi build",
		audience: "Package",
		description: "Build the resolved package entry.",
	},
	{
		command: "musi run",
		audience: "Package",
		description: "Run the package entry.",
	},
	{
		command: "musi test",
		audience: "Package",
		description: "Discover and run *.test.ms files.",
	},
	{
		command: "music check index.ms",
		audience: "Direct",
		description: "Check one direct source graph.",
	},
	{
		command: "music build index.ms",
		audience: "Direct",
		description: "Emit one direct .seam artifact.",
	},
	{
		command: "music run index.seam",
		audience: "Direct",
		description: "Run compiled bytecode directly.",
	},
];

const installCommandGroupsData: InstallCommandGroup[] = [
	{
		id: "script",
		title: "Install script",
		copy: "Pick one bootstrap command. The scripts download the repository archive, then install both binaries with cargo install.",
		tabs: [
			{
				id: "curl",
				label: "macOS / Linux",
				html: renderedSnippets.installCurlHtml,
			},
			{
				id: "powershell",
				label: "Windows",
				html: renderedSnippets.installPowershellHtml,
			},
		],
	},
	{
		id: "cargo",
		title: "Cargo install",
		copy: "Prefer this when you already cloned the repository and want an explicit local install.",
		html: renderedSnippets.installCargoHtml,
	},
];

export function installPrerequisites() {
	return installPrerequisitesData;
}

export function commandRows() {
	return commandRowsData;
}

export function installCommandGroups() {
	return installCommandGroupsData;
}

export function installCommandGroup(id: InstallCommandGroup["id"]) {
	return installCommandGroupsData.find((group) => group.id === id);
}

export const siteOrigin = "https://musi-lang.org";
export const homeSampleHtml = renderedSnippets.homeSampleHtml;
export const installCurlHtml = renderedSnippets.installCurlHtml;
export const installPowershellHtml = renderedSnippets.installPowershellHtml;
export const installCargoHtml = renderedSnippets.installCargoHtml;
export const quickstartHtml = renderedSnippets.quickstartHtml;
