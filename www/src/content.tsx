import { renderedSnippets } from "./generated-content";
import type { Locale } from "./lib/site-copy";

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

const installPrerequisitesByLocale: Record<Locale, InstallPrerequisite[]> = {
	en: [
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
	],
	ja: [
		{
			title: "Rust",
			value: "Rust 1.87 以上",
			copy: "package と direct mode の両方を build します。",
		},
		{
			title: "libffi",
			value: "システム開発パッケージ",
			copy: "現在の foreign function フローで必要です。",
		},
		{
			title: "Git",
			value: "最近の Git",
			copy: "project source の取得と更新に使います。",
		},
	],
};

const commandRowsByLocale: Record<Locale, CommandRow[]> = {
	en: [
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
	],
	ja: [
		{
			command: "musi check",
			audience: "Package",
			description: "現在の package を検査します。",
		},
		{
			command: "musi build",
			audience: "Package",
			description: "解決済み package entry を build します。",
		},
		{
			command: "musi run",
			audience: "Package",
			description: "package entry を実行します。",
		},
		{
			command: "musi test",
			audience: "Package",
			description: "*.test.ms file を見つけて実行します。",
		},
		{
			command: "music check index.ms",
			audience: "Direct",
			description: "1 つの direct source graph を検査します。",
		},
		{
			command: "music build index.ms",
			audience: "Direct",
			description: "1 つの direct .seam artifact を出力します。",
		},
		{
			command: "music run index.seam",
			audience: "Direct",
			description: "build 済み bytecode を直接実行します。",
		},
	],
};

export function installPrerequisites(locale: Locale) {
	return installPrerequisitesByLocale[locale];
}

export function commandRows(locale: Locale) {
	return commandRowsByLocale[locale];
}

export const siteOrigin = "https://musi-lang.org";
export const homeSampleHtml = renderedSnippets.homeSampleHtml;
export const installSourceHtml = renderedSnippets.installSourceHtml;
export const quickstartHtml = renderedSnippets.quickstartHtml;
