export type Locale = "en" | "ja";

export interface LocalizedSiteCopy {
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
		locale: string;
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

export const siteCopy: Record<Locale, LocalizedSiteCopy> = {
	en: {
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
			locale: "Switch language",
		},
		ui: {
			sample: "Sample",
			open: "Open",
			openSection: "Open section",
			primaryPaths: "Primary paths",
			docsEntryPoints: "Docs entry points",
			questions: "Questions",
			openFirstChapter: "Open first chapter",
			openQuestions: "Open questions",
			learnSection: "Learn",
			chapters: "Chapters",
			onThisPage: "On this page",
			chapterNavigation: "Chapter navigation",
			previousChapter: "Previous chapter",
			nextChapter: "Next chapter",
			communityLinks: "Community links",
			installCommands: "Install commands",
			commandReference: "Musi command reference",
			lane: "Lane",
			command: "Command",
			description: "Description",
		},
		home: {
			eyebrow: "Home",
			title: "What is Musi?",
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
					title: "Read the Musi book",
					copy: "Start with setup, then move through syntax, types, effects, and tooling.",
					href: "/learn",
					actionLabel: "Read Learn docs",
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
			title: "Install Musi with script or Cargo",
			description:
				"Install Rust and libffi first, then choose a script bootstrap or a local cargo install. Both paths install music and musi into Cargo's bin directory.",
			prerequisitesLabel: "Prerequisites",
			installScriptsLabel: "Install script",
			cargoInstallLabel: "Cargo install",
			quickStartLabel: "Quick start",
			commandMapLabel: "Command map",
			commandMapTitle: "Current commands",
			commandMapCopy:
				"Use package commands most of the time. Use direct commands when you want one file or one artifact.",
		},
		playground: {
			eyebrow: "Playground",
			title: "Playground status",
			copy: "Browser execution support is planned but not public yet.",
			statusTitle: "WASM playground is not public yet",
			statusCopy:
				"This page shows browser tooling status and points back to install and community work.",
		},
		community: {
			eyebrow: "Community",
			title: "Join the Musi project",
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
					title: "Issue tracking",
					copy: "See active bugs, language work, and open tasks.",
					href: "https://github.com/musi-lang/musi/issues",
					label: "Open issues",
				},
			],
		},
		learn: {
			eyebrow: "Learn",
			title: "Musi book",
			description:
				"Read Musi in chapter order through small beginner-first language chapters. Learning pages stay close to install steps and current commands.",
			startTitle: "Start at the beginning and keep moving",
			questionsTitle: "Follow the chapter sequence",
			partsTitle: "Parts and chapters",
		},
	},
	ja: {
		skipToContent: "メインコンテンツへ移動",
		menu: "メニュー",
		nav: {
			learn: "学ぶ",
			install: "導入",
			playground: "プレイグラウンド",
			community: "コミュニティ",
		},
		utilityLabels: {
			github: "GitHub リポジトリ",
			themeLight: "ライトテーマからダークテーマへ切り替え",
			themeDark: "ダークテーマからシステム設定へ切り替え",
			themeSystem: "システム設定からライトテーマへ切り替え",
			locale: "言語を切り替え",
		},
		ui: {
			sample: "サンプル",
			open: "開く",
			openSection: "章一覧を開く",
			primaryPaths: "主要な導線",
			docsEntryPoints: "学習の入口",
			questions: "よくある質問",
			openFirstChapter: "最初の章を開く",
			openQuestions: "質問一覧を開く",
			learnSection: "学ぶ",
			chapters: "章一覧",
			onThisPage: "このページ",
			chapterNavigation: "章の移動",
			previousChapter: "前の章",
			nextChapter: "次の章",
			communityLinks: "コミュニティリンク",
			installCommands: "導入コマンド",
			commandReference: "Musi コマンド一覧",
			lane: "区分",
			command: "コマンド",
			description: "説明",
		},
		home: {
			eyebrow: "学ぶ",
			title: "Musi を学ぶ",
			description:
				"Musi は式を中心に組み立てるプログラミング言語です。パターンマッチ、エフェクト、最小限のコマンド群を備えています。学習、導入、プレイグラウンドの状況、コミュニティへの入口を 1 つのサイトでたどれます。",
			sectionsTitle: "Musi の特徴",
			summary: [
				"式中心の流れでデータ移動が明確です。",
				"ソース起点のセットアップで、パッケージコマンドと直接実行コマンドを使い分けます。",
				"学習、導入、プレイグラウンド、コミュニティを別々の導線で案内します。",
			],
			primaryCta: "学習を始める",
			secondaryCta: "Musi を導入する",
			tertiaryCta: "コミュニティを見る",
			paths: [
				{
					label: "学ぶ",
					title: "Musi ブックを読む",
					copy: "セットアップから始め、構文、型、エフェクト、ツールへ進みます。",
					href: "/learn",
					actionLabel: "学習ページを開く",
				},
				{
					label: "導入",
					title: "ソースからビルドする",
					copy: "前提条件、ビルド手順、PATH 設定、コマンドの使い分けを確認します。",
					href: "/ja/install",
					actionLabel: "導入ガイドを開く",
				},
				{
					label: "コミュニティ",
					title: "プロジェクトに参加する",
					copy: "貢献、Issue 管理、議論 の入口をまとめます。",
					href: "/ja/community",
					actionLabel: "コミュニティリンクを開く",
				},
			],
			sections: [
				{
					title: "式中心の設計",
					copy: "let、match、通常の式で上から下へ読み進められます。",
				},
				{
					title: "エフェクトも読みやすい",
					copy: "エフェクトとハンドラを使っても、通常のコードが余計な定型に埋もれません。",
				},
				{
					title: "2 つのコマンド",
					copy: "パッケージ作業は musi、ソースや成果物を直接扱う作業は music を使います。",
				},
				{
					title: "役割ごとに分かれた公開サイト",
					copy: "学ぶ、導入、プレイグラウンド、コミュニティがそれぞれ 1 つの役割を持ちます。",
				},
			],
		},
		install: {
			eyebrow: "導入",
			title: "script か Cargo で Musi を導入する",
			description:
				"最初に Rust と libffi を導入し、その後 script bootstrap か local cargo install を選びます。どちらも music と musi を Cargo の bin directory に導入します。",
			prerequisitesLabel: "前提条件",
			installScriptsLabel: "導入スクリプト",
			cargoInstallLabel: "Cargo install",
			quickStartLabel: "クイックスタート",
			commandMapLabel: "コマンド一覧",
			commandMapTitle: "現在のコマンド",
			commandMapCopy:
				"通常はパッケージコマンドを使い、1 ファイルや 1 つの成果物を扱うときは直接実行コマンドを使います。",
		},
		playground: {
			eyebrow: "プレイグラウンド",
			title: "プレイグラウンドの状況",
			copy: "ブラウザ実行の対応は計画中ですが、まだ公開されていません。",
			statusTitle: "ブラウザ実行用プレイグラウンドはまだ公開していません",
			statusCopy:
				"このページではブラウザ実行機能の公開状況を案内し、導入とコミュニティへの入口を保ちます。",
		},
		community: {
			eyebrow: "コミュニティ",
			title: "Musi プロジェクトに参加する",
			description:
				"コミュニティでは、貢献方法、議論の場、Issue 一覧、リポジトリへの入口を 1 か所にまとめます。",
			sections: [
				{
					title: "リポジトリ",
					copy: "ソース、タグ、Issue、リリース履歴を確認します。",
					href: "https://github.com/musi-lang/musi",
					label: "GitHub を開く",
				},
				{
					title: "貢献ガイド",
					copy: "貢献時の前提、作業手順、プロジェクト標準を確認します。",
					href: "https://github.com/musi-lang/musi/blob/main/CONTRIBUTING.md",
					label: "貢献ガイドを読む",
				},
				{
					title: "Issue 一覧",
					copy: "進行中の不具合、言語機能の作業、未解決のタスクを確認します。",
					href: "https://github.com/musi-lang/musi/issues",
					label: "Issue を開く",
				},
			],
		},
		learn: {
			eyebrow: "学ぶ",
			title: "Musi ブック",
			description:
				"短い章を順に読み進める英語中心の言語ブックです。学習内容は導入手順と現在のコマンドの近くに置いています。",
			startTitle: "最初から順に進む",
			questionsTitle: "章順で読む",
			partsTitle: "パートと章",
		},
	},
};

export function isLocale(value: string): value is Locale {
	return value === "en" || value === "ja";
}

export function localePrefix(locale: Locale) {
	return locale === "ja" ? "/ja" : "";
}
