import { createTheme } from "@mantine/core";

export const theme = createTheme({
	primaryColor: "ocean",
	primaryShade: { light: 6, dark: 5 },
	defaultRadius: "xs",
	fontFamily:
		'"BIZ UDPGothic", "Hiragino Kaku Gothic ProN", "Yu Gothic UI", "Yu Gothic", "Meiryo", "Noto Sans JP", -apple-system, BlinkMacSystemFont, sans-serif',
	fontFamilyMonospace:
		'"IBM Plex Mono", "SFMono-Regular", "SF Mono", Menlo, Consolas, monospace',
	headings: {
		fontWeight: "700",
		fontFamily:
			'"BIZ UDPGothic", "Hiragino Kaku Gothic ProN", "Yu Gothic UI", "Yu Gothic", sans-serif',
	},
	colors: {
		ocean: [
			"#e4f2fc",
			"#c8e2f8",
			"#9dc9f1",
			"#6aafe8",
			"#3f95df",
			"#207dca",
			"#0072b2",
			"#005b8d",
			"#00456a",
			"#002f48",
		],
		teal: [
			"#def6f0",
			"#bcecdf",
			"#87ddc5",
			"#4dcda9",
			"#1dbb92",
			"#06a581",
			"#009e73",
			"#007c5a",
			"#005a41",
			"#003829",
		],
		dark: [
			"#dfe2e7",
			"#c2c8d1",
			"#9ea8b8",
			"#808b9d",
			"#667286",
			"#515c70",
			"#3d4759",
			"#2b3443",
			"#1b232f",
			"#0f141c",
		],
	},
	components: {
		Button: {
			defaultProps: {
				radius: "xs",
				size: "sm",
			},
		},
	},
	other: {
		colors: {
			wongBlue: "#0072B2",
			wongOrange: "#E69F00",
			wongGreen: "#009E73",
			wongSky: "#56B4E9",
			wongVermillion: "#D55E00",
			wongPurple: "#CC79A7",
		},
		surface: {
			lightBody: "#f4f7fb",
			lightPanel: "#ffffff",
			lightPanelAlt: "#edf2f8",
			lightBorder: "#c8d2e1",
			lightMuted: "#556275",
			darkBody: "#0f141c",
			darkPanel: "#171f2a",
			darkPanelAlt: "#1f2a37",
			darkBorder: "#2f3d51",
			darkMuted: "#a3afbf",
		},
	},
});
