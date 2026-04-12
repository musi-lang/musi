import { createTheme } from "@mantine/core";

export const theme = createTheme({
	primaryColor: "slate",
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
		slate: [
			"#f4f4f1",
			"#e7e5df",
			"#d3d0c7",
			"#bab6ab",
			"#a19c8e",
			"#838070",
			"#666354",
			"#49463c",
			"#2e2d27",
			"#171815",
		],
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
			"#e6e4dd",
			"#cfcbc0",
			"#b0aa9d",
			"#8f8979",
			"#6f695a",
			"#595446",
			"#433f35",
			"#2f2c24",
			"#1d1b16",
			"#0f0e0b",
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
		},
		surface: {
			lightBody: "#f4f4f1",
			lightPanel: "#fcfbf8",
			lightPanelAlt: "#efeee8",
			lightBorder: "#cdc8bd",
			lightMuted: "#5f5b52",
			darkBody: "#0f0e0b",
			darkPanel: "#171510",
			darkPanelAlt: "#1f1c17",
			darkBorder: "#373227",
			darkMuted: "#b0aa9d",
		},
	},
});
