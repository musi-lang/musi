import {
	createContext,
	type ReactNode,
	useContext,
	useEffect,
	useMemo,
	useState,
} from "react";

export type ColorScheme = "light" | "dark" | "system";

type ResolvedColorScheme = "light" | "dark";

interface ThemeContextValue {
	colorScheme: ColorScheme;
	resolvedColorScheme: ResolvedColorScheme;
	setColorScheme: (scheme: ColorScheme) => void;
}

const STORAGE_KEY = "musi-color-scheme";
const MEDIA_QUERY = "(prefers-color-scheme: dark)";

const ThemeContext = createContext<ThemeContextValue | null>(null);

function isColorScheme(value: string | null): value is ColorScheme {
	return value === "light" || value === "dark" || value === "system";
}

function readStoredColorScheme(): ColorScheme {
	if (typeof window === "undefined") {
		return "system";
	}

	const value = window.localStorage.getItem(STORAGE_KEY);
	return isColorScheme(value) ? value : "system";
}

function resolveColorScheme(scheme: ColorScheme): ResolvedColorScheme {
	if (scheme !== "system") {
		return scheme;
	}
	if (typeof window === "undefined") {
		return "light";
	}
	return window.matchMedia(MEDIA_QUERY).matches ? "dark" : "light";
}

function applyColorScheme(
	colorScheme: ColorScheme,
	resolvedColorScheme: ResolvedColorScheme,
) {
	const root = document.documentElement;
	const dataset = root.dataset as DOMStringMap & {
		colorScheme?: string;
		mantineColorScheme?: string;
	};
	dataset.colorScheme = colorScheme;
	dataset.mantineColorScheme = resolvedColorScheme;
	root.style.colorScheme = resolvedColorScheme;
}

export function ThemeProvider(props: { children: ReactNode }) {
	const [colorScheme, setColorScheme] = useState<ColorScheme>(() =>
		readStoredColorScheme(),
	);
	const [resolvedColorScheme, setResolvedColorScheme] =
		useState<ResolvedColorScheme>(() =>
			resolveColorScheme(readStoredColorScheme()),
		);

	useEffect(() => {
		const nextResolvedColorScheme = resolveColorScheme(colorScheme);
		setResolvedColorScheme(nextResolvedColorScheme);
		window.localStorage.setItem(STORAGE_KEY, colorScheme);
		applyColorScheme(colorScheme, nextResolvedColorScheme);

		if (colorScheme !== "system") {
			return;
		}

		const mediaQuery = window.matchMedia(MEDIA_QUERY);
		const listener = () => {
			const updatedScheme = mediaQuery.matches ? "dark" : "light";
			setResolvedColorScheme(updatedScheme);
			applyColorScheme("system", updatedScheme);
		};

		mediaQuery.addEventListener("change", listener);
		return () => mediaQuery.removeEventListener("change", listener);
	}, [colorScheme]);

	const value = useMemo(
		() => ({
			colorScheme,
			resolvedColorScheme,
			setColorScheme,
		}),
		[colorScheme, resolvedColorScheme],
	);

	return (
		<ThemeContext.Provider value={value}>
			{props.children}
		</ThemeContext.Provider>
	);
}

export function useTheme() {
	const value = useContext(ThemeContext);
	if (!value) {
		throw new Error("useTheme must run inside ThemeProvider");
	}
	return value;
}
