export interface TurnstileInstance {
	render(
		element: HTMLElement,
		options: {
			sitekey: string;
			theme?: "auto" | "light" | "dark";
			callback?: (token: string) => void;
			"expired-callback"?: () => void;
		},
	): string | number;
	reset(widgetId?: string | number): void;
}

export function turnstile() {
	if (typeof window === "undefined") {
		return undefined;
	}
	return (window as Window & { turnstile?: TurnstileInstance }).turnstile;
}
