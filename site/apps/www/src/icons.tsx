import type { CSSProperties, SVGProps } from "react";

type IconProps = SVGProps<SVGSVGElement> & {
	size?: number;
};

function baseStyle(size: number | undefined): CSSProperties {
	return {
		display: "block",
		width: size ?? 18,
		height: size ?? 18,
	};
}

export function GithubIcon({ size, ...props }: IconProps) {
	return (
		<svg
			viewBox="0 0 24 24"
			fill="none"
			stroke="currentColor"
			strokeLinecap="round"
			strokeLinejoin="round"
			strokeWidth="1.8"
			aria-hidden="true"
			style={baseStyle(size)}
			{...props}
		>
			<path d="M15 22v-4a3.5 3.5 0 0 0-1-2.6c3.3-.4 6.8-1.6 6.8-7.1A5.6 5.6 0 0 0 19.2 4a5.2 5.2 0 0 0-.1-3.9S17.8-.3 15 1.6a13.4 13.4 0 0 0-6 0C6.2-.3 4.9.1 4.9.1A5.2 5.2 0 0 0 4.8 4a5.6 5.6 0 0 0-1.6 4.3c0 5.5 3.5 6.7 6.8 7.1a3.5 3.5 0 0 0-1 2.6v4" />
			<path d="M9 18c-4 1.3-4-2.2-5.7-2.5" />
		</svg>
	);
}

export function SunIcon({ size, ...props }: IconProps) {
	return (
		<svg
			viewBox="0 0 24 24"
			fill="none"
			stroke="currentColor"
			strokeLinecap="round"
			strokeLinejoin="round"
			strokeWidth="1.8"
			aria-hidden="true"
			style={baseStyle(size)}
			{...props}
		>
			<circle cx="12" cy="12" r="4" />
			<path d="M12 2v2.2" />
			<path d="M12 19.8V22" />
			<path d="M4.9 4.9l1.6 1.6" />
			<path d="M17.5 17.5l1.6 1.6" />
			<path d="M2 12h2.2" />
			<path d="M19.8 12H22" />
			<path d="M4.9 19.1l1.6-1.6" />
			<path d="M17.5 6.5l1.6-1.6" />
		</svg>
	);
}

export function MoonIcon({ size, ...props }: IconProps) {
	return (
		<svg
			viewBox="0 0 24 24"
			fill="none"
			stroke="currentColor"
			strokeLinecap="round"
			strokeLinejoin="round"
			strokeWidth="1.8"
			aria-hidden="true"
			style={baseStyle(size)}
			{...props}
		>
			<path d="M21 12.8A9 9 0 1 1 11.2 3a7.1 7.1 0 0 0 9.8 9.8Z" />
		</svg>
	);
}

export function DesktopIcon({ size, ...props }: IconProps) {
	return (
		<svg
			viewBox="0 0 24 24"
			fill="none"
			stroke="currentColor"
			strokeLinecap="round"
			strokeLinejoin="round"
			strokeWidth="1.8"
			aria-hidden="true"
			style={baseStyle(size)}
			{...props}
		>
			<rect x="3" y="4" width="18" height="12" rx="2" />
			<path d="M8 20h8" />
			<path d="M12 16v4" />
		</svg>
	);
}
