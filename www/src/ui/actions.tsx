import type {
	AnchorHTMLAttributes,
	ButtonHTMLAttributes,
	ReactNode,
} from "preact/compat";

type ButtonTone = "primary" | "secondary" | "ghost";

function ActionAnchor(
	props: AnchorHTMLAttributes<HTMLAnchorElement> & {
		children: ReactNode;
		tone: ButtonTone;
	},
) {
	const className = `button button-${props.tone}${props.className ? ` ${props.className}` : ""}`;
	return (
		<a {...props} className={className}>
			{props.children}
		</a>
	);
}

export function PrimaryAction(props: { children: ReactNode; href: string }) {
	return (
		<ActionAnchor href={props.href} tone="primary">
			{props.children}
		</ActionAnchor>
	);
}

export function SecondaryAction(props: { children: ReactNode; href: string }) {
	return (
		<ActionAnchor href={props.href} tone="secondary">
			{props.children}
		</ActionAnchor>
	);
}

export function InlineAction(
	props: AnchorHTMLAttributes<HTMLAnchorElement> & {
		children: ReactNode;
		href: string;
	},
) {
	const className = `inline-action${props.className ? ` ${props.className}` : ""}`;
	return (
		<a {...props} className={className}>
			{props.children}
		</a>
	);
}

export function ThemeToggleButton(
	props: ButtonHTMLAttributes<HTMLButtonElement> & { children: ReactNode },
) {
	const className = `button button-ghost theme-toggle${props.className ? ` ${props.className}` : ""}`;
	return (
		<button type="button" {...props} className={className}>
			{props.children}
		</button>
	);
}

export function ActionStrip(props: { children: ReactNode }) {
	return <div className="action-strip">{props.children}</div>;
}
