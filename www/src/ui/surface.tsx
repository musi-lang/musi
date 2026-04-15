import { createElement, type ElementType, type ReactNode } from "preact/compat";

type SurfaceTone =
	| "base"
	| "panel"
	| "hero"
	| "code"
	| "accent"
	| "well"
	| "raised";

interface SurfaceProps {
	children?: ReactNode;
	className?: string;
	component?: ElementType;
	tone?: SurfaceTone;
}

export function Surface(props: SurfaceProps & Record<string, unknown>) {
	const {
		children,
		className,
		component = "section",
		tone = "panel",
		...rest
	} = props;
	return createElement(
		component,
		{
			...rest,
			className: `surface surface-${tone}${className ? ` ${className}` : ""}`,
		},
		children,
	);
}
