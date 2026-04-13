import {
	createPolymorphicComponent,
	Paper,
	type PaperProps,
} from "@mantine/core";
import { forwardRef, type ReactNode } from "react";

type SurfaceTone = "base" | "panel" | "hero" | "code";

interface SurfaceProps extends PaperProps {
	children?: ReactNode;
	tone?: SurfaceTone;
}

export const Surface = createPolymorphicComponent<"div", SurfaceProps>(
	forwardRef<HTMLDivElement, SurfaceProps>((props, ref) => {
		const tone = props.tone ?? "panel";
		return (
			<Paper
				{...props}
				ref={ref}
				radius={0}
				withBorder={true}
				className={`surface surface-${tone}${props.className ? ` ${props.className}` : ""}`}
			>
				{props.children}
			</Paper>
		);
	}),
);
