import { Paper, type PaperProps } from "@mantine/core";
import type { ReactNode } from "react";

type SurfaceTone = "base" | "panel" | "hero" | "code";

export function Surface(
	props: PaperProps & {
		children: ReactNode;
		tone?: SurfaceTone;
	},
) {
	const tone = props.tone ?? "panel";
	return (
		<Paper
			{...props}
			radius="xs"
			withBorder={true}
			className={`surface surface-${tone}${props.className ? ` ${props.className}` : ""}`}
		>
			{props.children}
		</Paper>
	);
}
