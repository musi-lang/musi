import { Anchor, type AnchorProps, Button, Group } from "@mantine/core";
import type { ReactNode } from "react";

interface ActionProps {
	children: ReactNode;
	href: string;
}

export function PrimaryAction(props: ActionProps) {
	return (
		<Button
			component="a"
			href={props.href}
			variant="filled"
			className="primary-action"
		>
			{props.children}
		</Button>
	);
}

export function SecondaryAction(props: ActionProps) {
	return (
		<Button
			component="a"
			href={props.href}
			variant="default"
			className="secondary-action"
		>
			{props.children}
		</Button>
	);
}

export function InlineAction(props: ActionProps & Omit<AnchorProps, "href">) {
	return (
		<Anchor href={props.href} underline="never" className="inline-action">
			{props.children}
		</Anchor>
	);
}

export function ActionStrip(props: { children: ReactNode }) {
	return (
		<Group gap="sm" wrap="wrap">
			{props.children}
		</Group>
	);
}
