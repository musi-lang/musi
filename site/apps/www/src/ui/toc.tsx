import { Anchor, Stack, Text } from "@mantine/core";
import { Surface } from "./surface";

export function OnThisPage(props: {
	headings: Array<{ depth: number; id: string; text: string }>;
	className?: string;
}) {
	if (props.headings.length === 0) {
		return null;
	}

	return (
		<Surface
			p="md"
			tone="panel"
			{...(props.className ? { className: props.className } : {})}
		>
			<Stack gap="xs">
				<Text className="eyebrow">On this page</Text>
				{props.headings
					.filter((heading) => heading.depth <= 3)
					.map((heading) => (
						<Anchor
							key={heading.id}
							href={`#${heading.id}`}
							className="toc-link"
							style={{
								paddingLeft: heading.depth === 3 ? "0.7rem" : "0",
							}}
						>
							{heading.text}
						</Anchor>
					))}
			</Stack>
		</Surface>
	);
}
