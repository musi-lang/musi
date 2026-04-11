import { Anchor, Group, Stack, Text, Title } from "@mantine/core";

interface DocListPage {
	path: string;
	slug: string;
	summaryHtml: string;
	title: string;
	summary: string;
}

export function DocListGroup(props: {
	group: string;
	pages: readonly DocListPage[];
}) {
	return (
		<Stack gap="xs">
			<Text className="eyebrow">{props.group}</Text>
			{props.pages.map((page) => (
				<Anchor
					key={page.slug}
					href={page.path}
					underline="never"
					className="doc-row"
				>
					<Group justify="space-between" align="start" wrap="nowrap">
						<div>
							<Title order={3} size="h5">
								{page.title}
							</Title>
							<Text
								mt={4}
								size="sm"
								c="dimmed"
								dangerouslySetInnerHTML={{ __html: page.summaryHtml }}
							/>
						</div>
						<Text className="doc-row-arrow" aria-hidden="true">
							&rarr;
						</Text>
					</Group>
				</Anchor>
			))}
		</Stack>
	);
}
