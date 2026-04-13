import { Anchor, Group, Stack, Text, Title } from "@mantine/core";
import { Surface } from "./surface";

interface DocListPage {
	path: string;
	slug: string;
	summaryHtml: string;
	title: string;
	summary: string;
}

export function DocListGroup(props: {
	group: string;
	path?: string;
	summaryHtml?: string;
	pages: readonly DocListPage[];
}) {
	return (
		<Surface p="md" tone="panel" className="doc-group">
			<Stack gap="xs">
				{props.path ? (
					<Anchor href={props.path} underline="never">
						<Text className="eyebrow">{props.group}</Text>
					</Anchor>
				) : (
					<Text className="eyebrow">{props.group}</Text>
				)}
				{props.summaryHtml ? (
					<Text
						size="sm"
						c="dimmed"
						dangerouslySetInnerHTML={{ __html: props.summaryHtml }}
					/>
				) : null}
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
		</Surface>
	);
}
