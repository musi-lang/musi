import { Box, Divider, Grid, Group, Stack, Text } from "@mantine/core";
import { docForPath, docGroups, docNeighbors } from "../../docs";
import { InlineAction, PrimaryAction, SecondaryAction } from "../../ui/actions";
import { DocListGroup } from "../../ui/doc-list";
import { HtmlSnippet } from "../../ui/html-snippet";
import { PageHeader } from "../../ui/page-header";
import { Surface } from "../../ui/surface";
import { OnThisPage } from "../../ui/toc";

export function DocsIndexPage() {
	return (
		<Stack gap="lg">
			<PageHeader
				eyebrow="Docs"
				title="Learn Musi by writing Musi."
				description="Start with files and packages, then move through expressions, data, effects, types, abstractions, and the toolchain."
				actions={
					<Group gap="sm">
						<PrimaryAction href="/docs/getting-started">
							Start reading
						</PrimaryAction>
						<SecondaryAction href="/install">Install first</SecondaryAction>
						<InlineAction href="/reference">Reference</InlineAction>
					</Group>
				}
			/>
			<Surface p={{ base: "md", md: "lg" }} tone="panel">
				<Stack gap="md">
					<Group justify="space-between">
						<Box>
							<Text className="eyebrow" mb={6}>
								Table of contents
							</Text>
							<Text component="h2" fw={700} fz="h3">
								Chapter groups
							</Text>
						</Box>
						<InlineAction href="https://github.com/musi-lang/musi/tree/main/www/src/content/docs">
							Source
						</InlineAction>
					</Group>
					<Divider />
					<Stack gap="lg">
						{docGroups.map((group) => (
							<DocListGroup
								key={group.group}
								group={group.group}
								pages={group.pages}
							/>
						))}
					</Stack>
				</Stack>
			</Surface>
		</Stack>
	);
}

function SectionNav(props: {
	previous: { path: string; title: string } | undefined;
	next: { path: string; title: string } | undefined;
}) {
	if (!(props.previous || props.next)) {
		return null;
	}

	return (
		<Group justify="space-between" align="center" mt={4}>
			<div>
				{props.previous ? (
					<InlineAction href={props.previous.path}>
						{"< "}
						{props.previous.title}
					</InlineAction>
				) : null}
			</div>
			<div>
				{props.next ? (
					<InlineAction href={props.next.path}>
						{props.next.title}
						{" >"}
					</InlineAction>
				) : null}
			</div>
		</Group>
	);
}

export function DocPage(props: { pathname: string }) {
	const page = docForPath(props.pathname);
	if (!page) {
		return null;
	}

	const neighbors = docNeighbors(page.slug);

	return (
		<Stack gap="lg">
			<PageHeader
				eyebrow={page.group}
				title={page.title}
				descriptionHtml={page.descriptionHtml}
			/>
			<OnThisPage
				headings={page.headings}
				className="toc-panel toc-panel-mobile"
			/>
			<Grid gap="lg" align="start">
				<Grid.Col span={{ base: 12, xl: 9 }}>
					<Surface p={{ base: "md", md: "lg" }} tone="base">
						<HtmlSnippet className="docs-content" html={page.html} />
					</Surface>
				</Grid.Col>
				<Grid.Col span={{ base: 12, xl: 3 }} visibleFrom="xl">
					<OnThisPage
						headings={page.headings}
						className="toc-panel toc-panel-desktop"
					/>
				</Grid.Col>
			</Grid>
			<SectionNav previous={neighbors.previous} next={neighbors.next} />
		</Stack>
	);
}
