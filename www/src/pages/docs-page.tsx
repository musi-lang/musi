import { Box, Divider, Grid, Group, Stack, Text, Title } from "@mantine/core";
import { docForPath, docGroups, docNeighbors } from "../docs";
import { InlineAction, PrimaryAction, SecondaryAction } from "../ui/actions";
import { DocListGroup } from "../ui/doc-list";
import { HtmlSnippet } from "../ui/html-snippet";
import { Surface } from "../ui/surface";
import { OnThisPage } from "../ui/toc";

export function DocsIndexPage() {
	return (
		<Stack gap="lg">
			<div>
				<Text className="eyebrow" mb={8}>
					Docs
				</Text>
				<Title order={1}>Learn Musi by writing Musi.</Title>
				<Text mt="md" maw={760} className="page-copy">
					Start with files and packages, then move through expressions, data,
					effects, types, abstractions, and the toolchain.
				</Text>
			</div>
			<Group gap="sm">
				<PrimaryAction href="/docs/getting-started">
					Start reading
				</PrimaryAction>
				<SecondaryAction href="/install">Install first</SecondaryAction>
				<InlineAction href="/reference">Reference</InlineAction>
			</Group>
			<Surface p={{ base: "md", md: "lg" }} tone="panel">
				<Stack gap="md">
					<Group justify="space-between">
						<Box>
							<Text className="eyebrow" mb={6}>
								Table of contents
							</Text>
							<Title order={2} size="h3">
								Chapter groups
							</Title>
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
			<div>
				<Text className="eyebrow" mb={8}>
					{page.group}
				</Text>
				<Title order={1}>{page.title}</Title>
				<Text
					mt="md"
					maw={720}
					className="page-copy"
					dangerouslySetInnerHTML={{ __html: page.descriptionHtml }}
				/>
			</div>
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
