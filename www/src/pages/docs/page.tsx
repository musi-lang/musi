import {
	Anchor,
	Box,
	Divider,
	Grid,
	Group,
	SimpleGrid,
	Stack,
	Text,
} from "@mantine/core";
import {
	docForPath,
	docGroups,
	docNeighbors,
	docQuestionIndex,
	pagesForPart,
} from "../../docs";
import { InlineAction, PrimaryAction, SecondaryAction } from "../../ui/actions";
import { DocListGroup } from "../../ui/doc-list";
import { HtmlSnippet } from "../../ui/html-snippet";
import { PageHeader } from "../../ui/page-header";
import { Surface } from "../../ui/surface";
import { OnThisPage } from "../../ui/toc";

const PART_REGEXP = /\/[^/]+$/;
const groupAudience: Record<string, string> = {
	Start: "Setup, first files, packages, and imports",
	"Core language": "Expressions, data, functions, and updates",
	"Types and abstractions": "Types, classes, effects, quote, and foreign",
	Tooling: "Standard library, tests, and commands",
	Questions: "Task-first links into the book",
};

export function DocsIndexPage() {
	return (
		<Stack gap="lg">
			<PageHeader
				eyebrow="Docs"
				title="Musi book"
				description="Read Musi in chapter order, or jump in by task. Chapters stay Musi-first, link forward clearly, and answer common follow-up questions."
				actions={
					<Group gap="sm">
						<PrimaryAction href="/docs/start/getting-started">
							Start reading
						</PrimaryAction>
						<SecondaryAction href="/install">Install</SecondaryAction>
						<InlineAction href="/reference">Reference</InlineAction>
					</Group>
				}
			/>
			<SimpleGrid cols={{ base: 1, md: 2 }} spacing="sm">
				<Surface p="md" tone="panel" className="portal-card">
					<Stack gap="xs">
						<Text className="eyebrow">Read in order</Text>
						<Text component="h2" fw={700} fz="h4">
							Start with setup and keep moving
						</Text>
						<Text>
							Chapters build from setup into syntax, data, types, effects, and
							tooling.
						</Text>
						<InlineAction href="/docs/start/getting-started">
							Open first chapter
						</InlineAction>
					</Stack>
				</Surface>
				<Surface p="md" tone="panel" className="portal-card">
					<Stack gap="xs">
						<Text className="eyebrow">Jump by task</Text>
						<Text component="h2" fw={700} fz="h4">
							Use common questions
						</Text>
						<Text>
							Open the task-first question list when you know what you need but
							not which chapter answers it.
						</Text>
						<InlineAction href="/docs/questions/common-questions">
							Open questions
						</InlineAction>
					</Stack>
				</Surface>
			</SimpleGrid>
			<Surface p={{ base: "md", md: "lg" }} tone="panel">
				<Stack gap="md">
					<Group justify="space-between" align="end">
						<Box>
							<Text className="eyebrow" mb={6}>
								Table of contents
							</Text>
							<Text component="h2" fw={700} fz="h3">
								Parts and chapters
							</Text>
						</Box>
						<InlineAction href="https://github.com/musi-lang/musi/tree/main/www/src/content">
							Source
						</InlineAction>
					</Group>
					<Divider />
					<Stack gap="lg">
						{docGroups.map((group) => (
							<DocListGroup
								key={group.group}
								group={group.group}
								path={group.path}
								summaryHtml={group.summaryHtml}
								pages={group.pages}
							/>
						))}
					</Stack>
				</Stack>
			</Surface>
			<Surface p={{ base: "md", md: "lg" }} tone="panel">
				<Stack gap="md">
					<Box>
						<Text className="eyebrow" mb={6}>
							Common questions
						</Text>
						<Text component="h2" fw={700} fz="h3">
							Task-first links
						</Text>
					</Box>
					<SimpleGrid cols={{ base: 1, md: 2 }} spacing="sm">
						{docQuestionIndex.map((question) => (
							<Anchor
								key={`${question.href}:${question.label}`}
								href={question.href}
								underline="never"
								className="doc-row"
							>
								<Text fw={700}>{question.label}</Text>
								<Text size="sm" c="dimmed" mt={4}>
									{question.pageTitle}
								</Text>
							</Anchor>
						))}
					</SimpleGrid>
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

function TopCards({
	page,
	neighbors,
}: {
	/* biome-ignore lint/suspicious/noExplicitAny: page structure */
	page: any;
	/* biome-ignore lint/suspicious/noExplicitAny: neighbors structure */
	neighbors: any;
}) {
	const hasNext = Boolean(neighbors.next);
	const hasQuestions = Boolean(page.questions && page.questions.length > 0);
	const showTopCards = page.kind === "chapter" && (hasNext || hasQuestions);

	if (!showTopCards) {
		return null;
	}

	return (
		<SimpleGrid cols={{ base: 1, lg: 2 }} spacing="sm">
			{hasNext && neighbors.next && (
				<Surface p="md" tone="panel" className="portal-card">
					<Text className="eyebrow" mb={6}>
						Read next
					</Text>
					<Anchor
						href={neighbors.next.path}
						underline="never"
						className="doc-row"
					>
						<Text fw={700}>{neighbors.next.title}</Text>
						<Text size="sm" c="dimmed" mt={4}>
							Open the next chapter in order.
						</Text>
					</Anchor>
				</Surface>
			)}
			{hasQuestions && (
				<Surface p="md" tone="panel" className="portal-card">
					<Text className="eyebrow" mb={6}>
						Common questions
					</Text>
					<Stack gap="xs">
						{page.questions.map((question: { label: string; href: string }) => (
							<Anchor
								key={question.label}
								href={question.href}
								underline="never"
							>
								<span dangerouslySetInnerHTML={{ __html: question.label }} />
							</Anchor>
						))}
					</Stack>
				</Surface>
			)}
		</SimpleGrid>
	);
}

export function DocPage(props: { pathname: string }) {
	const page = docForPath(props.pathname);
	if (!page) {
		return null;
	}

	const neighbors = page.kind === "chapter" ? docNeighbors(page.id) : {};
	const childPages = page.kind === "part" ? pagesForPart(page.id) : [];

	return (
		<Stack gap="lg">
			<PageHeader
				eyebrow={
					<Group gap="xs">
						<Anchor href="/docs" underline="never">
							Docs
						</Anchor>
						<Text c="dimmed">/</Text>
						{page.kind === "part" ? (
							<Text>{page.title}</Text>
						) : (
							<Anchor
								href={page.canonicalPath.replace(PART_REGEXP, "")}
								underline="never"
							>
								{page.partTitle}
							</Anchor>
						)}
					</Group>
				}
				title={page.title}
				descriptionHtml={page.descriptionHtml}
				meta={
					<Text c="dimmed" size="sm" mt="md">
						{groupAudience[
							page.kind === "part" ? page.title : page.partTitle
						] ??
							groupAudience[page.group] ??
							"Read chapters in order or jump in by task."}
					</Text>
				}
			/>
			<OnThisPage
				headings={page.headings}
				className="toc-panel toc-panel-mobile"
			/>
			<TopCards page={page} neighbors={neighbors} />
			<Grid gap="lg" align="start">
				<Grid.Col span={{ base: 12, xl: 9 }}>
					<Surface p={{ base: "md", md: "lg" }} tone="base">
						<HtmlSnippet className="docs-content" html={page.html} />
						{page.kind === "part" && childPages.length > 0 ? (
							<Stack gap="md" mt="lg">
								<Divider />
								<Text className="eyebrow">Chapters in this part</Text>
								<DocListGroup group={page.title} pages={childPages} />
							</Stack>
						) : null}
					</Surface>
				</Grid.Col>
				<Grid.Col span={{ base: 12, xl: 3 }} visibleFrom="xl">
					<OnThisPage
						headings={page.headings}
						className="toc-panel toc-panel-desktop"
					/>
				</Grid.Col>
			</Grid>
			{page.kind === "chapter" ? (
				<SectionNav previous={neighbors.previous} next={neighbors.next} />
			) : null}
		</Stack>
	);
}
