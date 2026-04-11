import {
	Box,
	Divider,
	Group,
	SimpleGrid,
	Stack,
	Text,
	Title,
} from "@mantine/core";
import { homeDescriptor, homeSampleHtml, homeSections } from "../content";
import { docGroups } from "../docs";
import {
	ActionStrip,
	InlineAction,
	PrimaryAction,
	SecondaryAction,
} from "../ui/actions";
import { DocListGroup } from "../ui/doc-list";
import { HtmlSnippet } from "../ui/html-snippet";
import { Surface } from "../ui/surface";

export function HomePage() {
	return (
		<Stack gap="lg">
			<Surface p={{ base: "md", md: "lg" }} tone="hero">
				<SimpleGrid cols={{ base: 1, lg: 2 }} spacing="md" verticalSpacing="md">
					<Stack gap="md">
						<div>
							<Text className="eyebrow" mb={8}>
								Programming language
							</Text>
							<Title order={1} size="h1">
								Musi
							</Title>
							<Text mt={10} maw={620}>
								{homeDescriptor}
							</Text>
						</div>
						<ActionStrip>
							<PrimaryAction href="/docs">Docs</PrimaryAction>
							<SecondaryAction href="/install">Install</SecondaryAction>
							<InlineAction href="/reference">Reference</InlineAction>
						</ActionStrip>
					</Stack>
					<Surface p="md" tone="code" className="snippet-panel">
						<Text className="eyebrow" mb="sm">
							Sample
						</Text>
						<HtmlSnippet className="docs-content" html={homeSampleHtml} />
					</Surface>
				</SimpleGrid>
			</Surface>
			<SimpleGrid cols={{ base: 1, sm: 2, lg: 3 }} spacing="sm">
				{homeSections.map((section) => (
					<Surface key={section.title} p="md" tone="panel">
						<Stack gap="sm">
							<Text className="eyebrow">{section.title}</Text>
							<Text>{section.copy}</Text>
						</Stack>
					</Surface>
				))}
			</SimpleGrid>
			<Surface p={{ base: "md", md: "lg" }} tone="panel">
				<Stack gap="md">
					<Group justify="space-between" align="end">
						<Box>
							<Text className="eyebrow" mb={6}>
								Docs map
							</Text>
							<Title order={2} size="h3">
								Explore the book
							</Title>
						</Box>
						<InlineAction href="/docs">All chapters</InlineAction>
					</Group>
					<Divider />
					<SimpleGrid cols={{ base: 1, md: 2 }} spacing="md">
						{docGroups.map((group) => (
							<DocListGroup
								key={group.group}
								group={group.group}
								pages={group.pages}
							/>
						))}
					</SimpleGrid>
				</Stack>
			</Surface>
			<Surface p="md" tone="panel">
				<div>
					<Text className="eyebrow" mb={8}>
						Project
					</Text>
					<Group gap="md">
						<InlineAction href="https://github.com/musi-lang/musi">
							GitHub
						</InlineAction>
						<InlineAction href="/install">Toolchain commands</InlineAction>
						<InlineAction href="/reference">
							Grammar and source links
						</InlineAction>
					</Group>
				</div>
			</Surface>
		</Stack>
	);
}
