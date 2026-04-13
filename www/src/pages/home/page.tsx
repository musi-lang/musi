import { Box, Divider, Group, SimpleGrid, Stack, Text } from "@mantine/core";
import {
	homeDescriptor,
	homeSampleHtml,
	homeSections,
	learningTracks,
	startHereLinks,
} from "../../content";
import { docGroups } from "../../docs";
import {
	ActionStrip,
	InlineAction,
	PrimaryAction,
	SecondaryAction,
} from "../../ui/actions";
import { DocListGroup } from "../../ui/doc-list";
import { HtmlSnippet } from "../../ui/html-snippet";
import { PageHeader } from "../../ui/page-header";
import { Surface } from "../../ui/surface";

export function HomePage() {
	return (
		<Stack gap="lg">
			<Surface p={{ base: "md", md: "lg" }} tone="hero">
				<SimpleGrid cols={{ base: 1, lg: 2 }} spacing="md" verticalSpacing="md">
					<PageHeader
						eyebrow="Language"
						title="Musi"
						description={homeDescriptor}
						meta={
							<SimpleGrid cols={{ base: 1, sm: 3 }} spacing="xs" mt="md">
								{learningTracks.map((track) => (
									<Surface
										key={track.title}
										component="a"
										href={track.href}
										p="sm"
										tone="panel"
										className="info-chip"
									>
										<Text className="eyebrow" mb={4}>
											{track.title}
										</Text>
										<Text size="sm">{track.copy}</Text>
									</Surface>
								))}
							</SimpleGrid>
						}
						actions={
							<ActionStrip>
								<PrimaryAction href="/docs">Docs</PrimaryAction>
								<SecondaryAction href="/install">Install</SecondaryAction>
								<InlineAction href="/reference">Reference</InlineAction>
							</ActionStrip>
						}
					/>
					<Surface p="md" tone="code" className="snippet-panel">
						<Text className="eyebrow" mb="sm">
							Sample
						</Text>
						<HtmlSnippet className="docs-content" html={homeSampleHtml} />
					</Surface>
				</SimpleGrid>
			</Surface>
			<SimpleGrid cols={{ base: 1, md: 3 }} spacing="sm">
				{startHereLinks.map((link, index) => (
					<Surface key={link.title} p="md" tone="panel" className="portal-card">
						<Stack gap="xs">
							<Text className="eyebrow">Path {index + 1}</Text>
							<Text component="h2" fw={700} fz="h4">
								{link.title}
							</Text>
							<Text>{link.copy}</Text>
							<InlineAction href={link.href}>Open</InlineAction>
						</Stack>
					</Surface>
				))}
			</SimpleGrid>
			<SimpleGrid cols={{ base: 1, sm: 2, lg: 3 }} spacing="sm">
				{homeSections.map((section) => (
					<Surface
						key={section.title}
						p="md"
						tone="panel"
						className="portal-card"
					>
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
								Docs
							</Text>
							<Text component="h2" fw={700} fz="h3">
								Read in order
							</Text>
							<Text c="dimmed" mt={6}>
								Start at setup, then move through syntax, types, and tooling.
								Jump by chapter group if you already know the topic you need.
							</Text>
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
