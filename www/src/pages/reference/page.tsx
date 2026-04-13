import { Anchor, Group, SimpleGrid, Stack, Text } from "@mantine/core";
import { referenceGroups } from "../../content";
import { InlineAction, SecondaryAction } from "../../ui/actions";
import { PageHeader } from "../../ui/page-header";
import { Surface } from "../../ui/surface";

export function ReferencePage() {
	return (
		<Stack gap="lg">
			<PageHeader
				eyebrow="Reference"
				title="Project links and source material."
				description="Use docs first. Come here for grammar, repository material, editor sources, and public reference links."
				actions={
					<Group gap="sm">
						<SecondaryAction href="/docs">Read docs</SecondaryAction>
						<InlineAction href="/install">Install commands</InlineAction>
					</Group>
				}
			/>
			<SimpleGrid cols={{ base: 1, sm: 2, lg: 3 }} spacing="sm">
				{referenceGroups.map((group) => (
					<Surface
						key={group.title}
						p="md"
						tone="panel"
						className="portal-card"
					>
						<Stack gap="md">
							<Text component="h2" fw={700} fz="h4">
								{group.title}
							</Text>
							{group.links.map((link) => (
								<div key={link.label}>
									<Anchor href={link.href}>{link.label}</Anchor>
									<Text mt={4} className="muted" size="sm">
										{link.copy}
									</Text>
								</div>
							))}
						</Stack>
					</Surface>
				))}
			</SimpleGrid>
		</Stack>
	);
}
