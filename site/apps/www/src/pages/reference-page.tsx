import { Anchor, Group, SimpleGrid, Stack, Text, Title } from "@mantine/core";
import { referenceGroups } from "../content";
import { InlineAction, SecondaryAction } from "../ui/actions";
import { Surface } from "../ui/surface";

export function ReferencePage() {
	return (
		<Stack gap="lg">
			<div>
				<Text className="eyebrow" mb={8}>
					Reference
				</Text>
				<Title order={1}>Project links and source material.</Title>
				<Text mt="md" maw={760} className="page-copy">
					Start with the public docs. Come here when you need the repository,
					grammar, editor sources, or source material that lives in the repo.
				</Text>
			</div>
			<Group gap="sm">
				<SecondaryAction href="/docs">Read docs</SecondaryAction>
				<InlineAction href="/install">Install commands</InlineAction>
			</Group>
			<SimpleGrid cols={{ base: 1, sm: 2, lg: 3 }} spacing="sm">
				{referenceGroups.map((group) => (
					<Surface key={group.title} p="md" tone="panel">
						<Stack gap="md">
							<Title order={2} size="h4">
								{group.title}
							</Title>
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
