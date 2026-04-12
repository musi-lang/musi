import { Group, SimpleGrid, Stack, Table, Text } from "@mantine/core";
import {
	commandRows,
	installPrerequisites,
	installSourceHtml,
	quickstartHtml,
} from "../../content";
import { InlineAction, SecondaryAction } from "../../ui/actions";
import { HtmlSnippet } from "../../ui/html-snippet";
import { PageHeader } from "../../ui/page-header";
import { Surface } from "../../ui/surface";

export function InstallPage() {
	return (
		<Stack gap="lg">
			<PageHeader
				eyebrow="Install"
				title="Build Musi from source."
				description="The current public path is source-first: install Rust and libffi, build the repository, and keep music plus musi on your PATH."
				actions={
					<Group gap="sm">
						<SecondaryAction href="/docs/getting-started">
							Start with docs
						</SecondaryAction>
						<InlineAction href="/reference">Toolchain reference</InlineAction>
					</Group>
				}
			/>
			<SimpleGrid cols={{ base: 1, sm: 2, lg: 3 }} spacing="sm">
				{installPrerequisites.map((item) => (
					<Surface key={item.title} p="md" tone="panel">
						<Stack gap="sm">
							<Text className="eyebrow">{item.title}</Text>
							<Text fw={700}>{item.value}</Text>
							<Text>{item.copy}</Text>
						</Stack>
					</Surface>
				))}
			</SimpleGrid>
			<SimpleGrid cols={{ base: 1, lg: 2 }} spacing="sm">
				<Surface p="md" tone="code" className="snippet-panel">
					<Text className="eyebrow" mb="sm">
						Install from source
					</Text>
					<HtmlSnippet className="docs-content" html={installSourceHtml} />
				</Surface>
				<Surface p="md" tone="code" className="snippet-panel">
					<Text className="eyebrow" mb="sm">
						Quick start
					</Text>
					<HtmlSnippet className="docs-content" html={quickstartHtml} />
				</Surface>
			</SimpleGrid>
			<Surface p="md" tone="panel">
				<Stack gap="md">
					<div>
						<Text className="eyebrow" mb={8}>
							Commands
						</Text>
						<Text component="h2" fw={700} fz="h3">
							Current entry points
						</Text>
					</div>
					<Table
						striped={true}
						highlightOnHover={true}
						withTableBorder={true}
						className="table-dense"
					>
						<Table.Thead>
							<Table.Tr>
								<Table.Th>Command</Table.Th>
								<Table.Th>Description</Table.Th>
							</Table.Tr>
						</Table.Thead>
						<Table.Tbody>
							{commandRows.map((row) => (
								<Table.Tr key={row.command}>
									<Table.Td>
										<Text ff="monospace">{row.command}</Text>
									</Table.Td>
									<Table.Td>{row.description}</Table.Td>
								</Table.Tr>
							))}
						</Table.Tbody>
					</Table>
				</Stack>
			</Surface>
		</Stack>
	);
}
