import { Box, Button, Group, Stack, Text, Textarea } from "@mantine/core";
import { Surface } from "../../ui/surface";

export function PlaygroundPage() {
	return (
		<Stack gap="lg" h="100%" style={{ opacity: 0.5, pointerEvents: "none" }}>
			<Surface p="md" tone="hero">
				<Stack gap="xs">
					<Text component="h1" fw={700} fz="h2">
						Playground (WASM Stub)
					</Text>
					<Text>
						This environment will run Musi code natively in the browser via
						WebAssembly, soon. For now, it is a structural stub, ready for the
						upcoming tooling integrations.
					</Text>
				</Stack>
			</Surface>
			<Group grow={true} align="flex-start" style={{ flex: 1 }}>
				<Surface p="md" tone="code" h="100%">
					<Stack h="100%" gap="sm">
						<Text fw={700}>Editor</Text>
						<Textarea
							value="let main () := 42;"
							readOnly={true}
							minRows={10}
							styles={{
								input: {
									height: "100%",
									fontFamily: "var(--mantine-font-family-monospace)",
								},
							}}
						/>
						<Button disabled={true} variant="outline">
							Run Code (Coming soon)
						</Button>
					</Stack>
				</Surface>
				<Surface p="md" tone="panel" h="100%">
					<Stack gap="sm" h="100%">
						<Text fw={700}>Output</Text>
						<Box
							p="sm"
							bg="var(--mantine-color-body)"
							style={{
								border: "2px solid var(--mantine-color-black)",
								flex: 1,
								minHeight: 200,
							}}
						>
							<Text c="dimmed" fs="italic">
								Compiler output will appear here...
							</Text>
						</Box>
					</Stack>
				</Surface>
			</Group>
		</Stack>
	);
}
