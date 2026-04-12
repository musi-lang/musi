import {
	ActionIcon,
	Anchor,
	AppShell,
	Burger,
	Container,
	Divider,
	Drawer,
	Group,
	NavLink,
	ScrollArea,
	Stack,
	Text,
	Tooltip,
	useMantineColorScheme,
} from "@mantine/core";
import { useDisclosure } from "@mantine/hooks";
import type { ReactNode } from "react";
import { docGroups } from "../docs";
import { DesktopIcon, GithubIcon, MoonIcon, SunIcon } from "../icons";
import type { AppRoute } from "../routes";
import { primaryRoutes } from "../routes";

type ColorScheme = "light" | "dark" | "auto";

const SCHEME_CYCLE: readonly ColorScheme[] = ["light", "dark", "auto"] as const;

const schemeLabel: Record<ColorScheme, string> = {
	light: "Switch to dark mode",
	dark: "Switch to system mode",
	auto: "Switch to light mode",
};

export function nextScheme(current: ColorScheme) {
	const index = SCHEME_CYCLE.indexOf(current);
	return SCHEME_CYCLE[(index + 1) % SCHEME_CYCLE.length];
}

function ColorSchemeIcon(props: { scheme: ColorScheme }) {
	switch (props.scheme) {
		case "light":
			return <SunIcon size={18} />;
		case "dark":
			return <MoonIcon size={18} />;
		case "auto":
			return <DesktopIcon size={18} />;
		default:
			return <DesktopIcon size={18} />;
	}
}

function HeaderLinks(props: { route: AppRoute; onNavigate?: () => void }) {
	return (
		<>
			{primaryRoutes
				.filter((route) => route.id !== "home")
				.map((route) => (
					<Anchor
						key={route.id}
						href={route.path}
						onClick={props.onNavigate}
						underline="never"
						c={props.route.path === route.path ? "text" : "dimmed"}
						fw={props.route.path === route.path ? 700 : 500}
						size="sm"
						className={`header-link ${props.route.path === route.path ? "header-link-active" : ""}`}
					>
						{route.label}
					</Anchor>
				))}
		</>
	);
}

function HeaderUtilities(props: { onNavigate?: () => void }) {
	const { colorScheme, setColorScheme } = useMantineColorScheme();
	const current = colorScheme as ColorScheme;

	return (
		<Group gap="xs">
			<Tooltip label="GitHub repository">
				<ActionIcon
					component="a"
					href="https://github.com/musi-lang/musi"
					target="_blank"
					rel="noreferrer"
					variant="subtle"
					color="gray"
					aria-label="GitHub repository"
					onClick={props.onNavigate}
				>
					<GithubIcon size={18} />
				</ActionIcon>
			</Tooltip>
			<Tooltip label={schemeLabel[current]}>
				<ActionIcon
					variant="subtle"
					color="gray"
					aria-label={schemeLabel[current]}
					onClick={() => setColorScheme(nextScheme(current))}
				>
					<ColorSchemeIcon scheme={current} />
				</ActionIcon>
			</Tooltip>
		</Group>
	);
}

function DocsSidebar(props: { route: AppRoute; onNavigate?: () => void }) {
	return (
		<Stack gap="md">
			{docGroups.map((group) => (
				<Stack key={group.group} gap={4} className="docs-nav-group">
					<Text fw={700} size="sm" tt="uppercase" c="dimmed">
						{group.group}
					</Text>
					{group.pages.map((page) => (
						<NavLink
							key={page.slug}
							component="a"
							href={page.path}
							label={page.title}
							active={props.route.path === page.path}
							variant="light"
							autoContrast={true}
							styles={{
								root: {
									borderRadius: "var(--mantine-radius-xs)",
									border: "1px solid var(--site-border)",
								},
							}}
							{...(props.onNavigate ? { onClick: props.onNavigate } : {})}
						/>
					))}
				</Stack>
			))}
		</Stack>
	);
}

export function SiteLayout(props: { route: AppRoute; children: ReactNode }) {
	const [opened, handlers] = useDisclosure(false);
	const docsMode = props.route.kind === "doc";

	const headerContent = (
		<Group h="100%" px={4} justify="space-between" wrap="nowrap">
			<Group gap="xs" wrap="nowrap">
				<Burger
					opened={opened}
					onClick={handlers.toggle}
					hiddenFrom="lg"
					size="sm"
					aria-label={
						docsMode ? "Toggle docs navigation" : "Toggle site navigation"
					}
				/>
				<Anchor href="/" underline="never" className="site-logo">
					<Text fw={700} size="lg" c="inherit">
						Musi
					</Text>
				</Anchor>
			</Group>
			<Group gap="md" wrap="nowrap">
				<Group gap="md" visibleFrom="lg">
					<HeaderLinks route={props.route} />
				</Group>
				<HeaderUtilities />
			</Group>
		</Group>
	);

	return (
		<AppShell
			header={{ height: 56 }}
			padding={{ base: "sm", md: "md" }}
			className="site-shell"
			{...(docsMode
				? {
						navbar: {
							width: 268,
							breakpoint: "lg" as const,
							collapsed: { mobile: !opened },
						},
					}
				: {})}
		>
			<AppShell.Header className="site-header">
				<Container size="lg" h="100%">
					{headerContent}
				</Container>
			</AppShell.Header>
			{docsMode ? (
				<AppShell.Navbar p="md" className="docs-navbar">
					<ScrollArea type="never" h="100%">
						<DocsSidebar route={props.route} onNavigate={handlers.close} />
					</ScrollArea>
				</AppShell.Navbar>
			) : null}
			{docsMode ? null : (
				<Drawer
					opened={opened}
					onClose={handlers.close}
					size="xs"
					title="Navigation"
					hiddenFrom="lg"
					padding="md"
					zIndex={1000}
				>
					<Stack gap="md">
						<Group gap="lg">
							<HeaderLinks route={props.route} onNavigate={handlers.close} />
						</Group>
						<Divider />
						<HeaderUtilities onNavigate={handlers.close} />
					</Stack>
				</Drawer>
			)}
			<AppShell.Main>
				<Container size="lg">{props.children}</Container>
			</AppShell.Main>
		</AppShell>
	);
}

export function PageFooter() {
	return (
		<Stack gap="sm" mt={56} mb={12}>
			<Divider />
			<Group justify="space-between" gap="sm" c="dimmed" fz="sm">
				<Text component="a" href="https://github.com/musi-lang/musi">
					github.com/musi-lang/musi
				</Text>
				<Text>musi-lang.com</Text>
			</Group>
		</Stack>
	);
}
