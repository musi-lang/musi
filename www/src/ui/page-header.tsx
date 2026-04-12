import { Stack, Text, Title, type TitleProps } from "@mantine/core";
import type { ReactNode } from "react";

export interface PageHeaderProps {
	eyebrow: ReactNode;
	title: ReactNode;
	description?: ReactNode;
	descriptionHtml?: string;
	actions?: ReactNode;
	titleOrder?: TitleProps["order"];
	titleSize?: TitleProps["size"];
}

export function PageHeader(props: PageHeaderProps) {
	return (
		<Stack gap="md" className="page-header">
			<div>
				<Text className="eyebrow" mb={8}>
					{props.eyebrow}
				</Text>
				<Title
					order={props.titleOrder ?? 1}
					size={props.titleSize ?? "h1"}
					className="page-header-title"
				>
					{props.title}
				</Title>
				{props.descriptionHtml ? (
					<Text
						mt="md"
						maw={760}
						className="page-copy page-header-copy"
						dangerouslySetInnerHTML={{ __html: props.descriptionHtml }}
					/>
				) : props.description ? (
					<Text mt="md" maw={760} className="page-copy page-header-copy">
						{props.description}
					</Text>
				) : null}
			</div>
			{props.actions ? (
				<div className="page-header-actions">{props.actions}</div>
			) : null}
		</Stack>
	);
}
