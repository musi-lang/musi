import { useEffect, useMemo, useRef, useState } from "react";
import {
	fetchGuestbook,
	type GuestbookEntry,
	guestbookDate,
	submitGuestbook,
	trimGuestbookPayload,
} from "../../lib/guestbook";
import { type Locale, siteCopy } from "../../lib/site-copy";
import { turnstile } from "../../lib/turnstile";
import type { AppRoute } from "../../routes";
import { InlineAction } from "../../ui/actions";
import { PageHeader } from "../../ui/page-header";
import { Surface } from "../../ui/surface";

function turnstileSiteKey() {
	return import.meta.env["VITE_TURNSTILE_SITE_KEY"]?.trim() ?? "";
}

function GuestbookSection(props: { locale: Locale }) {
	const copy = siteCopy[props.locale].community;
	const ui = siteCopy[props.locale].ui;
	const [entries, setEntries] = useState<GuestbookEntry[]>([]);
	const [loading, setLoading] = useState(true);
	const [submissionEnabled, setSubmissionEnabled] = useState(false);
	const [submitting, setSubmitting] = useState(false);
	const [feedback, setFeedback] = useState<null | {
		tone: "success" | "error";
		text: string;
	}>(null);
	const widgetRef = useRef<HTMLDivElement | null>(null);
	const widgetIdRef = useRef<string | number | null>(null);
	const tokenRef = useRef("");
	const turnstileClient = turnstile();
	const hasTurnstile = Boolean(turnstileClient);
	const siteKey = typeof document === "undefined" ? "" : turnstileSiteKey();
	const canRenderTurnstile = hasTurnstile && siteKey.length > 0;
	const websiteHint = useMemo(
		() => copy.guestbookPrivacyNote,
		[copy.guestbookPrivacyNote],
	);

	useEffect(() => {
		const controller = new AbortController();
		fetchGuestbook(controller.signal)
			.then((response) => {
				setEntries(response.entries);
				setSubmissionEnabled(response.submissionEnabled);
			})
			.catch(() => {
				setSubmissionEnabled(false);
			})
			.finally(() => {
				setLoading(false);
			});
		return () => controller.abort();
	}, []);

	useEffect(() => {
		if (
			!(canRenderTurnstile && widgetRef.current) ||
			widgetIdRef.current !== null
		) {
			return;
		}
		const currentTurnstile = turnstile();
		if (!currentTurnstile) {
			return;
		}
		widgetIdRef.current = currentTurnstile.render(widgetRef.current, {
			sitekey: siteKey,
			theme: "auto",
			callback(token: string) {
				tokenRef.current = token;
			},
			"expired-callback"() {
				tokenRef.current = "";
			},
		});
	}, [canRenderTurnstile, siteKey]);

	async function onSubmit(event: React.FormEvent<HTMLFormElement>) {
		event.preventDefault();
		const form = new FormData(event.currentTarget);
		const payload = trimGuestbookPayload({
			name: String(form.get("name") ?? ""),
			website: String(form.get("website") ?? ""),
			message: String(form.get("message") ?? ""),
			company: String(form.get("company") ?? ""),
			locale: props.locale,
			turnstileToken: tokenRef.current,
		});
		setSubmitting(true);
		setFeedback(null);
		const response = await submitGuestbook(payload).catch(() => null);
		setSubmitting(false);
		if (!response) {
			setFeedback({ tone: "error", text: copy.guestbookError });
			return;
		}
		if (!response.body.ok) {
			setFeedback({
				tone: "error",
				text:
					response.body.submissionEnabled === false
						? copy.guestbookUnavailable
						: copy.guestbookError,
			});
			return;
		}
		event.currentTarget.reset();
		tokenRef.current = "";
		if (canRenderTurnstile && widgetIdRef.current !== null) {
			turnstile()?.reset(widgetIdRef.current);
		}
		setFeedback({ tone: "success", text: copy.guestbookSuccess });
		setLoading(true);
		fetchGuestbook()
			.then((next) => {
				setEntries(next.entries);
				setSubmissionEnabled(next.submissionEnabled);
			})
			.finally(() => setLoading(false));
	}

	const canSubmit = submissionEnabled && canRenderTurnstile;

	return (
		<section className="community-grid">
			<Surface
				tone="well"
				className="section-panel section-panel-structured guestbook-panel"
			>
				<div className="section-heading-row section-heading-bar">
					<div>
						<div className="eyebrow">{copy.guestbookEyebrow}</div>
						<h2>{copy.guestbookTitle}</h2>
						<p className="muted">{copy.guestbookDescription}</p>
					</div>
				</div>
				{canSubmit ? (
					<form
						className="guestbook-form"
						aria-label={ui.guestbookForm}
						onSubmit={onSubmit}
					>
						<label className="field-row">
							<span>{copy.guestbookNameLabel}</span>
							<input
								name="name"
								type="text"
								maxLength={40}
								required={true}
								placeholder={copy.guestbookNamePlaceholder}
							/>
						</label>
						<label className="field-row">
							<span>{copy.guestbookWebsiteLabel}</span>
							<input
								name="website"
								type="url"
								maxLength={160}
								placeholder={copy.guestbookWebsitePlaceholder}
							/>
						</label>
						<label className="field-row field-row-wide">
							<span>{copy.guestbookMessageLabel}</span>
							<textarea
								name="message"
								rows={5}
								maxLength={500}
								required={true}
								placeholder={copy.guestbookMessagePlaceholder}
							/>
						</label>
						<input
							className="guestbook-honeypot"
							name="company"
							type="text"
							tabIndex={-1}
							autoComplete="off"
							aria-hidden="true"
						/>
						<div className="guestbook-meta-row">
							<p className="muted">{websiteHint}</p>
							<div className="guestbook-actions">
								<div ref={widgetRef} className="guestbook-turnstile" />
								<button
									type="submit"
									className="button button-primary"
									disabled={submitting}
								>
									{submitting
										? `${copy.guestbookSubmit}...`
										: copy.guestbookSubmit}
								</button>
							</div>
						</div>
						{feedback ? (
							<p
								className={`status-note status-note-${feedback.tone}`}
								role="status"
							>
								{feedback.text}
							</p>
						) : null}
					</form>
				) : (
					<p className="muted">{copy.guestbookReadonly}</p>
				)}
			</Surface>
			<Surface
				tone="raised"
				className="section-panel section-panel-structured guestbook-panel"
			>
				<div className="section-heading-row section-heading-bar">
					<div>
						<div className="eyebrow">{copy.guestbookEyebrow}</div>
						<h2>{ui.guestbookEntries}</h2>
					</div>
				</div>
				<div className="guestbook-list">
					{loading ? <p className="muted">{copy.guestbookLoading}</p> : null}
					{loading || canSubmit ? null : (
						<p className="muted">{copy.guestbookReadonly}</p>
					)}
					{!loading && entries.length === 0 ? (
						<p className="muted">{copy.guestbookEmpty}</p>
					) : null}
					{entries.map((entry) => (
						<article key={entry.id} className="guestbook-entry">
							<div className="guestbook-entry-header">
								<strong>{entry.name}</strong>
								<time dateTime={entry.createdAt}>
									{guestbookDate(props.locale, entry.createdAt)}
								</time>
							</div>
							<p>{entry.message}</p>
							{entry.website ? (
								<a
									href={entry.website}
									rel="noreferrer"
									target="_blank"
									className="inline-action"
								>
									{entry.website}
								</a>
							) : null}
						</article>
					))}
				</div>
			</Surface>
		</section>
	);
}

export function CommunityPage(props: { route: AppRoute }) {
	const copy = siteCopy[props.route.locale].community;
	return (
		<div className="page-stack">
			<PageHeader
				eyebrow={copy.eyebrow}
				title={copy.title}
				description={copy.description}
			/>
			<Surface tone="well" className="section-panel section-panel-structured">
				<div className="section-heading-row section-heading-bar">
					<div>
						<div className="eyebrow">{copy.eyebrow}</div>
						<h2>{siteCopy[props.route.locale].ui.communityLinks}</h2>
					</div>
				</div>
				<section
					className="portal-grid portal-grid-compact"
					aria-label={siteCopy[props.route.locale].ui.communityLinks}
				>
					{copy.sections.map((section) => (
						<Surface
							key={section.title}
							tone="raised"
							className="portal-card portal-card-raised"
						>
							<div className="eyebrow">{section.title}</div>
							<p>{section.copy}</p>
							<InlineAction href={section.href}>{section.label}</InlineAction>
						</Surface>
					))}
				</section>
			</Surface>
			<GuestbookSection locale={props.route.locale} />
		</div>
	);
}
