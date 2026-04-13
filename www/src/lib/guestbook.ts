import type { Locale } from "./site-copy";

export interface GuestbookEntry {
	id: number;
	name: string;
	message: string;
	website: string | null;
	locale: Locale;
	createdAt: string;
}

export interface GuestbookListResponse {
	submissionEnabled: boolean;
	entries: GuestbookEntry[];
}

export interface GuestbookSubmitPayload {
	name: string;
	website: string;
	message: string;
	locale: Locale;
	turnstileToken?: string;
	company?: string;
}

export interface GuestbookSubmitResponse {
	ok: boolean;
	submissionEnabled?: boolean;
	error?: string;
}

export async function fetchGuestbook(signal?: AbortSignal) {
	const requestInit: RequestInit = {
		headers: {
			accept: "application/json",
		},
	};
	if (signal) {
		requestInit.signal = signal;
	}
	const response = await fetch("/api/guestbook", {
		...requestInit,
	});
	if (!response.ok) {
		throw new Error(`guestbook get failed: ${response.status}`);
	}
	return (await response.json()) as GuestbookListResponse;
}

export async function submitGuestbook(payload: GuestbookSubmitPayload) {
	const response = await fetch("/api/guestbook", {
		method: "POST",
		headers: {
			"content-type": "application/json",
			accept: "application/json",
		},
		body: JSON.stringify(payload),
	});
	return {
		status: response.status,
		body: (await response.json()) as GuestbookSubmitResponse,
	};
}

export function guestbookDate(locale: Locale, value: string) {
	return new Intl.DateTimeFormat(locale === "ja" ? "ja-JP" : "en-US", {
		year: "numeric",
		month: "short",
		day: "numeric",
	}).format(new Date(value));
}

export function trimGuestbookPayload(payload: GuestbookSubmitPayload) {
	return {
		...payload,
		name: payload.name.trim(),
		website: payload.website.trim(),
		message: payload.message.trim(),
		company: payload.company?.trim() ?? "",
	};
}
