interface GuestbookStatement {
	all<T>(): Promise<{ results: T[] }>;
	run(): Promise<unknown>;
	bind(...values: unknown[]): GuestbookStatement;
}

interface GuestbookEnv {
	GUESTBOOK_DB?: {
		prepare(query: string): GuestbookStatement;
	};
	TURNSTILE_SECRET?: string;
}

interface GuestbookRow {
	id: number;
	name: string;
	message: string;
	website: string | null;
	locale: string;
	created_at: string;
}

const WHITESPACE_RE = /\s+/g;
const HTTP_URL_RE = /^https?:\/\//;

function json(body: unknown, status = 200) {
	return new Response(JSON.stringify(body), {
		status,
		headers: {
			"content-type": "application/json; charset=utf-8",
			"cache-control": "no-store",
		},
	});
}

function submissionEnabled(env: GuestbookEnv) {
	return Boolean(env.GUESTBOOK_DB && env.TURNSTILE_SECRET);
}

function cleanText(value: unknown, maxLength: number) {
	return String(value ?? "")
		.trim()
		.replace(WHITESPACE_RE, " ")
		.slice(0, maxLength);
}

function cleanMessage(value: unknown) {
	return String(value ?? "")
		.trim()
		.slice(0, 500);
}

function cleanWebsite(value: unknown) {
	const text = String(value ?? "")
		.trim()
		.slice(0, 160);
	if (text.length === 0) {
		return null;
	}
	if (!HTTP_URL_RE.test(text)) {
		return null;
	}
	return text;
}

async function hashValue(value: string) {
	const bytes = new TextEncoder().encode(value);
	const digest = await crypto.subtle.digest("SHA-256", bytes);
	return [...new Uint8Array(digest)]
		.slice(0, 16)
		.map((byte) => byte.toString(16).padStart(2, "0"))
		.join("");
}

async function verifyTurnstile(
	request: Request,
	env: GuestbookEnv,
	token: string,
) {
	if (!env.TURNSTILE_SECRET) {
		return true;
	}
	if (!token) {
		return false;
	}
	const ip = request.headers.get("CF-Connecting-IP") ?? "";
	const form = new FormData();
	form.set("secret", env.TURNSTILE_SECRET);
	form.set("response", token);
	if (ip) {
		form.set("remoteip", ip);
	}
	const response = await fetch(
		"https://challenges.cloudflare.com/turnstile/v0/siteverify",
		{
			method: "POST",
			body: form,
		},
	);
	if (!response.ok) {
		return false;
	}
	const payload = (await response.json()) as { success?: boolean };
	return payload.success === true;
}

export async function onRequestGet(context: { env: GuestbookEnv }) {
	if (!context.env.GUESTBOOK_DB) {
		return json({ submissionEnabled: false, entries: [] });
	}
	const result = await context.env.GUESTBOOK_DB.prepare(
		`SELECT id, name, message, website, locale, created_at
		 FROM guestbook_entries
		 WHERE status = 'published'
		 ORDER BY created_at DESC, id DESC
		 LIMIT 50`,
	).all<GuestbookRow>();
	return json({
		submissionEnabled: submissionEnabled(context.env),
		entries: result.results.map((entry: GuestbookRow) => ({
			id: entry.id,
			name: entry.name,
			message: entry.message,
			website: entry.website,
			locale: entry.locale,
			createdAt: entry.created_at,
		})),
	});
}

export async function onRequestPost(context: {
	request: Request;
	env: GuestbookEnv;
}) {
	if (!context.env.GUESTBOOK_DB) {
		return json(
			{ ok: false, submissionEnabled: false, error: "database_unavailable" },
			503,
		);
	}
	if (!context.env.TURNSTILE_SECRET) {
		return json(
			{ ok: false, submissionEnabled: false, error: "turnstile_unconfigured" },
			503,
		);
	}
	const body = (await context.request.json().catch(() => null)) as Record<
		string,
		unknown
	> | null;
	if (!body) {
		return json({ ok: false, error: "invalid_json" }, 400);
	}
	if (cleanText(body.company, 120).length > 0) {
		return json({ ok: true });
	}
	const name = cleanText(body.name, 40);
	const message = cleanMessage(body.message);
	const website = cleanWebsite(body.website);
	const locale = cleanText(body.locale, 2) === "ja" ? "ja" : "en";
	const turnstileToken = cleanText(body.turnstileToken, 2048);
	if (name.length === 0 || message.length === 0) {
		return json({ ok: false, error: "invalid_payload" }, 400);
	}
	const passedTurnstile = await verifyTurnstile(
		context.request,
		context.env,
		turnstileToken,
	);
	if (!passedTurnstile) {
		return json({ ok: false, error: "turnstile_failed" }, 400);
	}
	const ip = context.request.headers.get("CF-Connecting-IP") ?? "unknown";
	const ipHash = await hashValue(ip);
	await context.env.GUESTBOOK_DB.prepare(
		`INSERT INTO guestbook_entries
			(name, message, website, locale, ip_hash, status)
		 VALUES (?, ?, ?, ?, ?, 'published')`,
	)
		.bind(name, message, website, locale, ipHash)
		.run();
	return json({ ok: true, submissionEnabled: true });
}
