CREATE TABLE IF NOT EXISTS guestbook_entries (
	id INTEGER PRIMARY KEY AUTOINCREMENT,
	name TEXT NOT NULL,
	message TEXT NOT NULL,
	website TEXT,
	locale TEXT NOT NULL,
	ip_hash TEXT NOT NULL,
	status TEXT NOT NULL DEFAULT 'published',
	created_at TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX IF NOT EXISTS guestbook_entries_status_created_at_index
	ON guestbook_entries(status, created_at DESC, id DESC);
