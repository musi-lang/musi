#!/usr/bin/env bash
set -euo pipefail

readonly MUSI_DEFAULT_REF="main"

need_cmd() {
	if ! command -v "$1" >/dev/null 2>&1; then
		echo "missing required command: $1" >&2
		exit 1
	fi
}

pick_downloader() {
	if command -v curl >/dev/null 2>&1; then
		echo "curl"
		return
	fi
	if command -v wget >/dev/null 2>&1; then
		echo "wget"
		return
	fi
	echo "missing downloader: install curl or wget" >&2
	exit 1
}

download_archive() {
	local downloader="$1"
	local url="$2"
	local output="$3"
	case "$downloader" in
		curl)
			curl --fail --location --silent --show-error "$url" --output "$output"
			;;
		wget)
			wget -qO "$output" "$url"
			;;
		*)
			echo "unsupported downloader: $downloader" >&2
			exit 1
			;;
	esac
}

main() {
	need_cmd cargo
	need_cmd tar

	local ref="${MUSI_INSTALL_REF:-$MUSI_DEFAULT_REF}"
	local archive_url="${MUSI_INSTALL_ARCHIVE_URL:-https://github.com/musi-lang/musi/archive/refs/heads/${ref}.tar.gz}"
	local downloader
	downloader="$(pick_downloader)"

	local workdir
	workdir="$(mktemp -d "${TMPDIR:-/tmp}/musi-install.XXXXXX")"
	trap 'rm -rf "$workdir"' EXIT

	local archive_path="$workdir/musi.tar.gz"
	echo "downloading Musi from $archive_url"
	download_archive "$downloader" "$archive_url" "$archive_path"

	tar -xzf "$archive_path" -C "$workdir"

	local source_dir
	source_dir="$(find "$workdir" -mindepth 1 -maxdepth 1 -type d | head -n 1)"
	if [ -z "$source_dir" ]; then
		echo "archive extraction failed: missing source directory" >&2
		exit 1
	fi

	echo "installing music"
	cargo install --locked --force --path "$source_dir/crates/music"
	echo "installing musi"
	cargo install --locked --force --path "$source_dir/crates/musi"


	echo "installed binaries to Cargo bin directory"
	echo "make sure \$HOME/.cargo/bin is on PATH"
}

main "$@"
