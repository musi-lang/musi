$ErrorActionPreference = "Stop"

$ref = if ($env:MUSI_INSTALL_REF) { $env:MUSI_INSTALL_REF } else { "main" }
$archiveUrl = if ($env:MUSI_INSTALL_ARCHIVE_URL) {
	$env:MUSI_INSTALL_ARCHIVE_URL
} else {
	"https://github.com/musi-lang/musi/archive/refs/heads/$ref.zip"
}

if (-not (Get-Command cargo -ErrorAction SilentlyContinue)) {
	throw "missing required command: cargo"
}

$tempRoot = Join-Path ([System.IO.Path]::GetTempPath()) ("musi-install-" + [System.Guid]::NewGuid().ToString("N"))
$archivePath = Join-Path $tempRoot "musi.zip"

New-Item -ItemType Directory -Path $tempRoot | Out-Null
try {
	Write-Host "downloading Musi from $archiveUrl"
	Invoke-WebRequest -Uri $archiveUrl -OutFile $archivePath
	Expand-Archive -Path $archivePath -DestinationPath $tempRoot -Force

	$sourceDir = Get-ChildItem -Path $tempRoot -Directory |
		Where-Object { $_.Name -notlike '.*' } |
		Select-Object -First 1
	if (-not $sourceDir) {
		throw "archive extraction failed: missing source directory"
	}

	Write-Host "installing music"
	cargo install --locked --force --path (Join-Path $sourceDir.FullName "crates/music")
	Write-Host "installing musi"
	cargo install --locked --force --path (Join-Path $sourceDir.FullName "crates/musi")

	Write-Host "installed binaries to Cargo bin directory"
	Write-Host "make sure $HOME\\.cargo\\bin is on PATH"
} finally {
	if (Test-Path $tempRoot) {
		Remove-Item -Recurse -Force $tempRoot
	}
}
