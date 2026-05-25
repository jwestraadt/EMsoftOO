#Requires -Version 5.1
<#
.SYNOPSIS
    Build the EMsoft Python Interface Sphinx documentation.

.DESCRIPTION
    Installs doc dependencies into the uv venv, runs sphinx-build,
    and optionally opens the result in the default browser.

.PARAMETER NoBrowser
    Do not open the HTML output in a browser after building.

.PARAMETER Clean
    Remove docs/_build before building (full rebuild).

.EXAMPLE
    .\Source\pyEMsoftOO\run_docs.ps1

.EXAMPLE
    .\Source\pyEMsoftOO\run_docs.ps1 -Clean -NoBrowser
#>
param(
    [switch]$NoBrowser,
    [switch]$Clean
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

$PyDir   = "$PSScriptRoot"
$DocsDir = "$PyDir\docs"
$BuildDir = "$DocsDir\_build\html"

function Write-Step([string]$msg) {
    Write-Host "`n==> $msg" -ForegroundColor Cyan
}
function Fail([string]$msg) {
    Write-Host "ERROR: $msg" -ForegroundColor Red
    exit 1
}

if (-not (Get-Command uv -ErrorAction SilentlyContinue)) {
    Fail "uv not found. Install it with:`n  powershell -ExecutionPolicy Bypass -c `"irm https://astral.sh/uv/install.ps1 | iex`""
}

Push-Location $PyDir
try {
    # ── Step 1: install doc dependencies ─────────────────────────────────────
    Write-Step "Installing documentation dependencies"
    uv pip install -e ".[docs]" --quiet

    # ── Step 2: optional clean ────────────────────────────────────────────────
    if ($Clean -and (Test-Path $BuildDir)) {
        Write-Step "Removing existing build"
        Remove-Item -Recurse -Force $BuildDir
    }

    # ── Step 3: sphinx-build ──────────────────────────────────────────────────
    Write-Step "Running sphinx-build"
    uv run sphinx-build -b html "$DocsDir" "$BuildDir"
    if ($LASTEXITCODE -ne 0) { Fail "sphinx-build failed (exit $LASTEXITCODE)" }

    Write-Host "`nBuild succeeded. Output: $BuildDir" -ForegroundColor Green

    # ── Step 4: open browser ──────────────────────────────────────────────────
    if (-not $NoBrowser) {
        $index = "$BuildDir\index.html"
        if (Test-Path $index) {
            Write-Step "Opening browser"
            Start-Process $index
        }
    }
} finally {
    Pop-Location
}
