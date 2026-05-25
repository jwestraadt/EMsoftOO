#Requires -Version 5.1
<#
.SYNOPSIS
    Build the EMsoftOO_c.dll and set up the Python environment.

.DESCRIPTION
    1. Configures and builds the C-interop shared library (EMsoftOO_c.dll)
       using Intel ifx + VS2022 via NMake.
    2. Creates a uv virtual environment in Source/pyEMsoftOO/.venv.
    3. Installs h5py, jupyterlab, and the emsoft package (editable).
    4. Verifies the import works.

.PARAMETER SdkRoot
    Path to the EMsoftOO SDK directory (required).

.PARAMETER BuildDir
    Name of the build output directory relative to the repo root.
    Default: build-ifx-release

.PARAMETER SkipBuild
    Skip the cmake configure+build step (use if the DLL is already built).

.EXAMPLE
    .\Source\pyEMsoftOO\run_pyEMsoftOO.ps1 -SdkRoot C:\EMsoftOO_SDK

.EXAMPLE
    .\Source\pyEMsoftOO\run_pyEMsoftOO.ps1 -SdkRoot C:\EMsoftOO_SDK -BuildDir my-build -SkipBuild
#>
param(
    [Parameter(Mandatory = $false)]
    [string]$SdkRoot = "",

    [string]$BuildDir = "build-ifx-release",

    [switch]$SkipBuild
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

$RepoRoot = (Resolve-Path "$PSScriptRoot\..\..").Path
$PyDir    = "$RepoRoot\Source\pyEMsoftOO"

# ── helpers ──────────────────────────────────────────────────────────────────
function Write-Step([string]$msg) {
    Write-Host "`n==> $msg" -ForegroundColor Cyan
}
function Fail([string]$msg) {
    Write-Host "ERROR: $msg" -ForegroundColor Red
    exit 1
}

# ── Step 1: read EMsoftConfig.json for defaults ───────────────────────────────
$ConfigPath = "$env:USERPROFILE\.config\EMsoft\EMsoftConfig.json"
$Config = @{}
if (Test-Path $ConfigPath) {
    $Config = Get-Content $ConfigPath -Raw | ConvertFrom-Json
    Write-Host "Loaded EMsoftConfig.json" -ForegroundColor Green
} else {
    Write-Host "Warning: EMsoftConfig.json not found at $ConfigPath" -ForegroundColor Yellow
    Write-Host "         Run EMsoftinit after the build to create it." -ForegroundColor Yellow
}

# ── Step 2: cmake configure + build ──────────────────────────────────────────
if (-not $SkipBuild) {
    Write-Step "Configuring and building EMsoftOO_c.dll"

    if (-not $SdkRoot) {
        # Try to read SDK location from config
        if ($Config.PSObject.Properties["EMsoftOO_SDK"]) {
            $SdkRoot = $Config.EMsoftOO_SDK
        } else {
            Fail "Provide -SdkRoot <path> or set EMsoftOO_SDK in EMsoftConfig.json"
        }
    }

    $setvars = "C:\Program Files (x86)\Intel\oneAPI\setvars.bat"
    if (-not (Test-Path $setvars)) {
        Fail "Intel oneAPI setvars.bat not found at:`n  $setvars`nInstall the Intel oneAPI HPC Toolkit."
    }

    $cmakeArgs = @(
        "-S", ".",
        "-B", $BuildDir,
        "-G", "NMake Makefiles",
        "-DBUILD_SHARED_LIBS=ON",
        "-DCMAKE_BUILD_TYPE=Release",
        "-DCMAKE_Fortran_COMPILER=ifx",
        '-DCMAKE_C_FLAGS_DEBUG:STRING="/MDd /Z7 /Ob0 /Od /RTC1"',
        '-DCMAKE_CXX_FLAGS_DEBUG:STRING="/MDd /Z7 /Ob0 /Od /RTC1"',
        "-DEMsoftOO_SDK=$($SdkRoot -replace '\\','/')",
        "-DEMsoftOO_ENABLE_TESTING=OFF"
    )

    $cmdLine = "call `"$setvars`" intel64 vs2022 && cmake $($cmakeArgs -join ' ') && cmake --build $BuildDir"
    Write-Host "Running in cmd.exe subshell..." -ForegroundColor DarkGray

    $result = cmd /c $cmdLine
    if ($LASTEXITCODE -ne 0) { Fail "cmake build failed (exit $LASTEXITCODE)" }
    Write-Host $result
}

# ── Step 3: locate the DLL ────────────────────────────────────────────────────
Write-Step "Locating EMsoftOO_c.dll"

$dll = "$RepoRoot\$BuildDir\Bin\EMsoftOO_c.dll"
if (-not (Test-Path $dll)) {
    # Fall back to config
    if ($Config.PSObject.Properties["EMsoftLibraryLocation"]) {
        $dll = "$($Config.EMsoftLibraryLocation)\EMsoftOO_c.dll"
    }
}
if (-not (Test-Path $dll)) {
    Fail "Cannot find EMsoftOO_c.dll.`nExpected: $RepoRoot\$BuildDir\Bin\EMsoftOO_c.dll`nSet EMSOFTOO_LIB or update EMsoftConfig.json."
}
Write-Host "Found DLL: $dll" -ForegroundColor Green

# ── Step 4: uv venv + install ─────────────────────────────────────────────────
Write-Step "Setting up Python environment with uv"

if (-not (Get-Command uv -ErrorAction SilentlyContinue)) {
    Fail "uv not found. Install it with:`n  powershell -ExecutionPolicy Bypass -c `"irm https://astral.sh/uv/install.ps1 | iex`""
}

Push-Location $PyDir
try {
    uv venv --quiet
    uv pip install h5py jupyterlab --quiet
    uv pip install -e . --quiet
} finally {
    Pop-Location
}

# ── Step 5: verify import ─────────────────────────────────────────────────────
Write-Step "Verifying emsoft import"

$env:EMSOFTOO_LIB = $dll
Push-Location $PyDir
try {
    $check = uv run python -c "import emsoft; print('emsoft', emsoft.__version__, 'loaded OK')" 2>&1
    if ($LASTEXITCODE -ne 0) { Fail "Import check failed:`n$check" }
    Write-Host $check -ForegroundColor Green
} finally {
    Pop-Location
}

Write-Host @"

Done.  To start working:

  cd $PyDir
  `$env:EMSOFTOO_LIB = '$dll'
  uv run jupyter lab

"@ -ForegroundColor Cyan
