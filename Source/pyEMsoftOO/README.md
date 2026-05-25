# EMsoft Python Interface

Python wrappers for the EMsoftOO Fortran library.

## Requirements

- Python >= 3.9
- [Intel oneAPI HPC Toolkit](https://www.intel.com/content/www/us/en/developer/tools/oneapi/hpc-toolkit.html) with `ifx` compiler
- Visual Studio 2022 (C++ workload)
- [EMsoftOO SDK](https://github.com/EMsoft-org/EMsoftSuperbuild)
- [uv](https://docs.astral.sh/uv/) (Python environment manager)

---

## Step 1 — Build the shared library on Windows 11

Open a plain `cmd.exe` (not PowerShell, not a Developer Command Prompt — the Intel
`setvars.bat` call below sets up the full toolchain).

Run the following from the root of the EMsoftOO repository:

```bat
call "C:\Program Files (x86)\Intel\oneAPI\setvars.bat" intel64 vs2022

set "SDK_ROOT=C:/path/to/EMsoftOO_SDK"

cmake -S . -B build-ifx-release -G "NMake Makefiles" ^
  -DBUILD_SHARED_LIBS=ON ^
  -DCMAKE_BUILD_TYPE=Release ^
  -DCMAKE_Fortran_COMPILER=ifx ^
  -DCMAKE_C_FLAGS_DEBUG:STRING="/MDd /Z7 /Ob0 /Od /RTC1" ^
  -DCMAKE_CXX_FLAGS_DEBUG:STRING="/MDd /Z7 /Ob0 /Od /RTC1" ^
  -DEMsoftOO_SDK=%SDK_ROOT% ^
  -DEMsoftOO_ENABLE_TESTING=OFF

cmake --build build-ifx-release
```

Replace `C:/path/to/EMsoftOO_SDK` with the actual SDK location.

> **Note on `/Z7`:** the debug-info flags `/Z7` are required on some systems where the
> default `/Zi` causes a `fatal error C1041` during CMake compiler checks.

After the build completes, `EMsoftOO_c.dll` is in:

```text
build-ifx-release\Bin\EMsoftOO_c.dll
```

---

## Step 2 — Point Python at the DLL

The bindings look for the DLL in this order:

1. `EMSOFTOO_LIB` environment variable (full path or directory)
2. `EMsoftLibraryLocation` key in `~/.config/EMsoft/EMsoftConfig.json`
3. Directory of the `emsoft` package
4. System library search path

If you have already run `EMsoftinit` and your config contains `EMsoftLibraryLocation`,
**no extra step is needed** — the DLL is found automatically.

To set the env var manually in PowerShell (useful for non-default build locations):

```powershell
$env:EMSOFTOO_LIB = "C:\path\to\EMsoftOO\build-ifx-release\Bin\EMsoftOO_c.dll"
```

---

## Step 3 — Install uv

In PowerShell:

```powershell
powershell -ExecutionPolicy Bypass -c "irm https://astral.sh/uv/install.ps1 | iex"
```

Restart your terminal so `uv` is on the PATH, then verify:

```powershell
uv --version
```

---

## Step 4 — Create the Python environment and install dependencies

From the `Source/pyEMsoftOO` directory:

```powershell
cd Source\pyEMsoftOO

# Create a virtual environment
uv venv

# Install h5py, JupyterLab, and the emsoft package itself
uv pip install h5py jupyterlab
uv pip install -e .
```

---

## Step 5 — Run JupyterLab

Still in `Source/pyEMsoftOO`, with `EMSOFTOO_LIB` set in the same shell:

```powershell
uv run jupyter lab
```

JupyterLab will open in your browser.

---

## Step 6 — Run the examples notebook

In the JupyterLab file browser, open **`examples.ipynb`**.

The notebook covers:

| Section | Topic |
|---------|-------|
| 1 | Quaternions — creation, arithmetic, vector rotation |
| 2 | Rotation representations — Euler, matrix, axis-angle, Rodrigues, homochoric, cubochoric |
| 3 | Crystallography — metric tensors, d-spacings, coordinate transforms |
| 4 | Symmetry and space groups — orbits, reflection stars, extinction rules |
| 5 | Diffraction — structure factors, extinction distances (Weickenmeier-Kohl) |
| 6 | Lambert projection — hemisphere ↔ square, stereographic comparison |
| 7 | Fundamental zone and MacKenzie misorientation distributions |
| 8 | EBSD pattern simulation from a synthetic master pattern |
| 9 | Loading `.xtal` and `.h5` master pattern files |
| 10 | Dictionary indexing — NDP matching, CI, pattern-centre conversion |
| 11 | Single-pattern PC + orientation refinement (Nelder-Mead) |

Sections 9 and 11 require real data files (`Ni.xtal`, `Ni-master-20kV.h5`).
These are referenced by bare name (e.g. `"Ni"`, `"Ni-master-20kV"`) and resolved
automatically via `EMXtalFolderpathname` and `EMdatapathname` in
`~/.config/EMsoft/EMsoftConfig.json` — no path editing needed if `EMsoftinit`
has been run and the data files are in the configured folders.

---

## Quick Start

```python
from emsoft.quaternions import Quaternion, QuaternionArray
import numpy as np

# Create quaternions
q1 = Quaternion(1, 0, 0, 0)          # identity
q2 = Quaternion(0.5, 0.5, 0.5, 0.5) # 120 deg around [111]

# Arithmetic
q3 = q1 * q2        # Hamilton product
qc = q2.conjugate() # conjugate

# Rotate a vector
v = q2.rotate([1.0, 0.0, 0.0])

# Array operations
data = np.array([[1,0,0,0], [0,1,0,0], [0,0,1,0]], dtype=np.float64)
qa = QuaternionArray(data)
rotated = qa.rotate([1.0, 0.0, 0.0])  # shape (3, 3)
```

---

## Running Tests

```powershell
uv run pytest emsoft/tests/
```
