Installation
============

.. role:: bat(code)
   :language: bat

.. role:: powershell(code)
   :language: powershell

Prerequisites
-------------

- `Intel oneAPI HPC Toolkit <https://www.intel.com/content/www/us/en/developer/tools/oneapi/hpc-toolkit.html>`_
  with the ``ifx`` Fortran compiler
- Visual Studio 2022 (C++ Desktop workload)
- `EMsoftOO SDK <https://github.com/EMsoft-org/EMsoftSuperbuild>`_
- `uv <https://docs.astral.sh/uv/>`_ Python environment manager

Step 1 — Build the shared library on Windows 11
------------------------------------------------

Open a plain ``cmd.exe`` (not PowerShell, not a Developer Command Prompt — the
Intel ``setvars.bat`` call below sets up the full toolchain).

Run the following from the **root of the EMsoftOO repository**:

.. code-block:: bat

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

Replace ``C:/path/to/EMsoftOO_SDK`` with the actual SDK location.

.. note::

   The ``/Z7`` debug-info flags are required on some systems where the default
   ``/Zi`` causes a ``fatal error C1041`` during CMake compiler checks.

After the build, the library is at::

    build-ifx-release\Bin\EMsoftOO_c.dll

Step 2 — Point Python at the DLL
---------------------------------

The bindings search for the DLL in this order:

1. ``EMSOFTOO_LIB`` environment variable (full path or directory)
2. ``EMsoftLibraryLocation`` key in ``~/.config/EMsoft/EMsoftConfig.json``
3. Directory of the ``emsoft`` package itself
4. System library search path

If you have run ``EMsoftinit`` and your config contains ``EMsoftLibraryLocation``,
**no extra step is needed**.

To set the variable manually in PowerShell:

.. code-block:: powershell

    $env:EMSOFTOO_LIB = "C:\path\to\EMsoftOO\build-ifx-release\Bin\EMsoftOO_c.dll"

Step 3 — Install uv
--------------------

.. code-block:: powershell

    powershell -ExecutionPolicy Bypass -c "irm https://astral.sh/uv/install.ps1 | iex"

Restart your terminal, then verify:

.. code-block:: powershell

    uv --version

Step 4 — Create the Python environment
----------------------------------------

From the ``Source/pyEMsoftOO`` directory:

.. code-block:: powershell

    cd Source\pyEMsoftOO

    uv venv
    uv pip install h5py jupyterlab
    uv pip install -e .

Step 5 — Run JupyterLab
------------------------

With ``EMSOFTOO_LIB`` set (or auto-resolved via the config file):

.. code-block:: powershell

    uv run jupyter lab

Step 6 — Run the examples notebook
------------------------------------

In the JupyterLab file browser open **examples.ipynb**.
See :doc:`../notebooks/examples` for the full rendered notebook.

Crystal-structure (``.xtal``) and master-pattern (``.h5``) files are resolved
automatically from ``EMXtalFolderpathname`` and ``EMdatapathname`` in
``~/.config/EMsoft/EMsoftConfig.json`` — no path editing required as long as
``EMsoftinit`` has been run and the data files are in the configured folders.

Building the Documentation
--------------------------

.. code-block:: powershell

    cd Source\pyEMsoftOO
    uv pip install -e ".[docs]"
    uv run sphinx-build -b html docs docs/_build/html
    start docs\_build\html\index.html

Or using ``make.bat`` from inside the ``docs/`` folder:

.. code-block:: powershell

    cd Source\pyEMsoftOO\docs
    uv run make html

Running Tests
-------------

.. code-block:: powershell

    uv run pytest emsoft/tests/
