"""
Shared library loader for the EMsoftOO C-interop layer.
"""

import ctypes
import os
import sys

_lib = None


def _find_library():
    """Locate and load the libEMsoftOO_c shared library."""
    # Library name varies by platform
    if sys.platform == 'darwin':
        libname = 'libEMsoftOO_c.dylib'
    elif sys.platform == 'win32':
        libname = 'EMsoftOO_c.dll'
    else:
        libname = 'libEMsoftOO_c.so'

    # Search order:
    # 1. EMSOFTOO_LIB environment variable (file path or directory)
    # 2. EMsoftLibraryLocation from EMsoftConfig.json
    # 3. Same directory as this file
    # 4. ../lib relative to this file
    # 5. Standard library paths (let ctypes search)
    search_paths = []

    env_path = os.environ.get('EMSOFTOO_LIB')
    if env_path:
        # Accept either a full file path or a directory
        if os.path.isfile(env_path):
            search_paths.append(env_path)
        elif os.path.isdir(env_path):
            search_paths.append(os.path.join(env_path, libname))

    # Fall back to the path recorded in EMsoftConfig.json
    try:
        from emsoft.config import get_lib_dir
        lib_dir = get_lib_dir()
        if lib_dir is not None:
            search_paths.append(str(lib_dir / libname))
    except Exception:
        pass

    this_dir = os.path.dirname(os.path.abspath(__file__))
    search_paths.append(os.path.join(this_dir, libname))
    search_paths.append(os.path.join(this_dir, '..', 'lib', libname))
    search_paths.append(os.path.join(this_dir, '..', '..', 'lib', libname))

    # Try each candidate path
    for path in search_paths:
        if os.path.isfile(path):
            try:
                return ctypes.CDLL(path)
            except OSError as e:
                raise OSError(
                    f"Found {path} but failed to load it:\n  {e}\n\n"
                    f"This usually means a dependent library (e.g. Fortran runtime) "
                    f"cannot be found.\nOn macOS, try:\n"
                    f"  export DYLD_LIBRARY_PATH=/path/to/EMsoftOO_SDK/lib:$DYLD_LIBRARY_PATH"
                ) from e

    # Fall back to system search
    try:
        return ctypes.CDLL(libname)
    except OSError:
        searched = '\n  '.join(search_paths) if search_paths else '(none)'
        raise OSError(
            f"Could not find {libname}.\n"
            f"Either set the EMSOFTOO_LIB environment variable to the full path of the\n"
            f"shared library or its directory, or set 'EMsoftLibraryLocation' in\n"
            f"~/.config/EMsoft/EMsoftConfig.json (run EMsoftinit to create it).\n"
            f"Example (PowerShell): $env:EMSOFTOO_LIB = 'C:\\...\\build-ifx-release\\Bin\\{libname}'\n"
            f"Searched:\n  {searched}"
        )


def get_lib():
    """Return the loaded shared library, loading it on first call."""
    global _lib
    if _lib is None:
        _lib = _find_library()
    return _lib
