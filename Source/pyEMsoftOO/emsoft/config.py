"""
EMsoftOO configuration reader.

Reads ~/.config/EMsoft/EMsoftConfig.json (written by EMsoftinit) and exposes
helper functions for resolving crystal-structure and data-file paths.

Resolution order for every public helper:
  1. Argument is already an existing file  →  use it as-is
  2. Config-derived directory + argument (+ extension if missing)  →  use it
  3. Return the original argument unchanged (let the caller raise the I/O error)
"""

import json
import os
from pathlib import Path

_config_cache = None
_SENTINEL = object()


def _config_path():
    return Path.home() / ".config" / "EMsoft" / "EMsoftConfig.json"


def get_config():
    """Return the parsed EMsoftConfig.json as a dict, or {} if not found."""
    global _config_cache
    if _config_cache is None:
        p = _config_path()
        if p.is_file():
            with open(p, "r", encoding="utf-8") as fh:
                _config_cache = json.load(fh)
        else:
            _config_cache = {}
    return _config_cache


def get_xtal_dir():
    """Return the crystal-structure folder from the config, or None."""
    d = get_config().get("EMXtalFolderpathname")
    return Path(d) if d else None


def get_data_dir():
    """Return the EMsoft data folder from the config, or None."""
    d = get_config().get("EMdatapathname")
    return Path(d) if d else None


def get_lib_dir():
    """Return the EMsoft library location from the config, or None."""
    d = get_config().get("EMsoftLibraryLocation")
    return Path(d) if d else None


def resolve_xtal(name):
    """Resolve *name* to an absolute path to a .xtal file.

    Accepts:
      - A full absolute or relative path (returned unchanged if the file exists)
      - A bare material name like ``"Ni"`` or ``"Ni.xtal"``

    Lookup order:
      1. ``name`` itself (if it already points to an existing file)
      2. ``EMXtalFolderpathname / name``  (with ``.xtal`` appended if needed)
    """
    p = Path(name)
    if p.is_file():
        return str(p)

    xtal_dir = get_xtal_dir()
    if xtal_dir is not None:
        stem = p.name
        # Try with .xtal extension, then without (user may have passed "Ni.xtal")
        for candidate in (xtal_dir / stem, xtal_dir / (stem + ".xtal")):
            if candidate.is_file():
                return str(candidate)

    # Fall through: return original so the caller's open() raises a clear error
    return str(p)


def resolve_data(name, suffixes=(".h5", ".hdf5")):
    """Resolve *name* to an absolute path to a data file.

    Accepts:
      - A full path (returned unchanged if the file exists)
      - A bare name like ``"Ni-master-20kV"`` or ``"Ni-master-20kV.h5"``

    Lookup order:
      1. ``name`` itself
      2. ``EMdatapathname / name``  (direct child, tries each suffix when no
         extension is given)
      3. Recursive search under ``EMdatapathname`` for the filename
    """
    p = Path(name)
    if p.is_file():
        return str(p)

    data_dir = get_data_dir()
    if data_dir is not None:
        stem = p.name
        # Build filenames to look for (with and without explicit suffixes)
        filenames = [stem] if p.suffix else [stem + s for s in suffixes] + [stem]

        # 1. Partial relative path under data_dir (e.g. "TestWindows/Ni-master-20kV")
        #    Try the path as-is and with each suffix appended
        rel_candidates = [data_dir / p] if p.suffix else [
            data_dir / Path(str(p) + s) for s in suffixes
        ] + [data_dir / p]
        for candidate in rel_candidates:
            if candidate.is_file():
                return str(candidate)

        # 2. Direct child of data_dir (bare filename, no subdir)
        for fname in filenames:
            candidate = data_dir / fname
            if candidate.is_file():
                return str(candidate)

        # 3. Recursive search under data_dir (subdirectories like TestWindows/)
        for fname in filenames:
            matches = sorted(data_dir.rglob(fname))
            if matches:
                return str(matches[0])

    return str(p)
