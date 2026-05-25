import os
import sys
from pathlib import Path

# Point sys.path at Source/pyEMsoftOO so autodoc can import emsoft
sys.path.insert(0, str(Path(__file__).parents[1]))

# Stub out the C library so autodoc works without the DLL present
import unittest.mock as mock
sys.modules.setdefault('emsoft._lib', mock.MagicMock())

project = 'EMsoft Python Interface'
copyright = '2013-2025, Marc De Graef Research Group/Carnegie Mellon University'
author = "Marc De Graef's Research Group"
release = '6.0.0'

extensions = [
    'sphinx.ext.autodoc',
    'sphinx.ext.napoleon',
    'sphinx.ext.viewcode',
    'sphinx.ext.intersphinx',
    'sphinx.ext.autosummary',
    'nbsphinx',
]

nbsphinx_execute = 'never'

autosummary_generate = True
autodoc_member_order = 'bysource'
autodoc_typehints = 'description'
napoleon_google_docstring = True
napoleon_numpy_docstring = False

intersphinx_mapping = {
    'python': ('https://docs.python.org/3', None),
    'numpy': ('https://numpy.org/doc/stable', None),
}

templates_path = ['_templates']
exclude_patterns = [
    '_build', 'Thumbs.db', '.DS_Store',
    # Legacy f90wrap-based docs — not relevant to the current package
    'pyEMsoft.rst',
    'Modules',
]

html_theme = 'sphinx_rtd_theme'
html_static_path = ['_static']
html_theme_options = {
    'navigation_depth': 4,
    'titles_only': False,
}
