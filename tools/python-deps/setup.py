#!/usr/bin/env python3
#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: BSD-2-Clause
#

"""
Setup script for dependency metapackage.

To add a python dependency, add it to the DEPS list below.

To publish using these instructions, you need the virtualenv package installed,
and a ~/.pypirc file with authentication setup for testpypi and pypi.

To publish to pypitest:
python3 -m build
twine upload -r testpypi dist/*

To publish to pypi:
python3 -m build
twine upload -r pypi dist/*
"""

from setuptools import setup

DEPS = [
    'six',
    'future',
    'jinja2',
    'lxml',
    'ply',
    'psutil',
    'bs4',
    'pyelftools',
    'sh',
    'pexpect',
    'pyyaml>=5.1',
    'jsonschema',
    'pyfdt',
    'cmake-format==0.6.13',
    'guardonce',
    'autopep8==2.3.2',
    'libarchive-c',
]

setup(
    name='sel4-deps',
    version='0.7.0',
    description='Metapackage for downloading build dependencies for the seL4 microkernel',
    long_description="""
This meta package depends on all python packages you need to build the seL4 microkernel and manual.

This package is maintained on <https://github.com/seL4/seL4>,
in directory <https://github.com/seL4/seL4/tree/master/tools/python-deps>
""",
    long_description_content_type="text/markdown",
    url="https://sel4.systems",
    licence='BSD2',
    author='TrustworthySystems',
    author_email='pypi@trustworthy.systems',
    install_requires=DEPS,
    python_requires='>=3'
)
