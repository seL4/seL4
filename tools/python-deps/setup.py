#!/usr/bin/env python3
#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: BSD-2-Clause
#

"""
Setup script for dependency metapackage.

To add a python dependency, add it to the DEPS list below.

To publish to pypitest:
python setup.py sdist upload -r pypitest

To publish to pypi:
python setup.py sdist upload -r pypi
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
    'cmake-format==0.4.5',
    'guardonce',
    'autopep8==1.4.3',
    'libarchive-c',
]

setup(
    name='sel4-deps',
    version='0.4.0',
    description='Metapackage for downloading build dependencies for the seL4 microkernel',
    url="https://sel4.systems",
    licence='BSD2',
    author='TrustworthySystems',
    author_email='Stephen.Sherratt@data61.csiro.au',
    install_requires=DEPS,
)
