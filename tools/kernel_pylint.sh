#!/usr/bin/env bash
#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: BSD-2-Clause
#

python_sources=$(find ../tools ../manual/tools ../libsel4/tools -name '*.py')
if [ -z "$python_sources" ]; then
    echo "Unable to find python source files"
    exit 1
fi
pylintrc=$(find . -name 'pylintrc')
if [ -z "$pylintrc" ]; then
    echo "Unable to find pylintrc"
    exit 1
fi

pylint --errors-only --rcfile=${pylintrc} ${python_sources}
