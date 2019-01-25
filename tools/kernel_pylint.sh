#!/usr/bin/env bash

#
# Copyright 2018, Data61
# Commonwealth Scientific and Industrial Research Organisation (CSIRO)
# ABN 41 687 119 230.
#
# This software may be distributed and modified according to the terms of
# the BSD 2-Clause license. Note that NO WARRANTY is provided.
# See "LICENSE_BSD2.txt" for details.
#
# @TAG(DATA61_BSD)
#

python_sources=$(find ../tools ../manual/tools ../libsel4/tools -name '*.py')
if [[ -z $python_sources ]]; then
    echo "Unable to find python source files"
    exit 1
fi
pylintrc=$(find . -name 'pylintrc')
if [[ -z $pylintrc ]]; then
    echo "Unable to find pylintrc"
    exit 1
fi

pylint --errors-only --rcfile=${pylintrc} ${python_sources}
