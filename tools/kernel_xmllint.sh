#!/usr/bin/env bash
#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: BSD-2-Clause
#

xml_sources=$(find ../libsel4/arch_include/*/interfaces ../libsel4/sel4_arch_include/*/interfaces -name 'sel4arch.xml')
if [ -z "$xml_sources" ]; then
    echo "Unable to find sel4arch.xml files"
    exit 1
fi

idl_source=$(find ../libsel4/tools -name 'sel4_idl.dtd')
if [ -z "$idl_source" ]; then
    echo "Unable to find sel4_idl.dtd"
    exit 1
fi

xmllint --dtdvalid ${idl_source} --noout ${xml_sources}
