#!/bin/bash

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

xml_sources=$(find ../libsel4/arch_include/*/interfaces ../libsel4/sel4_arch_include/*/interfaces -name 'sel4arch.xml')
if [[ -z $xml_sources ]]; then
    echo "Unable to find sel4arch.xml files"
    exit 1
fi

idl_source=$(find ../libsel4/tools -name 'sel4_idl.dtd')
if [[ -z $idl_source ]]; then
    echo "Unable to find sel4_idl.dtd"
    exit 1
fi

xmllint --dtdvalid ${idl_source} --noout ${xml_sources}
