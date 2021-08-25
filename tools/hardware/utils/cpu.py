#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: GPL-2.0-only
#

from typing import List

from hardware.device import WrappedNode
from hardware.fdt import FdtParser

# documentation for CPU bindings:
# https://www.kernel.org/doc/Documentation/devicetree/bindings/arm/cpus.yaml


def get_cpus(tree: FdtParser) -> List[WrappedNode]:
    ' Return a list of all the CPUs described in this device tree. '
    cpus_node = tree.get_path('/cpus')

    found_cpus = []
    for node in cpus_node:
        if node.has_prop('device_type') and node.get_prop('device_type').strings[0] == 'cpu':
            found_cpus.append(node)

    return found_cpus
