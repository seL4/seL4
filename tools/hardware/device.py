#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: GPL-2.0-only
#

from collections import OrderedDict
from typing import Any, Dict, Generator, List, Tuple, cast

import logging
import pyfdt.pyfdt

from hardware.memory import Region


class WrappedNode:
    ''' A wrapper around an underlying pyfdt node '''

    # TODO: Python 3.7 with 'from __future__ import annotations' will remove the need
    # to put 'WrappedNode' in a string.
    def __init__(self, node: pyfdt.pyfdt.FdtNode, parent: 'WrappedNode', path: str):
        self.node = node
        self.parent = parent
        self.depth = 0
        self.path = path
        self.children: Dict[str, 'WrappedNode'] = OrderedDict()
        self.props: Dict[str, pyfdt.pyfdt.FdtProperty] = {}
        for prop in node:
            if not isinstance(prop, pyfdt.pyfdt.FdtProperty):
                continue
            self.props[prop.get_name()] = prop

        if parent is not None:
            parent.add_child(self)
            self.depth = parent.depth + 1
            self.is_cpu_addressable: bool = parent.is_cpu_addressable and \
                'ranges' in self.props
        else:
            self.is_cpu_addressable = True  # root node is always cpu addressable

    def add_child(self, child: 'WrappedNode'):
        ''' Add a child to this node '''
        self.children[child.node.get_name()] = child

    def get_prop(self, name: str) -> pyfdt.pyfdt.FdtProperty:
        ''' Returns prop with given name, or throws KeyError if not found '''
        return self.props[name]

    def has_prop(self, name: str) -> bool:
        ''' Returns True if prop with given name exists, otherwise False. '''
        return name in self.props

    def get_phandle(self) -> int:
        ''' Return this node's phandle '''
        if 'phandle' not in self.props:
            return False

        return self.props['phandle'].words[0]

    def get_interrupt_parent(self) -> int:
        ''' Return this node's interrupt parent's phandle '''
        if 'interrupt-parent' not in self.props:
            return self.parent.get_interrupt_parent()
        return self.props['interrupt-parent'].words[0]

    def is_mmio_device(self) -> bool:
        ''' Returns True if this node is an MMIO device '''
        return self.is_cpu_addressable

    def recursive_get_addr_cells(self) -> int:
        ''' This returns the #address-cells of this node, or otherwise
        the parent. Note that this contravenes the spec in that
        the default is meant to be 2 if unspecified, rather than the parent's value.
        This is used by the IRQ parsing code to match Linux's behaviour. '''
        if '#address-cells' in self.props:
            return self.props['#address-cells'].words[0]
        if self.parent is None:
            return 2

        return self.parent.recursive_get_addr_cells()

    def get_addr_cells(self) -> int:
        ''' Return the number of 32-bit cells that children of this node
        use to specify addresses '''
        if '#address-cells' in self.props:
            return self.props['#address-cells'].words[0]

        # see devicetree spec v0.2, 2.3.5 "#address-cells and #size-cells"
        return 2

    def get_size_cells(self) -> int:
        ''' Return the number of 32-bit cells that children of this node
        use to specify sizes. '''
        if '#size-cells' in self.props:
            return self.props['#size-cells'].words[0]

        # see devicetree spec v0.2, 2.3.5 "#address-cells and #size-cells"
        return 1

    def get_regions(self) -> List[Region]:
        if 'reg' not in self.props:
            return []

        reg = []
        prop = list(self.props['reg'].words)
        sizes = (self.parent.get_addr_cells(), self.parent.get_size_cells())
        for r in Utils.intarray_iter(prop, sizes):
            reg.append(Region(self.parent._translate_child_address(r[0]), r[1], self))
        return reg

    def parse_address(self, array) -> int:
        ''' parse a single address from the array. will pop values from the array '''
        size = self.parent.get_addr_cells()
        return Utils.make_number(size, array)

    def get_interrupts(self, tree: 'FdtParser') -> List[int]:
        irqs = []
        if 'interrupts-extended' in self.props:
            data = list(self.props['interrupts-extended'].words)
            while len(data) > 0:
                phandle = data.pop(0)
                interrupt_parent = tree.get_irq_controller(phandle)
                irqs.append(interrupt_parent.parse_irq(self, data))
        elif 'interrupts' in self.props:
            data = list(self.props['interrupts'].words)
            interrupt_parent = tree.get_irq_controller(self.get_interrupt_parent())
            while len(data) > 0:
                irqs.append(interrupt_parent.parse_irq(self, data))
        return irqs

    def get_interrupt_affinities(self) -> List[int]:
        if not self.has_prop('interrupt-affinity'):
            return []
        return list(self.get_prop('interrupt-affinity').words)

    def visit(self, visitor: Any):
        ''' Visit this node and all its children '''
        ret = [visitor(self)]
        if ret[0] is None:
            ret = []
        for child in self.children.values():
            ret += child.visit(visitor)
        return ret

    def __iter__(self) -> Generator['WrappedNode', None, None]:
        ''' Iterate over all immediate children of this node '''
        for child in self.children.values():
            yield child

    def _translate_child_address(self, addr: int) -> int:
        ''' translate an address in this node's address space
            to the parent's address space. '''
        # see devicetree spec v0.2, 2.3.8 "ranges"
        if self.parent is None:
            # the root node does not need to do any translation of addresses
            return addr

        if 'ranges' not in self.props:
            logging.warning('cannot translate address through node ' + self.path)
            return -1

        if not isinstance(self.props['ranges'], pyfdt.pyfdt.FdtPropertyWords):
            return self.parent._translate_child_address(addr)

        addr = Utils.translate_address(self, addr)
        return self.parent._translate_child_address(addr)

    def __hash__(self):
        return hash(self.path)


class Utils:
    @staticmethod
    def translate_address(node: WrappedNode, addr: int) -> int:
        ranges_prop = node.props['ranges']
        data = list(ranges_prop.words)

        device_type = cast(pyfdt.pyfdt.FdtPropertyStrings, node.get_prop(
            'device_type')).strings[0] if node.has_prop('device_type') else None

        is_pci = device_type in ('pci', 'pciex')
        # ranges is a list of tuples with the following format:
        # <child-addr> <parent-addr> <length>
        # child-addr is self.get_addr_cells() long
        # parent-addr is self.parent.get_addr_cells() long
        # length is self.get_size_cells() long
        # the address 'child-addr' is at 'parent-addr' in the parent node.
        child_addr_cells = node.get_addr_cells()
        parent_addr_cells = node.parent.get_addr_cells()
        size_cells = node.get_size_cells()

        if is_pci:
            # PCI needs to be handled specially, see
            # https://www.devicetree.org/open-firmware/bindings/pci/pci2_1.pdf
            child_addr_cells -= 1
            # the top 32 bits for PCI devices contains configuration flags.
            # we ignore it when doing this check.
            addr &= (1 << (4 * child_addr_cells)) - 1

        while len(data) > 0:
            child_addr = Utils.make_number(child_addr_cells, data)
            parent_addr = Utils.make_number(parent_addr_cells, data)
            length = Utils.make_number(size_cells, data)

            if child_addr <= addr < (child_addr + length):
                return addr - child_addr + parent_addr
        logging.warning("Could not translate 0x{:x} at {}, not translating".format(addr, node.path))
        return addr

    @staticmethod
    def make_number(cells: int, array: List[int]) -> int:
        ret = 0
        for i in range(cells):
            ret = (ret << 32)
            ret |= array.pop(0)
        return ret

    @staticmethod
    def intarray_iter(array: List[int], sizes: Tuple[int, ...]) -> Generator[List[int], None, None]:
        i = 0
        res = []
        while len(array) > 0:
            res.append(Utils.make_number(sizes[i], array))

            i += 1
            if i >= len(sizes):
                yield res
                i = 0
                res = []
