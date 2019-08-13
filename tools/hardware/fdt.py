#
# Copyright 2019, Data61
# Commonwealth Scientific and Industrial Research Organisation (CSIRO)
# ABN 41 687 119 230.
#
# This software may be distributed and modified according to the terms of
# the GNU General Public License version 2. Note that NO WARRANTY is provided.
# See "LICENSE_GPLv2.txt" for details.
#
# @TAG(DATA61_GPL)
#
from typing import Any, Dict, IO, List
import pyfdt.pyfdt

from hardware.device import WrappedNode
from hardware.irq import create_irq_controller, IrqController


class FdtParser:
    ''' Parse a DTB into a python representation '''

    def __init__(self, dtb_file: IO[bytes]):
        self.fdt = pyfdt.pyfdt.FdtBlobParse(dtb_file).to_fdt()
        self.wrapped_root: WrappedNode
        self.by_phandle: Dict[int, WrappedNode] = {}
        self.by_path: Dict[str, WrappedNode] = {}
        self.irq_controllers: Dict[int, IrqController] = {}
        # initialise the "nice" representation of the tree
        self._walk()

    def _walk(self):
        ''' walk the underlying fdt, constructing wrapped nodes as we go '''
        root = self.fdt.get_rootnode()
        self.wrapped_root = WrappedNode(root, None, '/')

        # for easier parent lookups
        self.by_path = {'/': self.wrapped_root}
        for (name, node) in root.walk():
            if not isinstance(node, pyfdt.pyfdt.FdtNode):
                continue
            parent_path = name.rsplit('/', 1)[0]
            if parent_path == '':
                parent_path = '/'

            parent = self.by_path[parent_path]
            wrapped = WrappedNode(node, parent, name)
            self.by_path[name] = wrapped

            if not wrapped.get_phandle():
                continue
            self.by_phandle[wrapped.get_phandle()] = wrapped
            # We only care about interrupt controllers with phandles -
            # if an interrupt controller has no phandle, it isn't used anywhere
            # and so we can safely ignore it.
            if wrapped.has_prop('interrupt-controller') or wrapped.has_prop('interrupt-map'):
                self.irq_controllers[wrapped.get_phandle()] = create_irq_controller(wrapped, self)

    def get_phandle(self, phandle: int) -> WrappedNode:
        ''' Look up a node by phandle '''
        return self.by_phandle[phandle]

    def get_path(self, path: str) -> WrappedNode:
        ''' Look up a node by path '''
        return self.by_path.get(path, None)

    def get_irq_controller(self, phandle: int) -> IrqController:
        ''' Get an IRQ controller by phandle '''
        return self.irq_controllers[phandle]

    def lookup_alias(self, alias: str) -> str:
        ''' Look up a node by its alias '''
        alias = alias.split(':')[0]
        aliases = self.get_path('/aliases')

        if aliases is None or not aliases.has_prop(alias):
            raise KeyError('Unmatched alias {}'.format(alias))

        prop = aliases.get_prop(alias)
        return prop.strings[0]

    def get_kernel_devices(self) -> List[WrappedNode]:
        ''' Returns a list of devices that are used by the kernel '''
        chosen = self.get_path('/chosen')
        if not chosen.has_prop('seL4,kernel-devices'):
            return []

        ret = []
        paths = chosen.get_prop('seL4,kernel-devices').strings

        for path in paths:
            if path[0] != '/':
                path = self.lookup_alias(path)
            ret.append(self.get_path(path))
        return ret

    def get_boot_cpu(self) -> int:
        ''' Returns phandle of the cpu node specified by the seL4,boot-cpu property '''
        chosen = self.get_path('/chosen')
        if not chosen.has_prop('seL4,boot-cpu'):
            raise KeyError('must specify seL4,boot-cpu in /chosen to get boot cpu')

        prop = chosen.get_prop('seL4,boot-cpu')
        return prop.words[0]

    def visit(self, visitor: Any):
        ''' Walk the tree, calling the given visitor function on each node '''
        self.wrapped_root.visit(visitor)
