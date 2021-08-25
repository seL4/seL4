#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
# Copyright 2020, HENSOLDT Cyber GmbH
#
# SPDX-License-Identifier: GPL-2.0-only
#

import logging

from hardware.device import Utils, WrappedNode


class IrqController:
    ''' Base class for IRQ controllers '''

    def parse_irq(self, child, data):
        ''' Given a node and a list of 32-bit integers representing
            that node's interrupt specifier list, parse one interrupt and return
            its number. '''
        logging.warning('Not sure how to parse interrupts for "{}"'.format(self.node.path))
        # pop the right number of irq cells
        for _ in range(self.get_interrupt_cells()):
            data.pop(0)
        return -1

    def __init__(self, node: WrappedNode, tree: 'FdtParser'):
        self.node = node
        self.tree = tree

    def get_nexus_addr_cells(self) -> int:
        ''' Get the IRQ controller's address-cells '''
        if self.node.has_prop('#address-cells'):
            return self.node.get_addr_cells()
        return 0

    def get_interrupt_cells(self) -> int:
        ''' Get the IRQ controller's interrupt-cells '''
        return self.node.get_prop('#interrupt-cells').words[0]

    def __repr__(self):
        return 'IrqController(node={},kind={})'.format(self.node.path, type(self).__name__)


class InterruptNexus(IrqController):
    ''' IrqController for interrupt nexuses, which are a mechanism for
        "routing" interrupts from a child to multiple IRQ controllers. '''

    def parse_irq(self, child, data):
        # interrupt-map is a list of the following:
        # <<child unit address> <child interrupt specifier> <interrupt parent>
        #  <parent unit address> <parent interrupt specifier>>

        # "child unit address" seems to be special: the docs say one thing, but
        # Linux implements something else. We go with the Linux implementation here:
        # child unit address size is specified by '#address-cells' in the nexus node,
        # or the first '#address-cells' specified in a parent node. (note: not interrupt parent)
        # see drivers/of/irq.c, 'of_irq_parse_raw' for the implementation.
        nexus_data = list(self.node.get_prop('interrupt-map').words)

        child_addr_cells = self.node.recursive_get_addr_cells()
        child_interrupt_cells = self.get_interrupt_cells()

        # only look at the first child address.
        # note we're using our #address-cells, not the child node's,
        # so we can't just call node.get_regions()
        if child.has_prop('reg'):
            addr = Utils.make_number(child_addr_cells, list(child.get_prop('reg').words))
        else:
            addr = 0

        specifier = Utils.make_number(child_interrupt_cells, data)

        # make default address masks.
        addr_mask = (1 << (32 * child_addr_cells)) - 1
        spec_mask = (1 << (32 * child_interrupt_cells)) - 1

        if self.node.has_prop('interrupt-map-mask'):
            masks = list(self.node.get_prop('interrupt-map-mask').words)
            addr_mask = Utils.make_number(child_addr_cells, masks)
            spec_mask = Utils.make_number(child_interrupt_cells, masks)

        addr &= addr_mask
        specifier &= spec_mask

        # find matching entry in the nexus.
        ok = False
        while len(nexus_data) > 0:
            # <child unit address>
            ent_addr = Utils.make_number(child_addr_cells, nexus_data) & addr_mask
            # <child interrupt specifier>
            ent_spec = Utils.make_number(child_interrupt_cells, nexus_data) & spec_mask
            # <interrupt parent>
            controller = self.tree.get_irq_controller(nexus_data.pop(0))

            # if it matches, stop here.
            if ent_addr == addr and ent_spec == specifier:
                ok = True
                break

            # otherwise, keep going.
            cells = controller.get_nexus_addr_cells()
            cells += controller.get_interrupt_cells()

            # slice off the rest of this entry and move on.
            nexus_data = nexus_data[cells:]

        if not ok:
            logging.warning("could not find matching interrupt in nexus '{}' for address/spec {:x} {:x} (from node '{}')".format(
                self.node.path, addr, specifier, child.path))
            return -1

        return controller.parse_irq(child, nexus_data)


class ArmGic(IrqController):
    ''' parses IRQs for ARM GICs '''
    IRQ_TYPE_SPI = 0
    IRQ_TYPE_PPI = 1
    IRQ_TYPE_EXTENDED_SPI = 2
    IRQ_TYPE_EXTENDED_PPI = 3

    def parse_irq(self, child, data):
        # at least 3 cells:
        # first cell is 1 if PPI, 0 if SPI
        # second cell: PPI or SPI number
        # third cell: interrupt trigger flags, ignored by us.
        # fourth cell (gicv3 only): PPI cpu affinity, ignored for now.
        #
        cells = self.get_interrupt_cells()
        interrupt_type = data.pop(0)
        number = data.pop(0)
        cells -= 2
        while cells > 0:
            data.pop(0)
            cells -= 1

        number += 16  # SGI takes 0-15
        if interrupt_type != ArmGic.IRQ_TYPE_PPI:
            number += 16  # PPI is 16-31

        if interrupt_type != ArmGic.IRQ_TYPE_SPI and interrupt_type != ArmGic.IRQ_TYPE_PPI:
            # we don't have any boards with extended SPI/PPI interrupts, so
            # we don't support them here.
            logging.warning('Node {} has interrupt with unsupported type ({}).'.format(
                self.node.path, interrupt_type))
            return -1

        return number


class RawIrqController(IrqController):
    ''' parses IRQs of format <irq-num data...> '''

    def parse_irq(self, child, data):
        cells = self.get_interrupt_cells()
        num = data.pop(0)
        while cells > 1:
            data.pop(0)
            cells -= 1
        return num


class PassthroughIrqController(IrqController):
    ''' passes off IRQ parsing to node's interrupt-parent '''

    def parse_irq(self, child, data):
        irq_parent_ph = self.node.get_interrupt_parent()
        irq_parent = self.tree.get_irq_controller(irq_parent_ph)

        return irq_parent.parse_irq(child, data)


CONTROLLERS = {
    'arm,gic-400': ArmGic,
    'arm,cortex-a7-gic': ArmGic,
    'arm,cortex-a9-gic': ArmGic,
    'arm,cortex-a15-gic': ArmGic,
    'arm,gic-v3': ArmGic,
    'brcm,bcm2836-l1-intc': RawIrqController,
    'fsl,avic': RawIrqController,
    'fsl,imx6q-gpc': PassthroughIrqController,
    'fsl,imx6sx-gpc': PassthroughIrqController,
    'fsl,imx7d-gpc': PassthroughIrqController,
    'nvidia,tegra124-ictlr': PassthroughIrqController,
    'qcom,msm-qgic2': ArmGic,
    'ti,am33xx-intc': RawIrqController,
    'ti,omap3-intc': RawIrqController,
    'riscv,cpu-intc': RawIrqController,
    'riscv,plic0': RawIrqController,
}


def create_irq_controller(node: WrappedNode, tree: 'FdtParser'):
    if node.has_prop('interrupt-map'):
        # interrupt nexus
        return InterruptNexus(node, tree)
    elif node.has_prop('compatible'):
        # try and find a matching class that will know how to parse it
        for compat in node.get_prop('compatible').strings:
            if compat in CONTROLLERS:
                return CONTROLLERS[compat](node, tree)
    # otherwise, just return a dummy irq controller
    return IrqController(node, tree)
