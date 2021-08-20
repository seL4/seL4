#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: GPL-2.0-only
#

''' generate a c header file from the device tree '''
import argparse
import builtins

from jinja2 import Environment, BaseLoader

from hardware import config, fdt
from hardware.utils import memory, rule

HEADER_TEMPLATE = '''/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

/*
 * This file is autogenerated by kernel/tools/hardware_gen.py.
 */

#pragma once

#include <linker.h>

#ifndef KDEV_BASE
#include <mode/hardware.h>
#endif

#define physBase {{ "0x{:x}".format(physBase) }}

/* INTERRUPTS */
{% for irq in kernel_irqs %}
/* {{ irq.desc }} */
{% if irq.has_enable() %}
{{ irq.get_enable_macro_str() }}
{% endif %}
{% if irq.has_sel() %}
{{ irq.get_sel_macro_str() }}
{% endif %}
#define {{ irq.label }} {{ irq.irq }}
{% if irq.has_sel() %}
#else
#define {{ irq.label }} {{ irq.false_irq }}
{{ irq.get_sel_endif() }}
{% endif %}
{% if irq.has_enable() %}
{{ irq.get_enable_endif() }}
{% endif %}

{% endfor -%}

/* KERNEL DEVICES */
{% for (addr, macro) in sorted(kernel_macros.items()) %}
#define {{ macro }} (KDEV_BASE + {{ "0x{:x}".format(addr) }})
{% endfor %}

#ifndef __ASSEMBLER__
{% if len(kernel_regions) > 0 %}
static const kernel_frame_t BOOT_RODATA kernel_devices[] = {
    {% for group in kernel_regions %}
    {% if group.has_macro() %}
    {{ group.get_macro() }}
    {% endif %}
    /* {{ group.get_desc() }} */
    {% for reg in group.regions %}
    {
        {% set map_addr = group.get_map_offset(reg) %}
        {{ "0x{:x}".format(reg.base) }},
        {% if map_addr in kernel_macros %}
        {{ kernel_macros[map_addr] }},
        {% else %}
        /* contains {{ ', '.join(group.labels.keys()) }} */
        KDEV_BASE + {{ "0x{:x}".format(map_addr) }},
        {% endif %}
        {% if args.arch == 'arm' %}
        true, /* armExecuteNever */
        {% endif %}
        {% if group.user_ok %}
        true, /* userAvailable */
        {% else %}
        false, /* userAvailable */
        {% endif %}
    },
    {% endfor %}
    {% if group.has_macro() %}
    {{ group.get_endif() }}
    {% endif %}
    {% endfor %}
};
{% else %}
static const kernel_frame_t BOOT_RODATA *const kernel_devices = NULL;
{% endif %}

/* PHYSICAL MEMORY */
static const p_region_t BOOT_RODATA avail_p_regs[] = {
    {% for reg in physical_memory %}
    { {{ "0x{:x}".format(reg.base) }}, {{ "0x{:x}".format(reg.base + reg.size) }} }, /* {{reg.owner.path}} */
    {% endfor %}
};

#endif /* !__ASSEMBLER__ */

'''


def get_kernel_devices(tree: fdt.FdtParser, rules: rule.HardwareYaml):
    ''' Given a device tree and a set of rules, returns a tuple (groups, offsets).

        Groups is a list of 'KernelRegionGroups', each of which represents a single contiguous region of memory that is associated with a device.
        Offsets is a dict of offset -> label, where label is the name given to the kernel for that address (e.g. SERIAL_PPTR) and offset is the offset from KDEV_BASE at which it's mapped.'''
    kernel_devices = tree.get_kernel_devices()

    kernel_offset = 0
    groups = []
    for dev in kernel_devices:
        dev_rule = rules.get_rule(dev)
        new_regions = dev_rule.get_regions(dev)
        for reg in new_regions:
            if reg in groups:
                other = groups[groups.index(reg)]
                other.take_labels(reg)
            else:
                groups.append(reg)

    offsets = {}
    for group in groups:
        kernel_offset = group.set_kernel_offset(kernel_offset)
        offsets.update(group.get_labelled_addresses())
    return (groups, offsets)


def get_interrupts(tree: fdt.FdtParser, rules: rule.HardwareYaml):
    ''' Get dict of interrupts, {label: KernelInterrupt} from the DT and hardware rules. '''
    kernel_devices = tree.get_kernel_devices()

    irqs = []
    for dev in kernel_devices:
        dev_rule = rules.get_rule(dev)
        if len(dev_rule.interrupts.items()) > 0:
            irqs += dev_rule.get_interrupts(tree, dev)

    ret = {}
    for irq in irqs:
        if irq.label in ret:
            if irq.prio > ret[irq.label].prio:
                ret[irq.label] = irq
        else:
            ret[irq.label] = irq

    ret = list(ret.values())
    ret.sort(key=lambda a: a.label)
    return ret


def run(tree: fdt.FdtParser, hardware: rule.HardwareYaml, config: config.Config, args: argparse.Namespace):
    if not args.header_out:
        raise ValueError('You need to specify a header-out to use c header output')

    physical_memory, reserved, physBase = memory.get_physical_memory(tree, config)
    kernel_regions, kernel_macros = get_kernel_devices(tree, hardware)
    kernel_irqs = get_interrupts(tree, hardware)
    template = Environment(loader=BaseLoader, trim_blocks=True,
                           lstrip_blocks=True).from_string(HEADER_TEMPLATE)

    template_args = dict(builtins.__dict__, **{
        'args': args,
        'kernel_irqs': kernel_irqs,
        'kernel_macros': kernel_macros,
        'kernel_regions': kernel_regions,
        'physBase': physBase,
        'physical_memory': physical_memory,
    })

    data = template.render(template_args)
    args.header_out.write(data)
    args.header_out.close()


def add_args(parser):
    parser.add_argument('--header-out', help='output file for c header',
                        type=argparse.FileType('w'))
