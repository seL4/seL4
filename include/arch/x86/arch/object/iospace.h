/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __ARCH_OBJECT_IOSPACE_H
#define __ARCH_OBJECT_IOSPACE_H

#include <types.h>
#include <api/failures.h>
#include <object/structures.h>
#include <plat/machine/hardware_gen.h>

#define get_pci_bus(x) (((x)>>8u) & 0xffu)
#define get_pci_dev(x) (((x)>>3u) & 0x1fu)
#define get_pci_fun(x) ((x) & 0x7u)
#define get_dev_id(bus, dev, fun) (((bus) << 8u) | ((dev) << 3u) | (fun))

#define PCI_BUS_MAX     255
#define PCI_DEV_MAX     31
#define PCI_FUNC_MAX    7

static inline int vtd_get_root_index(dev_id_t dev)
{
    return get_pci_bus(dev);
}

static inline int vtd_get_context_index(dev_id_t dev)
{
    return dev & 0xff;
}

struct lookupIOPTSlot_ret {
    exception_t status;
    vtd_pte_t*  ioptSlot;
    int         level;
};
typedef struct lookupIOPTSlot_ret lookupIOPTSlot_ret_t;

cap_t master_iospace_cap(void);
exception_t decodeX86IOPTInvocation(word_t invLabel, word_t length, cte_t* slot, cap_t cap, extra_caps_t excaps, word_t*  buffer);
exception_t decodeX86IOMapInvocation(word_t length, cte_t* slot, cap_t cap, extra_caps_t excaps, word_t* buffer);
exception_t decodeX86IOSpaceInvocation(word_t invLabel, cap_t cap);
exception_t performX86IOUnMapInvocation(cap_t cap, cte_t *ctSlot);
void unmapIOPage(cap_t cap);
void deleteIOPageTable(cap_t cap);
void unmapVTDContextEntry(cap_t cap);

#endif
