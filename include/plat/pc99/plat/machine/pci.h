/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __PLAT_MACHINE_PCI_H
#define __PLAT_MACHINE_PCI_H

#define get_pci_bus(x) (((x)>>8) & 0xff)
#define get_pci_dev(x) (((x)>>3) & 0x1f)
#define get_pci_fun(x) ((x) & 0x7)
#define get_dev_id(bus, dev, fun) (((bus) << 8) | ((dev) << 3) | (fun))

void pci_scan(uint32_t* bus_used_bitmap);

#endif
