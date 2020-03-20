/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#define get_pci_bus(x) (((x)>>8u) & 0xffu)
#define get_pci_dev(x) (((x)>>3u) & 0x1fu)
#define get_pci_fun(x) ((x) & 0x7u)
#define get_dev_id(bus, dev, fun) (((bus) << 8u) | ((dev) << 3u) | (fun))

#define PCI_BUS_MAX     255
#define PCI_DEV_MAX     31
#define PCI_FUNC_MAX    7


