/*
 * Copyright 2017, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(DATA61_GPL)
 */

#ifndef __PLAT_MACHINE_DEVICES_H
#define __PLAT_MACHINE_DEVICES_H

/* These devices are used by the seL4 kernel. */
#define UARTA_PPTR                  0xffffffffffff0000
#define GIC_DISTRIBUTOR_PPTR        0xffffffffffff3000
#define GIC_CONTROLLER_PPTR         0xffffffffffff4000

#define GIC_PL390_CONTROLLER_PPTR   GIC_CONTROLLER_PPTR
#define GIC_PL390_DISTRIBUTOR_PPTR  GIC_DISTRIBUTOR_PPTR

#define GIC_DISTRIBUTOR_PADDR       GICD_PADDR
#define GIC_CONTROLLER0_PADDR       GICI_PADDR

/* many of the device regions are not 4K page aligned, so some regions may contain more than
 * one devices. it is the user's responsibilty to figure out how to use these regions */
#define ARM_PERIPHBASE              (0x50040000)                /* 128 KB                  */
#define GICD_PADDR                  (ARM_PERIPHBASE + 0x1000)   /* interrupt distributor   */
#define GICI_PADDR                  (ARM_PERIPHBASE + 0x2000)   /* GIC CPU interface       */
#define UARTA_SYNC_PADDR            (0x70006000)                /* 12 KB, multiple         */
#define UARTA_PADDR                 (0x70006000)
#define TMR_PADDR                   (0x60005000)                /* 4 Kb                    */

#endif /* __PLAT_MACHINE_DEVICES_H */
