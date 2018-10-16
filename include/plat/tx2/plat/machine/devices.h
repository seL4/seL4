/*
 * Copyright 2018, Data61
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

#include <config.h>

/* These devices are used by the seL4 kernel. */
#define KDEV_PPTR                   0xffffffffffff0000lu

#define UARTA_PPTR                  (KDEV_PPTR)
#define GIC_DISTRIBUTOR_PPTR        (KDEV_PPTR + 0x3000)
#define GIC_CONTROLLER_PPTR         (KDEV_PPTR + 0x4000)
#define GICH_PPTR                   (KDEV_PPTR + 0x6000)

#define GIC_PL390_CONTROLLER_PPTR   GIC_CONTROLLER_PPTR
#define GIC_PL390_DISTRIBUTOR_PPTR  GIC_DISTRIBUTOR_PPTR

#define GIC_DISTRIBUTOR_PADDR       GICD_PADDR
#define GIC_CONTROLLER_PADDR        GICI_PADDR

#define GIC_PL400_VCPUCTRL_PPTR     GICH_PPTR


/*
 * This is a subset of the address map.
 */
#define ARM_PERIPHBASE              (0x03880000)
#define ARM_PERIPHBASE_END          (0x038affff)
#define ARM_PERIPHBASE_SIZE         ((ARM_PERIPHBASE_END+1) - ARM_PERIPHBASE) 	    /* 192KiB */
#define GICD_PADDR                  (ARM_PERIPHBASE + 0x1000)   /* interrupt distributor      */
#define GICI_PADDR                  (ARM_PERIPHBASE + 0x2000)   /* GIC CPU interface          */
#define GICH_PADDR                  (ARM_PERIPHBASE + 0x4000)   /* GIC hyp-view register      */
#define GICV_PADDR                  (ARM_PERIPHBASE + 0x6000)   /* GIC VCPU register          */
#define UARTA_PADDR                 (0x03100000)
#endif /* __PLAT_MACHINE_DEVICES_H */
