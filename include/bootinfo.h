/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __BOOTINFO_H
#define __BOOTINFO_H

#include <config.h>
#include <types.h>

#define BI_PTR(r) ((bi_t*)(r))
#define BI_REF(p) ((unsigned int)(p))

/* bootinfo data structures (directly corresponding to abstract specification) */

#define BI_FRAME_SIZE_BITS PAGE_BITS

/* fixed cap positions in root CNode */
#define BI_CAP_NULL          0 /* null cap */
#define BI_CAP_IT_TCB        1 /* initial thread's TCB cap */
#define BI_CAP_IT_CNODE      2 /* initial thread's root CNode cap */
#define BI_CAP_IT_PD         3 /* initial thread's PD cap */
#define BI_CAP_IRQ_CTRL      4 /* global IRQ controller cap */
#define BI_CAP_ASID_CTRL     5 /* global ASID controller cap */
#define BI_CAP_IT_ASID_POOL  6 /* initial thread's ASID pool cap */
#define BI_CAP_IO_PORT       7 /* global IO port cap (null cap if not supported) */
#define BI_CAP_IO_SPACE      8 /* global IO space cap (null cap if no IOMMU support) */
#define BI_CAP_BI_FRAME      9 /* bootinfo frame cap */
#define BI_CAP_IT_IPCBUF    10 /* initial thread's IPC buffer frame cap */
#define BI_CAP_DOM          11 /* domain cap */
#define BI_CAP_DYN_START    12 /* slot where dynamically allocated caps start */

/* type definitions */

typedef uint32_t slot_pos_t;

typedef struct slot_region {
    slot_pos_t start;
    slot_pos_t end;
} slot_region_t;

#define S_REG_EMPTY (slot_region_t){ .start = 0, .end = 0 }

typedef struct bi_dev_reg {
    paddr_t       base_paddr;      /* base physical address of device region */
    uint32_t      frame_size_bits; /* size (2^n bytes) of a device-region frame */
    slot_region_t frame_caps;      /* device-region frame caps */
} bi_dev_reg_t;

typedef struct bi {
    node_id_t     node_id;
    uint32_t      num_nodes;
    uint32_t      num_iopt_levels; /* number of IOMMU PT levels (0 if no IOMMU support) */
    vptr_t        ipcbuf_vptr;     /* vptr to initial thread's IPC buffer */
    slot_region_t null_caps;       /* null caps (empty slots) */
    slot_region_t sh_frame_caps;   /* shared-frame caps */
    slot_region_t ui_frame_caps;   /* userland-image frame caps */
    slot_region_t ui_pt_caps;      /* userland-image PT caps */
    slot_region_t ut_obj_caps;     /* untyped-object caps (UT caps) */
    paddr_t       ut_obj_paddr_list    [CONFIG_MAX_NUM_BOOTINFO_UNTYPED_CAPS]; /* physical address of each UT cap */
    uint8_t       ut_obj_size_bits_list[CONFIG_MAX_NUM_BOOTINFO_UNTYPED_CAPS]; /* size (2^n) bytes of each UT cap */
    uint8_t       it_cnode_size_bits; /* initial thread's root CNode size (2^n slots) */
    uint32_t      num_dev_regs;       /* number of device regions */
    bi_dev_reg_t  dev_reg_list[CONFIG_MAX_NUM_BOOTINFO_DEVICE_REGIONS]; /* device regions */
    uint8_t       it_domain;       /* initial thread's domain ID */
} bi_t;

/* adjust constants in config.h if this assert fails */
compile_assert(bi_size, sizeof(bi_t) <= BIT(BI_FRAME_SIZE_BITS))

#endif
