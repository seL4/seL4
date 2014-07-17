/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __PLAT_MACHINE_DEVICES_H
#define __PLAT_MACHINE_DEVICES_H

#include <stdint.h>

/* kernel devices */

#define EPIT_PADDR 0x53f94000
#define EPIT_PPTR  0xfff00000

#define AVIC_PADDR 0x68000000
#define AVIC_PPTR  0xfff01000

#define L2CC_PADDR 0x30000000
#define L2CC_PPTR  0xfff02000

struct imx31_l2cc_id {
    uint32_t id;           /* 000 */
    uint32_t type;         /* 004 */
};
#define imx31_l2cc_id_regs \
    ((volatile struct imx31_l2cc_id *)L2CC_PPTR)

struct imx31_l2cc_ctrl {
    uint32_t control;      /* 100 */
    uint32_t aux_control;  /* 104 */
};
#define imx31_l2cc_ctrl_regs \
    ((volatile struct imx31_l2cc_ctrl *)(L2CC_PPTR + 0x100))

struct imx31_l2cc_flush {
    uint32_t pad_0[12];
    uint32_t sync;          /* 730 */
    uint32_t pad_1[15];
    uint32_t inv_by_pa;     /* 770 */
    uint32_t pad_2[2];
    uint32_t inv_by_way;    /* 77c */
    uint32_t pad_3[12];
    uint32_t clean_by_pa;   /* 7b0 */
    uint32_t pad_4[1];
    uint32_t clean_by_ix;   /* 7b8 */
    uint32_t clean_by_way;  /* 7bc */
    uint32_t pad_5[12];
    uint32_t clinv_by_pa;   /* 7f0 */
    uint32_t pad_6[1];
    uint32_t clinv_by_ix;   /* 7f8 */
    uint32_t clinv_by_way;  /* 7fc */
};
#define imx31_l2cc_flush_regs \
    ((volatile struct imx31_l2cc_flush *)(L2CC_PPTR + 0x700))

struct imx31_l2cc_lockdown {
    uint32_t lock_way_D;  /* 900 */
    uint32_t lock_way_I;  /* 904 */
};
#define imx32_l2cc_lockdown_regs \
    ((volatile struct imx31_l2cc_lockdown *)(L2CC_PPTR + 0x900))

#define UART_PADDR 0x43f90000
#define UART_PPTR  0xfff03000

#endif
