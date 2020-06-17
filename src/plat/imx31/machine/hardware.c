/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <config.h>
#include <types.h>
#include <machine/io.h>
#include <kernel/vspace.h>
#include <arch/machine.h>
#include <arch/kernel/vspace.h>
#include <plat/machine.h>
#include <linker.h>
#include <plat/machine/hardware.h>

#define L2_LINE_SIZE_BITS 5
#define L2_LINE_SIZE BIT(L2_LINE_SIZE_BITS)

#define L2_LINE_START(a) ROUND_DOWN(a, L2_LINE_SIZE_BITS)
#define L2_LINE_INDEX(a) (L2_LINE_START(a)>>L2_LINE_SIZE_BITS)

/* kernel devices */
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

static void cleanL2(void)
{
    /* clean all ways */
    imx31_l2cc_flush_regs->clean_by_way = 0xff;
    /* Busy-wait for completion */
    while (imx31_l2cc_flush_regs->clean_by_way);
}

static void invalidateL2(void)
{
    /* Invalidate all ways. */
    imx31_l2cc_flush_regs->inv_by_way = 0xff;
    /* Busy-wait for completion. */
    while (imx31_l2cc_flush_regs->inv_by_way);
}

static void finaliseL2Op(void)
{
    /* We sync the l2 cache, which drains the write and eviction
       buffers, to ensure that everything is consistent with RAM. */
    imx31_l2cc_flush_regs->sync = 1;
}

void plat_cleanL2Range(paddr_t start, paddr_t end)
{
    paddr_t line;
    word_t index;

    for (index = L2_LINE_INDEX(start);
         index < L2_LINE_INDEX(end) + 1;
         index++) {
        line = index << L2_LINE_SIZE_BITS;
        imx31_l2cc_flush_regs->clean_by_pa = line;
    }
    finaliseL2Op();
}

void plat_invalidateL2Range(paddr_t start, paddr_t end)
{
    paddr_t line;
    word_t index;

    for (index = L2_LINE_INDEX(start);
         index < L2_LINE_INDEX(end) + 1;
         index++) {
        line = index << L2_LINE_SIZE_BITS;
        imx31_l2cc_flush_regs->inv_by_pa = line;
    }

    finaliseL2Op();
}

void plat_cleanInvalidateL2Range(paddr_t start, paddr_t end)
{
    paddr_t line;
    word_t index;

    for (index = L2_LINE_INDEX(start);
         index < L2_LINE_INDEX(end) + 1;
         index++) {
        line = index << L2_LINE_SIZE_BITS;
        imx31_l2cc_flush_regs->clinv_by_pa = line;
    }
    finaliseL2Op();
}

void plat_cleanInvalidateL2Cache(void)
{
    cleanL2();
    invalidateL2();
}

BOOT_CODE void initL2Cache(void)
{
#ifndef CONFIG_DEBUG_DISABLE_L2_CACHE
    /* Configure L2 cache */
    imx31_l2cc_ctrl_regs->aux_control = 0x0003001b;

    /* Invalidate the L2 cache */
    invalidateL2();

    /* Enable the L2 cache */
    imx31_l2cc_ctrl_regs->control = 1;
#endif
}

irq_t active_irq = irqInvalid;

BOOT_CODE void initIRQController(void)
{
    /* Do nothing */
}

BOOT_CODE void cpu_initLocalIRQController(void)
{
    /* Do nothing */
}
