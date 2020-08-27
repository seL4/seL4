/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 * Copyright 2015, 2016 Hesham Almatary <heshamelmatary@gmail.com>
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#ifndef __ASSEMBLER__
#include <arch/types.h>
#include <arch/object/structures.h>
#include <arch/machine/hardware.h>
#include <arch/model/statedata.h>
#include <arch/sbi.h>
#include <mode/machine.h>

#ifdef ENABLE_SMP_SUPPORT

static inline void fence_rw_rw(void)
{
    asm volatile("fence rw, rw" ::: "memory");
}

static inline void fence_w_rw(void)
{
    asm volatile("fence w, rw" ::: "memory");
}

static inline void fence_r_rw(void)
{
    asm volatile("fence r,rw" ::: "memory");
}

static inline void fence_w_r(void)
{
    asm volatile("fence w,r" ::: "memory");
}

static inline void ifence_local(void)
{
    asm volatile("fence.i":::"memory");
}

static inline void sfence_local(void)
{
    asm volatile("sfence.vma" ::: "memory");
}

static inline void ifence(void)
{
    ifence_local();

    unsigned long mask = 0;
    for (int i = 0; i < CONFIG_MAX_NUM_NODES; i++) {
        if (i != getCurrentCPUIndex()) {
            mask |= BIT(cpuIndexToID(i));
        }
    }
    sbi_remote_fence_i(&mask);
}

static inline void sfence(void)
{
    fence_w_rw();
    sfence_local();

    unsigned long mask = 0;
    for (int i = 0; i < CONFIG_MAX_NUM_NODES; i++) {
        if (i != getCurrentCPUIndex()) {
            mask |= BIT(cpuIndexToID(i));
        }
    }
    sbi_remote_sfence_vma(&mask, 0, 0);
}

static inline void hwASIDFlushLocal(asid_t asid)
{
    asm volatile("sfence.vma x0, %0" :: "r"(asid): "memory");
}

static inline void hwASIDFlush(asid_t asid)
{
    hwASIDFlushLocal(asid);

    unsigned long mask = 0;
    for (int i = 0; i < CONFIG_MAX_NUM_NODES; i++) {
        if (i != getCurrentCPUIndex()) {
            mask |= BIT(cpuIndexToID(i));
        }
    }
    sbi_remote_sfence_vma_asid(&mask, 0, 0, asid);
}

#else

static inline void sfence(void)
{
    asm volatile("sfence.vma" ::: "memory");
}

static inline void hwASIDFlush(asid_t asid)
{
    asm volatile("sfence.vma x0, %0" :: "r"(asid): "memory");
}

#endif /* end of !ENABLE_SMP_SUPPORT */

word_t PURE getRestartPC(tcb_t *thread);
void setNextPC(tcb_t *thread, word_t v);

/* Cleaning memory before user-level access */
static inline void clearMemory(void *ptr, unsigned int bits)
{
    memzero(ptr, BIT(bits));
}

static inline void write_satp(word_t value)
{
    asm volatile("csrw satp, %0" :: "rK"(value));
}

static inline void write_stvec(word_t value)
{
    asm volatile("csrw stvec, %0" :: "rK"(value));
}

static inline word_t read_stval(void)
{
    word_t temp;
    asm volatile("csrr %0, stval" : "=r"(temp));
    return temp;
}

static inline word_t read_scause(void)
{
    word_t temp;
    asm volatile("csrr %0, scause" : "=r"(temp));
    return temp;
}

static inline word_t read_sepc(void)
{
    word_t temp;
    asm volatile("csrr %0, sepc" : "=r"(temp));
    return temp;
}

static inline word_t read_sstatus(void)
{
    word_t temp;
    asm volatile("csrr %0, sstatus" : "=r"(temp));
    return temp;
}

/** MODIFIES: */
/** DONT_TRANSLATE */
static inline word_t read_sip(void)
{
    word_t temp;
    asm volatile("csrr %0, sip" : "=r"(temp));
    return temp;
}

static inline void set_sie_mask(word_t mask_high)
{
    word_t temp;
    asm volatile("csrrs %0, sie, %1" : "=r"(temp) : "rK"(mask_high));
}

static inline void clear_sie_mask(word_t mask_low)
{
    word_t temp;
    asm volatile("csrrc %0, sie, %1" : "=r"(temp) : "rK"(mask_low));
}

#ifdef CONFIG_HAVE_FPU
static inline uint32_t read_fcsr(void)
{
    uint32_t fcsr;
    asm volatile("csrr %0, fcsr" : "=r"(fcsr));
    return fcsr;
}

static inline void write_fcsr(uint32_t value)
{
    asm volatile("csrw fcsr, %0" :: "rK"(value));
}
#endif

#if CONFIG_PT_LEVELS == 2
#define SATP_MODE SATP_MODE_SV32
#elif CONFIG_PT_LEVELS == 3
#define SATP_MODE SATP_MODE_SV39
#elif CONFIG_PT_LEVELS == 4
#define SATP_MODE SATP_MODE_SV48
#else
#error "Unsupported PT levels"
#endif
static inline void setVSpaceRoot(paddr_t addr, asid_t asid)
{
    satp_t satp = satp_new(SATP_MODE,              /* mode */
                           asid,                   /* asid */
                           addr >> seL4_PageBits); /* PPN */

    write_satp(satp.words[0]);

    /* Order read/write operations */
#ifdef ENABLE_SMP_SUPPORT
    sfence_local();
#else
    sfence();
#endif
}

static inline void Arch_finaliseInterrupt(void)
{
}

int get_num_avail_p_regs(void);
p_region_t *get_avail_p_regs(void);
int get_num_dev_p_regs(void);
p_region_t get_dev_p_reg(word_t i);
void map_kernel_devices(void);

static inline void setInterruptMode(irq_t irq, bool_t levelTrigger, bool_t polarityLow) { }
/** MODIFIES: [*] */
void initTimer(void);
/* L2 cache control */
void initL2Cache(void);
void initLocalIRQController(void);
void initIRQController(void);
void setIRQTrigger(irq_t irq, bool_t trigger);

#ifdef ENABLE_SMP_SUPPORT
#define irq_remote_call_ipi     (INTERRUPT_IPI_0)
#define irq_reschedule_ipi      (INTERRUPT_IPI_1)

static inline void arch_pause(void)
{
    // use a memory fence to delay a bit.
    // other alternatives?
    fence_rw_rw();
}

#endif
void plat_cleanL2Range(paddr_t start, paddr_t end);

void plat_invalidateL2Range(paddr_t start, paddr_t end);

void plat_cleanInvalidateL2Range(paddr_t start, paddr_t end);

static inline void *CONST paddr_to_kpptr(paddr_t paddr)
{
    assert(paddr < KERNEL_ELF_PADDR_TOP);
    assert(paddr >= KERNEL_ELF_PADDR_BASE);
    return (void *)(paddr + KERNEL_ELF_BASE_OFFSET);
}

static inline paddr_t CONST kpptr_to_paddr(void *pptr)
{
    assert((word_t)pptr >= KERNEL_ELF_BASE);
    return (paddr_t)pptr - KERNEL_ELF_BASE_OFFSET;
}

/* Update the value of the actual regsiter to hold the expected value */
static inline void Arch_setTLSRegister(word_t tls_base)
{
    /* The register is always reloaded upon return from kernel. */
    setRegister(NODE_STATE(ksCurThread), TLS_BASE, tls_base);
}

#endif // __ASSEMBLER__


