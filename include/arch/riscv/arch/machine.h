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

/*
 *
 * Copyright 2016, 2017 Hesham Almatary, Data61/CSIRO <hesham.almatary@data61.csiro.au>
 * Copyright 2015, 2016 Hesham Almatary <heshamelmatary@gmail.com>
 */

#ifndef __ARCH_MACHINE_H
#define __ARCH_MACHINE_H

#ifndef __ASSEMBLER__
#include <arch/types.h>
#include <arch/object/structures.h>
#include <arch/machine/hardware.h>
#include <arch/encoding.h>
#include <arch/model/statedata.h>
#include <arch/sbi.h>

static inline void sfence(void)
{
    asm volatile("sfence.vma" ::: "memory");
}

static inline void hwASIDFlush(asid_t asid)
{
    asm volatile("sfence.vma x0, %0" :: "r"(asid): "memory");
}

word_t PURE getRestartPC(tcb_t *thread);
void setNextPC(tcb_t *thread, word_t v);

/* Cleaning memory before user-level access */
static inline void clearMemory(void *ptr, unsigned int bits)
{
    memzero(ptr, BIT(bits));
}

static inline void write_sptbr(word_t value)
{
    asm volatile("csrw sptbr, %0" :: "rK"(value));
}

static inline void write_stvec(word_t value)
{
    asm volatile("csrw stvec, %0" :: "rK"(value));
}

static inline word_t read_sbadaddr(void)
{
    word_t temp;
    asm volatile("csrr %0, sbadaddr" : "=r"(temp));
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

#if CONFIG_PT_LEVELS == 2
#define SATP_MODE SPTBR_MODE_SV32
#elif CONFIG_PT_LEVELS == 3
#define SATP_MODE SPTBR_MODE_SV39
#elif CONFIG_PT_LEVELS == 4
#define SATP_MODE SPTBR_MODE_SV48
#else
#error "Unsupported PT levels"
#endif
static inline void setVSpaceRoot(paddr_t addr, asid_t asid)
{
    satp_t satp = satp_new(SATP_MODE,              /* mode */
                           asid,                         /* asid */
                           addr >> seL4_PageBits); /* PPN */

    /* Current toolchain still uses sptbr register name although it got renamed in priv-1.10.
     * This will most likely need to change with newer toolchains
     */
    write_sptbr(satp.words[0]);

    /* Order read/write operations */
    sfence();
}

static inline void Arch_finaliseInterrupt(void)
{
}

int get_num_avail_p_regs(void);
p_region_t *get_avail_p_regs(void);
int get_num_dev_p_regs(void);
p_region_t get_dev_p_reg(word_t i);
void map_kernel_devices(void);

typedef uint32_t irq_t;
void ackInterrupt(irq_t irq);
bool_t isIRQPending(void);
void maskInterrupt(bool_t enable, irq_t irq);
irq_t getActiveIRQ(void);
static inline void setInterruptMode(irq_t irq, bool_t levelTrigger, bool_t polarityLow) { }
/** MODIFIES: [*] */
void initTimer(void);
/* L2 cache control */
void initL2Cache(void);

void initIRQController(void);
void setIRQTrigger(irq_t irq, bool_t trigger);

void handleSpuriousIRQ(void);

#ifdef ENABLE_SMP_SUPPORT
#define irq_remote_call_ipi     (INTERRUPT_IPI_0)
#define irq_reschedule_ipi      (INTERRUPT_IPI_1)

static inline void arch_pause(void)
{
    // use a memory fence to delay a bit.
    // other alternatives?
    asm volatile("fence rw,rw");
}

#endif
void plat_cleanL2Range(paddr_t start, paddr_t end);

void plat_invalidateL2Range(paddr_t start, paddr_t end);

void plat_cleanInvalidateL2Range(paddr_t start, paddr_t end);

static inline void *CONST paddr_to_kpptr(paddr_t paddr)
{
    assert(paddr < PADDR_HIGH_TOP);
    assert(paddr >= PADDR_LOAD);
    return (void *)(paddr + KERNEL_BASE_OFFSET);
}

static inline paddr_t CONST kpptr_to_paddr(void *pptr)
{
    assert((word_t)pptr >= KERNEL_BASE);
    return (paddr_t)pptr - KERNEL_BASE_OFFSET;
}

/* Update the value of the actual regsiter to hold the expected value */
static inline void Arch_setTLSRegister(word_t tls_base)
{
    /* The register is always reloaded upon return from kernel. */
    setRegister(NODE_STATE(ksCurThread), TLS_BASE, tls_base);
}

#endif // __ASSEMBLER__
#endif

