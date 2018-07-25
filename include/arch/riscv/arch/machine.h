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
    asm volatile ("sfence.vma" ::: "memory");
}

static inline void hwASIDFlush(asid_t asid)
{
    asm volatile ("sfence.vma x0, %0" :: "r" (asid): "memory");
}

word_t PURE getRestartPC(tcb_t *thread);
void setNextPC(tcb_t *thread, word_t v);

/* Cleaning memory before user-level access */
static inline void clearMemory(void* ptr, unsigned int bits)
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

static inline void clear_sip_mask(word_t mask_low)
{
    word_t temp;
    asm volatile("csrrc %0, sip, %1" : "=r"(temp) : "rK"(mask_low));
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

    /* Order read/write operations */
    sfence();

    /* Current toolchain still uses sptbr register name although it got renamed in priv-1.10.
     * This will most likely need to change with newer toolchains
     */
    write_sptbr(satp.words[0]);
}

static inline void Arch_finaliseInterrupt(void)
{
}

#endif // __ASSEMBLER__
#endif

