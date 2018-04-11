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
    write_csr(sptbr, satp.words[0]);
}

static inline void Arch_finaliseInterrupt(void)
{
}

#endif // __ASSEMBLER__
#endif

