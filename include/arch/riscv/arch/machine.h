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
#include <arch/model/smp.h>
#include <arch/sbi.h>

#ifdef ENABLE_SMP_SUPPORT
#define irq_remote_call_ipi        0
#define irq_reschedule_ipi         1
#define int_remote_call_ipi       irq_remote_call_ipi
#define int_reschedule_ipi        irq_reschedule_ipi

#define IPI_MEM_BARRIER asm volatile("sfence.vma");

void ipi_send_target(irq_t irq, word_t cpuTargetList)
{
    sbi_send_ipi(&cpuTargetList);
}

static inline void arch_pause(void)
{
}
#endif /* ENABLE_SMP_SUPPORT */

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
                           addr >> RISCV_4K_PageBits); /* PPN */

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

