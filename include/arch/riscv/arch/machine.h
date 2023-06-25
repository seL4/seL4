/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 * Copyright 2015, 2016 Hesham Almatary <heshamelmatary@gmail.com>
 * Copyright 2021, HENSOLDT Cyber
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

/* PPTR_BASE must be smaller than KERNEL_ELF_BASE. */
compile_assert(pptr_base_less_elf_base, PPTR_BASE < KERNEL_ELF_BASE_RAW);
/* physBase must be aligned to a page for verification to succeed. */
compile_assert(phys_base_page_aligned, IS_ALIGNED(PHYS_BASE_RAW, seL4_PageBits));

/* The following compile time checks are verification artifacts. Not necessarily
   all systems need to satisfy these, but proofs will need to be changed
   manually if they are not satisfied. These particular checks also are not
   necessarily sufficient for all real platforms, but they are good sanity
   checks to have always on. */

/* PPTR_BASE must be at least twice as far away from KERNEL_ELF_BASE as a LargePage. */
compile_assert(pptr_base_distance, PPTR_BASE + BIT(seL4_LargePageBits + 1) < KERNEL_ELF_BASE_RAW);
/* Kernel ELF window must have at least 64k space */
compile_assert(kernel_elf_distance, KERNEL_ELF_BASE_RAW + BIT(16) < KDEV_BASE);
/* End of kernel ELF window must not overflow as a word_t */
compile_assert(kernel_elf_no_overflow, KERNEL_ELF_BASE_RAW < KERNEL_ELF_BASE_RAW + BIT(16));

/* Bit flags in CSR MIP/SIP (interrupt pending). */
/* Bit 0 was SIP_USIP, but the N extension will be dropped in v1.12 */
#define SIP_SSIP   1 /* S-Mode software interrupt pending. */
/* Bit 2 was SIP_HSIP in v1.9, but the H extension was reworked afterwards. */
#define SIP_MSIP   3 /* M-Mode software interrupt pending (MIP only). */
/* Bit 4 was SIP_UTIP, but the N extension will be dropped in v1.12 */
#define SIP_STIP   5 /* S-Mode timer interrupt pending. */
/* Bit 6 was SIP_HTIP in v1.9, but the H extension was reworked afterwards. */
#define SIP_MTIP   7 /* M-Mode timer interrupt pending (MIP only). */
/* Bit 8 was SIP_UEIP, but the N extension will be dropped in v1.12 */
#define SIP_SEIP   9 /* S-Mode external interrupt pending. */
/* Bit 10 was SIP_HEIP in v1.9, but the H extension was reworked afterwards. */
#define SIP_MEIP  11 /* M-Mode external interrupt pending (MIP only). */
/* Bit 12 and above are reserved. */

/* Bit flags in CSR MIE/SIE (interrupt enable). */
/* Bit 0 was SIE_USIE, but the N extension will be dropped in v1.12 */
#define SIE_SSIE   1 /* S-Mode software interrupt enable. */
/* Bit 2 was SIE_HSIE in v1.9, but the H extension was reworked afterwards. */
#define SIE_MSIE   3 /* M-Mode software interrupt enable (MIP only). */
/* Bit 4 was SIE_UTIE, but the N extension will be dropped in v1.12 */
#define SIE_STIE   5 /* S-Mode timer interrupt enable. */
/* Bit 6 was SIE_HTIE in v1.9, but the H extension was reworked afterwards. */
#define SIE_MTIE   7 /* M-Mode timer interrupt enable (MIP only). */
/* Bit 8 was SIE_UEIE, but the N extension will be dropped in v1.12 */
#define SIE_SEIE   9 /* S-Mode external interrupt enable. */
/* Bit 10 was SIE_HEIE in v1.9, but the H extension was reworked afterwards. */
#define SIE_MEIE  11 /* M-Mode external interrupt enable (MIP only). */
/* Bit 12 and above are reserved. */

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

static inline void ifence_local(void)
{
    asm volatile("fence.i":::"memory");
}

static inline void sfence_local(void)
{
    asm volatile("sfence.vma" ::: "memory");
}

static inline word_t get_sbi_mask_for_all_remote_harts(void)
{
    word_t mask = 0;
    for (int i = 0; i < CONFIG_MAX_NUM_NODES; i++) {
        if (i != getCurrentCPUIndex()) {
            mask |= BIT(cpuIndexToID(i));
        }
    }
    return mask;
}

static inline void ifence(void)
{
    ifence_local();
    word_t mask = get_sbi_mask_for_all_remote_harts();
    sbi_remote_fence_i(mask);
}

static inline void sfence(void)
{
    fence_w_rw();
    sfence_local();
    word_t mask = get_sbi_mask_for_all_remote_harts();
    sbi_remote_sfence_vma(mask, 0, 0);
}

static inline void hwASIDFlushLocal(asid_t asid)
{
    asm volatile("sfence.vma x0, %0" :: "r"(asid): "memory");
}

static inline void hwASIDFlush(asid_t asid)
{
    hwASIDFlushLocal(asid);
    word_t mask = get_sbi_mask_for_all_remote_harts();
    sbi_remote_sfence_vma_asid(mask, 0, 0, asid);
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

static inline word_t read_sip(void)
{
    word_t temp;
    asm volatile("csrr %0, sip" : "=r"(temp));
    return temp;
}

static inline void write_sie(word_t value)
{
    asm volatile("csrw sie,  %0" :: "r"(value));
}

static inline word_t read_sie(void)
{
    word_t temp;
    asm volatile("csrr %0, sie" : "=r"(temp));
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

void map_kernel_devices(void);

/** MODIFIES: [*] */
void initTimer(void);
void initLocalIRQController(void);
void initIRQController(void);
void setIRQTrigger(irq_t irq, bool_t trigger);

#ifdef ENABLE_SMP_SUPPORT
#define irq_remote_call_ipi     (INTERRUPT_IPI_0)
#define irq_reschedule_ipi      (INTERRUPT_IPI_1)

static inline void arch_pause(void)
{
    /* Currently, a memory fence seems the best option to delay execution at
     * least a bit. The ZiHintPause extension defines PAUSE, it's encoded as
     * FENCE instruction with fm=0, pred=W, succ=0, rd=x0, rs1=x0. Once it is
     * supported we could use 'asm volatile("pause")' as an improvement.
     */
    fence_rw_rw();
}

#endif

/* Update the value of the actual register to hold the expected value */
static inline exception_t Arch_setTLSRegister(word_t tls_base)
{
    /* The register is always reloaded upon return from kernel. */
    setRegister(NODE_STATE(ksCurThread), TLS_BASE, tls_base);
    return EXCEPTION_NONE;
}

#endif // __ASSEMBLER__
