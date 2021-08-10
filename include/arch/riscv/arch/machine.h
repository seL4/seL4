/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 * Copyright 2015, 2016 Hesham Almatary <heshamelmatary@gmail.com>
 * Copyright 2021, HENSOLDT Cyber
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#ifndef __ASSEMBLER__
#include <config.h>
#include <arch/types.h>
#include <arch/object/structures.h>
#include <arch/machine/hardware.h>
#include <arch/model/statedata.h>
#include <arch/sbi.h>
#include <mode/machine.h>

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

/** MODIFIES: */
/** DONT_TRANSLATE */
static inline void write_sstatus(word_t value)
{
    asm volatile("csrw sstatus, %0" :: "r"(value));
}

static inline word_t read_sip(void)
{
    word_t temp;
    asm volatile("csrr %0, sip" : "=r"(temp));
    return temp;
}

static inline void write_sip(word_t value)
{
    asm volatile("csrw sip, %0" :: "r"(value));
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


#ifdef CONFIG_RISCV_HE

#define HSTATUS     0x600
#define HEDELEG     0x602
#define HIDELEG     0x603
#define HIE         0x604
#define HCOUNTEREN  0x606
#define HGEIE       0x607
#define HTVAL       0x643
#define HVIP        0x645
#define HIP         0x644
#define HTINST      0x64A
#define HGEIP       0xE12
#define HGATP       0x680
#define HTIMEDELTA  0x605
#define HTIMEDELTAH 0x615

#define VSSTATUS    0x200
#define VSIE        0x204
#define VSTVEC      0x205
#define VSSCRATCH   0x240
#define VSEPC       0x241
#define VSCAUSE     0x242
#define VSTVAL      0x243
#define VSIP        0x244
#define VSATP       0x280

/* The SPRV bit modifieds the privilege with which loads and stores execute
 * when not in M-mode. When SPRV=0, translation and protection behave as normal.
 * When SPRV=1, load and store memory addresses are translated and protected
 * as though the current virtualization mode were set to hstatus.SPV and
 * the current privilege mode were set to the HS-level SPP (sstatus.SPP when
 * V=0, or bsstatus.SPP when V=1).
 */
#define HSTATUS_SPRV    BIT(0)

/* GVA (guest virtual address) is written by the implementation whenever a trap
 * is taken into HS-mode. For any trap (breakpoint, address misaligned, access
 * fault, page fault, or guest page fault) that writes a guest virtual address
 * to stval, GVA is ste to 1. For any other trap to HS-mode, GVA is set to 0.
 */
#define HSTATUS_GVA     BIT(6)

/* SPV (supervisor previous virtualization mode) is written by the implementation
 * whenever a trap is taken into HS-mode. Just as the SPP bit in sstatus is set to
 * the privilege mode at the time of the trap, the SPV bit in hstatus is set to
 * the value of the virtualization mode V at the time of the trap. When an SERT
 * instruction is executed, when V=0, V is set to SPV.
 */
#define HSTATUS_SPV     BIT(7)

/* SPVP (supervisor previous virtual privilege). When V=1 and a trap is taken
 * into HS-mode, bit SPVP is set to the nominal privilege mode at the time
 * of the trap, the same as sstatus.SPP. But if V=0 before a trap, SPVP is
 * left unchanged on trap entry. SPVP controls the effective privilege of
 * explicit memory access made by the virtual-machine load/store instructions,
 * HLV, HLVX, and HSV.
 */
#define HSTATUS_SPVP    BIT(8)

/* HU (hypervisor user mode) controls whether the virtual-machine load/store
 * instructions, HLV, HLVX, and HSV, can be used also in U-mode. When HU=1,
 * these instructions can be executed in U-mode the same as in HS-mode. When
 * HU=0, all hypervisor instructions cause an illegal instruction trap n U-mode
 */
#define HSTATUS_HU      BIT(9)

/* When VTVM=1, an attempt in VS-mode to execute SFENCE.VMA or to access CSR
 * satp raises a virtual instruction exception.
 */
#define HSTATUS_VTVM    BIT(20)

/* When VTW=1 and VS-mode tries to execute WFI, a virtual instruction exception
 * is raised if the WFI does not complete within an implementtion-specific
 * bounded time limit
 */
#define HSTATUS_VTW     BIT(21)

/* When VTSR=1, an attempt in VS-mode to execute SRET raises a virtual insruction
 * exception.
 */
#define HSTATUS_VTSR    BIT(22)


static inline word_t read_hstatus(void)
{
    word_t r;
    asm volatile("csrr %0, "STRINGIFY(HSTATUS) : "=r"(r));
    return r;
}

static inline void write_hstatus(word_t v)
{
    asm volatile("csrw "STRINGIFY(HSTATUS)", %0" :: "r"(v));
}

static inline void hstatus_set(word_t field)
{
    asm volatile("csrs "STRINGIFY(HSTATUS)", %0" :: "r"(field));
}

static inline void hstatus_clear(word_t field)
{
    asm volatile("csrc "STRINGIFY(HSTATUS)", %0" :: "r"(field));
}

static inline word_t read_hedeleg(void)
{
    word_t r;
    asm volatile("csrr %0, "STRINGIFY(HEDELEG) : "=r"(r));
    return r;
}

static inline void write_hedeleg(word_t v)
{
    asm volatile("csrw "STRINGIFY(HEDELEG)", %0" :: "r"(v));
}

static inline word_t read_hideleg(void)
{
    word_t r;
    asm volatile("csrr %0, "STRINGIFY(HIDELEG) : "=r"(r));
    return r;
}

static inline void write_hideleg(word_t v)
{
    asm volatile("csrw "STRINGIFY(HIDELEG)", %0" :: "r"(v));
}

static inline void write_hcounteren(word_t v)
{
    asm volatile("csrw "STRINGIFY(HCOUNTEREN)", %0" :: "r"(v));
}

static inline word_t read_hcounteren(void)
{
    word_t r;
    asm volatile("csrr %0, "STRINGIFY(HCOUNTEREN) : "=r"(r));
    return r;
}

static inline word_t read_hgatp(void)
{
    word_t r;
    asm volatile("csrr %0, "STRINGIFY(HGATP) : "=r"(r));
    return r;
}

static inline void write_hgatp(word_t v)
{
    asm volatile("csrw "STRINGIFY(HGATP)", %0" :: "r"(v));
}
/*
 * HTINST may contain the following values:
 *   zero;
 *   a transformation of trapping instruction;
 *   a custome value; or
 *   a special pesudoinstruction.
 *
 * For interrupts, the value is always zero.
 * For a synchronous exception, if a nonzero value is written, one
 * of the following shall be true:
 *   Bit 0 is 1, and replacing bit 1 with 1 makes the value into a valid
 *   encoding of a standard instruction.
 *
 *   Bit 0 is 1, and replacing bit 1 with 1 makes the value into an
 *   instruction encoding that is explicitly available for a custom
 *   instruction.
 *
 *   The value is one of the special pseudoinstructions defined later, all
 *   of which have bits 1:0 equal to 00.
 *
 */
static inline word_t read_htinst(void)
{
    word_t r;
    asm volatile("csrr %0, "STRINGIFY(HTINST) : "=r"(r));
    return r;
}

static inline void write_htinst(word_t v)
{
    asm volatile("csrw "STRINGIFY(HTINST)", %0" :: "r"(v));
}

/*
 * HTVAL contains 0 or the guest physical address that faulted,
 * shifted right by 2 bits for guest-page faults.
 */
static inline word_t read_htval(void)
{
    word_t r;
    asm volatile("csrr %0, "STRINGIFY(HTVAL) : "=r"(r));
    return r;
}

static inline void write_htval(word_t v)
{
    asm volatile("csrw "STRINGIFY(HTVAL)", %0" :: "r"(v));
}

static inline word_t read_vsstatus(void)
{
    word_t r;
    asm volatile("csrr %0, "STRINGIFY(VSSTATUS) : "=r"(r));
    return r;
}

static inline void write_hvip(word_t v)
{
    asm volatile("csrw "STRINGIFY(HVIP)", %0" :: "r"(v));
}

static inline word_t read_hvip(void)
{
    word_t r;
    asm volatile ("csrr %0, "STRINGIFY(HVIP) : "=r"(r));
    return r;
}

static inline void write_hip(word_t v)
{
    asm volatile("csrw "STRINGIFY(HIP)", %0" :: "r"(v));
}

static inline word_t read_hip(void)
{
    word_t r;
    asm volatile ("csrr %0, "STRINGIFY(HIP) : "=r"(r));
    return r;
}

static inline void write_hie(word_t v)
{
    asm volatile("csrw "STRINGIFY(HIE)", %0" :: "r"(v));
}

static inline word_t read_hie(void)
{
    word_t r;
    asm volatile ("csrr %0, "STRINGIFY(HIE) : "=r"(r));
    return r;
}

static inline void write_vsstatus(word_t v)
{
    asm volatile("csrw "STRINGIFY(VSSTATUS)", %0" :: "r"(v));
}

static inline word_t read_vsie(void)
{
    word_t r;
    asm volatile("csrr %0, "STRINGIFY(VSIE) : "=r"(r));
    return r;
}

static inline void write_vsie(word_t v)
{
    asm volatile("csrw "STRINGIFY(VSIE)", %0" :: "r"(v));
}

static inline word_t read_vstvec(void)
{
    word_t r;
    asm volatile("csrr %0, "STRINGIFY(VSTVEC) : "=r"(r));
    return r;
}

static inline void write_vstvec(word_t v)
{
    asm volatile("csrw "STRINGIFY(VSTVEC)", %0" :: "r"(v));
}

static inline word_t read_vsscratch(void)
{
    word_t r;
    asm volatile("csrr %0, "STRINGIFY(VSSCRATCH) : "=r"(r));
    return r;
}

static inline void write_vsscratch(word_t v)
{
    asm volatile("csrw "STRINGIFY(VSSCRATCH)", %0" :: "r"(v));
}

static inline word_t read_vsepc(void)
{
    word_t r;
    asm volatile("csrr %0, "STRINGIFY(VSEPC) : "=r"(r));
    return r;
}

static inline void write_vsepc(word_t v)
{
    asm volatile("csrw "STRINGIFY(VSEPC)", %0" :: "r"(v));
}

static inline word_t read_vscause(void)
{
    word_t r;
    asm volatile("csrr %0, "STRINGIFY(VSCAUSE) : "=r"(r));
    return r;
}

static inline void write_vscause(word_t v)
{
    asm volatile("csrw "STRINGIFY(VSCAUSE)", %0" :: "r"(v));
}

static inline word_t read_vstval(void)
{
    word_t r;
    asm volatile("csrr %0, "STRINGIFY(VSTVAL) : "=r"(r));
    return r;
}

static inline void write_vstval(word_t v)
{
    asm volatile("csrw "STRINGIFY(VSTVAL)", %0" :: "r"(v));
}

static inline word_t read_vsip(void)
{
    word_t r;
    asm volatile("csrr %0, "STRINGIFY(VSIP) : "=r"(r));
    return r;
}

static inline void write_vsip(word_t v)
{
    asm volatile("csrw "STRINGIFY(VSIP)", %0" :: "r"(v));
}

static inline word_t read_vsatp(void)
{
    word_t r;
    asm volatile("csrr %0, "STRINGIFY(VSATP) : "=r"(r));
    return r;
}

static inline void write_vsatp(word_t v)
{
    asm volatile("csrw "STRINGIFY(VSATP)", %0" :: "r"(v));
}

static inline void hfence(void)
{
    /* v0.4 hfence.gvma instruction */
    asm volatile(".word 0x62000073" ::: "memory");
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

#define HGATP_MODE SATP_MODE

static inline void setVSpaceRoot(paddr_t addr, asid_t asid)
{
#ifdef CONFIG_RISCV_HE
    /* We just use the stage-2 translation */
    hgatp_t hgatp = hgatp_new(HGATP_MODE, asid, addr >> seL4_PageBits);
    write_hgatp(hgatp.words[0]);
    hfence();
#else
    satp_t satp = satp_new(SATP_MODE,              /* mode */
                           asid,                         /* asid */
                           addr >> seL4_PageBits); /* PPN */

    /* Current toolchain still uses sptbr register name although it got renamed in priv-1.10.
     * This will most likely need to change with newer toolchains
     */
    write_sptbr(satp.words[0]);

    /* Order read/write operations */
    sfence();
#endif
}


#ifdef CONFIG_RISCV_HE
/* Kernel still uses the normal stage-1 translation */
static inline void setKernelVSpaceRoot(paddr_t addr, asid_t asid)
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
#endif

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
