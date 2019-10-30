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
#include <config.h>
#include <arch/types.h>
#include <arch/object/structures.h>
#include <arch/machine/hardware.h>
#include <arch/encoding.h>
#include <arch/model/statedata.h>
#include <arch/sbi.h>

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

/* Hypervisor extension register
 * 0xaxx are v0.3 values
 * 0x6xx are v0.4 values
 * The Bregs are renamed as Vregs in v0.4.
 */
#if CONFIG_RISCV_HE_VER == 3

#define HSTATUS     0xa00
#define HEDELEG     0xa02
#define HIDELEG     0xa03
#define HGATP       0xa80

#elif CONFIG_RISCV_HE_VER == 4

#define HSTATUS     0x600
#define HEDELEG     0x602
#define HIDELEG     0x603
#define HGATP       0x680

#else
#error Unspecified RISCV HE version
#endif

#define BSSTATUS    0x200
#define BSIE        0x204
#define BSTVEC      0x205
#define BSSCRATCH   0x240
#define BSEPC       0x241
#define BSCAUSE     0x242
#define BSTVAL      0x243
#define BSIP        0x244
#define BSATP       0x280

/* The SPRV bit modifieds the privilege with which loads and stores execute
 * when not in M-mode. When SPRV=0, translation and protection behave as normal.
 * When SPRV=1, load and store memory addresses are translated and protected
 * as though the current virtualization mode were set to hstatus.SPV and
 * the current privilege mode were set to the HS-level SPP (sstatus.SPP when
 * V=0, or bsstatus.SPP when V=1).
 */
#define HSTATUS_SPRV    BIT(0)
/* STL (supervisor translation level) indicates which address-translation
 * level caused an access-fault or page-fault exception. It is also written
 * by implementation when whenever a trap is taken into HS-mode. On an access
 * or page fault due to guest physical address translation, STL is set to 1.
 * For any other trap into HS-mode STL is set to 0.
 */
#define HSTATUS_STL     BIT(6)
/* SPV (supervisor previous virtualization mode) is written by the implementation
 * whenever a trap is taken into HS-mode. Just as the SPP bit in sstatus is set to
 * the privilege mode at the time of the trap, the SPV bit in hstatus is set to
 * the value of the virtualization mode V at the time of the trap. When an SERT
 * instruction is executed, when V=0, V is set to SPV.
 */
#define HSTATUS_SPV     BIT(7)
/* When a trap is taken into HS-mode, bits SP2V and SP2P are set to the values
 * that SPV and the HS-level SPP had before the trap. (Before the trap, the
 * HS-level SPP is sstatus.SPP if V=0, or bsstatus.SPP is V=1.) When an SRET
 * instruction is executed when V=0, the reverse assignments occur: after
 * SPV and sstatus.SPP have supplied the new virtualization and privilege modes,
 * they are written with SP2V and SP2P, respectively.
 */
#define HSTATUS_SP2P    BIT(8)
#define HSTATUS_SP2V    BIT(9)


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

static inline word_t read_bsstatus(void)
{
    word_t r;
    asm volatile("csrr %0, "STRINGIFY(BSSTATUS) : "=r"(r));
    return r;
}

static inline void write_bsstatus(word_t v)
{
    asm volatile("csrw "STRINGIFY(BSSTATUS)", %0" :: "r"(v));
}

static inline word_t read_bsie(void)
{
    word_t r;
    asm volatile("csrr %0, "STRINGIFY(BSIE) : "=r"(r));
    return r;
}

static inline void write_bsie(word_t v)
{
    asm volatile("csrw "STRINGIFY(BSIE)", %0" :: "r"(v));
}

static inline word_t read_bstvec(void)
{
    word_t r;
    asm volatile("csrr %0, "STRINGIFY(BSTVEC) : "=r"(r));
    return r;
}

static inline void write_bstvec(word_t v)
{
    asm volatile("csrw "STRINGIFY(BSTVEC)", %0" :: "r"(v));
}

static inline word_t read_bsscratch(void)
{
    word_t r;
    asm volatile("csrr %0, "STRINGIFY(BSSCRATCH) : "=r"(r));
    return r;
}

static inline void write_bsscratch(word_t v)
{
    asm volatile("csrw "STRINGIFY(BSSCRATCH)", %0" :: "r"(v));
}

static inline word_t read_bsepc(void)
{
    word_t r;
    asm volatile("csrr %0, "STRINGIFY(BSEPC) : "=r"(r));
    return r;
}

static inline void write_bsepc(word_t v)
{
    asm volatile("csrw "STRINGIFY(BSEPC)", %0" :: "r"(v));
}

static inline word_t read_bscause(void)
{
    word_t r;
    asm volatile("csrr %0, "STRINGIFY(BSCAUSE) : "=r"(r));
    return r;
}

static inline void write_bscause(word_t v)
{
    asm volatile("csrw "STRINGIFY(BSCAUSE)", %0" :: "r"(v));
}

static inline word_t read_bstval(void)
{
    word_t r;
    asm volatile("csrr %0, "STRINGIFY(BSTVAL) : "=r"(r));
    return r;
}

static inline void write_bstval(word_t v)
{
    asm volatile("csrw "STRINGIFY(BSTVAL)", %0" :: "r"(v));
}

static inline word_t read_bsip(void)
{
    word_t r;
    asm volatile("csrr %0, "STRINGIFY(BSIP) : "=r"(r));
    return r;
}

static inline void write_bsip(word_t v)
{
    asm volatile("csrw "STRINGIFY(BSIP)", %0" :: "r"(v));
}

static inline word_t read_bsatp(void)
{
    word_t r;
    asm volatile("csrr %0, "STRINGIFY(BSATP) : "=r"(r));
    return r;
}

static inline void write_bsatp(word_t v)
{
    asm volatile("csrw "STRINGIFY(BSATP)", %0" :: "r"(v));
}

static inline void hfence(void)
{
    /* asm volatile("hfence.vma" ::: "memory");
     * not supported by GCC yet.
     */
#if CONFIG_RISCV_HE_VER == 3
      asm volatile(".word 0xa2000073" ::: "memory");
#elif CONFIG_RISCV_HE_VER == 4
    /* v0.4 hfence.gvma instruction */
    asm volatile(".word 0x62000073" ::: "memory");
#else
#error Unspecified RISCV HE version
#endif
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
                           asid,                         /* asid */
                           addr >> seL4_PageBits); /* PPN */

    /* Current toolchain still uses sptbr register name although it got renamed in priv-1.10.
     * This will most likely need to change with newer toolchains
     */
    write_sptbr(satp.words[0]);

    /* Order read/write operations */
#ifdef ENABLE_SMP_SUPPORT
    sfence_local();
#else
    sfence();
#endif
}

#endif

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
void initLocalIRQController(void);
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
    fence_rw_rw();
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

