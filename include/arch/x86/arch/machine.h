/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <arch/types.h>
#include <arch/object/structures.h>
#include <arch/machine/hardware.h>
#include <arch/machine/pat.h>
#include <arch/machine/cpu_registers.h>
#include <model/statedata.h>
#include <arch/model/statedata.h>
#include <object/interrupt.h>

#define IA32_APIC_BASE_MSR      0x01B
#define IA32_ARCH_CAPABILITIES_MSR 0x10A
#define IA32_SYSENTER_CS_MSR    0x174
#define IA32_SYSENTER_ESP_MSR   0x175
#define IA32_SYSENTER_EIP_MSR   0x176
#define IA32_TSC_DEADLINE_MSR   0x6e0
#define IA32_FS_BASE_MSR        0xC0000100
#define IA32_GS_BASE_MSR        0xC0000101
#define IA32_LSTAR_MSR          0xC0000082
#define IA32_STAR_MSR           0xC0000081
#define IA32_FMASK_MSR          0xC0000084
#define IA32_EFER_MSR 0xC0000080
#define IA32_PLATFORM_INFO_MSR  0xCE
#define IA32_XSS_MSR            0xD0A
#define IA32_FEATURE_CONTROL_MSR 0x3A
#define IA32_KERNEL_GS_BASE_MSR 0xC0000102
#define IA32_VMX_BASIC_MSR      0x480
#define IA32_VMX_PINBASED_CTLS_MSR 0x481
#define IA32_VMX_PROCBASED_CTLS_MSR 0x482
#define IA32_VMX_PROCBASED_CTLS2_MSR 0x48B
#define IA32_VMX_EXIT_CTLS_MSR  0x483
#define IA32_VMX_ENTRY_CTLS_MSR 0x484
#define IA32_VMX_TRUE_PINBASED_CTLS_MSR 0x48D
#define IA32_VMX_TRUE_PROCBASED_CTLS_MSR 0x48E
#define IA32_VMX_TRUE_EXIT_CTLS_MSR 0x48F
#define IA32_VMX_TRUE_ENTRY_CTLS_MSR 0x490
#define IA32_VMX_CR0_FIXED0_MSR 0x486
#define IA32_VMX_CR0_FIXED1_MSR 0x487
#define IA32_VMX_CR4_FIXED0_MSR 0x488
#define IA32_VMX_CR4_FIXED1_MSR 0x489
#define IA32_VMX_EPT_VPID_CAP_MSR 0x48C

#define IA32_PREFETCHER_COMPATIBLE_FAMILIES_ID (0x06)

/* These values taken from:
 *  * Intel manuals, Vol3, table 35-1.
 *  * https://software.intel.com/en-us/articles/intel-architecture-and-processor-identification-with-cpuid-model-and-family-numbers
 */
#define SKYLAKE_1_MODEL_ID      0x4E
#define SKYLAKE_2_MODEL_ID      0x5E
#define BROADWELL_1_MODEL_ID    0x4D
#define BROADWELL_2_MODEL_ID    0x56
#define BROADWELL_3_MODEL_ID    0x4F
#define BROADWELL_4_MODEL_ID    0x47
#define BROADWELL_5_MODEL_ID    0x3D
#define HASWELL_1_MODEL_ID      0x3C
#define HASWELL_2_MODEL_ID      0x3F
#define HASWELL_3_MODEL_ID      0x45
#define HASWELL_4_MODEL_ID      0x46
#define IVY_BRIDGE_1_MODEL_ID   0x9A
#define IVY_BRIDGE_2_MODEL_ID   0x3E
#define IVY_BRIDGE_3_MODEL_ID   0x3A
#define SANDY_BRIDGE_1_MODEL_ID 0x2A /* Sandy Bridge */
#define SANDY_BRIDGE_2_MODEL_ID 0x2D /* Sandy Bridge-E, Sandy Bridge-EN and Sandy Bridge-EP */
#define WESTMERE_1_MODEL_ID     0x25 /* Arrandale and Clarksdale */
#define WESTMERE_2_MODEL_ID     0x2C /* Gulftown and Westmere-EP */
#define WESTMERE_3_MODEL_ID     0x2F /* Westemere-EX */
#define NEHALEM_1_MODEL_ID      0x1E /* Clarksfield, Lynnfield and Jasper Forest */
#define NEHALEM_2_MODEL_ID      0x1A /* Bloomfield and Nehalem-EP */
#define NEHALEM_3_MODEL_ID      0x2E /* Nehalem-EX */

#define X86_CPUID_VENDOR_STRING_MAXLENGTH   (12)
#define X86_CPU_MODEL_STRING_MAXLENGTH      (47)

/* This article discloses prefetcher control on Intel processors; Nehalem, Westmere, Sandy Bridge,
   Ivy Bridge, Haswell, and Broadwell. It is currently undocumented in the regular intel manuals.
   https://software.intel.com/en-us/articles/disclosure-of-hw-prefetcher-control-on-some-intel-processors */
#define IA32_PREFETCHER_MSR                 0x1A4
#define IA32_PREFETCHER_MSR_L2              BIT(0)
#define IA32_PREFETCHER_MSR_L2_ADJACENT     BIT(1)
#define IA32_PREFETCHER_MSR_DCU             BIT(2)
#define IA32_PREFETCHER_MSR_DCU_IP          BIT(3)

#define IA32_SPEC_CTRL_MSR                  0x48
#define IA32_SPEC_CTRL_MSR_IBRS             BIT(0) /* Indirect Branch Restricted Speculation */
#define IA32_SPEC_CTRL_MSR_STIBP            BIT(1) /* Single Thread Indirect Branch Predictors */

#define IA32_PRED_CMD_MSR                   0x49

word_t PURE getRestartPC(tcb_t *thread);
void setNextPC(tcb_t *thread, word_t v);

static uint64_t x86_rdmsr(const uint32_t reg)
{
    uint32_t low, high;
    uint64_t value;
    asm volatile("rdmsr" : "=a"(low), "=d"(high) : "c"(reg));
    value = ((uint64_t)high << 32) | (uint64_t)low;
    return value;
}

/* Read model specific register */
static inline uint32_t x86_rdmsr_low(const uint32_t reg)
{
    return (uint32_t)x86_rdmsr(reg);
}

static inline uint32_t x86_rdmsr_high(const uint32_t reg)
{
    return (uint32_t)(x86_rdmsr(reg) >> 32ull);
}

/* Write model specific register */
static inline void x86_wrmsr_parts(const uint32_t reg, const uint32_t high, const uint32_t low)
{
    asm volatile("wrmsr" :: "a"(low), "d"(high), "c"(reg));
}

static inline void x86_wrmsr(const uint32_t reg, const uint64_t val)
{
    uint32_t low = (uint32_t)val;
    uint32_t high = (uint32_t)(val >> 32);
    x86_wrmsr_parts(reg, high, low);
}

/* Read different parts of CPUID */
static inline uint32_t x86_cpuid_edx(uint32_t eax, uint32_t ecx)
{
    uint32_t edx, ebx;
    asm volatile("cpuid"
                 : "=a"(eax),
                 "=b"(ebx),
                 "=c"(ecx),
                 "=d"(edx)
                 : "a"(eax), "c"(ecx)
                 : "memory");
    return edx;
}

static inline uint32_t x86_cpuid_eax(uint32_t eax, uint32_t ecx)
{
    uint32_t edx, ebx;
    asm volatile("cpuid"
                 : "=a"(eax),
                 "=b"(ebx),
                 "=c"(ecx),
                 "=d"(edx)
                 : "a"(eax), "c"(ecx)
                 : "memory");
    return eax;
}

static inline uint32_t x86_cpuid_ecx(uint32_t eax, uint32_t ecx)
{
    uint32_t edx, ebx;
    asm volatile("cpuid"
                 : "=a"(eax),
                 "=b"(ebx),
                 "=c"(ecx),
                 "=d"(edx)
                 : "a"(eax), "c"(ecx)
                 : "memory");
    return ecx;
}

static inline uint32_t x86_cpuid_ebx(uint32_t eax, uint32_t ecx)
{
    uint32_t edx, ebx;
    asm volatile("cpuid"
                 : "=a"(eax),
                 "=b"(ebx),
                 "=c"(ecx),
                 "=d"(edx)
                 : "a"(eax), "c"(ecx)
                 : "memory");
    return ebx;
}

static inline uint64_t x86_rdtsc(void)
{
    uint32_t hi, lo;
    asm volatile("rdtsc"
                 : "=a"(lo),
                 "=d"(hi)
                );
    return ((uint64_t) hi) << 32llu | (uint64_t) lo;
}

#ifdef ENABLE_SMP_SUPPORT
static inline void arch_pause(void)
{
    asm volatile("pause");
}
#endif /* ENABLE_SMP_SUPPORT */

enum x86_vendor {
    X86_VENDOR_INTEL = 0,
    X86_VENDOR_AMD,
    X86_VENDOR_OTHER
};

typedef struct _x86_cpu_identity {
    uint8_t family, model, stepping, extended_family, extended_model;
    uint8_t brand;
} x86_cpu_identity_t;

typedef struct _cpu_identity {
    enum x86_vendor vendor;
    char vendor_string[X86_CPUID_VENDOR_STRING_MAXLENGTH + 1];

    /* Adjusted and normalized family, model and stepping values as recommended
     * by Intel. The name "display" was chosen because that's the nomenclature
     * Intel uses.
     */
    x86_cpu_identity_t display;
} cpu_identity_t;

/* This, and all its adjoint routines will be called at init time; see boot.c */
BOOT_CODE bool_t x86_cpuid_initialize(void);

/** To be used by code that wants to know the family/model/stepping/brand of
 * a CPU.
 */
x86_cpu_identity_t *x86_cpuid_get_model_info(void);

/** To be used by code that wants to get the CPU vendor name.
 */
cpu_identity_t *x86_cpuid_get_identity(void);

/*
 * Forward declarations here as these may instead be later defined in
 * mode-specific machine.h
 */

static inline void x86_write_fs_base_impl(word_t base);
static inline word_t x86_read_fs_base_impl(void);
static inline void x86_write_gs_base_impl(word_t base);
static inline word_t x86_read_gs_base_impl(void);

#ifdef CONFIG_FSGSBASE_MSR

static inline void x86_write_fs_base_impl(word_t base)
{
    x86_wrmsr(IA32_FS_BASE_MSR, base);
}

static inline word_t x86_read_fs_base_impl(void)
{
    return x86_rdmsr(IA32_FS_BASE_MSR);
}

#endif


#ifdef CONFIG_FSGSBASE_INST

/*
 * With fsgsbase, these registers can and are allowed to be changed from
 * user-space.
 *
 * These calls are also cheap as they read from the hidden register
 * state for the segment selectors rather than from the GDT.
 */

static inline void x86_write_fs_base(word_t base, cpu_id_t cpu)
{
    x86_write_fs_base_impl(base);
}

static inline void x86_write_gs_base(word_t base, cpu_id_t cpu)
{
    x86_write_gs_base_impl(base);
}

static inline word_t x86_read_fs_base(cpu_id_t cpu)
{
    return x86_read_fs_base_impl();
}

static inline word_t x86_read_gs_base(cpu_id_t cpu)
{
    return x86_read_gs_base_impl();
}

#else

/* Writing the fs/gs bases can be expensive (especially if it requires a MSR
   write), so we avoid actually writing them if they aren't actually changed. */

static inline void x86_write_fs_base(word_t base, cpu_id_t cpu)
{
    if (base != ARCH_NODE_STATE_ON_CORE(x86KSCurrentFSBase, cpu)) {
        ARCH_NODE_STATE_ON_CORE(x86KSCurrentFSBase, cpu) = base;
        x86_write_fs_base_impl(base);
    }
}

static inline void x86_write_gs_base(word_t base, cpu_id_t cpu)
{
    if (likely(base != ARCH_NODE_STATE_ON_CORE(x86KSCurrentGSBase, cpu))) {
        ARCH_NODE_STATE_ON_CORE(x86KSCurrentGSBase, cpu) = base;
        x86_write_gs_base_impl(base);
    }
}

static inline word_t x86_read_fs_base(cpu_id_t cpu)
{
    return ARCH_NODE_STATE_ON_CORE(x86KSCurrentFSBase, cpu);
}

static inline word_t x86_read_gs_base(cpu_id_t cpu)
{
    return ARCH_NODE_STATE_ON_CORE(x86KSCurrentGSBase, cpu);
}

#endif


static inline void x86_load_fsgs_base(tcb_t *thread, cpu_id_t cpu)
{
    /*
     * Restore the FS and GS base registers.
     *
     * These should only be accessed inside the kernel, between the
     * entry and exit calls to swapgs if used.
     */
    word_t fs_base = getRegister(thread, FS_BASE);
    x86_write_fs_base(fs_base, cpu);
    word_t gs_base = getRegister(thread, GS_BASE);
    x86_write_gs_base(gs_base, cpu);
}

/* Cleaning memory before user-level access */
static inline void clearMemory(void *ptr, unsigned int bits)
{
    memzero(ptr, BIT(bits));
    /* no cleaning of caches necessary on IA-32 */
}

/* Initialises MSRs required to setup sysenter and sysexit */
void init_sysenter_msrs(void);

/* Read/write memory fence */
static inline void x86_mfence(void)
{
    asm volatile("mfence" ::: "memory");
}

/* Get page fault address from CR2 register */
static inline unsigned long getFaultAddr(void)
{
    return read_cr2();
}

static inline void x86_set_tls_segment_base(word_t tls_base);

/* Update the value of the actual register to hold the expected value */
static inline exception_t Arch_setTLSRegister(word_t tls_base)
{
    word_t sanitised = Mode_sanitiseRegister(TLS_BASE, tls_base);

    if (sanitised != tls_base) {
        return EXCEPTION_SYSCALL_ERROR;
    }

#ifndef CONFIG_FSGSBASE_INST
    /*
     * The context is only updated from the register on a context switch
     * if the FSGS instructions are enabled. When they aren't it must be
     * manually stored here.
     */
    setRegister(NODE_STATE(ksCurThread), TLS_BASE, tls_base);
#endif

    x86_set_tls_segment_base(sanitised);

    return EXCEPTION_NONE;
}

/* we do not cache the IBRS value as writing the enable bit is meaningful even if it
 * is already set. On some processors if the enable bit was set it must be 're-written'
 * in order for a higher privilege to correctly not have its branch predictions affected */
static inline void x86_enable_ibrs(void)
{
    /* we always enable the STIBP bit since we want it on if its supported and it
     * isn't a fault to set the bit if support is missing */
    x86_wrmsr(IA32_SPEC_CTRL_MSR, IA32_SPEC_CTRL_MSR_IBRS | IA32_SPEC_CTRL_MSR_STIBP);
}

static inline void x86_disable_ibrs(void)
{
    /* we always enable the STIBP bit since we want it on if its supported and it
     * isn't a fault to set the bit if support is missing */
    x86_wrmsr(IA32_SPEC_CTRL_MSR, IA32_SPEC_CTRL_MSR_STIBP);
}

static inline void x86_ibpb(void)
{
    x86_wrmsr(IA32_PRED_CMD_MSR, 1);
}

static inline void x86_flush_rsb(void)
{
    /* perform 32 near calls with a non zero displacement to flush the rsb with
     * speculation traps. */
    word_t iter = 32;
    asm volatile(
        "1:\n"
        "sub $2, %[iter]\n"
        "call 2f\n"
        "pause\n"
        "jmp 1b\n"
        "2:\n"
        "call 3f\n"
        "pause\n"
        "jmp 2b\n"
        "3:\n"
        "cmp $0, %[iter]\n"
        "jne 1b\n"
#ifdef CONFIG_ARCH_X86_64
        "add %[stack_amount], %%rsp\n"
#else
        "add %[stack_amount], %%esp\n"
#endif
        : [iter]"+r"(iter)
        : [stack_amount]"i"(sizeof(word_t) * iter)
        : "cc"
    );
}

/* sysenter entry point */
void handle_syscall(void);

/** MODIFIES: phantom_machine_state */
void int_00(void);
/** MODIFIES: phantom_machine_state */
void int_01(void);
/** MODIFIES: phantom_machine_state */
void int_02(void);
/** MODIFIES: phantom_machine_state */
void int_03(void);
/** MODIFIES: phantom_machine_state */
void int_04(void);
/** MODIFIES: phantom_machine_state */
void int_05(void);
/** MODIFIES: phantom_machine_state */
void int_06(void);
/** MODIFIES: phantom_machine_state */
void int_07(void);
/** MODIFIES: phantom_machine_state */
void int_08(void);
/** MODIFIES: phantom_machine_state */
void int_09(void);
/** MODIFIES: phantom_machine_state */
void int_0a(void);
/** MODIFIES: phantom_machine_state */
void int_0b(void);
/** MODIFIES: phantom_machine_state */
void int_0c(void);
/** MODIFIES: phantom_machine_state */
void int_0d(void);
/** MODIFIES: phantom_machine_state */
void int_0e(void);
/** MODIFIES: phantom_machine_state */
void int_0f(void);

/** MODIFIES: phantom_machine_state */
void int_10(void);
/** MODIFIES: phantom_machine_state */
void int_11(void);
/** MODIFIES: phantom_machine_state */
void int_12(void);
/** MODIFIES: phantom_machine_state */
void int_13(void);
/** MODIFIES: phantom_machine_state */
void int_14(void);
/** MODIFIES: phantom_machine_state */
void int_15(void);
/** MODIFIES: phantom_machine_state */
void int_16(void);
/** MODIFIES: phantom_machine_state */
void int_17(void);
/** MODIFIES: phantom_machine_state */
void int_18(void);
/** MODIFIES: phantom_machine_state */
void int_19(void);
/** MODIFIES: phantom_machine_state */
void int_1a(void);
/** MODIFIES: phantom_machine_state */
void int_1b(void);
/** MODIFIES: phantom_machine_state */
void int_1c(void);
/** MODIFIES: phantom_machine_state */
void int_1d(void);
/** MODIFIES: phantom_machine_state */
void int_1e(void);
/** MODIFIES: phantom_machine_state */
void int_1f(void);

/** MODIFIES: phantom_machine_state */
void int_20(void);
/** MODIFIES: phantom_machine_state */
void int_21(void);
/** MODIFIES: phantom_machine_state */
void int_22(void);
/** MODIFIES: phantom_machine_state */
void int_23(void);
/** MODIFIES: phantom_machine_state */
void int_24(void);
/** MODIFIES: phantom_machine_state */
void int_25(void);
/** MODIFIES: phantom_machine_state */
void int_26(void);
/** MODIFIES: phantom_machine_state */
void int_27(void);
/** MODIFIES: phantom_machine_state */
void int_28(void);
/** MODIFIES: phantom_machine_state */
void int_29(void);
/** MODIFIES: phantom_machine_state */
void int_2a(void);
/** MODIFIES: phantom_machine_state */
void int_2b(void);
/** MODIFIES: phantom_machine_state */
void int_2c(void);
/** MODIFIES: phantom_machine_state */
void int_2d(void);
/** MODIFIES: phantom_machine_state */
void int_2e(void);
/** MODIFIES: phantom_machine_state */
void int_2f(void);

/** MODIFIES: phantom_machine_state */
void int_30(void);
/** MODIFIES: phantom_machine_state */
void int_31(void);
/** MODIFIES: phantom_machine_state */
void int_32(void);
/** MODIFIES: phantom_machine_state */
void int_33(void);
/** MODIFIES: phantom_machine_state */
void int_34(void);
/** MODIFIES: phantom_machine_state */
void int_35(void);
/** MODIFIES: phantom_machine_state */
void int_36(void);
/** MODIFIES: phantom_machine_state */
void int_37(void);
/** MODIFIES: phantom_machine_state */
void int_38(void);
/** MODIFIES: phantom_machine_state */
void int_39(void);
/** MODIFIES: phantom_machine_state */
void int_3a(void);
/** MODIFIES: phantom_machine_state */
void int_3b(void);
/** MODIFIES: phantom_machine_state */
void int_3c(void);
/** MODIFIES: phantom_machine_state */
void int_3d(void);
/** MODIFIES: phantom_machine_state */
void int_3e(void);
/** MODIFIES: phantom_machine_state */
void int_3f(void);

/** MODIFIES: phantom_machine_state */
void int_40(void);
/** MODIFIES: phantom_machine_state */
void int_41(void);
/** MODIFIES: phantom_machine_state */
void int_42(void);
/** MODIFIES: phantom_machine_state */
void int_43(void);
/** MODIFIES: phantom_machine_state */
void int_44(void);
/** MODIFIES: phantom_machine_state */
void int_45(void);
/** MODIFIES: phantom_machine_state */
void int_46(void);
/** MODIFIES: phantom_machine_state */
void int_47(void);
/** MODIFIES: phantom_machine_state */
void int_48(void);
/** MODIFIES: phantom_machine_state */
void int_49(void);
/** MODIFIES: phantom_machine_state */
void int_4a(void);
/** MODIFIES: phantom_machine_state */
void int_4b(void);
/** MODIFIES: phantom_machine_state */
void int_4c(void);
/** MODIFIES: phantom_machine_state */
void int_4d(void);
/** MODIFIES: phantom_machine_state */
void int_4e(void);
/** MODIFIES: phantom_machine_state */
void int_4f(void);

/** MODIFIES: phantom_machine_state */
void int_50(void);
/** MODIFIES: phantom_machine_state */
void int_51(void);
/** MODIFIES: phantom_machine_state */
void int_52(void);
/** MODIFIES: phantom_machine_state */
void int_53(void);
/** MODIFIES: phantom_machine_state */
void int_54(void);
/** MODIFIES: phantom_machine_state */
void int_55(void);
/** MODIFIES: phantom_machine_state */
void int_56(void);
/** MODIFIES: phantom_machine_state */
void int_57(void);
/** MODIFIES: phantom_machine_state */
void int_58(void);
/** MODIFIES: phantom_machine_state */
void int_59(void);
/** MODIFIES: phantom_machine_state */
void int_5a(void);
/** MODIFIES: phantom_machine_state */
void int_5b(void);
/** MODIFIES: phantom_machine_state */
void int_5c(void);
/** MODIFIES: phantom_machine_state */
void int_5d(void);
/** MODIFIES: phantom_machine_state */
void int_5e(void);
/** MODIFIES: phantom_machine_state */
void int_5f(void);

/** MODIFIES: phantom_machine_state */
void int_60(void);
/** MODIFIES: phantom_machine_state */
void int_61(void);
/** MODIFIES: phantom_machine_state */
void int_62(void);
/** MODIFIES: phantom_machine_state */
void int_63(void);
/** MODIFIES: phantom_machine_state */
void int_64(void);
/** MODIFIES: phantom_machine_state */
void int_65(void);
/** MODIFIES: phantom_machine_state */
void int_66(void);
/** MODIFIES: phantom_machine_state */
void int_67(void);
/** MODIFIES: phantom_machine_state */
void int_68(void);
/** MODIFIES: phantom_machine_state */
void int_69(void);
/** MODIFIES: phantom_machine_state */
void int_6a(void);
/** MODIFIES: phantom_machine_state */
void int_6b(void);
/** MODIFIES: phantom_machine_state */
void int_6c(void);
/** MODIFIES: phantom_machine_state */
void int_6d(void);
/** MODIFIES: phantom_machine_state */
void int_6e(void);
/** MODIFIES: phantom_machine_state */
void int_6f(void);

/** MODIFIES: phantom_machine_state */
void int_70(void);
/** MODIFIES: phantom_machine_state */
void int_71(void);
/** MODIFIES: phantom_machine_state */
void int_72(void);
/** MODIFIES: phantom_machine_state */
void int_73(void);
/** MODIFIES: phantom_machine_state */
void int_74(void);
/** MODIFIES: phantom_machine_state */
void int_75(void);
/** MODIFIES: phantom_machine_state */
void int_76(void);
/** MODIFIES: phantom_machine_state */
void int_77(void);
/** MODIFIES: phantom_machine_state */
void int_78(void);
/** MODIFIES: phantom_machine_state */
void int_79(void);
/** MODIFIES: phantom_machine_state */
void int_7a(void);
/** MODIFIES: phantom_machine_state */
void int_7b(void);
/** MODIFIES: phantom_machine_state */
void int_7c(void);
/** MODIFIES: phantom_machine_state */
void int_7d(void);
/** MODIFIES: phantom_machine_state */
void int_7e(void);
/** MODIFIES: phantom_machine_state */
void int_7f(void);

/** MODIFIES: phantom_machine_state */
void int_80(void);
/** MODIFIES: phantom_machine_state */
void int_81(void);
/** MODIFIES: phantom_machine_state */
void int_82(void);
/** MODIFIES: phantom_machine_state */
void int_83(void);
/** MODIFIES: phantom_machine_state */
void int_84(void);
/** MODIFIES: phantom_machine_state */
void int_85(void);
/** MODIFIES: phantom_machine_state */
void int_86(void);
/** MODIFIES: phantom_machine_state */
void int_87(void);
/** MODIFIES: phantom_machine_state */
void int_88(void);
/** MODIFIES: phantom_machine_state */
void int_89(void);
/** MODIFIES: phantom_machine_state */
void int_8a(void);
/** MODIFIES: phantom_machine_state */
void int_8b(void);
/** MODIFIES: phantom_machine_state */
void int_8c(void);
/** MODIFIES: phantom_machine_state */
void int_8d(void);
/** MODIFIES: phantom_machine_state */
void int_8e(void);
/** MODIFIES: phantom_machine_state */
void int_8f(void);

/** MODIFIES: phantom_machine_state */
void int_90(void);
/** MODIFIES: phantom_machine_state */
void int_91(void);
/** MODIFIES: phantom_machine_state */
void int_92(void);
/** MODIFIES: phantom_machine_state */
void int_93(void);
/** MODIFIES: phantom_machine_state */
void int_94(void);
/** MODIFIES: phantom_machine_state */
void int_95(void);
/** MODIFIES: phantom_machine_state */
void int_96(void);
/** MODIFIES: phantom_machine_state */
void int_97(void);
/** MODIFIES: phantom_machine_state */
void int_98(void);
/** MODIFIES: phantom_machine_state */
void int_99(void);
/** MODIFIES: phantom_machine_state */
void int_9a(void);
/** MODIFIES: phantom_machine_state */
void int_9b(void);
/** MODIFIES: phantom_machine_state */
void int_9c(void);
/** MODIFIES: phantom_machine_state */
void int_9d(void);
/** MODIFIES: phantom_machine_state */
void int_9e(void);
/** MODIFIES: phantom_machine_state */
void int_9f(void);

/** MODIFIES: phantom_machine_state */
void int_a0(void);
/** MODIFIES: phantom_machine_state */
void int_a1(void);
/** MODIFIES: phantom_machine_state */
void int_a2(void);
/** MODIFIES: phantom_machine_state */
void int_a3(void);
/** MODIFIES: phantom_machine_state */
void int_a4(void);
/** MODIFIES: phantom_machine_state */
void int_a5(void);
/** MODIFIES: phantom_machine_state */
void int_a6(void);
/** MODIFIES: phantom_machine_state */
void int_a7(void);
/** MODIFIES: phantom_machine_state */
void int_a8(void);
/** MODIFIES: phantom_machine_state */
void int_a9(void);
/** MODIFIES: phantom_machine_state */
void int_aa(void);
/** MODIFIES: phantom_machine_state */
void int_ab(void);
/** MODIFIES: phantom_machine_state */
void int_ac(void);
/** MODIFIES: phantom_machine_state */
void int_ad(void);
/** MODIFIES: phantom_machine_state */
void int_ae(void);
/** MODIFIES: phantom_machine_state */
void int_af(void);

/** MODIFIES: phantom_machine_state */
void int_b0(void);
/** MODIFIES: phantom_machine_state */
void int_b1(void);
/** MODIFIES: phantom_machine_state */
void int_b2(void);
/** MODIFIES: phantom_machine_state */
void int_b3(void);
/** MODIFIES: phantom_machine_state */
void int_b4(void);
/** MODIFIES: phantom_machine_state */
void int_b5(void);
/** MODIFIES: phantom_machine_state */
void int_b6(void);
/** MODIFIES: phantom_machine_state */
void int_b7(void);
/** MODIFIES: phantom_machine_state */
void int_b8(void);
/** MODIFIES: phantom_machine_state */
void int_b9(void);
/** MODIFIES: phantom_machine_state */
void int_ba(void);
/** MODIFIES: phantom_machine_state */
void int_bb(void);
/** MODIFIES: phantom_machine_state */
void int_bc(void);
/** MODIFIES: phantom_machine_state */
void int_bd(void);
/** MODIFIES: phantom_machine_state */
void int_be(void);
/** MODIFIES: phantom_machine_state */
void int_bf(void);

/** MODIFIES: phantom_machine_state */
void int_c0(void);
/** MODIFIES: phantom_machine_state */
void int_c1(void);
/** MODIFIES: phantom_machine_state */
void int_c2(void);
/** MODIFIES: phantom_machine_state */
void int_c3(void);
/** MODIFIES: phantom_machine_state */
void int_c4(void);
/** MODIFIES: phantom_machine_state */
void int_c5(void);
/** MODIFIES: phantom_machine_state */
void int_c6(void);
/** MODIFIES: phantom_machine_state */
void int_c7(void);
/** MODIFIES: phantom_machine_state */
void int_c8(void);
/** MODIFIES: phantom_machine_state */
void int_c9(void);
/** MODIFIES: phantom_machine_state */
void int_ca(void);
/** MODIFIES: phantom_machine_state */
void int_cb(void);
/** MODIFIES: phantom_machine_state */
void int_cc(void);
/** MODIFIES: phantom_machine_state */
void int_cd(void);
/** MODIFIES: phantom_machine_state */
void int_ce(void);
/** MODIFIES: phantom_machine_state */
void int_cf(void);

/** MODIFIES: phantom_machine_state */
void int_d0(void);
/** MODIFIES: phantom_machine_state */
void int_d1(void);
/** MODIFIES: phantom_machine_state */
void int_d2(void);
/** MODIFIES: phantom_machine_state */
void int_d3(void);
/** MODIFIES: phantom_machine_state */
void int_d4(void);
/** MODIFIES: phantom_machine_state */
void int_d5(void);
/** MODIFIES: phantom_machine_state */
void int_d6(void);
/** MODIFIES: phantom_machine_state */
void int_d7(void);
/** MODIFIES: phantom_machine_state */
void int_d8(void);
/** MODIFIES: phantom_machine_state */
void int_d9(void);
/** MODIFIES: phantom_machine_state */
void int_da(void);
/** MODIFIES: phantom_machine_state */
void int_db(void);
/** MODIFIES: phantom_machine_state */
void int_dc(void);
/** MODIFIES: phantom_machine_state */
void int_dd(void);
/** MODIFIES: phantom_machine_state */
void int_de(void);
/** MODIFIES: phantom_machine_state */
void int_df(void);

/** MODIFIES: phantom_machine_state */
void int_e0(void);
/** MODIFIES: phantom_machine_state */
void int_e1(void);
/** MODIFIES: phantom_machine_state */
void int_e2(void);
/** MODIFIES: phantom_machine_state */
void int_e3(void);
/** MODIFIES: phantom_machine_state */
void int_e4(void);
/** MODIFIES: phantom_machine_state */
void int_e5(void);
/** MODIFIES: phantom_machine_state */
void int_e6(void);
/** MODIFIES: phantom_machine_state */
void int_e7(void);
/** MODIFIES: phantom_machine_state */
void int_e8(void);
/** MODIFIES: phantom_machine_state */
void int_e9(void);
/** MODIFIES: phantom_machine_state */
void int_ea(void);
/** MODIFIES: phantom_machine_state */
void int_eb(void);
/** MODIFIES: phantom_machine_state */
void int_ec(void);
/** MODIFIES: phantom_machine_state */
void int_ed(void);
/** MODIFIES: phantom_machine_state */
void int_ee(void);
/** MODIFIES: phantom_machine_state */
void int_ef(void);

/** MODIFIES: phantom_machine_state */
void int_f0(void);
/** MODIFIES: phantom_machine_state */
void int_f1(void);
/** MODIFIES: phantom_machine_state */
void int_f2(void);
/** MODIFIES: phantom_machine_state */
void int_f3(void);
/** MODIFIES: phantom_machine_state */
void int_f4(void);
/** MODIFIES: phantom_machine_state */
void int_f5(void);
/** MODIFIES: phantom_machine_state */
void int_f6(void);
/** MODIFIES: phantom_machine_state */
void int_f7(void);
/** MODIFIES: phantom_machine_state */
void int_f8(void);
/** MODIFIES: phantom_machine_state */
void int_f9(void);
/** MODIFIES: phantom_machine_state */
void int_fa(void);
/** MODIFIES: phantom_machine_state */
void int_fb(void);
/** MODIFIES: phantom_machine_state */
void int_fc(void);
/** MODIFIES: phantom_machine_state */
void int_fd(void);
/** MODIFIES: phantom_machine_state */
void int_fe(void);
/** MODIFIES: phantom_machine_state */
void int_ff(void);

#ifdef CONFIG_VTX
void handle_vmexit(void);
#endif

