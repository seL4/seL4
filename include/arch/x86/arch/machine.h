/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __ARCH_MACHINE_H
#define __ARCH_MACHINE_H

#include <arch/types.h>
#include <arch/object/structures.h>
#include <arch/machine/hardware.h>
#include <arch/machine/pat.h>
#include <arch/machine/cpu_registers.h>
#include <model/statedata.h>
#include <arch/model/statedata.h>

#define IA32_APIC_BASE_MSR      0x01B
#define IA32_SYSENTER_CS_MSR    0x174
#define IA32_SYSENTER_ESP_MSR   0x175
#define IA32_SYSENTER_EIP_MSR   0x176
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
                 : "=a" (eax),
                 "=b" (ebx),
                 "=c" (ecx),
                 "=d" (edx)
                 : "a" (eax), "c" (ecx)
                 : "memory");
    return edx;
}

static inline uint32_t x86_cpuid_eax(uint32_t eax, uint32_t ecx)
{
    uint32_t edx, ebx;
    asm volatile("cpuid"
                 : "=a" (eax),
                 "=b" (ebx),
                 "=c" (ecx),
                 "=d" (edx)
                 : "a" (eax), "c" (ecx)
                 : "memory");
    return eax;
}

static inline uint32_t x86_cpuid_ecx(uint32_t eax, uint32_t ecx)
{
    uint32_t edx, ebx;
    asm volatile("cpuid"
                 : "=a" (eax),
                 "=b" (ebx),
                 "=c" (ecx),
                 "=d" (edx)
                 : "a" (eax), "c" (ecx)
                 : "memory");
    return ecx;
}

static inline uint32_t x86_cpuid_ebx(uint32_t eax, uint32_t ecx)
{
    uint32_t edx, ebx;
    asm volatile("cpuid"
                 : "=a" (eax),
                 "=b" (ebx),
                 "=c" (ecx),
                 "=d" (edx)
                 : "a" (eax), "c" (ecx)
                 : "memory");
    return ebx;
}

static inline uint64_t x86_rdtsc(void)
{
    uint32_t hi, lo;
    asm volatile("rdtsc"
                 : "=a" (lo),
                 "=d" (hi)
                );
    return ((uint64_t) hi) << 32llu | (uint64_t) lo;
}

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

#ifdef CONFIG_FSGSBASE_MSR

static inline void x86_write_fs_base_impl(word_t base)
{
    x86_wrmsr(IA32_FS_BASE_MSR, base);
}

static inline void x86_write_gs_base_impl(word_t base)
{
    x86_wrmsr(IA32_GS_BASE_MSR, base);
}

static inline word_t x86_read_fs_base_impl(void)
{
    return x86_rdmsr(IA32_FS_BASE_MSR);
}

static inline word_t x86_read_gs_base_impl(void)
{
    return x86_rdmsr(IA32_GS_BASE_MSR);
}

#endif

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

/* Cleaning memory before user-level access */
static inline void clearMemory(void* ptr, unsigned int bits)
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

/* sysenter entry point */
void handle_syscall(void);

void int_00(void);
void int_01(void);
void int_02(void);
void int_03(void);
void int_04(void);
void int_05(void);
void int_06(void);
void int_07(void);
void int_08(void);
void int_09(void);
void int_0a(void);
void int_0b(void);
void int_0c(void);
void int_0d(void);
void int_0e(void);
void int_0f(void);

void int_10(void);
void int_11(void);
void int_12(void);
void int_13(void);
void int_14(void);
void int_15(void);
void int_16(void);
void int_17(void);
void int_18(void);
void int_19(void);
void int_1a(void);
void int_1b(void);
void int_1c(void);
void int_1d(void);
void int_1e(void);
void int_1f(void);

void int_20(void);
void int_21(void);
void int_22(void);
void int_23(void);
void int_24(void);
void int_25(void);
void int_26(void);
void int_27(void);
void int_28(void);
void int_29(void);
void int_2a(void);
void int_2b(void);
void int_2c(void);
void int_2d(void);
void int_2e(void);
void int_2f(void);

void int_30(void);
void int_31(void);
void int_32(void);
void int_33(void);
void int_34(void);
void int_35(void);
void int_36(void);
void int_37(void);
void int_38(void);
void int_39(void);
void int_3a(void);
void int_3b(void);
void int_3c(void);
void int_3d(void);
void int_3e(void);
void int_3f(void);

void int_40(void);
void int_41(void);
void int_42(void);
void int_43(void);
void int_44(void);
void int_45(void);
void int_46(void);
void int_47(void);
void int_48(void);
void int_49(void);
void int_4a(void);
void int_4b(void);
void int_4c(void);
void int_4d(void);
void int_4e(void);
void int_4f(void);

void int_50(void);
void int_51(void);
void int_52(void);
void int_53(void);
void int_54(void);
void int_55(void);
void int_56(void);
void int_57(void);
void int_58(void);
void int_59(void);
void int_5a(void);
void int_5b(void);
void int_5c(void);
void int_5d(void);
void int_5e(void);
void int_5f(void);

void int_60(void);
void int_61(void);
void int_62(void);
void int_63(void);
void int_64(void);
void int_65(void);
void int_66(void);
void int_67(void);
void int_68(void);
void int_69(void);
void int_6a(void);
void int_6b(void);
void int_6c(void);
void int_6d(void);
void int_6e(void);
void int_6f(void);

void int_70(void);
void int_71(void);
void int_72(void);
void int_73(void);
void int_74(void);
void int_75(void);
void int_76(void);
void int_77(void);
void int_78(void);
void int_79(void);
void int_7a(void);
void int_7b(void);
void int_7c(void);
void int_7d(void);
void int_7e(void);
void int_7f(void);

void int_80(void);
void int_81(void);
void int_82(void);
void int_83(void);
void int_84(void);
void int_85(void);
void int_86(void);
void int_87(void);
void int_88(void);
void int_89(void);
void int_8a(void);
void int_8b(void);
void int_8c(void);
void int_8d(void);
void int_8e(void);
void int_8f(void);

void int_90(void);
void int_91(void);
void int_92(void);
void int_93(void);
void int_94(void);
void int_95(void);
void int_96(void);
void int_97(void);
void int_98(void);
void int_99(void);
void int_9a(void);
void int_9b(void);
void int_9c(void);
void int_9d(void);
void int_9e(void);
void int_9f(void);

void int_a0(void);
void int_a1(void);
void int_a2(void);
void int_a3(void);
void int_a4(void);
void int_a5(void);
void int_a6(void);
void int_a7(void);
void int_a8(void);
void int_a9(void);
void int_aa(void);
void int_ab(void);
void int_ac(void);
void int_ad(void);
void int_ae(void);
void int_af(void);

void int_b0(void);
void int_b1(void);
void int_b2(void);
void int_b3(void);
void int_b4(void);
void int_b5(void);
void int_b6(void);
void int_b7(void);
void int_b8(void);
void int_b9(void);
void int_ba(void);
void int_bb(void);
void int_bc(void);
void int_bd(void);
void int_be(void);
void int_bf(void);

void int_c0(void);
void int_c1(void);
void int_c2(void);
void int_c3(void);
void int_c4(void);
void int_c5(void);
void int_c6(void);
void int_c7(void);
void int_c8(void);
void int_c9(void);
void int_ca(void);
void int_cb(void);
void int_cc(void);
void int_cd(void);
void int_ce(void);
void int_cf(void);

void int_d0(void);
void int_d1(void);
void int_d2(void);
void int_d3(void);
void int_d4(void);
void int_d5(void);
void int_d6(void);
void int_d7(void);
void int_d8(void);
void int_d9(void);
void int_da(void);
void int_db(void);
void int_dc(void);
void int_dd(void);
void int_de(void);
void int_df(void);

void int_e0(void);
void int_e1(void);
void int_e2(void);
void int_e3(void);
void int_e4(void);
void int_e5(void);
void int_e6(void);
void int_e7(void);
void int_e8(void);
void int_e9(void);
void int_ea(void);
void int_eb(void);
void int_ec(void);
void int_ed(void);
void int_ee(void);
void int_ef(void);

void int_f0(void);
void int_f1(void);
void int_f2(void);
void int_f3(void);
void int_f4(void);
void int_f5(void);
void int_f6(void);
void int_f7(void);
void int_f8(void);
void int_f9(void);
void int_fa(void);
void int_fb(void);
void int_fc(void);
void int_fd(void);
void int_fe(void);
void int_ff(void);

#ifdef CONFIG_VTX
void handle_vmexit(void);
#endif

#endif
