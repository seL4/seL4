/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <types.h>
#include <machine/registerset.h>
#include <model/statedata.h>
#include <object/structures.h>
#include <arch/machine.h>
#include <arch/machine/hardware.h>
#include <arch/machine/registerset.h>
#include <linker.h>

/* initialises MSRs required to setup sysenter and sysexit */
BOOT_CODE void init_sysenter_msrs(void)
{
    x86_wrmsr(IA32_SYSENTER_CS_MSR, (uint64_t)(word_t)SEL_CS_0);
    x86_wrmsr(IA32_SYSENTER_EIP_MSR, (uint64_t)(word_t)&handle_syscall);
    if (config_set(CONFIG_ARCH_IA32) && !config_set(CONFIG_HARDWARE_DEBUG_API)) {
        /* manually add 4 bytes to x86KStss so that it is valid for both
         * 32-bit and 64-bit, although only ia32 actually requires a valid
         * sysenter esp */
        x86_wrmsr(IA32_SYSENTER_ESP_MSR, (uint64_t)(word_t)((char *)&x86KSGlobalState[CURRENT_CPU_INDEX()].x86KStss.tss.words[0]
                                                            + 4));
    }
}

word_t PURE getRestartPC(tcb_t *thread)
{
    return getRegister(thread, FaultIP);
}

void setNextPC(tcb_t *thread, word_t v)
{
    setRegister(thread, NextIP, v);
}

/* Returns the size of CPU's cacheline */
BOOT_CODE uint32_t CONST getCacheLineSizeBits(void)
{
    uint32_t line_size;
    uint32_t n;

    line_size = getCacheLineSize();
    if (line_size == 0) {
        printf("Cacheline size must be >0\n");
        return 0;
    }

    /* determine size_bits */
    n = 0;
    while (!(line_size & 1)) {
        line_size >>= 1;
        n++;
    }

    if (line_size != 1) {
        printf("Cacheline size must be a power of two\n");
        return 0;
    }

    return n;
}

/* Flushes a specific memory range from the CPU cache */

void flushCacheRange(void *vaddr, uint32_t size_bits)
{
    word_t v;

    assert(size_bits < seL4_WordBits);
    assert(IS_ALIGNED((word_t)vaddr, size_bits));

    x86_mfence();

    for (v = ROUND_DOWN((word_t)vaddr, x86KScacheLineSizeBits);
         v < (word_t)vaddr + BIT(size_bits);
         v += BIT(x86KScacheLineSizeBits)) {
        flushCacheLine((void *)v);
    }
    x86_mfence();
}

/* Disables as many prefetchers as possible */
BOOT_CODE bool_t disablePrefetchers(void)
{
    x86_cpu_identity_t *model_info;
    uint32_t low, high;
    word_t i;

    uint32_t valid_models[] = { BROADWELL_1_MODEL_ID, BROADWELL_2_MODEL_ID,
                                BROADWELL_3_MODEL_ID, BROADWELL_4_MODEL_ID,
                                BROADWELL_5_MODEL_ID,
                                HASWELL_1_MODEL_ID,  HASWELL_2_MODEL_ID,
                                HASWELL_3_MODEL_ID, HASWELL_4_MODEL_ID,
                                IVY_BRIDGE_1_MODEL_ID,  IVY_BRIDGE_2_MODEL_ID,
                                IVY_BRIDGE_3_MODEL_ID,
                                SANDY_BRIDGE_1_MODEL_ID, SANDY_BRIDGE_2_MODEL_ID, WESTMERE_1_MODEL_ID, WESTMERE_2_MODEL_ID,
                                WESTMERE_3_MODEL_ID, NEHALEM_1_MODEL_ID, NEHALEM_2_MODEL_ID, NEHALEM_3_MODEL_ID,
                                SKYLAKE_1_MODEL_ID, SKYLAKE_2_MODEL_ID
                              };

    model_info = x86_cpuid_get_model_info();

    for (i = 0; i < ARRAY_SIZE(valid_models); i++) {
        /* The model ID is only useful when hashed together with the family ID.
         * They are both meant to be combined to form a unique identifier.
         *
         * As far as I can tell though, we might be able to actually just
         * disable prefetching on anything that matches family_ID==0x6, and
         * there is no need to also look at the model_ID.
         */
        if (model_info->family == IA32_PREFETCHER_COMPATIBLE_FAMILIES_ID
            && model_info->model == valid_models[i]) {
            low = x86_rdmsr_low(IA32_PREFETCHER_MSR);
            high = x86_rdmsr_high(IA32_PREFETCHER_MSR);

            low |= IA32_PREFETCHER_MSR_L2;
            low |= IA32_PREFETCHER_MSR_L2_ADJACENT;
            low |= IA32_PREFETCHER_MSR_DCU;
            low |= IA32_PREFETCHER_MSR_DCU_IP;

            x86_wrmsr(IA32_PREFETCHER_MSR, ((uint64_t)high) << 32 | low);

            return true;
        }
    }

    printf("Disabling prefetchers not implemented for CPU fam %x model %x\n",
           model_info->family, model_info->model);
    return false;
}

BOOT_CODE void enablePMCUser(void)
{
    write_cr4(read_cr4() | CR4_PCE);
}

BOOT_CODE bool_t init_ibrs(void)
{
    cpuid_007h_edx_t edx;
    edx.words[0] = x86_cpuid_edx(0x7, 0);
    bool_t support_ibrs = cpuid_007h_edx_get_ibrs_ibpb(edx);
    if ((config_set(CONFIG_KERNEL_X86_IBRS_BASIC) || config_set(CONFIG_KERNEL_X86_IBRS_STIBP)) && !support_ibrs) {
        printf("IBRS not supported by CPU\n");
        return false;
    } else if (support_ibrs) {
        /* 'disable' IBRS. For IBRS_BASIC this does nothing, and for STIBP this will cause
         * us to enable STIBP, and we can then forget about it */
        x86_disable_ibrs();
    }
    /* IBRS is also the feature flag for IBPB */
    if (config_set(CONFIG_KERNEL_X86_IBPB_ON_CONTEXT_SWITCH) && !support_ibrs) {
        printf("IBPB not supported by CPU\n");
        return false;
    }
    /* check for enhanced IBRS */
    bool_t enhanced_ibrs = false;
    if (cpuid_007h_edx_get_ia32_arch_cap_msr(edx)) {
        ia32_arch_capabilities_msr_t cap_msr;
        cap_msr.words[0] = x86_rdmsr(IA32_ARCH_CAPABILITIES_MSR);
        if (ia32_arch_capabilities_msr_get_ibrs_all(cap_msr)) {
            enhanced_ibrs = true;
        }
    }
    if (config_set(CONFIG_KERNEL_X86_IBRS_BASIC) && enhanced_ibrs) {
        printf("Kernel configured for basic IBRS, but CPU supports enhanced IBRS. "
               "Enable enhanced IBRS for improved performance\n");
    }
    if (config_set(CONFIG_KERNEL_X86_IBRS_ALL)) {
        if (!enhanced_ibrs) {
            printf("Enhanced IBRS not supported by CPU\n");
            return false;
        }
        /* enable IBRS and then we can forget about it */
        x86_enable_ibrs();
    }
    return true;
}
