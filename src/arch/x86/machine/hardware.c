/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#include <types.h>
#include <machine/registerset.h>
#include <model/statedata.h>
#include <object/structures.h>
#include <arch/machine.h>
#include <arch/machine/hardware.h>
#include <arch/machine/registerset.h>
#include <arch/linker.h>

/* initialises MSRs required to setup sysenter and sysexit */
BOOT_CODE void
init_sysenter_msrs(void)
{
    x86_wrmsr(IA32_SYSENTER_CS_MSR,  (uint64_t)(word_t)SEL_CS_0);
    x86_wrmsr(IA32_SYSENTER_EIP_MSR, (uint64_t)(word_t)&handle_syscall);
#ifndef CONFIG_HARDWARE_DEBUG_API
    /* manually add 4 bytes to x86KStss so that it is valid for both
     * 32-bit and 64-bit */
    x86_wrmsr(IA32_SYSENTER_ESP_MSR, (uint64_t)(word_t)((char *)&ARCH_NODE_STATE(x86KStss).tss.words[0] + 4));
#endif
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
BOOT_CODE uint32_t CONST
getCacheLineSizeBits(void)
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

void flushCacheRange(void* vaddr, uint32_t size_bits)
{
    word_t v;

    assert(size_bits < seL4_WordBits);
    assert(IS_ALIGNED((word_t)vaddr, size_bits));

    x86_mfence();

    for (v = ROUND_DOWN((word_t)vaddr, x86KScacheLineSizeBits);
            v < (word_t)vaddr + BIT(size_bits);
            v += BIT(x86KScacheLineSizeBits)) {
        flushCacheLine((void*)v);
    }
    x86_mfence();
}

/* Disables as many prefetchers as possible */
BOOT_CODE bool_t
disablePrefetchers()
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
                                WESTMERE_3_MODEL_ID, NEHALEM_1_MODEL_ID, NEHALEM_2_MODEL_ID, NEHALEM_3_MODEL_ID
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
