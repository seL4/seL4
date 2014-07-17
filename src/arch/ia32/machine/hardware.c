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
    ia32_wrmsr(IA32_SYSENTER_CS_MSR,  0, (uint32_t)SEL_CS_0);
    ia32_wrmsr(IA32_SYSENTER_EIP_MSR, 0, (uint32_t)&handle_syscall);
    ia32_wrmsr(IA32_SYSENTER_ESP_MSR, 0, (uint32_t)&ia32KStss.words[1]);
}

word_t PURE getRestartPC(tcb_t *thread)
{
    return getRegister(thread, FaultEIP);
}

void setNextPC(tcb_t *thread, word_t v)
{
    setRegister(thread, NextEIP, v);
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
    uint32_t v;

    assert(size_bits < WORD_BITS);
    assert(IS_ALIGNED((uint32_t)vaddr, size_bits));

    ia32_mfence();

    for (v = ROUND_DOWN((uint32_t)vaddr, ia32KScacheLineSizeBits);
            v < (uint32_t)vaddr + BIT(size_bits);
            v += BIT(ia32KScacheLineSizeBits)) {
        flushCacheLine((void*)v);
    }
    ia32_mfence();
}
