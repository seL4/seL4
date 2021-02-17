/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <types.h>
#include <util.h>
#include <object/structures.h>
#include <arch/types.h>
#include <plat/machine/devices.h>
#include <arch/object/vcpu.h>
#include <arch/object/iospace.h>
#include <arch/object/ioport.h>
#include <plat/machine.h>

#include <mode/model/statedata.h>


#define TSS_IO_MAP_SIZE (65536 / 8 / sizeof(word_t) + 1)

typedef struct {
    tss_t   tss;
    word_t  io_map[TSS_IO_MAP_SIZE];
} PACKED tss_io_t;

NODE_STATE_BEGIN(archNodeState)
/* Interrupt currently being handled, not preserved across kernel entries */
NODE_STATE_DECLARE(interrupt_t, x86KScurInterrupt);
/* Interrupt that the hardware believes we are currently handling (is marked in service
 * in the APIC) but we have not yet gotten around to handling */
NODE_STATE_DECLARE(interrupt_t, x86KSPendingInterrupt);
/* Bitmask of all cores should receive the reschedule IPI */
NODE_STATE_DECLARE(word_t, ipiReschedulePending);

#ifdef CONFIG_VTX
NODE_STATE_DECLARE(vcpu_t *, x86KSCurrentVCPU);
#endif

NODE_STATE_DECLARE(word_t, x86KSCurrentFSBase);
NODE_STATE_DECLARE(word_t, x86KSCurrentGSBase);

/* If a GP exception occurs and this is non NULL then the exception should return to
 * this location instead of faulting. In addition the GP exception will clear this
 * back to NULL */
NODE_STATE_DECLARE(word_t, x86KSGPExceptReturnTo);

NODE_STATE_TYPE_DECLARE(modeNodeState, mode);
NODE_STATE_END(archNodeState);

/* this is per core state grouped into a separate struct as it needs to be available
 * at all times by the hardware, even when we are running in user mode */
typedef struct x86_arch_global_state {
    /* Task State Segment (TSS), contains currently running TCB in ESP0 */
    tss_io_t x86KStss;
    /* Global Descriptor Table (GDT) */
    gdt_entry_t x86KSgdt[GDT_ENTRIES];
    /* Interrupt Descriptor Table (IDT) */
    idt_entry_t x86KSidt[IDT_ENTRIES];
    PAD_TO_NEXT_CACHE_LN(sizeof(tss_io_t) + GDT_ENTRIES *sizeof(gdt_entry_t) + IDT_ENTRIES *sizeof(idt_entry_t));
} x86_arch_global_state_t;
compile_assert(x86_arch_global_state_padded, (sizeof(x86_arch_global_state_t) % L1_CACHE_LINE_SIZE) == 0)

extern x86_arch_global_state_t x86KSGlobalState[CONFIG_MAX_NUM_NODES] ALIGN(L1_CACHE_LINE_SIZE) SKIM_BSS;

extern asid_pool_t *x86KSASIDTable[];
extern uint32_t x86KScacheLineSizeBits;
extern user_fpu_state_t x86KSnullFpuState ALIGN(MIN_FPU_ALIGNMENT);

#ifdef CONFIG_IOMMU
extern uint32_t x86KSnumDrhu;
extern vtd_rte_t *x86KSvtdRootTable;
extern uint32_t x86KSnumIOPTLevels;
extern uint32_t x86KSnumIODomainIDBits;
extern uint32_t x86KSFirstValidIODomain;
#endif

#ifdef CONFIG_PRINTING
extern uint16_t x86KSconsolePort;
#endif
#if defined(CONFIG_PRINTING) || defined(CONFIG_DEBUG_BUILD)
extern uint16_t x86KSdebugPort;
#endif

extern x86_irq_state_t x86KSIRQState[];

extern word_t x86KSAllocatedIOPorts[NUM_IO_PORTS / CONFIG_WORD_SIZE];
#ifdef CONFIG_KERNEL_MCS
extern uint32_t x86KStscMhz;
extern uint32_t x86KSapicRatio;
#endif

