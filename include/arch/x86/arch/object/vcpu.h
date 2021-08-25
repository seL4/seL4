/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <api/failures.h>

#define VCPU_VMCS_SIZE 4096
#define VCPU_IOBITMAP_SIZE 8192

#define VMX_CONTROL_VPID 0x00000000

#define VMX_GUEST_ES_SELECTOR 0x00000800
#define VMX_GUEST_CS_SELECTOR 0x00000802
#define VMX_GUEST_SS_SELECTOR 0x00000804
#define VMX_GUEST_DS_SELECTOR 0x00000806
#define VMX_GUEST_FS_SELECTOR 0x00000808
#define VMX_GUEST_GS_SELECTOR 0x0000080A
#define VMX_GUEST_LDTR_SELECTOR 0x0000080C
#define VMX_GUEST_TR_SELECTOR 0x0000080E

#define VMX_HOST_ES_SELECTOR 0x00000C00
#define VMX_HOST_CS_SELECTOR 0x00000C02
#define VMX_HOST_SS_SELECTOR 0x00000C04
#define VMX_HOST_DS_SELECTOR 0x00000C06
#define VMX_HOST_FS_SELECTOR 0x00000C08
#define VMX_HOST_GS_SELECTOR 0x00000C0A
#define VMX_HOST_TR_SELECTOR 0x00000C0C

#define VMX_CONTROL_IOA_ADDRESS 0x00002000
#define VMX_CONTROL_IOB_ADDRESS 0x00002002
#define VMX_CONTROL_MSR_ADDRESS 0x00002004
#define VMX_CONTROL_EXIT_MSR_STORE_ADDRESS 0x00002006
#define VMX_CONTROL_EXIT_MSR_LOAD_ADDRESS 0x00002008
#define VMX_CONTROL_ENTRY_MSR_LOAD_ADDRESS 0x0000200A
#define VMX_CONTROL_EXECUTIVE_VMCS_POINTER 0x0000200C
#define VMX_CONTROL_TSC_OFFSET 0x00002010
#define VMX_CONTROL_VIRTUAL_APIC_ADDRESS 0x00002012
#define VMX_CONTROL_APIC_ACCESS_ADDRESS 0x00002014
#define VMX_CONTROL_EPT_POINTER 0x0000201A

#define VMX_DATA_GUEST_PHYSICAL 0x00002400

#define VMX_GUEST_VMCS_LINK_POINTER 0x00002800
#define VMX_GUEST_VMCS_LINK_POINTER_HIGH 0x00002801
#define VMX_GUEST_DEBUGCTRL 0x00002802
#define VMX_GUEST_PAT 0x00002804
#define VMX_GUEST_EFER 0x00002806
#define VMX_GUEST_PERF_GLOBAL_CTRL 0x00002808
#define VMX_GUEST_PDPTE0 0x0000280A
#define VMX_GUEST_PDPTE1 0x0000280C
#define VMX_GUEST_PDPTE2 0x0000280E
#define VMX_GUEST_PDPTE3 0x00002810

#define VMX_HOST_PAT 0x00002C00
#define VMX_HOST_EFER 0x00002C02
#define VMX_HOST_PERF_GLOBAL_CTRL 0x00002C04

#define VMX_CONTROL_PIN_EXECUTION_CONTROLS 0x00004000
#define VMX_CONTROL_PRIMARY_PROCESSOR_CONTROLS 0x00004002
#define VMX_CONTROL_EXCEPTION_BITMAP 0x00004004
#define VMX_CONTROL_PAGE_FAULT_MASK 0x00004006
#define VMX_CONTROL_PAGE_FAULT_MATCH 0x00004008
#define VMX_CONTROL_CR3_TARGET_COUNT 0x0000400A
#define VMX_CONTROL_EXIT_CONTROLS 0x0000400C
#define VMX_CONTROL_EXIT_MSR_STORE_COUNT 0x0000400E
#define VMX_CONTROL_EXIT_MSR_LOAD_COUNT 0x00004010
#define VMX_CONTROL_ENTRY_CONTROLS 0x00004012
#define VMX_CONTROL_ENTRY_MSR_LOAD_COUNT 0x00004014
#define VMX_CONTROL_ENTRY_INTERRUPTION_INFO 0x00004016
#define VMX_CONTROL_ENTRY_EXCEPTION_ERROR_CODE 0x00004018
#define VMX_CONTROL_ENTRY_INSTRUCTION_LENGTH 0x0000401A
#define VMX_CONTROL_TPR_THRESHOLD 0x0000401C
#define VMX_CONTROL_SECONDARY_PROCESSOR_CONTROLS 0x0000401E
#define VMX_CONTROL_PLE_GAP 0x00004020
#define VMX_CONTROL_PLE_WINDOW 0x00004022

#define VMX_DATA_INSTRUCTION_ERROR 0x00004400
#define VMX_DATA_EXIT_REASON 0x00004402
#define VMX_DATA_EXIT_INTERRUPT_INFO 0x00004404
#define VMX_DATA_EXIT_INTERRUPT_ERROR 0x00004406
#define VMX_DATA_IDT_VECTOR_INFO 0x00004408
#define VMX_DATA_IDT_VECTOR_ERROR 0x0000440A
#define VMX_DATA_EXIT_INSTRUCTION_LENGTH 0x0000440C
#define VMX_DATA_EXIT_INSTRUCTION_INFO 0x0000440E

#define VMX_GUEST_ES_LIMIT 0x00004800
#define VMX_GUEST_CS_LIMIT 0x00004802
#define VMX_GUEST_SS_LIMIT 0x00004804
#define VMX_GUEST_DS_LIMIT 0x00004806
#define VMX_GUEST_FS_LIMIT 0x00004808
#define VMX_GUEST_GS_LIMIT 0x0000480A
#define VMX_GUEST_LDTR_LIMIT 0x0000480C
#define VMX_GUEST_TR_LIMIT 0x0000480E
#define VMX_GUEST_GDTR_LIMIT 0x00004810
#define VMX_GUEST_IDTR_LIMIT 0x00004812
#define VMX_GUEST_ES_ACCESS_RIGHTS 0x00004814
#define VMX_GUEST_CS_ACCESS_RIGHTS 0x00004816
#define VMX_GUEST_SS_ACCESS_RIGHTS 0x00004818
#define VMX_GUEST_DS_ACCESS_RIGHTS 0x0000481A
#define VMX_GUEST_FS_ACCESS_RIGHTS 0x0000481C
#define VMX_GUEST_GS_ACCESS_RIGHTS 0x0000481E
#define VMX_GUEST_LDTR_ACCESS_RIGHTS 0x00004820
#define VMX_GUEST_TR_ACCESS_RIGHTS 0x00004822
#define VMX_GUEST_INTERRUPTABILITY 0x00004824
#define VMX_GUEST_ACTIVITY 0x00004826
#define VMX_GUEST_SMBASE 0x00004828
#define VMX_GUEST_SYSENTER_CS 0x0000482A
#define VMX_GUEST_PREEMPTION_TIMER_VALUE 0x0000482E

#define VMX_HOST_SYSENTER_CS 0x00004C00

#define VMX_CONTROL_CR0_MASK 0x00006000
#define VMX_CONTROL_CR4_MASK 0x00006002
#define VMX_CONTROL_CR0_READ_SHADOW 0x00006004
#define VMX_CONTROL_CR4_READ_SHADOW 0x00006006
#define VMX_CONTROL_CR3_TARGET0 0x00006008
#define VMX_CONTROL_CR3_TARGET1 0x0000600A
#define VMX_CONTROL_CR3_TARGET2 0x0000600C
#define VMX_CONTROL_CR3_TARGET3 0x0000600E

#define VMX_DATA_EXIT_QUALIFICATION 0x00006400
#define VMX_DATA_IO_RCX 0x00006402
#define VMX_DATA_IO_RSI 0x00006404
#define VMX_DATA_IO_RDI 0x00006406
#define VMX_DATA_IO_RIP 0x00006408
#define VMX_DATA_GUEST_LINEAR_ADDRESS 0x0000640A

#define VMX_GUEST_CR0 0x00006800
#define VMX_GUEST_CR3 0x00006802
#define VMX_GUEST_CR4 0x00006804
#define VMX_GUEST_ES_BASE 0x00006806
#define VMX_GUEST_CS_BASE 0x00006808
#define VMX_GUEST_SS_BASE 0x0000680A
#define VMX_GUEST_DS_BASE 0x0000680C
#define VMX_GUEST_FS_BASE 0x0000680E
#define VMX_GUEST_GS_BASE 0x00006810
#define VMX_GUEST_LDTR_BASE 0x00006812
#define VMX_GUEST_TR_BASE 0x00006814
#define VMX_GUEST_GDTR_BASE 0x00006816
#define VMX_GUEST_IDTR_BASE 0x00006818
#define VMX_GUEST_DR7 0x0000681A
#define VMX_GUEST_RSP 0x0000681C
#define VMX_GUEST_RIP 0x0000681E
#define VMX_GUEST_RFLAGS 0x00006820
#define VMX_GUEST_PENDING_DEBUG_EXCEPTIONS 0x00006822
#define VMX_GUEST_SYSENTER_ESP 0x00006824
#define VMX_GUEST_SYSENTER_EIP 0x00006826

#define VMX_HOST_CR0 0x00006C00
#define VMX_HOST_CR3 0x00006C02
#define VMX_HOST_CR4 0x00006C04
#define VMX_HOST_FS_BASE 0x00006C06
#define VMX_HOST_GS_BASE 0x00006C08
#define VMX_HOST_TR_BASE 0x00006C0A
#define VMX_HOST_GDTR_BASE 0x00006C0C
#define VMX_HOST_IDTR_BASE 0x00006C0E
#define VMX_HOST_SYSENTER_ESP 0x00006C10
#define VMX_HOST_SYSENTER_EIP 0x00006C12
#define VMX_HOST_RSP 0x00006C14
#define VMX_HOST_RIP 0x00006C16

/* Exit reasons. */
enum exit_reasons {
    EXCEPTION_OR_NMI = 0x00,
    EXTERNAL_INTERRUPT = 0x01,
    TRIPLE_FAULT = 0x02,
    INIT_SIGNAL = 0x03,
    SIPI = 0x04,
    /*IO_SMI = 0x05,
     *   OTHER_SMI = 0x06,*/
    INTERRUPT_WINDOW = 0x07,
    NMI_WINDOW = 0x08,
    TASK_SWITCH = 0x09,
    CPUID = 0x0A,
    GETSEC = 0x0B,
    HLT = 0x0C,
    INVD = 0x0D,
    INVLPG = 0x0E,
    RDPMC = 0x0F,
    RDTSC = 0x10,
    RSM = 0x11,
    VMCALL = 0x12,
    VMCLEAR = 0x13,
    VMLAUNCH = 0x14,
    VMPTRLD = 0x15,
    VMPTRST = 0x16,
    VMREAD = 0x17,
    VMRESUME = 0x18,
    VMWRITE = 0x19,
    VMXOFF = 0x1A,
    VMXON = 0x1B,
    CONTROL_REGISTER = 0x1C,
    MOV_DR = 0x1D,
    IO = 0x1E,
    RDMSR = 0x1F,
    WRMSR = 0x20,
    INVALID_GUEST_STATE = 0x21,
    MSR_LOAD_FAIL = 0x22,
    /* 0x23 */
    MWAIT = 0x24,
    MONITOR_TRAP_FLAG = 0x25,
    /* 0x26 */
    MONITOR = 0x27,
    PAUSE = 0x28,
    MACHINE_CHECK = 0x29,
    /* 0x2A */
    TPR_BELOW_THRESHOLD = 0x2B,
    APIC_ACCESS = 0x2C,
    GDTR_OR_IDTR = 0x2E,
    LDTR_OR_TR = 0x2F,
    EPT_VIOLATION = 0x30,
    EPT_MISCONFIGURATION = 0x31,
    INVEPT = 0x32,
    RDTSCP = 0x33,
    VMX_PREEMPTION_TIMER = 0x34,
    INVVPID = 0x35,
    WBINVD = 0x36,
    XSETBV = 0x37
};

#define VPID_INVALID 0
#define VPID_FIRST 1
#define VPID_LAST (CONFIG_MAX_VPIDS - 1)

typedef uint16_t vpid_t;

#ifdef CONFIG_VTX
#define VTX_TERNARY(vtx, nonvtx) vtx

enum vcpu_gp_register {
    VCPU_EAX = 0,
    VCPU_EBX,
    VCPU_ECX,
    VCPU_EDX,
    VCPU_ESI,
    VCPU_EDI,
    VCPU_EBP,
    n_vcpu_gp_register,
    /* We need to define a sentinal value to detect ESP that is strictly distinct
     * from any of our other GP register indexes, so put that here */
    VCPU_ESP,
};

typedef enum vcpu_gp_register vcpu_gp_register_t;;

const vcpu_gp_register_t crExitRegs[];

struct vcpu {
    /* Storage for VMCS region. First field of vcpu_t so they share address.
     * Will use at most 4KiB of memory. Statically reserve 4KiB for convenience. */
    char vmcs[VCPU_VMCS_SIZE];
    word_t io[VCPU_IOBITMAP_SIZE / sizeof(word_t)];

    /* Place the fpu state here so that it is aligned */
    user_fpu_state_t fpuState;

    /* General purpose registers that we have to save and restore as they
     * are not part of the vmcs */
    word_t gp_registers[n_vcpu_gp_register];
#if defined(ENABLE_SMP_SUPPORT) && defined(CONFIG_ARCH_IA32)
    word_t kernelSP;
#endif

    /* TCB associated with this VCPU. */
    struct tcb *vcpuTCB;
    bool_t launched;

    /* Currently assigned VPID */
    vpid_t vpid;

    /* This is the cr0 value has requested by the VCPU owner. The actual cr0 value set at
     * any particular time may be different to this for lazy fpu management, but we will
     * still respect whatever semantics the user has put into cr0. The cr0 value may
     * additionally be modified by the guest depending on how the VCPU owner has configured
     * the cr0 shadow/mask */
    word_t cr0;

    /* These are the values for the shadow and mask that the VCPU owner has requested.
     * Like cr0, they actual values set may be slightly different but semantics will
     * be preserved */
    word_t cr0_shadow;
    word_t cr0_mask;
    word_t exception_bitmap;

    /* These values serve as a cache of what is presently in the VMCS allowing for
     * optimizing away unnecessary calls to vmwrite/vmread */
    word_t cached_exception_bitmap;
    word_t cached_cr0_shadow;
    word_t cached_cr0_mask;
    word_t cached_cr0;

    /* Last used EPT root */
    word_t last_ept_root;

#ifndef CONFIG_KERNEL_SKIM_WINDOW
    /* Last set host cr3 */
    word_t last_host_cr3;
#endif

#ifdef ENABLE_SMP_SUPPORT
    /* Core this VCPU was last loaded on, or is currently loaded on */
    word_t last_cpu;
#endif /* ENABLE_SMP_SUPPORT */
};
typedef struct vcpu vcpu_t;

compile_assert(vcpu_size_sane, sizeof(vcpu_t) <= BIT(seL4_X86_VCPUBits))
unverified_compile_assert(vcpu_fpu_state_alignment_valid,
                          OFFSETOF(vcpu_t, fpuState) % MIN_FPU_ALIGNMENT == 0)

/* Initializes a VCPU object with default values. A VCPU object that is not inititlized
 * must not be run/loaded with vmptrld */
void vcpu_init(vcpu_t *vcpu);

/* Cleans up the VCPU object such that its memory can be freed */
void vcpu_finalise(vcpu_t *vcpu);

exception_t decodeX86VCPUInvocation(
    word_t invLabel,
    word_t length,
    cptr_t cptr,
    cte_t *slot,
    cap_t cap,
    word_t *buffer
);

/* Updates the state of the provided VCPU for a SysVMEnter syscall. The state
 * is pulled from the current threads messages registers */
void vcpu_update_state_sysvmenter(vcpu_t *vcpu);

void vcpu_sysvmenter_reply_to_user(tcb_t *tcb);

bool_t vtx_init(void);
exception_t handleVmexit(void);
exception_t handleVmEntryFail(void);
void restoreVMCS(void);
void clearCurrentVCPU(void);

#ifdef ENABLE_SMP_SUPPORT
void VMCheckBoundNotification(tcb_t *tcb);
#endif /* ENABLE_SMP_SUPPORT */

void invept(ept_pml4e_t *ept_pml4);

/* Removes any IO port mappings that have been cached for the given VPID */
void clearVPIDIOPortMappings(vpid_t vpid, uint16_t first, uint16_t last);

static inline word_t vmread(word_t field)
{
    word_t value;
    asm volatile(
        "vmread %1, %0"
        : "=r"(value)
        : "r"(field)
        : "cc"
    );
    return value;
}

#include <machine/io.h>

static inline void vmwrite(word_t field, word_t value)
{
    asm volatile(
        "vmwrite %0, %1"
        :
        : "r"(value), "r"(field)
        : "cc"
    );
}

#else /* CONFIG_VTX */
#define VTX_TERNARY(vtx, nonvtx) nonvtx
#endif /* CONFIG_VTX */
