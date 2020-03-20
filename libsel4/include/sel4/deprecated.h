/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <sel4/macros.h>
#include <sel4/arch/deprecated.h>
#include <sel4/sel4_arch/deprecated.h>

#define SEL4_PFIPC_LABEL SEL4_DEPRECATE_MACRO(seL4_Fault_VMFault)
#define SEL4_PFIPC_LENGTH SEL4_DEPRECATE_MACRO(seL4_VMFault_Length)
#define SEL4_PFIPC_FAULT_IP SEL4_DEPRECATE_MACRO(seL4_VMFault_IP)
#define SEL4_PFIPC_FAULT_ADDR SEL4_DEPRECATE_MACRO(seL4_VMFault_Addr)
#define SEL4_PFIPC_PREFETCH_FAULT SEL4_DEPRECATE_MACRO(seL4_VMFault_PrefetchFault)
#define SEL4_PFIPC_FSR SEL4_DEPRECATE_MACRO(seL4_VMFault_FSR)

#define SEL4_EXCEPT_IPC_LABEL SEL4_DEPRECATE_MACRO(seL4_Fault_UnknownSyscall)
#define SEL4_USER_EXCEPTION_LABEL SEL4_DEPRECATE_MACRO(seL4_Fault_UserException)
#define SEL4_USER_EXCEPTION_LENGTH  SEL4_DEPRECATE_MACRO(seL4_UserException_Length)

#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
#define SEL4_VGIC_MAINTENANCE_LENGTH SEL4_DEPRECATE_MACRO(seL4_VGICMaintenance_Length)
#define SEL4_VGIC_MAINTENANCE_LABEL  SEL4_DEPRECATE_MACRO(seL4_Fault_VGICMaintenance)
#define SEL4_VCPU_FAULT_LENGTH SEL4_DEPRECATE_MACRO(seL4_VCPUFault_Length)
#define SEL4_VCPU_FAULT_LABEL SEL4_DEPRECATE_MACRO(seL4_Fault_VCPUFault)
#endif /* CONFIG_ARM_HYPERVISOR_SUPPORT */

typedef seL4_CapRights_t seL4_CapRights SEL4_DEPRECATED("use seL4_CapRights_t");

typedef union {
    struct {
        seL4_Word fault_ip;
        seL4_Word fault_addr;
        seL4_Word prefetch_fault;
        seL4_Word fsr;
    } regs;
    seL4_Word raw[4];
} seL4_PageFaultIpcRegisters SEL4_DEPRECATED("use seL4_Fault_VMFault_new()");

typedef seL4_Fault_tag_t seL4_FaultType SEL4_DEPRECATED("use seL4_Fault_tag_t");

#define seL4_NoFault SEL4_DEPRECATE_MACRO(seL4_Fault_NullFault)
#define seL4_CapFault SEL4_DEPRECATE_MACRO(seL4_Fault_CapFault)
#define seL4_UnknownSyscall SEL4_DEPRECATE_MACRO(seL4_Fault_UnknownSyscall)
#define seL4_UserException SEL4_DEPRECATE_MACRO(seL4_Fault_UserException)
#define seL4_VMFault SEL4_DEPRECATE_MACRO(seL4_Fault_VMFault)

static inline SEL4_DEPRECATED("removed") seL4_MessageInfo_t
seL4_GetTag(void)
{
    return seL4_GetIPCBuffer()->tag;
}

static inline SEL4_DEPRECATED("removed") void
seL4_SetTag(seL4_MessageInfo_t tag)
{
    seL4_GetIPCBuffer()->tag = tag;
}

static inline SEL4_DEPRECATED("use seL4_GetMR(seL4_VMFault_IP)") seL4_Word
seL4_PF_FIP(void)
{
    return seL4_GetMR(seL4_VMFault_IP);
}

static inline SEL4_DEPRECATED("use seL4_GetMR(seL4_VMFault_Addr)") seL4_Word
seL4_PF_Addr(void)
{
    return seL4_GetMR(seL4_VMFault_Addr);
}

static inline SEL4_DEPRECATED("use seL4_isVMFault_tag") seL4_Word
seL4_isPageFault_MSG(void)
{
    return seL4_isVMFault_tag(seL4_GetIPCBuffer()->tag);
}

static inline SEL4_DEPRECATED("use seL4_isVMFault_tag") seL4_Word
seL4_isPageFault_Tag(seL4_MessageInfo_t tag)
{
    return seL4_isVMFault_tag(tag);
}

static inline SEL4_DEPRECATED("use seL4_isUnknownSyscall_tag") seL4_Word
seL4_isExceptIPC_Tag(seL4_MessageInfo_t tag)
{
    return seL4_isUnknownSyscall_tag(tag);
}

static inline SEL4_DEPRECATED("use seL4_GetMR") seL4_Word
seL4_ExceptIPC_Get(seL4_Word mr)
{
    return seL4_GetMR(mr);
}

static inline SEL4_DEPRECATED("use seL4_SetMR") void
seL4_ExceptIPC_Set(seL4_Word index, seL4_Word val)
{
    seL4_SetMR(index, val);
}

static inline SEL4_DEPRECATED("") seL4_Word
seL4_IsArchSyscallFrom(seL4_MessageInfo_t tag)
{
    return seL4_MessageInfo_get_length(tag) == seL4_UnknownSyscall_Length;
}

static inline SEL4_DEPRECATED("") seL4_Word
seL4_IsArchExceptionFrom(seL4_MessageInfo_t tag)
{
    return seL4_MessageInfo_get_length(tag) == seL4_UnknownSyscall_Length;
}

typedef seL4_Word seL4_CapData_t SEL4_DEPRECATED("Badge and guard data are just seL4_Word type");

static inline SEL4_DEPRECATED("Badges do not need to be constructed") seL4_Word seL4_CapData_Badge_new(seL4_Word badge)
{
    return badge;
}

static inline SEL4_DEPRECATED("Use seL4_CNode_CapData_new().words[0]") seL4_Word seL4_CapData_Guard_new(seL4_Word guard,
                                                                                                        seL4_Word bits)
{
    return seL4_CNode_CapData_new(guard, bits).words[0];
}


