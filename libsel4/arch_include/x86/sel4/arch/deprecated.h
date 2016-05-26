/*
 * Copyright 2014, NICTA
 *
 * This software may be distributed and modified according to the terms of
 * the BSD 2-Clause license. Note that NO WARRANTY is provided.
 * See "LICENSE_BSD2.txt" for details.
 *
 * @TAG(NICTA_BSD)
 */

#ifndef __LIBSEL4_ARCH_DEPRECATED_H
#define __LIBSEL4_ARCH_DEPRECATED_H

#include <autoconf.h>
#include <sel4/types.h>
#include <sel4/arch/syscalls.h>

#ifdef CONFIG_LIB_SEL4_HAVE_REGISTER_STUBS
static inline seL4_MessageInfo_t __attribute__((deprecated("Use seL4_ReplyRecvWithMRs")))
seL4_ReplyWaitWithMRs(seL4_CPtr src, seL4_MessageInfo_t msgInfo, seL4_Word *sender,
                      seL4_Word *mr0, seL4_Word *mr1)
{
    return seL4_ReplyRecvWithMRs(src, msgInfo, sender, mr0, mr1);
}
#endif

#define IA32_DEPRECATED
// __attribute__((deprecated("use seL4_X86_* instead of seL4_IA32_*")))

#define seL4_IA32_PageDirectoryObject/* _Pragma("seL4_IA32_PageDirectoryObject is depreacted, use seL4_X86_PageDirectoryObject")*/ seL4_X86_PageDirectoryObject
#define seL4_IA32_4K /*_Pragma("seL4_IA32_4K is depreacted, use seL4_X86_4K")*/ seL4_X86_4K
#define seL4_IA32_LargePage /*_Pragma("seL4_IA32_LargePage is depreacted, use seL4_X86_LargePageObject")*/ seL4_X86_LargePageObject
#define seL4_IA32_PageTableObject /*_Pragma("seL4_IA32_PageTableObject is depreacted, use seL4_X86_PageTableObject")*/ seL4_X86_PageTableObject
//#define seL4_IA32_4M /*_Pragma("seL4_IA32_4M is depreacted, use seL4_X86_LargePageObject")*/ seL4_X86_LargePageObject

#define seL4_IA32_Default_VMAttributes /*_Pragma("seL4_IA32_Default_VMAttributes is depreacted, use seL4_X86_Default_VMAttributes")*/ seL4_X86_Default_VMAttributes
#define seL4_IA32_WriteBack /*_Pragma("seL4_IA32_WriteBack is depreacted, use seL4_X86_WriteBack")*/ seL4_X86_WriteBack
#define seL4_IA32_WriteThrough /*_Pragma("seL4_IA32_WriteThrough is depreacted, use seL4_X86_WriteThrough")*/ seL4_X86_WriteThrough
#define seL4_IA32_CacheDisabled /*_Pragma("seL4_IA32_CacheDisabled is depreacted, use seL4_X86_CacheDisabled")*/ seL4_X86_CacheDisabled
#define seL4_IA32_Uncacheable /*_Pragma("seL4_IA32_Uncacheable is depreacted, use seL4_X86_Uncacheable")*/ seL4_X86_Uncacheable
#define seL4_IA32_WriteCombining /*_Pragma("seL4_IA32_WriteCombining is depreacted, use seL4_X86_WriteCombining")*/ seL4_X86_WriteCombining

#define seL4_CapInitThreadASIDPool -1



typedef seL4_X86_VMAttributes seL4_IA32_VMAttributes IA32_DEPRECATED;
typedef seL4_X86_Page_GetAddress_t seL4_IA32_Page_GetAddress_t IA32_DEPRECATED;
typedef seL4_X86_IOPort_In8_t seL4_IA32_IOPort_In8_t IA32_DEPRECATED;
typedef seL4_X86_IOPort_In16_t seL4_IA32_IOPort_In16_t IA32_DEPRECATED;
typedef seL4_X86_IOPort_In32_t seL4_IA32_IOPort_In32_t IA32_DEPRECATED;
typedef seL4_X86_IOSpace seL4_IA32_IOSpace IA32_DEPRECATED;
typedef seL4_X86_IOPort seL4_IA32_IOPort IA32_DEPRECATED;
typedef seL4_X86_Page seL4_IA32_Page IA32_DEPRECATED;
//typedef seL4_X86_PDPT seL4_IA32_PDPT IA32_DEPRECATED;
typedef seL4_X86_PageDirectory seL4_IA32_PageDirectory IA32_DEPRECATED;
typedef seL4_X86_PageTable seL4_IA32_PageTable IA32_DEPRECATED;
typedef seL4_X86_IOPageTable seL4_IA32_IOPageTable IA32_DEPRECATED;
typedef seL4_X86_VCPU_ReadVMCS_t seL4_IA32_VCPU_ReadVMCS_t;
typedef seL4_X86_VCPU_WriteVMCS_t seL4_IA32_VCPU_WriteVMCS_t;

static inline seL4_IA32_VCPU_ReadVMCS_t
seL4_IA32_VCPU_ReadVMCS(seL4_CPtr service, seL4_Uint32 field) {
    return seL4_X86_VCPU_ReadVMCS(service, field);
}

static inline seL4_IA32_VCPU_WriteVMCS_t
seL4_IA32_VCPU_WriteVMCS(seL4_CPtr service, seL4_Uint32 field, seL4_Uint32 value)
{
    return seL4_X86_VCPU_WriteVMCS(service, field, value);
}

static inline int
seL4_IA32_VCPU_SetIOPort(seL4_CPtr service, seL4_CNode ioPort)
{
    return seL4_X86_VCPU_SetIOPort(service, ioPort);
}

static inline int
seL4_IA32_VCPU_SetIOPortMask(seL4_X86_VCPU service, seL4_Uint32 low, seL4_Uint32 high, seL4_Uint32 mask)
{
    return seL4_X86_VCPU_SetIOPortMask(service, low, high, mask);
}

static inline int
seL4_IA32_VCPU_WriteRegisters(seL4_X86_VCPU service, seL4_VCPUContext *regs)
{
    return seL4_X86_VCPU_WriteRegisters(service, regs);
}

static inline int
seL4_IA32_VCPU_SetTCB(seL4_X86_VCPU service, seL4_CNode tcb)
{
    return seL4_X86_VCPU_SetTCB(service, tcb);
}

static inline int IA32_DEPRECATED
seL4_IA32_PageDirectory_Map(seL4_X86_PageDirectory service, seL4_CPtr vroot, seL4_Word vaddr, seL4_X86_VMAttributes attr)
{
    return seL4_X86_PageDirectory_Map(service, vroot, vaddr, attr);
}

static inline int IA32_DEPRECATED
seL4_IA32_PageDirectory_Unmap(seL4_X86_PageDirectory service)
{
    return seL4_X86_PageDirectory_Unmap(service);
}

static inline int IA32_DEPRECATED
seL4_IA32_PageTable_Map(seL4_X86_PageTable service, seL4_CPtr vroot, seL4_Word vaddr, seL4_X86_VMAttributes attr)
{
    return seL4_X86_PageTable_Map(service, vroot, vaddr, attr);
}

static inline int IA32_DEPRECATED
seL4_IA32_PageTable_Unmap(seL4_X86_PageTable service)
{
    return seL4_X86_PageTable_Unmap(service);
}

#ifdef CONFIG_IOMMU
static inline int IA32_DEPRECATED
seL4_IA32_IOPageTable_Map(seL4_X86_IOPageTable service, seL4_X86_IOSpace iospace, seL4_Word ioaddr)
{
    return seL4_X86_IOPageTable_Map(service, iospace, ioaddr);
}

static inline int IA32_DEPRECATED
seL4_IA32_IOPageTable_Unmap(seL4_X86_IOPageTable service)
{
    return seL4_X86_IOPageTable_Unmap(service);
}

static inline int IA32_DEPRECATED
seL4_IA32_Page_MapIO(seL4_X86_Page service, seL4_X86_IOSpace iospace, seL4_CapRights rights, seL4_Word ioaddr)
{
    return seL4_X86_Page_MapIO(service, iospace, rights, ioaddr);
}
#endif

static inline int IA32_DEPRECATED
seL4_IA32_Page_Map(seL4_X86_Page service, seL4_CPtr vroot, seL4_Word vaddr, seL4_CapRights rights, seL4_X86_VMAttributes attr)
{
    return seL4_X86_Page_Map(service, vroot, vaddr, rights, attr);
}

static inline int IA32_DEPRECATED
seL4_IA32_Page_Unmap(seL4_X86_Page service)
{
    return seL4_X86_Page_Unmap(service);
}

static inline seL4_X86_Page_GetAddress_t IA32_DEPRECATED
seL4_IA32_Page_GetAddress(seL4_X86_Page service)
{
    return seL4_X86_Page_GetAddress(service);
}

static inline seL4_X86_IOPort_In8_t IA32_DEPRECATED
seL4_IA32_IOPort_In8(seL4_X86_IOPort service, seL4_Uint16 port)
{
    return seL4_X86_IOPort_In8(service, port);
}

static inline seL4_X86_IOPort_In16_t IA32_DEPRECATED
seL4_IA32_IOPort_In16(seL4_X86_IOPort service, seL4_Uint16 port)
{
    return seL4_X86_IOPort_In16(service, port);
}

static inline seL4_X86_IOPort_In32_t IA32_DEPRECATED
seL4_IA32_IOPort_In32(seL4_X86_IOPort service, seL4_Uint16 port)
{
    return seL4_X86_IOPort_In32(service, port);
}

static inline int IA32_DEPRECATED
seL4_IA32_IOPort_Out8(seL4_X86_IOPort service, seL4_Word port, seL4_Word data)
{
    return seL4_X86_IOPort_Out8(service, port, data);
}
static inline int IA32_DEPRECATED
seL4_IA32_IOPort_Out16(seL4_X86_IOPort service, seL4_Word port, seL4_Word data)
{
    return seL4_X86_IOPort_Out16(service, port, data);
}

static inline int IA32_DEPRECATED
seL4_IA32_IOPort_Out32(seL4_X86_IOPort service, seL4_Word port, seL4_Word data)
{
    return seL4_X86_IOPort_Out32(service, port, data);
}

static inline int IA32_DEPRECATED
seL4_IA32_ASIDPool_Assign(seL4_CPtr service, seL4_CPtr vroot)
{
    return seL4_NoError;
}

static inline int IA32_DEPRECATED
seL4_X86_ASIDPool_Assign(seL4_CPtr service, seL4_CPtr vroot)
{
    return seL4_NoError;
}

#undef IA32_DEPRECATED

#endif /* __ARCH_DEPRECATED_H__ */
