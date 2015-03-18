/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#include <config.h>

#ifdef CONFIG_VTX

#include <types.h>
#include <machine/io.h>
#include <api/failures.h>
#include <api/syscall.h>
#include <kernel/thread.h>
#include <object/objecttype.h>
#include <arch/machine/cpu_registers.h>
#include <arch/model/statedata.h>
#include <arch/object/vcpu.h>
#include <arch/object/vtx.h>
#include <util.h>

#define MSR_VMX_PINBASED_CTLS 0x481
#define MSR_VMX_PROCBASED_CTLS 0x482
#define MSR_VMX_PROCBASED_CTLS2 0x48B
#define MSR_VMX_EXIT_CTLS 0x483
#define MSR_VMX_ENTRY_CTLS 0x484
#define MSR_VMX_TRUE_PINBASED_CTLS 0x48D
#define MSR_VMX_TRUE_PROCBASED_CTLS 0x48E
#define MSR_VMX_TRUE_EXIT_CTLS 0x48F
#define MSR_VMX_TRUE_ENTRY_CTLS 0x490
#define MSR_VMX_CR0_FIXED0 0x486
#define MSR_VMX_CR0_FIXED1 0x487
#define MSR_VMX_CR4_FIXED0 0x488
#define MSR_VMX_CR4_FIXED1 0x489


/* Store fixed field values. */
uint32_t pin_control_high;
uint32_t pin_control_low;
uint32_t primary_control_high;
uint32_t primary_control_low;
uint32_t secondary_control_high;
uint32_t secondary_control_low;
uint32_t entry_control_high;
uint32_t entry_control_low;
uint32_t exit_control_high;
uint32_t exit_control_low;
uint32_t cr0_high;
uint32_t cr0_low;
uint32_t cr4_high;
uint32_t cr4_low;

static void
applyHardwareFixedBits(uint32_t msr, uint32_t* high, uint32_t* low)
{
    uint32_t old_high UNUSED, old_low UNUSED;
    old_high = *high;
    old_low = *low;
    *high |= ia32_rdmsr_low(msr);
    *low &= ia32_rdmsr_high(msr);
}

static void
applyHardwareFixedBitsSplit(uint32_t msr_high, uint32_t msr_low, uint32_t* high, uint32_t* low)
{
    uint32_t old_high UNUSED, old_low UNUSED;
    old_high = *high;
    old_low = *low;
    *high |= ia32_rdmsr_low(msr_high);
    *low &= ia32_rdmsr_low(msr_low);
}

bool_t
init_vtx_fixed_values(bool_t useTrueMsrs)
{
    uint32_t msr_true_offset = useTrueMsrs ? MSR_VMX_TRUE_PINBASED_CTLS - MSR_VMX_PINBASED_CTLS : 0;
    const uint32_t pin_control_mask =
        BIT(0) | //Extern interrlt exiting
        BIT(3) | //NMI exiting
        BIT(5); //virtual NMIs
    const uint32_t primary_control_mask =
        BIT(25) | //Use I/O bitmaps
        BIT(28) | //Use MSR bitmaps
        BIT(31); //Activate secondary controls
    const uint32_t secondary_control_mask =
        BIT(1) | //Enable EPT
        BIT(5); //Enable VPID
    pin_control_high = pin_control_mask;
    pin_control_low = ~0;
    applyHardwareFixedBits(MSR_VMX_PINBASED_CTLS + msr_true_offset, &pin_control_high, &pin_control_low);
    if ((pin_control_low & pin_control_mask) != pin_control_mask) {
        return false;
    }
    primary_control_high = primary_control_mask;
    primary_control_low = ~0;
    applyHardwareFixedBits(MSR_VMX_PROCBASED_CTLS + msr_true_offset, &primary_control_high, &primary_control_low);
    if ((primary_control_low & primary_control_mask) != primary_control_mask) {
        return false;
    }
    secondary_control_high = secondary_control_mask;
    secondary_control_low = ~0;
    applyHardwareFixedBits(MSR_VMX_PROCBASED_CTLS2, &secondary_control_high, &secondary_control_low);
    if ((secondary_control_low & secondary_control_mask) != secondary_control_mask) {
        return false;
    }
    exit_control_high = BIT(15); //Acknowledge interrupt on exit
    exit_control_low = ~0;
    applyHardwareFixedBits(MSR_VMX_EXIT_CTLS + msr_true_offset, &exit_control_high, &exit_control_low);
    if ((exit_control_low & BIT(15)) != BIT(15)) {
        return false;
    }
    entry_control_high = 0;
    entry_control_low = ~0;
    applyHardwareFixedBits(MSR_VMX_ENTRY_CTLS + msr_true_offset, &entry_control_high, &entry_control_low);
    cr0_high = 0;
    cr0_low = ~0;
    applyHardwareFixedBitsSplit(0x486, 0x487, &cr0_high, &cr0_low);
    cr4_high = 0;
    cr4_low = ~0;
    applyHardwareFixedBitsSplit(0x488, 0x489, &cr4_high, &cr4_low);
    return true;
}

static uint32_t
applyFixedBits(uint32_t original, uint32_t high, uint32_t low)
{
    original |= high;
    original &= low;
    return original;
}

void
vcpu_init(vcpu_t *vcpu)
{
    uint32_t *vmcs = (uint32_t*)vcpu;
    vcpu->tcb = NULL;
    vcpu->launched = false;

    *vmcs = vmcs_revision;

    vmclear(vcpu);
    vmptrld(vcpu);

    /* Set fixed host state. */
    /*vmwrite(VMX_HOST_PAT, 0);
    vmwrite(VMX_HOST_EFER, 0);
    vmwrite(VMX_HOST_PERF_GLOBAL_CTRL, 0);*/
    vmwrite(VMX_HOST_CR0, read_cr0());
    /* CR3 is set dynamically. */
    vmwrite(VMX_HOST_CR4, read_cr4());
    vmwrite(VMX_HOST_FS_BASE, 0);
    vmwrite(VMX_HOST_GS_BASE, 0);
    vmwrite(VMX_HOST_TR_BASE, (uint32_t)&ia32KStss);
    vmwrite(VMX_HOST_GDTR_BASE, (uint32_t)ia32KSgdt);
    vmwrite(VMX_HOST_IDTR_BASE, (uint32_t)ia32KSidt);
    vmwrite(VMX_HOST_SYSENTER_CS, (uint32_t)SEL_CS_0);
    vmwrite(VMX_HOST_SYSENTER_EIP, (uint32_t)&handle_syscall);
    vmwrite(VMX_HOST_SYSENTER_ESP, (uint32_t)&ia32KStss.words[1]);
    /* Set host SP to point just beyond the first field to be stored on exit. */
    vmwrite(VMX_HOST_RSP, (uint32_t)&vcpu->gp_registers[EBP + 1]);
    vmwrite(VMX_HOST_RIP, (uint32_t)&handle_vmexit);

    vmwrite(VMX_HOST_ES_SELECTOR, SEL_DS_0);
    vmwrite(VMX_HOST_CS_SELECTOR, SEL_CS_0);
    vmwrite(VMX_HOST_SS_SELECTOR, SEL_DS_0);
    vmwrite(VMX_HOST_DS_SELECTOR, SEL_DS_0);
    vmwrite(VMX_HOST_FS_SELECTOR, 0);
    vmwrite(VMX_HOST_GS_SELECTOR, 0);
    vmwrite(VMX_HOST_TR_SELECTOR, SEL_TSS);

    /* Set fixed VMCS control fields. */
    vmwrite(VMX_CONTROL_PIN_EXECUTION_CONTROLS, applyFixedBits(0, pin_control_high, pin_control_low));
    vmwrite(VMX_CONTROL_PRIMARY_PROCESSOR_CONTROLS, applyFixedBits(0, primary_control_high, primary_control_low));
    vmwrite(VMX_CONTROL_SECONDARY_PROCESSOR_CONTROLS, applyFixedBits(0, secondary_control_high, secondary_control_low));
    vmwrite(VMX_CONTROL_EXIT_CONTROLS, applyFixedBits(0, exit_control_high, exit_control_low));
    vmwrite(VMX_CONTROL_ENTRY_CONTROLS, applyFixedBits(0, entry_control_high, entry_control_low));
    vmwrite(VMX_CONTROL_MSR_ADDRESS, (uint32_t)pptr_to_paddr(msr_bitmap));
    vmwrite(VMX_GUEST_CR0, applyFixedBits(0, cr0_high, cr0_low));
    vmwrite(VMX_GUEST_CR4, applyFixedBits(0, cr4_high, cr4_low));
    /* VPID is the same as the VCPU */
    vmwrite(VMX_CONTROL_VPID, vpid_for_vcpu(vcpu));

    vmwrite(VMX_GUEST_VMCS_LINK_POINTER, ~0);
    vmwrite(VMX_GUEST_VMCS_LINK_POINTER_HIGH, ~0);

    memset(vcpu->io, ~0, 8192);
    vmwrite(VMX_CONTROL_IOA_ADDRESS, pptr_to_paddr(vcpu->io));
    vmwrite(VMX_CONTROL_IOB_ADDRESS, pptr_to_paddr((char *)vcpu->io + 4096));
    vcpu->io_min = -1;
    vcpu->io_max = -1;
    vcpu->cr0 = applyFixedBits(0, cr0_high, cr0_low);
    vcpu->cr0_shadow = 0;
    vcpu->cr0_mask = 0;
    vcpu->exception_mask = 0;
}

void
vcpu_finalise(vcpu_t *vcpu)
{
    if (vcpu->tcb) {
        dissociateVcpuTcb(vcpu->tcb, vcpu);
    }
    if (current_vmcs == vcpu) {
        current_vmcs = NULL;
    }
    vmclear(vcpu);
}

void
associateVcpuTcb(tcb_t *tcb, vcpu_t *vcpu)
{
    if (tcb->tcbArch.vcpu) {
        dissociateVcpuTcb(tcb, tcb->tcbArch.vcpu);
    }
    if (vcpu->tcb) {
        dissociateVcpuTcb(vcpu->tcb, vcpu);
    }
    vcpu->tcb = tcb;
    tcb->tcbArch.vcpu = vcpu;
}

void
dissociateVcpuTcb(tcb_t *tcb, vcpu_t *vcpu)
{
    if (tcb->tcbArch.vcpu != vcpu || vcpu->tcb != tcb) {
        fail("TCB and VCPU not associated.");
    }
    tcb->tcbArch.vcpu = NULL;
    vcpu->tcb = NULL;
}

exception_t decodeIA32VCPUInvocation(
    word_t label,
    unsigned int length,
    cptr_t cptr,
    cte_t* slot,
    cap_t cap,
    extra_caps_t extraCaps,
    word_t* buffer
)
{
    switch (label) {
    case IA32VCPUSetTCB:
        return decodeSetTCB(cap, length, buffer, extraCaps);
    case IA32VCPUReadVMCS:
        return decodeReadVMCS(cap, length, buffer);
    case IA32VCPUWriteVMCS:
        return decodeWriteVMCS(cap, length, buffer);
    case IA32VCPUSetIOPort:
        return decodeSetIOPort(cap, length, buffer, extraCaps);
    case IA32VCPUSetIOPortMask:
        return decodeSetIOPortMask(cap, length, buffer);
    case IA32VCPUWriteRegisters:
        return decodeVCPUWriteRegisters(cap, length, buffer);
    default:
        userError("VCPU: Illegal operation.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }
}

exception_t
decodeVCPUWriteRegisters(cap_t cap, unsigned int length, word_t *buffer)
{
    vcpu_t *vcpu;
    if (length < 7) {
        userError("VCPU WriteRegisters: Truncated message.");
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }
    vcpu = VCPU_PTR(cap_vcpu_cap_get_capVCPUPtr(cap));
    setThreadState(ksCurThread, ThreadState_Restart);
    return invokeVCPUWriteRegisters(vcpu, buffer);
}

exception_t
decodeSetIOPortMask(cap_t cap, unsigned int length, word_t *buffer)
{
    uint32_t low, high;
    int mask;
    vcpu_t *vcpu;
    if (length < 3) {
        userError("VCPU SetIOPortMask: Truncated message.");
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }
    low = getSyscallArg(0, buffer);
    high = getSyscallArg(1, buffer);
    mask = getSyscallArg(2, buffer) == 0 ? 0 : 1;
    vcpu = VCPU_PTR(cap_vcpu_cap_get_capVCPUPtr(cap));
    if (low < vcpu->io_min || high > vcpu->io_max) {
        userError("VCPU SetIOPortMask: Invalid range.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }
    if (vcpu->io_min == -1 || vcpu->io_max == -1) {
        userError("VCPU SetIOPortMask: No IO port set.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }
    setThreadState(ksCurThread, ThreadState_Restart);
    return invokeSetIOPortMask(vcpu, low, high, mask);
}

exception_t
invokeSetIOPortMask(vcpu_t *vcpu, uint32_t low, uint32_t high, int mask)
{
    while (low <= high) {
        /* See if we can optimize a whole word of bits */
        if (low % 32 == 0 && low / 32 != high / 32) {
            vcpu->io[low / 32] = mask ? ~0 : 0;
            low += 32;
        } else {
            if (mask) {
                vcpu->io[low / 32] |= BIT(low % 32);
            } else {
                vcpu->io[low / 32] &= ~BIT(low % 32);
            }
            low++;
        }
    }
    return EXCEPTION_NONE;
}

exception_t
decodeSetIOPort(cap_t cap, unsigned int length, word_t* buffer, extra_caps_t extraCaps)
{
    cap_t ioCap;
    cte_t *tcbSlot;
    deriveCap_ret_t dc_ret;
    if (extraCaps.excaprefs[0] == NULL) {
        userError("VCPU SetIOPort: Truncated message.");
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }
    tcbSlot = extraCaps.excaprefs[0];
    ioCap  = extraCaps.excaprefs[0]->cap;

    dc_ret = deriveCap(tcbSlot, ioCap);
    if (dc_ret.status != EXCEPTION_NONE) {
        return dc_ret.status;
    }
    ioCap = dc_ret.cap;
    if (cap_get_capType(ioCap) != cap_io_port_cap) {
        userError("IOPort cap is not a IOPort cap.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    setThreadState(ksCurThread, ThreadState_Restart);
    return invokeSetIOPort(VCPU_PTR(cap_vcpu_cap_get_capVCPUPtr(cap)), ioCap);
}

exception_t
invokeVCPUWriteRegisters(vcpu_t *vcpu, word_t *buffer)
{
    int i;
    for (i = 0; i <= EBP; i++) {
        vcpu->gp_registers[i] = getSyscallArg(i, buffer);
    }
    return EXCEPTION_NONE;
}

exception_t
invokeSetIOPort(vcpu_t *vcpu, cap_t cap)
{
    uint32_t high, low;
    vcpu->io_port = cap;
    low = cap_io_port_cap_get_capIOPortFirstPort(cap);
    high = cap_io_port_cap_get_capIOPortLastPort(cap) + 1;
    // Set the range
    vcpu->io_min = low;
    vcpu->io_max = high;
    // Clear the IO ports
    /* There is no point clearing the IO ports as we have no
     * security model anyway, so might as well let multiple
     * io ports be set to allow different ranges to be
     * masked */
//    memset(vcpu->io, ~0, 8192);
    return EXCEPTION_NONE;
}

#define MAX_VMCS_FIELDS 32

exception_t
decodeWriteVMCS(cap_t cap, unsigned int length, word_t* buffer)
{
    uint32_t fields[MAX_VMCS_FIELDS];
    uint32_t values[MAX_VMCS_FIELDS];
    int num_fields;
    int i;
    if (length > MAX_VMCS_FIELDS * 2) {
        userError("VCPU WriteVMCS: Too many arguments.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }
    num_fields = length / 2;
    for (i = 0; i < num_fields; i++) {
        uint32_t field = getSyscallArg(i * 2 + 0, buffer);
        uint32_t value = getSyscallArg(i * 2 + 1, buffer);
        switch (field) {
        case VMX_GUEST_RIP:
        case VMX_GUEST_RSP:
        case VMX_GUEST_ES_SELECTOR:
        case VMX_GUEST_CS_SELECTOR:
        case VMX_GUEST_SS_SELECTOR:
        case VMX_GUEST_DS_SELECTOR:
        case VMX_GUEST_FS_SELECTOR:
        case VMX_GUEST_GS_SELECTOR:
        case VMX_GUEST_LDTR_SELECTOR:
        case VMX_GUEST_TR_SELECTOR:
        case VMX_GUEST_DEBUGCTRL:
        case VMX_GUEST_PAT:
        case VMX_GUEST_EFER:
        case VMX_GUEST_PERF_GLOBAL_CTRL:
        case VMX_GUEST_PDPTE0:
        case VMX_GUEST_PDPTE1:
        case VMX_GUEST_PDPTE2:
        case VMX_GUEST_PDPTE3:
        case VMX_GUEST_ES_LIMIT:
        case VMX_GUEST_CS_LIMIT:
        case VMX_GUEST_SS_LIMIT:
        case VMX_GUEST_DS_LIMIT:
        case VMX_GUEST_FS_LIMIT:
        case VMX_GUEST_GS_LIMIT:
        case VMX_GUEST_LDTR_LIMIT:
        case VMX_GUEST_TR_LIMIT:
        case VMX_GUEST_GDTR_LIMIT:
        case VMX_GUEST_IDTR_LIMIT:
        case VMX_GUEST_ES_ACCESS_RIGHTS:
        case VMX_GUEST_CS_ACCESS_RIGHTS:
        case VMX_GUEST_SS_ACCESS_RIGHTS:
        case VMX_GUEST_DS_ACCESS_RIGHTS:
        case VMX_GUEST_FS_ACCESS_RIGHTS:
        case VMX_GUEST_GS_ACCESS_RIGHTS:
        case VMX_GUEST_LDTR_ACCESS_RIGHTS:
        case VMX_GUEST_TR_ACCESS_RIGHTS:
        case VMX_GUEST_INTERRUPTABILITY:
        case VMX_GUEST_ACTIVITY:
        case VMX_GUEST_SMBASE:
        case VMX_GUEST_SYSENTER_CS:
        case VMX_GUEST_PREEMPTION_TIMER_VALUE:
        case VMX_GUEST_ES_BASE:
        case VMX_GUEST_CS_BASE:
        case VMX_GUEST_SS_BASE:
        case VMX_GUEST_DS_BASE:
        case VMX_GUEST_FS_BASE:
        case VMX_GUEST_GS_BASE:
        case VMX_GUEST_LDTR_BASE:
        case VMX_GUEST_TR_BASE:
        case VMX_GUEST_GDTR_BASE:
        case VMX_GUEST_IDTR_BASE:
        case VMX_GUEST_DR7:
        case VMX_GUEST_RFLAGS:
        case VMX_GUEST_PENDING_DEBUG_EXCEPTIONS:
        case VMX_GUEST_SYSENTER_ESP:
        case VMX_GUEST_SYSENTER_EIP:
        case VMX_CONTROL_CR0_MASK:
        case VMX_CONTROL_CR4_MASK:
        case VMX_CONTROL_CR0_READ_SHADOW:
        case VMX_CONTROL_CR4_READ_SHADOW:
        case VMX_GUEST_CR3:
        case VMX_CONTROL_EXCEPTION_BITMAP:
            break;
        case VMX_CONTROL_ENTRY_INTERRUPTION_INFO:
            value &= ~(MASK(31 - 12) << 12);
            break;
        case VMX_CONTROL_PIN_EXECUTION_CONTROLS:
            value = applyFixedBits(value, pin_control_high, pin_control_low);
            break;
        case VMX_CONTROL_PRIMARY_PROCESSOR_CONTROLS:
            value = applyFixedBits(value, primary_control_high, primary_control_low);
            break;
        case VMX_CONTROL_SECONDARY_PROCESSOR_CONTROLS:
            value = applyFixedBits(value, secondary_control_high, secondary_control_low);
            break;
        case VMX_CONTROL_EXIT_CONTROLS:
            value = applyFixedBits(value, exit_control_high, exit_control_low);
            break;
        case VMX_GUEST_CR0:
            value = applyFixedBits(value, cr0_high, cr0_low);
            break;
        case VMX_GUEST_CR4:
            value = applyFixedBits(value, cr4_high, cr4_low);
            break;
        default:
            userError("VCPU WriteVMCS: Invalid field %x.", field);
            current_syscall_error.type = seL4_IllegalOperation;
            return EXCEPTION_SYSCALL_ERROR;
        }
        fields[i] = field;
        values[i] = value;
    }
    setThreadState(ksCurThread, ThreadState_Restart);
    return invokeWriteVMCS(VCPU_PTR(cap_vcpu_cap_get_capVCPUPtr(cap)), num_fields, fields, values);
}
exception_t
invokeWriteVMCS(vcpu_t *vcpu, int num_fields, uint32_t *fields, uint32_t *values)
{
    tcb_t *thread;
    int i;
    thread = ksCurThread;
    if (current_vmcs != vcpu) {
        vmptrld(vcpu);
    }
    for (i = 0; i < num_fields; i++) {
        uint32_t field = fields[i];
        uint32_t value = values[i];
        switch (field) {
        case VMX_CONTROL_EXCEPTION_BITMAP:
            vcpu->exception_mask = vcpu->written_exception_mask = value;
            break;
        case VMX_GUEST_CR0:
            vcpu->cr0 = vcpu->written_cr0 = value;
            break;
        case VMX_CONTROL_CR0_MASK:
            vcpu->cr0_mask = vcpu->written_cr0_mask = value;
            break;
        case VMX_CONTROL_CR0_READ_SHADOW:
            vcpu->cr0_shadow = vcpu->written_cr0_shadow = value;
            break;
        }
        setRegister(thread, msgRegisters[0], value);
        vmwrite(field, value);
    }
    return EXCEPTION_NONE;
}

exception_t
decodeReadVMCS(cap_t cap, unsigned int length, word_t* buffer)
{
    uint32_t fields[MAX_VMCS_FIELDS];
    int num_fields;
    int i;
    if (length > MAX_VMCS_FIELDS) {
        userError("VCPU ReadVMCS: Too many arguments.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }
    num_fields = length;
    for (i = 0; i < num_fields; i++) {
        uint32_t field = getSyscallArg(i, buffer);
        switch (field) {
        case VMX_GUEST_RIP:
        case VMX_GUEST_RSP:
        case VMX_GUEST_ES_SELECTOR:
        case VMX_GUEST_CS_SELECTOR:
        case VMX_GUEST_SS_SELECTOR:
        case VMX_GUEST_DS_SELECTOR:
        case VMX_GUEST_FS_SELECTOR:
        case VMX_GUEST_GS_SELECTOR:
        case VMX_GUEST_LDTR_SELECTOR:
        case VMX_GUEST_TR_SELECTOR:
        case VMX_GUEST_DEBUGCTRL:
        case VMX_GUEST_PAT:
        case VMX_GUEST_EFER:
        case VMX_GUEST_PERF_GLOBAL_CTRL:
        case VMX_GUEST_PDPTE0:
        case VMX_GUEST_PDPTE1:
        case VMX_GUEST_PDPTE2:
        case VMX_GUEST_PDPTE3:
        case VMX_GUEST_ES_LIMIT:
        case VMX_GUEST_CS_LIMIT:
        case VMX_GUEST_SS_LIMIT:
        case VMX_GUEST_DS_LIMIT:
        case VMX_GUEST_FS_LIMIT:
        case VMX_GUEST_GS_LIMIT:
        case VMX_GUEST_LDTR_LIMIT:
        case VMX_GUEST_TR_LIMIT:
        case VMX_GUEST_GDTR_LIMIT:
        case VMX_GUEST_IDTR_LIMIT:
        case VMX_GUEST_ES_ACCESS_RIGHTS:
        case VMX_GUEST_CS_ACCESS_RIGHTS:
        case VMX_GUEST_SS_ACCESS_RIGHTS:
        case VMX_GUEST_DS_ACCESS_RIGHTS:
        case VMX_GUEST_FS_ACCESS_RIGHTS:
        case VMX_GUEST_GS_ACCESS_RIGHTS:
        case VMX_GUEST_LDTR_ACCESS_RIGHTS:
        case VMX_GUEST_TR_ACCESS_RIGHTS:
        case VMX_GUEST_INTERRUPTABILITY:
        case VMX_GUEST_ACTIVITY:
        case VMX_GUEST_SMBASE:
        case VMX_GUEST_SYSENTER_CS:
        case VMX_GUEST_PREEMPTION_TIMER_VALUE:
        case VMX_GUEST_ES_BASE:
        case VMX_GUEST_CS_BASE:
        case VMX_GUEST_SS_BASE:
        case VMX_GUEST_DS_BASE:
        case VMX_GUEST_FS_BASE:
        case VMX_GUEST_GS_BASE:
        case VMX_GUEST_LDTR_BASE:
        case VMX_GUEST_TR_BASE:
        case VMX_GUEST_GDTR_BASE:
        case VMX_GUEST_IDTR_BASE:
        case VMX_GUEST_DR7:
        case VMX_GUEST_RFLAGS:
        case VMX_GUEST_PENDING_DEBUG_EXCEPTIONS:
        case VMX_GUEST_SYSENTER_ESP:
        case VMX_GUEST_SYSENTER_EIP:
        case VMX_CONTROL_CR0_MASK:
        case VMX_CONTROL_CR4_MASK:
        case VMX_CONTROL_CR0_READ_SHADOW:
        case VMX_CONTROL_CR4_READ_SHADOW:
        case VMX_DATA_INSTRUCTION_ERROR:
        case VMX_DATA_EXIT_INTERRUPT_INFO:
        case VMX_DATA_EXIT_INTERRUPT_ERROR:
        case VMX_DATA_IDT_VECTOR_INFO:
        case VMX_DATA_IDT_VECTOR_ERROR:
        case VMX_DATA_EXIT_INSTRUCTION_LENGTH:
        case VMX_DATA_EXIT_INSTRUCTION_INFO:
        case VMX_DATA_GUEST_PHYSICAL:
        case VMX_DATA_IO_RCX:
        case VMX_DATA_IO_RSI:
        case VMX_DATA_IO_RDI:
        case VMX_DATA_IO_RIP:
        case VMX_DATA_GUEST_LINEAR_ADDRESS:
        case VMX_CONTROL_ENTRY_INTERRUPTION_INFO:
        case VMX_CONTROL_PIN_EXECUTION_CONTROLS:
        case VMX_CONTROL_PRIMARY_PROCESSOR_CONTROLS:
        case VMX_CONTROL_EXCEPTION_BITMAP:
        case VMX_CONTROL_EXIT_CONTROLS:
        case VMX_GUEST_CR0:
        case VMX_GUEST_CR3:
        case VMX_GUEST_CR4:
            break;
        default:
            userError("VCPU ReadVMCS: Invalid field %x.", field);
            current_syscall_error.type = seL4_IllegalOperation;
            return EXCEPTION_SYSCALL_ERROR;
        }
        fields[i] = field;
    }
    return invokeReadVMCS(VCPU_PTR(cap_vcpu_cap_get_capVCPUPtr(cap)), num_fields, fields);
}

static uint32_t readVMCSfield(vcpu_t *vcpu, uint32_t field)
{
    switch (field) {
    case VMX_DATA_EXIT_INTERRUPT_INFO:
        return vcpu->interrupt_info;
    case VMX_CONTROL_EXCEPTION_BITMAP:
        return vcpu->exception_mask;
    case VMX_GUEST_CR0:
        return vcpu->cr0;
    case VMX_CONTROL_CR0_MASK:
        return vcpu->cr0_mask;
    case VMX_CONTROL_CR0_READ_SHADOW:
        return vcpu->cr0_shadow;
    }
    if (current_vmcs != vcpu) {
        vmptrld(vcpu);
    }
    return vmread(field);
}

exception_t
invokeReadVMCS(vcpu_t *vcpu, int num_fields, uint32_t *fields)
{
    tcb_t *thread;
    int i;
    word_t *sendBuf;
    thread = ksCurThread;
    sendBuf = lookupIPCBuffer(true, thread);

    for (i = 0; i < n_msgRegisters && i < num_fields; i++) {
        setRegister(thread, msgRegisters[i], readVMCSfield(vcpu, fields[i]));
    }
    if (sendBuf) {
        for (; i < num_fields; i++) {
            sendBuf[i + 1] = readVMCSfield(vcpu, fields[i]);
        }
    }
    setRegister(thread, msgInfoRegister, wordFromMessageInfo(
                    message_info_new(0, 0, 0, i)));
    setThreadState(thread, ThreadState_Running);
    return EXCEPTION_NONE;
}


exception_t
decodeSetTCB(cap_t cap, unsigned int length, word_t* buffer, extra_caps_t extraCaps)
{
    cap_t tcbCap;
    cte_t *tcbSlot;
    deriveCap_ret_t dc_ret;
    if ( extraCaps.excaprefs[0] == NULL) {
        userError("VCPU SetTCB: Truncated message.");
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }
    tcbSlot = extraCaps.excaprefs[0];
    tcbCap  = extraCaps.excaprefs[0]->cap;

    dc_ret = deriveCap(tcbSlot, tcbCap);
    if (dc_ret.status != EXCEPTION_NONE) {
        return dc_ret.status;
    }
    tcbCap = dc_ret.cap;
    if (cap_get_capType(tcbCap) != cap_thread_cap) {
        userError("TCB cap is not a TCB cap.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    setThreadState(ksCurThread, ThreadState_Restart);
    return invokeSetTCB(VCPU_PTR(cap_vcpu_cap_get_capVCPUPtr(cap)), TCB_PTR(cap_thread_cap_get_capTCBPtr(tcbCap)));
}

exception_t
invokeSetTCB(vcpu_t *vcpu, tcb_t *tcb)
{
    associateVcpuTcb(tcb, vcpu);

    return EXCEPTION_NONE;
}

uint16_t vpid_for_vcpu(vcpu_t *vcpu)
{
    return (((uint32_t)vcpu) >> 13) & MASK(16);
}

void vcpu_update_vmenter_state(vcpu_t *vcpu)
{
    word_t *buffer;
    if (current_vmcs != vcpu) {
        vmptrld(vcpu);
    }
    vmwrite(VMX_GUEST_RIP, getRegister(ksCurThread, msgRegisters[0]));
    vmwrite(VMX_CONTROL_PRIMARY_PROCESSOR_CONTROLS, applyFixedBits(getRegister(ksCurThread, msgRegisters[1]), primary_control_high, primary_control_low));
    buffer = lookupIPCBuffer(false, ksCurThread);
    if (!buffer) {
        return;
    }
    vmwrite(VMX_CONTROL_ENTRY_INTERRUPTION_INFO, buffer[3] & (~(MASK(31 - 12) << 12)));
}

#endif
