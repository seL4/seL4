

#include <config.h>

#ifdef CONFIG_RISCV_HE

#include <arch/object/vcpu.h>

#define HIDELEG_USR_SOFT_INT       BIT(0)
#define HIDELEG_SVC_SOFT_INT       BIT(1)
#define HIDELEG_RESERVED           BIT(2)
#define HIDELEG_USR_TIMER_INT      BIT(4)
#define HIDELEG_SVC_TIMER_INT      BIT(5)
#define HIDELEG_USR_EXTERNAL_INT   BIT(8)
#define HIDELEG_SVC_EXTERNAL_INT   BIT(9)

#define HIDELEG_VM  (HIDELEG_USR_SOFT_INT | HIDELEG_SVC_SOFT_INT | HIDELEG_USR_TIMER_INT | \
                     HIDELEG_SVC_TIMER_INT | HIDELEG_USR_EXTERNAL_INT | HIDELEG_SVC_EXTERNAL_INT)

#define HEDELEG_INST_ADDR_MISALIGNED    BIT(0)
#define HEDELEG_INST_ACCESS_FAULT       BIT(1)
#define HEDELEG_ILLEGAL_INST            BIT(2)
#define HEDELEG_BREAKPOINT              BIT(3)
#define HEDELEG_LD_ADDR_MISALIGNED      BIT(4)
#define HEDELEG_LD_ACCESS_FAULT         BIT(5)
#define HEDELEG_ST_AMO_ADDR_MISALIGEND  BIT(6)
#define HEDELEG_ST_AMO_ACCESS_FAULT     BIT(7)
#define HEDELEG_ECALL_USR               BIT(8)
#define HEDELEG_ECALL_SVC               BIT(9)
#define HEDELEG_INST_PAGE_FAULT         BIT(12)
#define HEDELEG_LD_PAGE_FAULT           BIT(13)
#define HEDELEG_ST_AMO_PAGE_FAULT       BIT(15)

#define HEDELEG_VM   (HEDELEG_INST_ADDR_MISALIGNED | HEDELEG_INST_ACCESS_FAULT | \
                      HEDELEG_ILLEGAL_INST | HEDELEG_BREAKPOINT | HEDELEG_LD_ADDR_MISALIGNED | \
                      HEDELEG_LD_ACCESS_FAULT | HEDELEG_ST_AMO_ADDR_MISALIGEND | \
                      HEDELEG_ST_AMO_ACCESS_FAULT | HEDELEG_LD_ACCESS_FAULT | \
                      HEDELEG_ECALL_USR | HEDELEG_ECALL_SVC | HEDELEG_INST_PAGE_FAULT | \
                      HEDELEG_LD_PAGE_FAULT | HEDELEG_ST_AMO_PAGE_FAULT)

#define SSTATUS_SPRV_BIT    (8)

#define DEFAULT_BSSTATUS    0

static word_t vcpu_hw_read_reg(word_t reg_index)
{
    word_t reg = 0;
    switch (reg_index) {
        case seL4_VCPUReg_SSTATUS:
            reg = read_bsstatus();
            break;
        case seL4_VCPUReg_SIE:
            reg = read_bsie();
            break;
        case seL4_VCPUReg_STVEC:
            reg = read_bstvec();
            break;
        case seL4_VCPUReg_SSTRATCH:
            reg = read_bsstratch();
            break;
        case seL4_VCPUReg_SEPC:
            reg = read_bsepc();
            break;
        case seL4_VCPUReg_SCAUSE:
            reg = read_bscause();
            break;
        case seL4_VCPUReg_STVAL:
            reg = read_bstval();
            break;
        case seL4_VCPUReg_SIP:
            reg = read_bsip();
            break;
        case seL4_VCPUReg_SATP:
            reg = read_bsatp();
            break;
        default:
            fail("RISCV/HE: Invalid register index");
    }
    return reg;
}

static void vcpu_hw_write_reg(word_t reg_index, word_t reg)
{
    switch (reg_index) {
        case seL4_VCPUReg_SSTATUS:
            write_bsstatus(reg);
            break;
        case seL4_VCPUReg_SIE:
            write_bsie(reg);
            break;
        case seL4_VCPUReg_STVEC:
            write_bstvec(reg);
            break;
        case seL4_VCPUReg_SSTRATCH:
            write_bsstratch(reg);
            break;
        case seL4_VCPUReg_SEPC:
            write_bsepc(reg);
            break;
        case seL4_VCPUReg_SCAUSE:
            write_bscause(reg);
            break;
        case seL4_VCPUReg_STVAL:
            write_bstval(reg);
            break;
        case seL4_VCPUReg_SIP:
            write_bsip(reg);
            break;
        case seL4_VCPUReg_SATP:
            write_bsatp(reg);
            break;
        default:
            fail("RISCV/HE: Invalid register index");
    }
    return;
}


static inline void vcpu_save_reg(vcpu_t *vcpu, word_t reg)
{
    if (reg >= seL4_VCPUReg_Num || vcpu == NULL) {
        fail("RISCV/HYP: Invalid register index or NULL VCPU");
        return;
    }
    vcpu->regs[reg] = vcpu_hw_read_reg(reg);
}

static inline void vcpu_save_reg_range(vcpu_t *vcpu, word_t start, word_t end)
{
    for (word_t i = start; i <= end; i++) {
        vcpu_save_reg(vcpu, i);
    }
}

static inline void vcpu_restore_reg(vcpu_t *vcpu, word_t reg)
{
    if (reg >= seL4_VCPUReg_Num || vcpu == NULL) {
        fail("RISCV/HYP: Invalid register index or NULL VCPU");
        return;
    }
    vcpu_hw_write_reg(reg, vcpu->regs[reg]);
}

static inline void vcpu_restore_reg_range(vcpu_t *vcpu, word_t start, word_t end)
{
    for (word_t i = start; i <= end; i++) {
        vcpu_restore_reg(vcpu, i);
    }
}

static inline word_t vcpu_read_reg(vcpu_t *vcpu, word_t reg)
{
    if (reg >= seL4_VCPUReg_Num || vcpu == NULL) {
        fail("ARM/HYP: Invalid register index or NULL VCPU");
        return 0;
    }
    return vcpu->regs[reg];
}

static inline void vcpu_write_reg(vcpu_t *vcpu, word_t reg, word_t value)
{
    if (reg >= seL4_VCPUReg_Num || vcpu == NULL) {
        fail("ARM/HYP: Invalid register index or NULL VCPU");
        return;
    }
    vcpu->regs[reg] = value;
}

static inline void vcpu_native(void)
{
    // disble exception delegate
    write_hedeleg(0);

    // disable interrupt delegate
    write_hideleg(0);

    write_bsstatus(0);
    write_bsip(0);
    write_bsie(0);
    write_bstvec(0);
    write_bsstratch(0);
    write_bsepc(0);
    write_bscause(0);
    write_bstval(0);

    /* this disables stage-1 translation */
    write_bsatp(0);
}

static void vcpu_enable(vcpu_t *vcpu)
{
    if (vcpu->init) {
        vcpu->init = false;

        /* Enable the SPRV bit so that the VCPU starts in S level.
         * TODO:
         * This should be handled better: user VMM should be able
         * to set the bit rather than relying on the init.
         */
        word_t sstatus = getRegister(vcpu->vcpuTCB, SSTATUS);
        sstatus |= BIT(SSTATUS_SPRV_BIT);
        setRegister(vcpu->vcpuTCB, SSTATUS, sstatus);
    }

    /* Delegate exceptions */
    word_t hedeleg = HEDELEG_VM;
    write_hedeleg(hedeleg);
    /* Delegate interrupts */
    word_t hideleg = HIDELEG_VM;
    write_hideleg(hideleg);

    vcpu_restore_reg_range(vcpu, seL4_VCPUReg_SSTATUS, seL4_VCPUReg_SATP);
}

static void vcpu_disable(vcpu_t *vcpu)
{
    if (vcpu != NULL) {
        vcpu_save_reg_range(vcpu, seL4_VCPUReg_SSTATUS, seL4_VCPUReg_SATP);
    }

    vcpu_native();
}

static void vcpu_save(vcpu_t *vcpu, bool_t active)
{
    assert(vcpu);
    /* If we aren't active then this state already got stored when
     * we were disabled */
    if (active) {
        vcpu_save_reg_range(vcpu, seL4_VCPUReg_SSTATUS, seL4_VCPUReg_SATP);
    }
}

static word_t readVCPUReg(vcpu_t *vcpu, word_t field)
{
    assert(vcpu);
    return vcpu_read_reg(vcpu, field);
}

static void writeVCPUReg(vcpu_t *vcpu, word_t field, word_t value)
{
    assert(vcpu);
    if (field == seL4_VCPUReg_TIMER) {
        setVTimer(1, value);
    }
    return vcpu_write_reg(vcpu, field, value);
}

void vcpu_restore(vcpu_t *vcpu)
{
    assert(vcpu);

    /* restore registers */
    vcpu_restore_reg_range(vcpu, seL4_VCPUReg_SSTATUS, seL4_VCPUReg_SATP);
    vcpu_enable(vcpu);
}


void vcpu_init(vcpu_t *vcpu)
{
    vcpu->vcpuTCB = NULL;
    for (int i = 0; i < seL4_VCPUReg_Num; i++) {
        vcpu->regs[i] = 0;
    }
    vcpu->init = true;
}

void vcpu_switch(vcpu_t *new)
{
    if (likely(ARCH_NODE_STATE(riscvHSCurVCPU) != new)) {
        if (unlikely(new != NULL)) {
            if (unlikely(ARCH_NODE_STATE(riscvHSCurVCPU) != NULL)) {
                vcpu_save(ARCH_NODE_STATE(riscvHSCurVCPU), ARCH_NODE_STATE(riscvHSVCPUActive));
            }
            vcpu_restore(new);
            ARCH_NODE_STATE(riscvHSCurVCPU) = new;
            ARCH_NODE_STATE(riscvHSVCPUActive) = true;
        } else if (unlikely(ARCH_NODE_STATE(riscvHSVCPUActive))) {
            /* leave the current VCPU state loaded, but disable vgic and mmu */
            vcpu_disable(ARCH_NODE_STATE(riscvHSCurVCPU));
            ARCH_NODE_STATE(riscvHSVCPUActive) = false;
        }
    } else if (likely(!ARCH_NODE_STATE(riscvHSVCPUActive) && new != NULL)) {
        vcpu_enable(new);
        ARCH_NODE_STATE(riscvHSVCPUActive) = true;
    }
}

static void vcpu_invalidate_active(void)
{
    if (ARCH_NODE_STATE(riscvHSVCPUActive)) {
        vcpu_disable(NULL);
        ARCH_NODE_STATE(riscvHSVCPUActive) = false;
    }
    ARCH_NODE_STATE(riscvHSCurVCPU) = NULL;
}

void vcpu_finalise(vcpu_t *vcpu)
{
    if (vcpu->vcpuTCB) {
        dissociateVCPUTCB(vcpu, vcpu->vcpuTCB);
    }
}

void associateVCPUTCB(vcpu_t *vcpu, tcb_t *tcb)
{
    if (tcb->tcbArch.tcbVCPU) {
        dissociateVCPUTCB(tcb->tcbArch.tcbVCPU, tcb);
    }
    if (vcpu->vcpuTCB) {
        dissociateVCPUTCB(vcpu, vcpu->vcpuTCB);
    }
    tcb->tcbArch.tcbVCPU = vcpu;
    vcpu->vcpuTCB = tcb;
}

void dissociateVCPUTCB(vcpu_t *vcpu, tcb_t *tcb)
{
    if (tcb->tcbArch.tcbVCPU != vcpu || vcpu->vcpuTCB != tcb) {
        fail("TCB and VCPU not associated.");
    }
    if (vcpu == ARCH_NODE_STATE(riscvHSCurVCPU)) {
        vcpu_invalidate_active();
    }
    tcb->tcbArch.tcbVCPU = NULL;
    vcpu->vcpuTCB = NULL;

    /* TODO sanitise the control registers */
}

exception_t invokeVCPUWriteReg(vcpu_t *vcpu, word_t field, word_t value)
{
    writeVCPUReg(vcpu, field, value);
    return EXCEPTION_NONE;
}

exception_t decodeVCPUWriteReg(cap_t cap, unsigned int length, word_t *buffer)
{
    word_t field;
    word_t value;
    if (length < 2) {
        userError("VCPUWriteReg: Truncated message.");
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }
    field = getSyscallArg(0, buffer);
    value = getSyscallArg(1, buffer);
    if (field >= seL4_VCPUReg_Num) {
        userError("VCPUWriteReg: Invalid field 0x%lx.", (long)field);
        current_syscall_error.type = seL4_InvalidArgument;
        current_syscall_error.invalidArgumentNumber = 1;
        return EXCEPTION_SYSCALL_ERROR;
    }
    setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);
    return invokeVCPUWriteReg(VCPU_PTR(cap_vcpu_cap_get_capVCPUPtr(cap)), field, value);
}

exception_t invokeVCPUReadReg(vcpu_t *vcpu, word_t field, bool_t call)
{
    tcb_t *thread;
    thread = NODE_STATE(ksCurThread);
    word_t value = readVCPUReg(vcpu, field);
    if (call) {
        word_t *ipcBuffer = lookupIPCBuffer(true, thread);
        setRegister(thread, badgeRegister, 0);
        unsigned int length = setMR(thread, ipcBuffer, 0, value);
        setRegister(thread, msgInfoRegister, wordFromMessageInfo(
                        seL4_MessageInfo_new(0, 0, 0, length)));
    }
    setThreadState(NODE_STATE(ksCurThread), ThreadState_Running);
    return EXCEPTION_NONE;
}

exception_t decodeVCPUReadReg(cap_t cap, unsigned int length, bool_t call, word_t *buffer)
{
    word_t field;
    if (length < 1) {
        userError("VCPUReadReg: Truncated message.");
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

    field = getSyscallArg(0, buffer);

    if (field >= seL4_VCPUReg_Num) {
        userError("VCPUReadReg: Invalid field 0x%lx.", (long)field);
        current_syscall_error.type = seL4_InvalidArgument;
        current_syscall_error.invalidArgumentNumber = 1;
        return EXCEPTION_SYSCALL_ERROR;
    }

    setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);
    return invokeVCPUReadReg(VCPU_PTR(cap_vcpu_cap_get_capVCPUPtr(cap)), field, call);
}

exception_t decodeRISCVVCPUInvocation(
    word_t label,
    unsigned int length,
    cptr_t cptr,
    cte_t *slot,
    cap_t cap,
    extra_caps_t extraCaps,
    bool_t call,
    word_t *buffer
)
{
    switch (label) {
    case RISCVVCPUSetTCB:
        return decodeVCPUSetTCB(cap, extraCaps);
    case RISCVVCPUReadReg:
        return decodeVCPUReadReg(cap, length, call, buffer);
    case RISCVVCPUWriteReg:
        return decodeVCPUWriteReg(cap, length, buffer);
    default:
        userError("RISCV VCPU: Illegal operation.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }
}

exception_t decodeVCPUSetTCB(cap_t cap, extra_caps_t extraCaps)
{
    cap_t tcbCap;
    if (extraCaps.excaprefs[0] == NULL) {
        userError("VCPU SetTCB: Truncated message.");
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }
    tcbCap  = extraCaps.excaprefs[0]->cap;

    if (cap_get_capType(tcbCap) != cap_thread_cap) {
        userError("TCB cap is not a TCB cap.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);
    return invokeVCPUSetTCB(VCPU_PTR(cap_vcpu_cap_get_capVCPUPtr(cap)), TCB_PTR(cap_thread_cap_get_capTCBPtr(tcbCap)));
}

exception_t invokeVCPUSetTCB(vcpu_t *vcpu, tcb_t *tcb)
{
    associateVCPUTCB(vcpu, tcb);

    return EXCEPTION_NONE;
}


BOOT_CODE void vcpu_boot_init(void)
{
    vcpu_native();
    ARCH_NODE_STATE(riscvHSCurVCPU) = NULL;
    ARCH_NODE_STATE(riscvHSVCPUActive) = false;
}

void handleVCPUFault(word_t cause)
{
    current_fault = seL4_Fault_VCPUFault_new(cause);
    handleFault(NODE_STATE(ksCurThread));
    schedule();
    activateThread();
}

#endif
