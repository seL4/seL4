/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <config.h>

#ifdef CONFIG_HARDWARE_DEBUG_API

#include <arch/machine/debug.h>
#include <mode/machine/debug.h>
#include <arch/machine.h>
#include <machine/registerset.h>
#include <sel4/plat/api/constants.h> /* seL4_NumHWBReakpoints */

/* Intel manual Vol3, 17.2.4 */
#define X86_DEBUG_BP_SIZE_1B                (0x0u)
#define X86_DEBUG_BP_SIZE_2B                (0x1u)
#define X86_DEBUG_BP_SIZE_4B                (0x3u)
#define X86_DEBUG_BP_SIZE_8B                (0x2u)

#define X86_DEBUG_BP0_SIZE_SHIFT            (18)
#define X86_DEBUG_BP1_SIZE_SHIFT            (22)
#define X86_DEBUG_BP2_SIZE_SHIFT            (26)
#define X86_DEBUG_BP3_SIZE_SHIFT            (30)

/* NOTE: Intel manual 17.2.4:
 * I/O breakpoints are supported by every processor later than i486, but only
 * when CR4.DE=1.
 * When CR4.DE=0, or if processor is earlier than i586, this bit is "Undefined",
 * which is not the same as "Reserved", so it won't trigger an exception - it
 * will just cause an undefined reaction from the CPU.
 */
#define X86_DEBUG_BP_TYPE_IO                (0x2u)
#define X86_DEBUG_BP_TYPE_INSTR             (0x0u)
#define X86_DEBUG_BP_TYPE_DATA_WRITE        (0x1u)
#define X86_DEBUG_BP_TYPE_DATA_READWRITE    (0x3u)

#define X86_DEBUG_BP0_TYPE_SHIFT            (16)
#define X86_DEBUG_BP1_TYPE_SHIFT            (20)
#define X86_DEBUG_BP2_TYPE_SHIFT            (24)
#define X86_DEBUG_BP3_TYPE_SHIFT            (28)

#define X86_DEBUG_EFLAGS_TRAP_FLAG    ((word_t)BIT(8))
#define X86_DEBUG_EFLAGS_RESUME_FLAG    ((word_t)BIT(16))
#define X86_DEBUG_DR6_SINGLE_STEP_FLAG  ((word_t)BIT(14))

#define X86_DEBUG_DR6_BP_MASK     (0xFu)

static bool_t byte8_bps_supported = false;

bool_t byte8BreakpointsSupported(void)
{
    return byte8_bps_supported;
}

static inline void bitwiseAndDr6Reg(word_t mask)
{
    word_t tmp;

    tmp = readDr6Reg() & mask;
    writeDr6Reg(tmp);
}

static inline word_t readDr7Context(tcb_t *t)
{
    return t->tcbArch.tcbContext.breakpointState.dr[5];
}

static inline void bitwiseOrDr7Context(tcb_t *t, word_t val)
{
    t->tcbArch.tcbContext.breakpointState.dr[5] |= val;
}

static inline void bitwiseAndDr7Context(tcb_t *t, word_t mask)
{
    t->tcbArch.tcbContext.breakpointState.dr[5] &= mask;
}

static void unsetDr7BitsFor(tcb_t *t, uint16_t bp_num)
{
    word_t mask;

    switch (bp_num) {
    case 0:
        mask = (0x3u << X86_DEBUG_BP0_SIZE_SHIFT) | (0x3u << X86_DEBUG_BP0_TYPE_SHIFT);
        break;
    case 1:
        mask = (0x3u << X86_DEBUG_BP1_SIZE_SHIFT) | (0x3u << X86_DEBUG_BP1_TYPE_SHIFT);
        break;
    case 2:
        mask = (0x3u << X86_DEBUG_BP2_SIZE_SHIFT) | (0x3u << X86_DEBUG_BP2_TYPE_SHIFT);
        break;
    default: /* 3 */
        assert(bp_num == 3);
        mask = (0x3u << X86_DEBUG_BP3_SIZE_SHIFT) | (0x3u << X86_DEBUG_BP3_TYPE_SHIFT);
        break;
    }

    mask = ~mask;
    bitwiseAndDr7Context(t, mask);
}

/** Converts an seL4_BreakpointType value into the underlying hardware
 * equivalent.
 * @param bp_num Breakpoint number.
 * @param type One of the values of seL4_BreakpointType.
 * @param rw Access trigger condition (read/write).
 * @return Hardware specific register value representing the inputs.
 */
PURE static inline word_t convertTypeAndAccessToArch(uint16_t bp_num, word_t type, word_t rw)
{
    switch (type) {
    case seL4_InstructionBreakpoint:
        type = X86_DEBUG_BP_TYPE_INSTR;
        break;
    default: /* seL4_DataBreakpoint */
        assert(type == seL4_DataBreakpoint);
        type = (rw == seL4_BreakOnWrite)
               ? X86_DEBUG_BP_TYPE_DATA_WRITE
               : X86_DEBUG_BP_TYPE_DATA_READWRITE;
    }

    switch (bp_num) {
    case 0:
        return type << X86_DEBUG_BP0_TYPE_SHIFT;
    case 1:
        return type << X86_DEBUG_BP1_TYPE_SHIFT;
    case 2:
        return type << X86_DEBUG_BP2_TYPE_SHIFT;
    default: /* 3 */
        assert(bp_num == 3);
        return type << X86_DEBUG_BP3_TYPE_SHIFT;
    }
}

/** Reverse of convertTypeAndAccessToArch(): converts hardware values into
 * seL4 API values.
 * @param dr7 Hardware register value as input for conversion.
 * @param bp_num Breakpoint number.
 * @param type[out] Converted type value.
 * @param rw[out] Converted output access trigger value.
 */
typedef struct {
    word_t type, rw;
} convertedTypeAndAccess_t;

PURE static inline convertedTypeAndAccess_t convertArchToTypeAndAccess(word_t dr7, uint16_t bp_num)
{
    convertedTypeAndAccess_t ret;

    switch (bp_num) {
    case 0:
        dr7 &= 0x3u << X86_DEBUG_BP0_TYPE_SHIFT;
        dr7 >>= X86_DEBUG_BP0_TYPE_SHIFT;
        break;
    case 1:
        dr7 &= 0x3u << X86_DEBUG_BP1_TYPE_SHIFT;
        dr7 >>= X86_DEBUG_BP1_TYPE_SHIFT;
        break;
    case 2:
        dr7 &= 0x3u << X86_DEBUG_BP2_TYPE_SHIFT;
        dr7 >>= X86_DEBUG_BP2_TYPE_SHIFT;
        break;
    default: /* 3 */
        assert(bp_num == 3);
        dr7 &= 0x3u << X86_DEBUG_BP3_TYPE_SHIFT;
        dr7 >>= X86_DEBUG_BP3_TYPE_SHIFT;
    }

    switch (dr7) {
    case X86_DEBUG_BP_TYPE_INSTR:
        ret.type = seL4_InstructionBreakpoint;
        ret.rw = seL4_BreakOnRead;
        break;
    case X86_DEBUG_BP_TYPE_DATA_WRITE:
        ret.type = seL4_DataBreakpoint;
        ret.rw = seL4_BreakOnWrite;
        break;
    default: /* Read-write */
        assert(dr7 == X86_DEBUG_BP_TYPE_DATA_READWRITE);
        ret.type = seL4_DataBreakpoint;
        ret.rw = seL4_BreakOnReadWrite;
        break;
    }
    return ret;
}

/** Converts an integer size number into an equivalent hardware register value.
 * @param n Breakpoint number.
 * @param type One value from seL4_BreakpointType.
 * @param size An integer for the operand size of the breakpoint.
 * @return Converted, hardware-specific value.
 */
PURE static inline word_t convertSizeToArch(uint16_t bp_num, word_t type, word_t size)
{
    if (type == seL4_InstructionBreakpoint) {
        /* Intel manual vol3 17.2.4:
         * "If the corresponding RWn field in register DR7 is 00 (instruction
         * execution), then the LENn field should also be 00"
         */
        size = 0;
    } else {
        switch (size) {
        case 1:
            size = X86_DEBUG_BP_SIZE_1B;
            break;
        case 2:
            size = X86_DEBUG_BP_SIZE_2B;
            break;
        case 8:
            size = X86_DEBUG_BP_SIZE_8B;
            break;
        default: /* 4B */
            assert(size == 4);
            size = X86_DEBUG_BP_SIZE_4B;
        }
    }

    switch (bp_num) {
    case 0:
        return size << X86_DEBUG_BP0_SIZE_SHIFT;
    case 1:
        return size << X86_DEBUG_BP1_SIZE_SHIFT;
    case 2:
        return size << X86_DEBUG_BP2_SIZE_SHIFT;
    default: /* 3 */
        assert(bp_num == 3);
        return size << X86_DEBUG_BP3_SIZE_SHIFT;
    }
}

/** Reverse of convertSizeToArch(): converts a hardware-specific size value
 * into an integer representation.
 * @param dr7 Hardware register value as input.
 * @param n Breakpoint number.
 * @return Converted size value.
 */
PURE static inline word_t convertArchToSize(word_t dr7, uint16_t bp_num)
{
    word_t type;

    switch (bp_num) {
    case 0:
        type = dr7 & (0x3u << X86_DEBUG_BP0_TYPE_SHIFT);
        type >>= X86_DEBUG_BP0_TYPE_SHIFT;
        dr7 &= 0x3u << X86_DEBUG_BP0_SIZE_SHIFT;
        dr7 >>= X86_DEBUG_BP0_SIZE_SHIFT;
        break;
    case 1:
        type = dr7 & (0x3u << X86_DEBUG_BP1_TYPE_SHIFT);
        type >>= X86_DEBUG_BP1_TYPE_SHIFT;
        dr7 &= 0x3u << X86_DEBUG_BP1_SIZE_SHIFT;
        dr7 >>= X86_DEBUG_BP1_SIZE_SHIFT;
        break;
    case 2:
        type = dr7 & (0x3u << X86_DEBUG_BP2_TYPE_SHIFT);
        type >>= X86_DEBUG_BP2_TYPE_SHIFT;
        dr7 &= 0x3u << X86_DEBUG_BP2_SIZE_SHIFT;
        dr7 >>= X86_DEBUG_BP2_SIZE_SHIFT;
        break;
    default: /* 3 */
        assert(bp_num == 3);
        type = dr7 & (0x3u << X86_DEBUG_BP3_TYPE_SHIFT);
        type >>= X86_DEBUG_BP3_TYPE_SHIFT;
        dr7 &= 0x3u << X86_DEBUG_BP3_SIZE_SHIFT;
        dr7 >>= X86_DEBUG_BP3_SIZE_SHIFT;
    }

    /* Force size to 0 if type is instruction breakpoint. */
    if (type == X86_DEBUG_BP_TYPE_INSTR) {
        return 0;
    }

    switch (dr7) {
    case X86_DEBUG_BP_SIZE_1B:
        return 1;
    case X86_DEBUG_BP_SIZE_2B:
        return 2;
    case X86_DEBUG_BP_SIZE_8B:
        return 8;
    default: /* 4B */
        assert(dr7 == X86_DEBUG_BP_SIZE_4B);
        return 4;
    }
}

/** Enables a breakpoint.
 * @param bp_num Hardware breakpoint ID. Usually an integer from 0..N.
 */
static void enableBreakpoint(tcb_t *t, uint16_t bp_num)
{
    word_t enable_bit;

    assert(t != NULL);
    assert(bp_num < X86_DEBUG_BP_N_REGS);

    switch (bp_num) {
    case 0:
        enable_bit = X86_DEBUG_BP0_ENABLE_BIT;
        break;
    case 1:
        enable_bit = X86_DEBUG_BP1_ENABLE_BIT;
        break;
    case 2:
        enable_bit = X86_DEBUG_BP2_ENABLE_BIT;
        break;
    default:
        enable_bit = X86_DEBUG_BP3_ENABLE_BIT;
        break;
    }

    bitwiseOrDr7Context(t, enable_bit);
}

/** Disables a breakpoint without clearing its configuration.
 * @param bp_num Hardware breakpoint ID. Usually an integer from 0..N.
 */
static void disableBreakpoint(tcb_t *t, uint16_t bp_num)
{
    word_t disable_mask;

    assert(t != NULL);
    assert(bp_num < X86_DEBUG_BP_N_REGS);

    switch (bp_num) {
    case 0:
        disable_mask = ~X86_DEBUG_BP0_ENABLE_BIT;
        break;
    case 1:
        disable_mask = ~X86_DEBUG_BP1_ENABLE_BIT;
        break;
    case 2:
        disable_mask = ~X86_DEBUG_BP2_ENABLE_BIT;
        break;
    default:
        disable_mask = ~X86_DEBUG_BP3_ENABLE_BIT;
        break;
    }

    bitwiseAndDr7Context(t, disable_mask);
}

/** Returns a boolean for whether or not a breakpoint is enabled.
 * @param bp_num Hardware breakpoint ID. Usually an integer from 0..N.
 */
static bool_t breakpointIsEnabled(tcb_t *t, uint16_t bp_num)
{
    word_t dr7;

    assert(t != NULL);
    assert(bp_num < X86_DEBUG_BP_N_REGS);

    dr7 = readDr7Context(t);
    switch (bp_num) {
    case 0:
        return !!(dr7 & X86_DEBUG_BP0_ENABLE_BIT);
    case 1:
        return !!(dr7 & X86_DEBUG_BP1_ENABLE_BIT);
    case 2:
        return !!(dr7 & X86_DEBUG_BP2_ENABLE_BIT);
    default:
        return !!(dr7 & X86_DEBUG_BP3_ENABLE_BIT);
    }
}

static void setBpVaddrContext(tcb_t *t, uint16_t bp_num, word_t vaddr)
{
    assert(t != NULL);
    user_breakpoint_state_t *ubs = &t->tcbArch.tcbContext.breakpointState;

    switch (bp_num) {
    case 0:
        ubs->dr[0] = vaddr;
        break;
    case 1:
        ubs->dr[1] = vaddr;
        break;
    case 2:
        ubs->dr[2] = vaddr;
        break;
    default:
        assert(bp_num == 3);
        ubs->dr[3] = vaddr;
        break;
    }
    return;
}

/** Backend for the seL4_TCB_SetBreakpoint invocation.
 *
 * @param uds Arch TCB register context structure.
 * @param bp_num Hardware breakpoint ID.
 * @param vaddr USerspace virtual address on which you'd like this breakpoing
 *        to trigger.
 * @param types One of the seL4_BreakpointType values.
 * @param size positive integer indicating the byte-range size that should
 *        trigger the breakpoint. 0 is valid for Instruction breakpoints.
 * @param rw Access type that should trigger the BP (read/write).
 */
void setBreakpoint(tcb_t *t,
                   uint16_t bp_num, word_t vaddr, word_t types, word_t size, word_t rw)
{
    word_t dr7val;

    assert(t != NULL);

    dr7val = convertTypeAndAccessToArch(bp_num, types, rw);
    dr7val |= convertSizeToArch(bp_num, types, size);

    setBpVaddrContext(t, bp_num, vaddr);
    unsetDr7BitsFor(t, bp_num);
    bitwiseOrDr7Context(t, dr7val);
    enableBreakpoint(t, bp_num);
}

static word_t getBpVaddrContext(tcb_t *t, uint16_t bp_num)
{
    assert(t != NULL);
    user_breakpoint_state_t *ubs = &t->tcbArch.tcbContext.breakpointState;

    switch (bp_num) {
    case 0:
        return ubs->dr[0];
    case 1:
        return ubs->dr[1];
    case 2:
        return ubs->dr[2];
    default:
        assert(bp_num == 3);
        return ubs->dr[3];
    }
}

/** Backend for the x86 seL4_TCB_GetBreakpoint invocation.
 *
 * Returns information about a particular breakpoint ID, including whether or
 * not it's enabled.
 *
 * @param uds Arch TCB register context pointer.
 * @param bp_num Hardware breakpoint ID of the BP you'd like to query.
 * @return Structure containing information about the status of the breakpoint.
 */
getBreakpoint_t getBreakpoint(tcb_t *t, uint16_t bp_num)
{
    word_t dr7val;
    getBreakpoint_t ret;
    convertedTypeAndAccess_t res;

    dr7val = readDr7Context(t);
    ret.vaddr = getBpVaddrContext(t, bp_num);
    ret.size = convertArchToSize(dr7val, bp_num);
    res = convertArchToTypeAndAccess(dr7val, bp_num);
    ret.type = res.type;
    ret.rw = res.rw;
    ret.is_enabled = breakpointIsEnabled(t, bp_num);
    return ret;
}

/** Backend for the x86 seL4_TCB_UnsetBreakpoint invocation.
 *
 * Unsets and *clears* a hardware breakpoint.
 * @param uds Arch TCB register context pointer.
 * @param bp_num The hardware breakpoint ID you'd like to clear.
 */
void unsetBreakpoint(tcb_t *t, uint16_t bp_num)
{
    disableBreakpoint(t, bp_num);
    unsetDr7BitsFor(t, bp_num);
    setBpVaddrContext(t, bp_num, 0);
}

/** Used in the exception path to determine if an exception was caused by
 * single-stepping being active.
 *
 * @param uc Arch TCB register context structure.
 * @return a structure stating whether or not the exception was caused by
 *          hardware single-stepping, and what the instruction vaddr was.
 */
typedef struct {
    bool_t ret;
    word_t instr_vaddr;
} testAndResetSingleStepException_t;

static testAndResetSingleStepException_t testAndResetSingleStepException(tcb_t *t)
{
    testAndResetSingleStepException_t ret;
    word_t dr6;

    dr6 = readDr6Reg();
    if (!(dr6 & X86_DEBUG_DR6_SINGLE_STEP_FLAG)) {
        ret.ret = false;
        return ret;
    }

    ret.ret = true;
    ret.instr_vaddr = t->tcbArch.tcbContext.registers[FaultIP];
    bitwiseAndDr6Reg(~X86_DEBUG_DR6_SINGLE_STEP_FLAG);

    /* And that's not all: if the breakpoint is an instruction breakpoint, we
     * also need to set EFLAGS.RF. The processor raises the #DB exception BEFORE
     * the instruction executes. This means that when we IRET to userspace, the
     * SAME breakpoint will trigger again, and so on ad infinitum. EFLAGS.RF
     * solves this problem:
     *
     * When EFLAGS.RF is set, the processor will ignore instruction breakpoints
     * that should be raised, for one instruction. After that instruction
     * executes, the processor will also automatically unset EFLAGS.RF. See
     * Intel manuals, vol3, section 17.3.1.1.
     */
    /* This will automatically be popped by restore_user_context() */
    t->tcbArch.tcbContext.registers[FLAGS] |= X86_DEBUG_EFLAGS_RESUME_FLAG;
    return ret;
}

bool_t configureSingleStepping(tcb_t *t, uint16_t bp_num, word_t n_instr,
                               UNUSED bool_t is_reply)
{
    /* On x86 no hardware breakpoints are needed for single stepping. */
    if (n_instr == 0) {
        /* If n_instr (number of instructions to single-step) is 0, that is the
          * same as requesting that single-stepping be disabled.
          */
        t->tcbArch.tcbContext.breakpointState.single_step_enabled = false;
        t->tcbArch.tcbContext.registers[FLAGS] &= ~X86_DEBUG_EFLAGS_TRAP_FLAG;
    } else {
        t->tcbArch.tcbContext.breakpointState.single_step_enabled = true;
    }

    t->tcbArch.tcbContext.breakpointState.n_instructions = n_instr;
    return false;
}

/** Used in the exception path to determine which breakpoint triggered the
 * exception.
 *
 * First, checks to see which hardware breakpoint was triggered, and saves
 * the ID of that breakpoint. Secondly, resets that breakpoint such that its
 * "triggered" bit is no longer in the asserted state -- whatever that means
 * for the arch. So on x86, that means clearing the indicator bit in DR6.
 *
 * Aside from the ID of the breakpoint that was raised, also returns
 * information about the breakpoint (vaddr, access, type, etc).
 *
 * @param uc Arch TCB register context pointer.
 * @return Structure with a "bp_num" member that states which hardware
 *          breakpoint was triggered, and gives information describing the
 *          breakpoint.
 */
typedef struct {
    int bp_num;
    word_t vaddr, reason;
} getAndResetActiveBreakpoint_t;

static getAndResetActiveBreakpoint_t getAndResetActiveBreakpoint(tcb_t *t)
{
    convertedTypeAndAccess_t tmp;
    getAndResetActiveBreakpoint_t ret;

    /* Read from the hardware regs, not user context */
    word_t dr6 = readDr6Reg();
    if (dr6 & BIT(0)) {
        ret.bp_num = 0;
    } else if (dr6 & BIT(1)) {
        ret.bp_num = 1;
    } else if (dr6 & BIT(2)) {
        ret.bp_num = 2;
    } else if (dr6 & BIT(3)) {
        ret.bp_num = 3;
    } else {
        ret.bp_num = -1;
        return ret;
    }

    tmp = convertArchToTypeAndAccess(readDr7Context(t), ret.bp_num);
    ret.vaddr = getBpVaddrContext(t, ret.bp_num);
    ret.reason = tmp.type;

    bitwiseAndDr6Reg(~BIT(ret.bp_num));
    return ret;
}

exception_t handleUserLevelDebugException(int int_vector)
{
    tcb_t *ct;
    getAndResetActiveBreakpoint_t active_bp;
    testAndResetSingleStepException_t single_step_info;

#if defined(CONFIG_DEBUG_BUILD) || defined(CONFIG_BENCHMARK_TRACK_KERNEL_ENTRIES)
    ksKernelEntry.path = Entry_UserLevelFault;
    ksKernelEntry.word = int_vector;
#endif /* DEBUG */

#ifdef CONFIG_BENCHMARK_TRACK_KERNEL_ENTRIES
    benchmark_track_start();
#endif

    ct = NODE_STATE(ksCurThread);

    /* Software break request (INT3) is detected by the vector number */
    if (int_vector == int_software_break_request) {
        current_fault = seL4_Fault_DebugException_new(getRestartPC(NODE_STATE(ksCurThread)),
                                                      0, seL4_SoftwareBreakRequest);
    } else {
        /* Hardware breakpoint trigger is detected using DR6 */
        active_bp = getAndResetActiveBreakpoint(ct);
        if (active_bp.bp_num >= 0) {
            current_fault = seL4_Fault_DebugException_new(active_bp.vaddr,
                                                          active_bp.bp_num,
                                                          active_bp.reason);
        } else {
            single_step_info = testAndResetSingleStepException(ct);
            if (single_step_info.ret == true) {
                /* If the caller asked us to skip over N instructions before
                 * generating the next single-step breakpoint, we shouldn't
                 * bother to construct a fault message until we've skipped N
                 * instructions.
                 */
                if (singleStepFaultCounterReady(ct) == false) {
                    return EXCEPTION_NONE;
                }
                current_fault = seL4_Fault_DebugException_new(single_step_info.instr_vaddr,
                                                              0, seL4_SingleStep);
            } else {
                return EXCEPTION_SYSCALL_ERROR;
            }
        }
    }

    handleFault(NODE_STATE(ksCurThread));

    schedule();
    activateThread();

    return EXCEPTION_NONE;
}

BOOT_CODE bool_t Arch_initHardwareBreakpoints(void)
{
    x86_cpu_identity_t *modelinfo;

    modelinfo = x86_cpuid_get_model_info();
    /* Intel manuals, vol3, section 17.2.4, "NOTES". */
    if (modelinfo->family == 15) {
        if (modelinfo->model == 3 || modelinfo->model == 4
            || modelinfo->model == 6) {
            byte8_bps_supported = true;
        }
    }
    if (modelinfo->family == 6) {
        if (modelinfo->model == 15 || modelinfo->model == 23
            || modelinfo->model == 0x1C) {
            byte8_bps_supported = true;
        }
    }
    return true;
}

void Arch_initBreakpointContext(user_breakpoint_state_t *uds)
{
    memset(uds, 0, sizeof(*uds));

    /* Preload reserved values into register context */
    uds->dr[4] = readDr6Reg() &
                 ~(BIT(0)
                   | BIT(1)
                   | BIT(2)
                   | BIT(3)
                   | X86_DEBUG_DR6_SINGLE_STEP_FLAG);

    uds->dr[5] = readDr7Reg() &
                 ~(X86_DEBUG_BP0_ENABLE_BIT | X86_DEBUG_BP1_ENABLE_BIT
                   | X86_DEBUG_BP2_ENABLE_BIT
                   | X86_DEBUG_BP3_ENABLE_BIT);
}

#endif
