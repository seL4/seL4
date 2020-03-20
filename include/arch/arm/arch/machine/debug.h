/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <util.h>
#include <api/types.h>
#include <arch/machine/debug_conf.h>
#include <sel4/plat/api/constants.h>
#include <armv/debug.h>

#ifdef ARM_BASE_CP14_SAVE_AND_RESTORE
void restore_user_debug_context(tcb_t *target_thread);
void saveAllBreakpointState(tcb_t *t);
void loadAllDisabledBreakpointState(void);
#endif
#ifdef ARM_HYP_CP14_SAVE_AND_RESTORE_VCPU_THREADS
void Arch_debugAssociateVCPUTCB(tcb_t *t);
void Arch_debugDissociateVCPUTCB(tcb_t *t);
#endif

#ifdef ARM_HYP_TRAP_CP14
/* Those of these that trap NS accesses trap all NS accesses; we can't cause the
 * processor to only trap NS-PL0 or NS-PL1, but if we want to trap the accesses,
 * we get both (PL0 and PL1) non-secure modes' accesses.
 */
#define ARM_CP15_HDCR "p15, 4, %0, c1, c1, 1"
#define HDCR_DEBUG_TDRA_SHIFT     (11) /* Trap debug ROM access from non-secure world */
#define HDCR_DEBUG_TDOSA_SHIFT    (10) /* Trap debug OS related access from NS world */
#define HDCR_DEBUG_TDA_SHIFT      (9)  /* Trap debug CP14 register access from NS world */
#define HDCR_DEBUG_TDE_SHIFT      (8)  /* Trap debug exceptions taken from NS world */
#define HDCR_PERFMON_HPME_SHIFT   (7)  /* Enable the hyp-mode perfmon counters. */
#define HDCR_PERFMON_TPM_SHIFT    (6)  /* Trap NS PM accesses */
#define HDCR_PERFMON_TPMCR_SHIFT  (5)  /* Trap NS PMCR reg access */

/** When running seL4 as a hypervisor, if we're building with support for the
 * hardware debug API, we have a case of indirection that we need to handle.
 *
 * For native PL0 user threads in the hypervisor seL4 build, if a debug
 * exception is triggered in one of them, the CPU will raise the exception and
 * naturally, it will attempt to deliver it to a PL1 exception vector table --
 * but no such table exists for native hypervisor-seL4 threads, so the CPU will
 * end up encountering a VM fault while trying to vector into the vector table.
 *
 * For this reason, for native hypervisor-seL4 threads, we need to trap the
 * debug exception DIRECTLY into the hypervisor-seL4 instance, and handle it
 * directly. So we need to SET HDCR.TDE for this case.
 *
 * For the Guest VM, if it programs the CPU to trigger breakpoints, and a
 * debug exception gets triggered, we don't want to catch those debug exceptions
 * since we can let the Guest VM handle them on its own. So we need to UNSET
 * HDCR.TDE for this case.
 *
 * This function encapsulates the setting/unsetting, and it is called when we
 * are about to enable/disable a VCPU.
 *
 * If we are enabling a vcpu (vcpu_enable) we UNSET HDCR.TDE.
 * If we are disabling a vcpu (vcpu_disable) we SET HDCR.TDE.
 */
static inline void setHDCRTrapDebugExceptionState(bool_t enable_trapping)
{
    word_t hdcr;
#ifdef CONFIG_ARCH_AARCH64
    MRS("mdcr_el2", hdcr);
#else
    MRC(ARM_CP15_HDCR, hdcr);
#endif
    if (enable_trapping) {
        /* Trap and redirect debug faults that occur in PL0 native threads by
         * setting HDCR.TDE (trap debug exceptions).
         */
        hdcr |= (BIT(HDCR_DEBUG_TDE_SHIFT)
                 | BIT(HDCR_DEBUG_TDA_SHIFT)
                 | BIT(HDCR_DEBUG_TDRA_SHIFT)
                 | BIT(HDCR_DEBUG_TDOSA_SHIFT));
    } else {
        /* Let the PL1 Guest VM handle debug events on its own */
        hdcr &= ~(BIT(HDCR_DEBUG_TDE_SHIFT)
                  | BIT(HDCR_DEBUG_TDA_SHIFT)
                  | BIT(HDCR_DEBUG_TDRA_SHIFT)
                  | BIT(HDCR_DEBUG_TDOSA_SHIFT));
    }
#ifdef CONFIG_ARCH_AARCH64
    MSR("mdcr_el2", hdcr);
#else
    MCR(ARM_CP15_HDCR, hdcr);
#endif
}

static inline void initHDCR(void)
{
    /* By default at boot, we SET HDCR.TDE to catch and redirect native threads'
     * PL0 debug exceptions.
     *
     * Unfortunately, this is complicated a bit by ARM's strange requirement that
     * if you set HDCR.TDE, you must also set TDA, TDOSA, and TDRA:
     *  ARMv7 archref manual: section B1.8.9:
     *      "When HDCR.TDE is set to 1, the HDCR.{TDRA, TDOSA, TDA} bits must all
     *      be set to 1, otherwise behavior is UNPREDICTABLE"
     *
     * Subsequently on calls to vcpu_enable/disable, we will modify HDCR.TDE
     * as needed.
     */
    setHDCRTrapDebugExceptionState(true);
}
#endif /* ARM_HYP_TRAP_CP14 */

#ifdef CONFIG_HARDWARE_DEBUG_API

static uint16_t convertBpNumToArch(uint16_t bp_num)
{
    if (bp_num >= seL4_NumExclusiveBreakpoints) {
        bp_num -= seL4_NumExclusiveBreakpoints;
    }
    return bp_num;
}

static word_t getTypeFromBpNum(uint16_t bp_num)
{
    return (bp_num >= seL4_NumExclusiveBreakpoints)
           ? seL4_DataBreakpoint
           : seL4_InstructionBreakpoint;
}

static inline syscall_error_t Arch_decodeConfigureSingleStepping(tcb_t *t,
                                                                 uint16_t bp_num,
                                                                 word_t n_instr,
                                                                 bool_t is_reply)
{
    word_t type;
    syscall_error_t ret = {
        .type = seL4_NoError
    };

    if (is_reply) {
        /* If this is a single-step fault reply, just default to the already-
         * configured bp_num. Of course, this assumes that a register had
         * already previously been configured for single-stepping.
         */
        if (!t->tcbArch.tcbContext.breakpointState.single_step_enabled) {
            userError("Debug: Single-step reply when single-stepping not "
                      "enabled.");
            ret.type = seL4_IllegalOperation;
            return ret;
        }

        type = seL4_InstructionBreakpoint;
        bp_num = t->tcbArch.tcbContext.breakpointState.single_step_hw_bp_num;
    } else {
        type = getTypeFromBpNum(bp_num);
        bp_num = convertBpNumToArch(bp_num);
    }

    if (type != seL4_InstructionBreakpoint || bp_num >= seL4_FirstWatchpoint) {
        /* Must use an instruction BP register */
        userError("Debug: Single-stepping can only be used with an instruction "
                  "breakpoint.");
        ret.type = seL4_InvalidArgument;
        ret.invalidArgumentNumber = 0;
        return ret;
    }
    if (t->tcbArch.tcbContext.breakpointState.single_step_enabled == true) {
        if (bp_num != t->tcbArch.tcbContext.breakpointState.single_step_hw_bp_num) {
            /* Can't configure more than one register for stepping. */
            userError("Debug: Only one register can be configured for "
                      "single-stepping at a time.");
            ret.type = seL4_InvalidArgument;
            ret.invalidArgumentNumber = 0;
            return ret;
        }
    }

    return ret;
}

bool_t byte8WatchpointsSupported(void);

static inline syscall_error_t Arch_decodeSetBreakpoint(tcb_t *t,
                                                       uint16_t bp_num, word_t vaddr, word_t type,
                                                       word_t size, word_t rw)
{
    syscall_error_t ret = {
        .type = seL4_NoError
    };

    bp_num = convertBpNumToArch(bp_num);

    if (type == seL4_DataBreakpoint) {
        if (bp_num >= seL4_NumExclusiveWatchpoints) {
            userError("Debug: invalid data-watchpoint number %u.", bp_num);
            ret.type = seL4_RangeError;
            ret.rangeErrorMin = 0;
            ret.rangeErrorMax = seL4_NumExclusiveBreakpoints - 1;
            return ret;
        }
    } else if (type == seL4_InstructionBreakpoint) {
        if (bp_num >= seL4_NumExclusiveBreakpoints) {
            userError("Debug: invalid instruction breakpoint nunber %u.", bp_num);
            ret.type = seL4_RangeError;
            ret.rangeErrorMin = 0;
            ret.rangeErrorMax = seL4_NumExclusiveWatchpoints - 1;
            return ret;
        }
    }

    if (size == 8 && !byte8WatchpointsSupported()) {
        userError("Debug: 8-byte watchpoints not supported on this CPU.");
        ret.type = seL4_InvalidArgument;
        ret.invalidArgumentNumber = 3;
        return ret;
    }
    if (size == 8 && type != seL4_DataBreakpoint) {
        userError("Debug: 8-byte sizes can only be used with watchpoints.");
        ret.type = seL4_InvalidArgument;
        ret.invalidArgumentNumber = 3;
        return ret;
    }

    return ret;
}

static inline syscall_error_t Arch_decodeGetBreakpoint(tcb_t *t, uint16_t bp_num)
{
    syscall_error_t ret = {
        .type = seL4_NoError
    };

    if (bp_num >= seL4_FirstWatchpoint + seL4_NumExclusiveWatchpoints) {
        userError("Arch Debug: Invalid API bp_num %u.", bp_num);
        ret.type = seL4_NoError;
        return ret;
    }
    return ret;
}

static inline syscall_error_t Arch_decodeUnsetBreakpoint(tcb_t *t, uint16_t bp_num)
{
    syscall_error_t ret = {
        .type = seL4_NoError
    };

    if (bp_num >= seL4_FirstWatchpoint + seL4_NumExclusiveWatchpoints) {
        userError("Arch Debug: Invalid API bp_num %u.", bp_num);
        ret.type = seL4_NoError;
        return ret;
    }

    word_t type;
    dbg_bcr_t bcr;

    type = getTypeFromBpNum(bp_num);
    bp_num = convertBpNumToArch(bp_num);

    bcr.words[0] = t->tcbArch.tcbContext.breakpointState.breakpoint[bp_num].cr;
    if (type == seL4_InstructionBreakpoint) {
        if (Arch_breakpointIsMismatch(bcr) == true && dbg_bcr_get_enabled(bcr)) {
            userError("Rejecting call to unsetBreakpoint on breakpoint configured "
                      "for single-stepping (hwid %u).", bp_num);
            ret.type = seL4_IllegalOperation;
            return ret;
        }
    }

    return ret;
}

#endif /* CONFIG_HARDWARE_DEBUG_API */

