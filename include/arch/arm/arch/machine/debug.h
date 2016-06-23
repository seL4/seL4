/*
 * Copyright 2016, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(D61_GPL)
 */
#ifndef __ARCH_MACHINE_DEBUG_H
#define __ARCH_MACHINE_DEBUG_H

#include <util.h>
#include <plat/api/constants.h>
#include <armv/debug.h>

#ifdef CONFIG_HARDWARE_DEBUG_API

static uint16_t
convertBpNumToArch(uint16_t bp_num)
{
    if (bp_num >= seL4_NumExclusiveBreakpoints) {
        bp_num -= seL4_NumExclusiveBreakpoints;
    }
    return bp_num;
}

static word_t
getTypeFromBpNum(uint16_t bp_num)
{
    return (bp_num >= seL4_NumExclusiveBreakpoints)
           ? seL4_DataBreakpoint
           : seL4_InstructionBreakpoint;
}

static inline syscall_error_t
Arch_decodeConfigureSingleStepping(arch_tcb_t *at,
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
        if (!at->tcbContext.breakpointState.single_step_enabled) {
            userError("Debug: Single-step reply when single-stepping not "
                      "enabled.");
            ret.type = seL4_IllegalOperation;
            return ret;
        }

        type = seL4_InstructionBreakpoint;
        bp_num = at->tcbContext.breakpointState.single_step_hw_bp_num;
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
    if (at->tcbContext.breakpointState.single_step_enabled == true) {
        if (bp_num != at->tcbContext.breakpointState.single_step_hw_bp_num) {
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

static inline syscall_error_t
Arch_decodeSetBreakpoint(arch_tcb_t *uds,
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

static inline syscall_error_t
Arch_decodeGetBreakpoint(arch_tcb_t *uds, uint16_t bp_num)
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

static inline syscall_error_t
Arch_decodeUnsetBreakpoint(arch_tcb_t *uds, uint16_t bp_num)
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

    bcr.words[0] = uds->tcbContext.breakpointState.breakpoint[bp_num].cr;
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

#endif /* !__ARCH_MACHINE_DEBUG_H */
