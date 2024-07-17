/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#ifdef ARM_BASE_CP14_SAVE_AND_RESTORE

void aarch64_restore_user_debug_context(tcb_t *target_thread);

/** Determines and carries out what needs to be done for a debug exception.
 *
 * This could be handling a single-stepping exception, or a breakpoint or
 * watchpoint.
 */
seL4_Fault_t handleUserLevelDebugException(word_t esr, word_t fault_vaddr);
bool_t isDebugFault(word_t esr);

#define MAKE_DBGBVR(num) "DBGBVR" #num "_EL1"
#define MAKE_DBGBCR(num) "DBGBCR" #num "_EL1"
#define MAKE_DBGWVR(num) "DBGWVR" #num "_EL1"
#define MAKE_DBGWCR(num) "DBGWCR" #num "_EL1"

/** Generates read functions for the CP14 control and value registers.
 */
#define DEBUG_GENERATE_READ_FN(_name, _reg)                                    \
  static inline word_t _name(uint16_t bp_num) {                                \
    word_t ret;                                                                \
                                                                               \
    switch (bp_num) {                                                          \
    case 1:                                                                    \
      MRS(MAKE_##_reg(1), ret);                                                \
      return ret;                                                              \
    case 2:                                                                    \
      MRS(MAKE_##_reg(2), ret);                                                \
      return ret;                                                              \
    case 3:                                                                    \
      MRS(MAKE_##_reg(3), ret);                                                \
      return ret;                                                              \
    case 4:                                                                    \
      MRS(MAKE_##_reg(4), ret);                                                \
      return ret;                                                              \
    case 5:                                                                    \
      MRS(MAKE_##_reg(5), ret);                                                \
      return ret;                                                              \
    case 6:                                                                    \
      MRS(MAKE_##_reg(6), ret);                                                \
      return ret;                                                              \
    case 7:                                                                    \
      MRS(MAKE_##_reg(7), ret);                                                \
      return ret;                                                              \
    case 8:                                                                    \
      MRS(MAKE_##_reg(8), ret);                                                \
      return ret;                                                              \
    case 9:                                                                    \
      MRS(MAKE_##_reg(9), ret);                                                \
      return ret;                                                              \
    case 10:                                                                   \
      MRS(MAKE_##_reg(10), ret);                                               \
      return ret;                                                              \
    case 11:                                                                   \
      MRS(MAKE_##_reg(11), ret);                                               \
      return ret;                                                              \
    case 12:                                                                   \
      MRS(MAKE_##_reg(12), ret);                                               \
      return ret;                                                              \
    case 13:                                                                   \
      MRS(MAKE_##_reg(13), ret);                                               \
      return ret;                                                              \
    case 14:                                                                   \
      MRS(MAKE_##_reg(14), ret);                                               \
      return ret;                                                              \
    case 15:                                                                   \
      MRS(MAKE_##_reg(15), ret);                                               \
      return ret;                                                              \
    default:                                                                   \
      assert(bp_num == 0);                                                     \
      MRS(MAKE_##_reg(0), ret);                                                \
      return ret;                                                              \
    }                                                                          \
  }

/** Generates write functions for the CP14 control and value registers.
 */
#define DEBUG_GENERATE_WRITE_FN(_name, _reg)                                   \
  static inline void _name(uint16_t bp_num, word_t val) {                      \
    switch (bp_num) {                                                          \
    case 1:                                                                    \
      MSR(MAKE_##_reg(1), val);                                                \
      return;                                                                  \
    case 2:                                                                    \
      MSR(MAKE_##_reg(2), val);                                                \
      return;                                                                  \
    case 3:                                                                    \
      MSR(MAKE_##_reg(3), val);                                                \
      return;                                                                  \
    case 4:                                                                    \
      MSR(MAKE_##_reg(4), val);                                                \
      return;                                                                  \
    case 5:                                                                    \
      MSR(MAKE_##_reg(5), val);                                                \
      return;                                                                  \
    case 6:                                                                    \
      MSR(MAKE_##_reg(6), val);                                                \
      return;                                                                  \
    case 7:                                                                    \
      MSR(MAKE_##_reg(7), val);                                                \
      return;                                                                  \
    case 8:                                                                    \
      MSR(MAKE_##_reg(8), val);                                                \
      return;                                                                  \
    case 9:                                                                    \
      MSR(MAKE_##_reg(9), val);                                                \
      return;                                                                  \
    case 10:                                                                   \
      MSR(MAKE_##_reg(10), val);                                               \
      return;                                                                  \
    case 11:                                                                   \
      MSR(MAKE_##_reg(11), val);                                               \
      return;                                                                  \
    case 12:                                                                   \
      MSR(MAKE_##_reg(12), val);                                               \
      return;                                                                  \
    case 13:                                                                   \
      MSR(MAKE_##_reg(13), val);                                               \
      return;                                                                  \
    case 14:                                                                   \
      MSR(MAKE_##_reg(14), val);                                               \
      return;                                                                  \
    case 15:                                                                   \
      MSR(MAKE_##_reg(15), val);                                               \
      return;                                                                  \
    default:                                                                   \
      assert(bp_num == 0);                                                     \
      MSR(MAKE_##_reg(0), val);                                                \
      return;                                                                  \
    }                                                                          \
  }

#endif /* ARM_BASE_CP14_SAVE_AND_RESTORE */

#ifdef CONFIG_HARDWARE_DEBUG_API
exception_t handleDebugFaultEvent(word_t esr);
#endif /* CONFIG_HARDWARE_DEBUG_API */