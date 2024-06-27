/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>

#define DBGDSCR_int "p14,0,%0,c0,c1,0"
/* Not guaranteed in v7, only v7.1+ */
#define DBGDSCR_ext "p14, 0, %0, c0, c2, 2"
#define DBGSDER "p15, 0, %0, c1, c1, 1"

#define DBGWFAR "p14,0,%0,c0,c6,0"
#define DFAR "p15,0,%0,c6,c0,0"

#define DBGDSCR_SECURE_MODE_DISABLED  (BIT(18))

#define DBGSDER_ENABLE_SECURE_USER_NON_INVASIVE_DEBUG   (BIT(1))

#if defined(CONFIG_DEBUG_BUILD) || defined (CONFIG_HARDWARE_DEBUG_API)

#ifndef __ASSEMBLER__
#include <stdint.h>
#include <arch/machine/registerset.h>

void debug_init(void) VISIBLE;

typedef void (*break_handler_t)(user_context_t *context);

void software_breakpoint(uint32_t va, user_context_t *context) VISIBLE;
void breakpoint_multiplexer(uint32_t va, user_context_t *context) VISIBLE;

int set_breakpoint(uint32_t va, break_handler_t handler) VISIBLE;
void clear_breakpoint(uint32_t va) VISIBLE;

enum vector_ids {
    VECTOR_RESET =          0,
    VECTOR_UNDEFINED =      1,
    VECTOR_SWI =            2,
    VECTOR_PREFETCH_ABORT = 3,
    VECTOR_DATA_ABORT =     4,
    VECTOR_IRQ =            6,
    VECTOR_FIQ =            7
};
typedef uint32_t vector_t;

typedef void (*catch_handler_t)(user_context_t *context, vector_t vector);

void set_catch_handler(catch_handler_t handler) VISIBLE;
void catch_vector(vector_t vector) VISIBLE;
void uncatch_vector(vector_t vector) VISIBLE;
#endif /* !__ASSEMBLER__ */

/*********************************/
/*** cp14 register definitions ***/
/*********************************/

/* Debug ID Register */
#define DIDR_BRP_OFFSET             24
#define DIDR_BRP_SIZE                4
#define DIDR_VERSION_OFFSET         16
#define DIDR_VERSION_SIZE            4
#define DIDR_VARIANT_OFFSET          4
#define DIDR_VARIANT_SIZE            4
#define DIDR_REVISION_OFFSET         0
#define DIDR_REVISION_SIZE           4

#ifndef __ASSEMBLER__
static inline uint32_t getDIDR(void)
{
    uint32_t x;

    asm volatile("mrc p14, 0, %0, c0, c0, 0" : "=r"(x));

    return x;
}

#ifdef CONFIG_HARDWARE_DEBUG_API

#define DEBUG_REPLY_N_REQUIRED_REGISTERS        (1)

/* Get Watchpoint Fault Address register value (for async watchpoints). */
static inline word_t getWFAR(void)
{
    word_t ret;

    MRC(DBGWFAR, ret);
    return ret;
}
#endif
#endif /* !__ASSEMBLER__ */

/* Debug Status and Control Register */
#define DSCR_MONITOR_MODE_ENABLE     15
#define DSCR_MODE_SELECT             14
#define DSCR_ENTRY_OFFSET             2
#define DSCR_ENTRY_SIZE               4

#define DEBUG_ENTRY_DBGTAP_HALT       0
#define DEBUG_ENTRY_BREAKPOINT        1
#define DEBUG_ENTRY_ASYNC_WATCHPOINT  2
#define DEBUG_ENTRY_EXPLICIT_BKPT     3
#define DEBUG_ENTRY_EDBGRQ            4
#define DEBUG_ENTRY_VECTOR_CATCH      5
#define DEBUG_ENTRY_DATA_ABORT        6
#define DEBUG_ENTRY_INSTRUCTION_ABORT 7
#define DEBUG_ENTRY_SYNC_WATCHPOINT   (0xA)

/* Vector Catch Register */
#define VCR_FIQ      7
#define VCR_IRQ      6
#define VCR_DATA     4
#define VCR_PREFETCH 3
#define VCR_SWI      2
#define VCR_UNDEF    1
#define VCR_RESET    0

#ifndef __ASSEMBLER__
static inline uint32_t getVCR(void)
{
    uint32_t x;

    asm volatile("mrc p14, 0, %0, c0, c7, 0" : "=r"(x));

    return x;
}

static inline void setVCR(uint32_t x)
{
    asm volatile("mcr p14, 0, %0, c0, c7, 0" : : "r"(x));
}

#endif /* !__ASSEMBLER__ */

/* Breakpoint Control Registers */
#define BCR_MEANING            21
#define BCR_ENABLE_LINKING     20
#define BCR_LINKED_BRP         16
#define BCR_BYTE_SELECT         5
#define BCR_SUPERVISOR          1
#define BCR_ENABLE              0

#define FSR_SHORTDESC_STATUS_DEBUG_EVENT       (0x2)
#define FSR_LONGDESC_STATUS_DEBUG_EVENT        (0x22)
#define FSR_LPAE_SHIFT                         (9)
#define FSR_STATUS_BIT4_SHIFT                  (10)

#ifndef __ASSEMBLER__

#ifdef CONFIG_HARDWARE_DEBUG_API
/** Determines whether or not a Prefetch Abort or Data Abort was really a debug
 * exception.
 *
 * Examines the FSR bits, looking for the "Debug event" value, and also examines
 * DBGDSCR looking for the "Async watchpoint abort" value, since async
 * watchpoints behave differently.
 */
bool_t isDebugFault(word_t hsr_or_fsr);

/** Determines and carries out what needs to be done for a debug exception.
 *
 * This could be handling a single-stepping exception, or a breakpoint or
 * watchpoint.
 */
seL4_Fault_t handleUserLevelDebugException(word_t fault_vaddr);

#endif /* CONFIG_HARDWARE_DEBUG_API */

#endif /* !__ASSEMBLER__ */

#endif /* defined(CONFIG_DEBUG_BUILD) || defined (CONFIG_HARDWARE_DEBUG_API) */

#ifdef ARM_BASE_CP14_SAVE_AND_RESTORE

#define MAKE_P14(crn, crm, opc2) "p14, 0, %0, c" #crn ", c" #crm ", " #opc2
#define MAKE_DBGBVR(num) MAKE_P14(0, num, 4)
#define MAKE_DBGBCR(num) MAKE_P14(0, num, 5)
#define MAKE_DBGWVR(num) MAKE_P14(0, num, 6)
#define MAKE_DBGWCR(num) MAKE_P14(0, num, 7)
#define MAKE_DBGXVR(num) MAKE_P14(1, num, 1)

/** Generates read functions for the CP14 control and value registers.
 */
#define DEBUG_GENERATE_READ_FN(_name, _reg) \
static inline word_t \
_name(uint16_t bp_num) \
{ \
    word_t ret; \
 \
    switch (bp_num) { \
    case 1: \
        MRC(MAKE_ ## _reg(1), ret); \
        return ret; \
    case 2: \
        MRC(MAKE_ ## _reg(2), ret); \
        return ret; \
    case 3: \
        MRC(MAKE_ ## _reg(3), ret); \
        return ret; \
    case 4: \
        MRC(MAKE_ ## _reg(4), ret); \
        return ret; \
    case 5: \
        MRC(MAKE_ ## _reg(5), ret); \
        return ret; \
    case 6: \
        MRC(MAKE_ ## _reg(6), ret); \
        return ret; \
    case 7: \
        MRC(MAKE_ ## _reg(7), ret); \
        return ret; \
    case 8: \
        MRC(MAKE_ ## _reg(8), ret); \
        return ret; \
    case 9: \
        MRC(MAKE_ ## _reg(9), ret); \
        return ret; \
    case 10: \
        MRC(MAKE_ ## _reg(10), ret); \
        return ret; \
    case 11: \
        MRC(MAKE_ ## _reg(11), ret); \
        return ret; \
    case 12: \
        MRC(MAKE_ ## _reg(12), ret); \
        return ret; \
    case 13: \
        MRC(MAKE_ ## _reg(13), ret); \
        return ret; \
    case 14: \
        MRC(MAKE_ ## _reg(14), ret); \
        return ret; \
    case 15: \
        MRC(MAKE_ ## _reg(15), ret); \
        return ret; \
    default: \
        assert(bp_num == 0); \
        MRC(MAKE_ ## _reg(0), ret); \
        return ret; \
    } \
}

/** Generates write functions for the CP14 control and value registers.
 */
#define DEBUG_GENERATE_WRITE_FN(_name, _reg)  \
static inline void \
_name(uint16_t bp_num, word_t val) \
{ \
    switch (bp_num) { \
    case 1: \
        MCR(MAKE_ ## _reg(1), val); \
        return; \
    case 2: \
        MCR(MAKE_ ## _reg(2), val); \
        return; \
    case 3: \
        MCR(MAKE_ ## _reg(3), val); \
        return; \
    case 4: \
        MCR(MAKE_ ## _reg(4), val); \
        return; \
    case 5: \
        MCR(MAKE_ ## _reg(5), val); \
        return; \
    case 6: \
        MCR(MAKE_ ## _reg(6), val); \
        return; \
    case 7: \
        MCR(MAKE_ ## _reg(7), val); \
        return; \
    case 8: \
        MCR(MAKE_ ## _reg(8), val); \
        return; \
    case 9: \
        MCR(MAKE_ ## _reg(9), val); \
        return; \
    case 10: \
        MCR(MAKE_ ## _reg(10), val); \
        return; \
    case 11: \
        MCR(MAKE_ ## _reg(11), val); \
        return; \
    case 12: \
        MCR(MAKE_ ## _reg(12), val); \
        return; \
    case 13: \
        MCR(MAKE_ ## _reg(13), val); \
        return; \
    case 14: \
        MCR(MAKE_ ## _reg(14), val); \
        return; \
    case 15: \
        MCR(MAKE_ ## _reg(15), val); \
        return; \
    default: \
        assert(bp_num == 0); \
        MCR(MAKE_ ## _reg(0), val); \
        return; \
    } \
}

#endif /* ARM_BASE_CP14_SAVE_AND_RESTORE */
