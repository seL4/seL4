/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __ARCH_MACHINE_DEBUG_H
#define __ARCH_MACHINE_DEBUG_H

#ifdef DEBUG

#define MAX_BREAKPOINTS 16

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
static inline uint32_t
getDIDR(void)
{
    uint32_t x;

    asm volatile("mrc p14, 0, %0, c0, c0, 0" : "=r"(x));

    return x;
}
#endif /* !__ASSEMBLER__ */

/* Debug Status and Control Register */
#define DSCR_MONITOR_MODE_ENABLE     15
#define DSCR_MODE_SELECT             14
#define DSCR_ENTRY_OFFSET             2
#define DSCR_ENTRY_SIZE               4

#define DEBUG_ENTRY_DBGTAP_HALT       0
#define DEBUG_ENTRY_BREAKPOINT        1
#define DEBUG_ENTRY_WATCHPOINT        2
#define DEBUG_ENTRY_EXPLICIT_BKPT     3
#define DEBUG_ENTRY_EDBGRQ            4
#define DEBUG_ENTRY_VECTOR_CATCH      5
#define DEBUG_ENTRY_DATA_ABORT        6
#define DEBUG_ENTRY_INSTRUCTION_ABORT 7

#ifndef __ASSEMBLER__
static inline uint32_t
getDSCR(void)
{
    uint32_t x;

    asm volatile("mrc p14, 0, %0, c0, c1, 0" : "=r"(x));

    return x;
}

static inline void
setDSCR(uint32_t x)
{
    asm volatile("mcr p14, 0, %0, c0, c1, 0" : : "r"(x));
}
#endif /* !__ASSEMBLER__ */

/* Vector Catch Register */
#define VCR_FIQ      7
#define VCR_IRQ      6
#define VCR_DATA     4
#define VCR_PREFETCH 3
#define VCR_SWI      2
#define VCR_UNDEF    1
#define VCR_RESET    0

#ifndef __ASSEMBLER__
static inline uint32_t
getVCR(void)
{
    uint32_t x;

    asm volatile("mrc p14, 0, %0, c0, c7, 0" : "=r"(x));

    return x;
}

static inline void
setVCR(uint32_t x)
{
    asm volatile("mcr p14, 0, %0, c0, c7, 0" : : "r"(x));
}

/* Breakpoint Value Registers */
static inline uint32_t
getBVR(int n)
{
    uint32_t x;

    switch (n) {
    case 0:
        asm volatile("mrc p14, 0, %0, c0, c0, 4" : "=r"(x));
        break;
    case 1:
        asm volatile("mrc p14, 0, %0, c0, c1, 4" : "=r"(x));
        break;
    case 2:
        asm volatile("mrc p14, 0, %0, c0, c2, 4" : "=r"(x));
        break;
    case 3:
        asm volatile("mrc p14, 0, %0, c0, c3, 4" : "=r"(x));
        break;
    case 4:
        asm volatile("mrc p14, 0, %0, c0, c4, 4" : "=r"(x));
        break;
    case 5:
        asm volatile("mrc p14, 0, %0, c0, c5, 4" : "=r"(x));
        break;
    default:
        break;
    }

    return x;
}

static inline void
setBVR(int n, uint32_t x)
{
    switch (n) {
    case 0:
        asm volatile("mcr p14, 0, %0, c0, c0, 4" : : "r"(x));
        break;
    case 1:
        asm volatile("mcr p14, 0, %0, c0, c1, 4" : : "r"(x));
        break;
    case 2:
        asm volatile("mcr p14, 0, %0, c0, c2, 4" : : "r"(x));
        break;
    case 3:
        asm volatile("mcr p14, 0, %0, c0, c3, 4" : : "r"(x));
        break;
    case 4:
        asm volatile("mcr p14, 0, %0, c0, c4, 4" : : "r"(x));
        break;
    case 5:
        asm volatile("mcr p14, 0, %0, c0, c5, 4" : : "r"(x));
        break;
    default:
        break;
    }
}
#endif /* !__ASSEMBLER__ */

/* Breakpoint Control Registers */
#define BCR_MEANING            21
#define BCR_ENABLE_LINKING     20
#define BCR_LINKED_BRP         16
#define BCR_BYTE_SELECT         5
#define BCR_SUPERVISOR          1
#define BCR_ENABLE              0

#ifndef __ASSEMBLER__
static inline uint32_t
getBCR(int n)
{
    uint32_t x;

    switch (n) {
    case 0:
        asm volatile("mrc p14, 0, %0, c0, c0, 5" : "=r"(x));
        break;
    case 1:
        asm volatile("mrc p14, 0, %0, c0, c1, 5" : "=r"(x));
        break;
    case 2:
        asm volatile("mrc p14, 0, %0, c0, c2, 5" : "=r"(x));
        break;
    case 3:
        asm volatile("mrc p14, 0, %0, c0, c3, 5" : "=r"(x));
        break;
    case 4:
        asm volatile("mrc p14, 0, %0, c0, c4, 5" : "=r"(x));
        break;
    case 5:
        asm volatile("mrc p14, 0, %0, c0, c5, 5" : "=r"(x));
        break;
    default:
        break;
    }

    return x;
}

static inline void
setBCR(int n, uint32_t x)
{
    switch (n) {
    case 0:
        asm volatile("mcr p14, 0, %0, c0, c0, 5" : : "r"(x));
        break;
    case 1:
        asm volatile("mcr p14, 0, %0, c0, c1, 5" : : "r"(x));
        break;
    case 2:
        asm volatile("mcr p14, 0, %0, c0, c2, 5" : : "r"(x));
        break;
    case 3:
        asm volatile("mcr p14, 0, %0, c0, c3, 5" : : "r"(x));
        break;
    case 4:
        asm volatile("mcr p14, 0, %0, c0, c4, 5" : : "r"(x));
        break;
    case 5:
        asm volatile("mcr p14, 0, %0, c0, c5, 5" : : "r"(x));
        break;
    default:
        break;
    }
}
#endif /* !__ASSEMBLER__ */

#endif /* DEBUG */

#endif /* !__ARCH_MACHINE_DEBUG_H */
