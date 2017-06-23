/*
 * Copyright 2017, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the BSD 2-Clause license. Note that NO WARRANTY is provided.
 * See "LICENSE_BSD2.txt" for details.
 *
 * @TAG(DATA61_BSD)
 */

#ifndef BENCHMARK_TRACK_TYPES_H
#define BENCHMARK_TRACK_TYPES_H

#ifdef HAVE_AUTOCONF
#include <autoconf.h>
#endif

#if (defined CONFIG_BENCHMARK_TRACK_KERNEL_ENTRIES || defined CONFIG_DEBUG_BUILD)

/* the following code can be used at any point in the kernel
 * to determine detail about the kernel entry point */
typedef enum {
    Entry_Interrupt,
    Entry_UnknownSyscall,
    Entry_UserLevelFault,
    Entry_DebugFault,
    Entry_VMFault,
    Entry_Syscall,
    Entry_UnimplementedDevice,
#ifdef CONFIG_ARCH_ARM
    Entry_VCPUFault,
#endif
#ifdef CONFIG_ARCH_X86
    Entry_VMExit,
#endif
} entry_type_t;

/**
 * @brief Kernel entry logging
 *
 * Encapsulates useful info about the cause of the kernel entry
 */
typedef struct PACKED kernel_entry {
    seL4_Word path: 3;
    union {
        struct {
            seL4_Word word: 29;
        };
        /* Tracked kernel entry info filled from outside this file */
        struct {
            seL4_Word syscall_no: 4;
            seL4_Word cap_type: 5;
            seL4_Word is_fastpath: 1;
            seL4_Word invocation_tag: 19;
        };
    };
} kernel_entry_t;

#endif /* CONFIG_BENCHMARK_TRACK_KERNEL_ENTRIES || DEBUG */

#ifdef CONFIG_BENCHMARK_TRACK_KERNEL_ENTRIES

typedef struct benchmark_syscall_log_entry {
    uint64_t  start_time;
    uint32_t  duration;
    kernel_entry_t entry;
} benchmark_track_kernel_entry_t;

#endif /* CONFIG_BENCHMARK_TRACK_KERNEL_ENTRIES || CONFIG_DEBUG_BUILD */

#endif /* BENCHMARK_TRACK_TYPES_H */
