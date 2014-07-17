/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

/*
 * Profiler Interface
 *
 * 2006 - 2007  David Greenaway
 * 2007 Ported to seL4 C kernel by Philip Derrin
 */

#ifndef __MACHINE__PROFILER_H__
#define __MACHINE__PROFILER_H__

#include <machine/registerset.h>
#include <machine/hardware.h>

#ifdef PROFILER

/* Approximate number of unique addresses that we can record.
 *
 * This value corresponds to the size of a hash table, and needs
 * to be a prime number to ensure correctness.
 */
//#define MAX_UNIQUE_INSTRUCTIONS ((256 * 1024) + 3)  /* 262147 is prime */
/* Downsized to fit in the default 1M kernel section - davec */
#define MAX_UNIQUE_INSTRUCTIONS 94349

#define MAX_UNIQUE_CHECKPOINTS 2000

/* Record the given program counter */
void profiler_record_sample(word_t pc) VISIBLE;

/* Reset all counters */
void profiler_reset(void);

/* List the recorded values to stdout */
void profiler_list(void);

/* Should we be profiling the system? */
extern bool_t profiler_enabled;

/* Set the current status of the profiler */
static inline void profiler_set_enabled(bool_t enabled)
{
    profiler_enabled = enabled;
}

/* Get the current status of the profiler */
static inline bool_t profiler_is_enabled(void)
{
    return profiler_enabled;
}

/* Number of entries the profiler currently keeps track of */
extern int profiler_num_entries;

/* Number of instructions the profiler could not record */
extern long long profiler_dropped_instructions;

/* The instructions recorded by the profiler */
typedef struct {
    word_t pc;
    word_t count;
} profiler_entry_t;

#ifdef CHECKPOINT_PROFILER
extern volatile unsigned int checkpoint;
extern profiler_entry_t profiler_entries[MAX_UNIQUE_CHECKPOINTS];
#else
extern profiler_entry_t profiler_entries[MAX_UNIQUE_INSTRUCTIONS];
#endif

#endif /* PROFILER */

#endif /* !__MACHINE__PROFILER_H__ */

