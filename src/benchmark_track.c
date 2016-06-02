/*
 * Copyright 2016, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#include <config.h>
#include <benchmark_track.h>
#include <model/statedata.h>

#ifdef CONFIG_BENCHMARK_TRACK_KERNEL_ENTRIES

benchmark_track_kernel_entry_t *ksLog;
timestamp_t ksEnter;
word_t ksLogIndex;
uint32_t ksLogIndexFinalized;

void benchmark_track_exit(void)
{
    timestamp_t duration = 0;
    timestamp_t ksExit = timestamp();

    /* If Log buffer is filled, do nothing */
    if (likely(ksLogIndex < MAX_LOG_SIZE)) {
        duration = ksExit - ksEnter;
        ksLog[ksLogIndex].entry = ksKernelEntry;
        ksLog[ksLogIndex].start_time = ksEnter;
        ksLog[ksLogIndex].duration = duration;
        ksLogIndex++;
    }
}

void benchmark_track_dump(
    benchmark_track_kernel_entry_t* buffer,
    word_t start_index,
    word_t num_entries
)
{
    if (!buffer) {
        userError("Invalid IPC buffer pointer = %p\n", buffer);
        return;
    }

    if (start_index > ksLogIndex) {
        userError("Invalid start index = %lu\n", start_index);
        return;
    }

    if ((start_index + num_entries) > ksLogIndex) {
        userError("Requested entries exceed the range of tracked syscall invocations [%lu:%lu] \
                \n", start_index, num_entries);
        return;
    }

    for (int i = start_index; i < (start_index + num_entries); i++) {
        buffer[i - start_index] = ksLog[i];
    }
}

#endif /* CONFIG_BENCHMARK_TRACK_KERNEL_ENTRIES */
