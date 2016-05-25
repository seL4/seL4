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

#ifdef CONFIG_BENCHMARK_TRACK_SYSCALLS

void benchmark_track_syscall_exit(void)
{
    timestamp_t duration = 0;
    timestamp_t ksExit = timestamp();

    /* If Log buffer is filled, do nothing */
    if (likely(ksIndex < MAX_SYSCALL_INVOCATIONS_ENTRIES)) {
        duration = ksExit - ksEnter;
        ksLog[ksIndex].entry = ksKernelEntry;
        ksLog[ksIndex].start_time = ksEnter;
        ksLog[ksIndex].duration = duration;
        ksIndex++;
    }
}

void benchmark_track_syscall_dump(
    word_t* buffer,
    word_t start_index,
    word_t num_entries
)
{
    benchmark_track_syscall_invocation_t *send_buffer;

    if (!buffer) {
        userError("Invalid IPC buffer pointer = %p\n", buffer);
        return;
    }

    if (start_index >= ksIndex) {
        userError("Invalid start index = %lu\n", start_index);
        return;
    }

    if ((start_index + num_entries) > ksIndex - 1) {
        userError("Requested entries exceed the range of tracked syscall invocations [%lu:%lu] \
                \n", start_index, num_entries);
        return;
    }

    send_buffer = (benchmark_track_syscall_invocation_t *) buffer;

    for (int i = start_index; i < (start_index + num_entries); i++) {
        send_buffer[i - start_index] = ksLog[i];
    }
}

#endif /* CONFIG_BENCHMARK_TRACK_SYSCALLS */
