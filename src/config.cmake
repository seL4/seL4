#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: GPL-2.0-only
#

cmake_minimum_required(VERSION 3.7.2)

add_sources(
    CFILES
        src/inlines.c
        src/assert.c
        src/util.c
        src/string.c
        src/fastpath/fastpath.c
        src/api/syscall.c
        src/api/faults.c
        src/kernel/cspace.c
        src/kernel/faulthandler.c
        src/kernel/thread.c
        src/kernel/boot.c
        src/kernel/stack.c
        src/object/notification.c
        src/object/cnode.c
        src/object/endpoint.c
        src/object/interrupt.c
        src/object/objecttype.c
        src/object/tcb.c
        src/object/untyped.c
        src/model/preemption.c
        src/model/statedata.c
        src/model/smp.c
        src/machine/io.c
        src/machine/capdl.c
        src/machine/registerset.c
        src/machine/fpu.c
        src/benchmark/benchmark.c
        src/benchmark/benchmark_track.c
        src/benchmark/benchmark_utilisation.c
        src/smp/lock.c
        src/smp/ipi.c
)
add_sources(
    DEP KernelIsMCS
    CFILES
        src/object/reply.c
        src/object/schedcontext.c
        src/object/schedcontrol.c
        src/kernel/sporadic.c
)

add_sources(
    DEP HaveCheri
    CFILES
        src/machine/cheri.c
)
