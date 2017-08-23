#
# Copyright 2017, Data61
# Commonwealth Scientific and Industrial Research Organisation (CSIRO)
# ABN 41 687 119 230.
#
# This software may be distributed and modified according to the terms of
# the GNU General Public License version 2. Note that NO WARRANTY is provided.
# See "LICENSE_GPLv2.txt" for details.
#
# @TAG(DATA61_GPL)
#

cmake_minimum_required(VERSION 3.7.2)

add_sources(CFILES
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
    src/machine/registerset.c
    src/machine/fpu.c
    src/benchmark/benchmark_track.c
    src/benchmark/benchmark_utilisation.c
    src/smp/lock.c
    src/smp/ipi.c
)
