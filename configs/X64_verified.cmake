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

set(KernelArch "x86" CACHE STRING "")
set(KernelX86Sel4Arch "x86_64" CACHE STRING "")
set(KernelVerificationBuild ON CACHE BOOL "")
set(KernelMaxNumNodes "1" CACHE STRING "")
set(KernelOptimisation "-O2" CACHE STRING "")
set(KernelRetypeFanOutLimit "256" CACHE STRING "")
set(KernelBenchmarks "none" CACHE STRING "")
set(KernelDangerousCodeInjection OFF CACHE BOOL "")
set(KernelFastpath ON CACHE BOOL "")
set(KernelPrinting OFF CACHE BOOL "")
set(KernelNumDomains 16 CACHE STRING "")
set(KernelMaxNumBootinfoUntypedCap 166 CACHE STRING "")
set(KernelRootCNodeSizeBits 19 CACHE STRING "")
set(KernelMaxNumBootinfoUntypedCaps 50 CACHE STRING "")
