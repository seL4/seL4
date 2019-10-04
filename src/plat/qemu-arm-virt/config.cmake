#
# Copyright 2019, Data61
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

declare_platform(qemu-arm-virt KernelPlatformQEMUArmVirt PLAT_QEMU_ARM_VIRT KernelArchARM)

if(KernelPlatformQEMUArmVirt)
    if("${ARM_CPU}" STREQUAL "cortex-a15")
        declare_seL4_arch(aarch32)
        set(QEMU_ARCH "arm")
        set(KernelArmCortexA15 ON)
        set(KernelArchArmV7a ON)
    elseif("${ARM_CPU}" STREQUAL "cortex-a53")
        declare_seL4_arch(aarch64)
        set(QEMU_ARCH "aarch64")
        set(KernelArmCortexA53 ON)
        set(KernelArchArmV8a ON)
    elseif("${ARM_CPU}" STREQUAL "cortex-a57")
        declare_seL4_arch(aarch64)
        set(QEMU_ARCH "aarch64")
        set(KernelArmCortexA57 ON)
        set(KernelArchArmV8a ON)
    else()
        message("Warning: no cpu specified for virt board, fallback on cortex-a53")
        declare_seL4_arch(aarch64)
        set(ARM_CPU "cortex-a53")
        set(QEMU_ARCH "aarch64")
        set(KernelArmCortexA53 ON)
        set(KernelArchArmV8a ON)
    endif()
    if("${QEMU_MEMORY}" STREQUAL "")
        set(QEMU_MEMORY "1024")
    endif()
    config_set(KernelARMPlatform ARM_PLAT qemu-arm-virt)
    set(DTBPath "${CMAKE_BINARY_DIR}/virt.dtb")
    set(DTSPath "${CMAKE_BINARY_DIR}/virt.dts")
    execute_process(
        COMMAND
            qemu-system-${QEMU_ARCH} -machine virt,dumpdtb=${DTBPath} -m ${QEMU_MEMORY} -cpu
            ${ARM_CPU} -nographic
    )
    execute_process(
        COMMAND
            dtc -q -I dtb -O dts ${DTBPath}
        OUTPUT_FILE ${DTSPath}
    )
    list(APPEND KernelDTSList "${DTSPath}")
    list(APPEND KernelDTSList "src/plat/qemu-arm-virt/overlay-qemu-arm-virt.dts")
    declare_default_headers(
        TIMER_FREQUENCY 1200000llu
        MAX_IRQ 159
        TIMER drivers/timer/arm_generic.h
        INTERRUPT_CONTROLLER arch/machine/gic_v2.h
        CLK_MAGIC 458129845llu
        CLK_SHIFT 39u
        KERNEL_WCET 10u
    )
endif()

add_sources(
    DEP "KernelPlatformQEMUArmVirt"
    CFILES src/arch/arm/machine/gic_v2.c src/arch/arm/machine/l2c_nop.c
)
