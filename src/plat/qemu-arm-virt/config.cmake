#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: GPL-2.0-only
#

cmake_minimum_required(VERSION 3.7.2)

declare_platform(qemu-arm-virt KernelPlatformQEMUArmVirt PLAT_QEMU_ARM_VIRT KernelArchARM)

set(MIN_QEMU_VERSION "3.1.0")

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
    execute_process(COMMAND qemu-system-${QEMU_ARCH} -version OUTPUT_VARIABLE QEMU_VERSION_STR)
    string(
        REGEX
            MATCH
            "[0-9](\\.[0-9])+"
            QEMU_VERSION
            ${QEMU_VERSION_STR}
    )
    if("${QEMU_VERSION}" VERSION_LESS "${MIN_QEMU_VERSION}")
        message(WARNING "Warning: qemu version should be at least ${MIN_QEMU_VERSION}")
    endif()

    if("${QEMU_MEMORY}" STREQUAL "")
        set(QEMU_MEMORY "1024")
    endif()
    message("QEMU MEMORY is: ${QEMU_MEMORY}")
    config_set(KernelARMPlatform ARM_PLAT qemu-arm-virt)
    set(DTBPath "${CMAKE_BINARY_DIR}/virt.dtb")
    set(DTSPath "${CMAKE_BINARY_DIR}/virt.dts")
    if(KernelArmHypervisorSupport)
        set(QEMU_VIRT_OPTION "virtualization=on,highmem=off,secure=off")
    else()
        set(QEMU_VIRT_OPTION "virtualization=off")
    endif()
    if(KernelMaxNumNodes)
        set(QEMU_SMP_OPTION "${KernelMaxNumNodes}")
    else()
        set(QEMU_SMP_OPTION "1")
    endif()
    find_program(QEMU_BINARY qemu-system-${QEMU_ARCH})
    execute_process(
        COMMAND
            ${QEMU_BINARY} -machine virt,dumpdtb=${DTBPath},${QEMU_VIRT_OPTION} -m ${QEMU_MEMORY}
            -cpu ${ARM_CPU} -smp ${QEMU_SMP_OPTION} -nographic
    )
    execute_process(
        COMMAND
            dtc -q -I dtb -O dts ${DTBPath}
        OUTPUT_FILE ${DTSPath}
    )
    list(APPEND KernelDTSList "${DTSPath}")
    list(APPEND KernelDTSList "src/plat/qemu-arm-virt/overlay-qemu-arm-virt.dts")
    if(KernelArmHypervisorSupport)
        list(APPEND KernelDTSList "src/plat/qemu-arm-virt/overlay-reserve-vm-memory.dts")
    endif()
    declare_default_headers(
        TIMER_FREQUENCY 62500000llu
        MAX_IRQ 159
        NUM_PPI 32
        TIMER drivers/timer/arm_generic.h
        INTERRUPT_CONTROLLER arch/machine/gic_v2.h
        CLK_MAGIC 4611686019llu
        CLK_SHIFT 58u
        KERNEL_WCET 10u
    )
endif()

add_sources(
    DEP "KernelPlatformQEMUArmVirt"
    CFILES src/arch/arm/machine/gic_v2.c src/arch/arm/machine/l2c_nop.c
)
