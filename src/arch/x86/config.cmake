#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: GPL-2.0-only
#

cmake_minimum_required(VERSION 3.7.2)

if(KernelArchX86)
    set_property(TARGET kernel_config_target APPEND PROPERTY TOPLEVELTYPES pde_C)
    # x86 always has an FPU
    set(KernelHaveFPU ON)

endif()

# Add any top level types
if(KernelSel4ArchX86_64)
    set_property(TARGET kernel_config_target APPEND PROPERTY TOPLEVELTYPES pdpte_C pml4e_C)
endif()

config_choice(
    KernelX86MicroArch
    KERNEL_X86_MICRO_ARCH
    "Select the x86 micro architecture"
    "nehalem;KernelX86MicroArchNehalem;ARCH_X86_NEHALEM;KernelArchX86"
    "generic;KernelX86MicroArchGeneric;ARCH_X86_GENERIC;KernelArchX86"
    "westmere;KernelX86MicroArchWestmere;ARCH_X86_WESTMERE;KernelArchX86"
    "sandy;KernelX86MicroArchSandy;ARCH_X86_SANDY;KernelArchX86"
    "ivy;KernelX86MicroArchIvy;ARCH_X86_IVY;KernelArchX86"
    "haswell;KernelX86MicroArchHaswell;ARCH_X86_HASWELL;KernelArchX86"
    "broadwell;KernelX86MicroArchBroadwell;ARCH_X86_BROADWELL;KernelArchX86"
    "skylake;KernelX86MicroArchSkylake;ARCH_X86_SKYLAKE;KernelArchX86"
)

config_choice(
    KernelIRQController
    KERNEL_IRQ_CONTROLLER
    "Select the IRQ controller seL4 will use. Code for others may still be included if \
    needed to disable at run time. \
    PIC -> Use the legacy PIC controller. \
    IOAPIC -> Use one or more IOAPIC controllers"
    "IOAPIC;KernelIRQControllerIOAPIC;IRQ_IOAPIC;KernelArchX86"
    "PIC;KernelIRQControllerPIC;IRQ_PIC;KernelArchX86"
)

config_string(
    KernelMaxNumIOAPIC MAX_NUM_IOAPIC
    "Configure the maximum number of IOAPIC controllers that can be supported. SeL4 \
    will detect IOAPICs regardless of whether the IOAPIC will actually be used as \
    the final IRQ controller."
    DEFAULT 1
    DEPENDS "KernelIRQControllerIOAPIC" DEFAULT_DISABLED 0
    UNQUOTE
)

config_choice(
    KernelLAPICMode
    KERNEL_LAPIC_MODE
    "Select the mode local APIC will use. Not all machines support X2APIC mode."
    "XAPIC;KernelLAPICModeXPAIC;XAPIC;KernelArchX86"
    "X2APIC;KernelLAPICModeX2APIC;X2APIC;KernelArchX86"
)

config_option(
    KernelUseLogicalIDs USE_LOGICAL_IDS
    "Use logical IDs to broadcast IPI between cores. Not all machines support logical \
    IDs. In xAPIC mode only 8 cores can be addressed using logical IDs."
    DEFAULT OFF
    DEPENDS "NOT ${KernelMaxNumNodes} EQUAL 1;KernelArchX86"
)

config_string(
    KernelCacheLnSz CACHE_LN_SZ "Define cache line size for the current architecture"
    DEFAULT 64
    DEPENDS "KernelArchX86" UNDEF_DISABLED
    UNQUOTE
)

config_option(
    KernelVTX VTX "VTX support"
    DEFAULT OFF
    DEPENDS "KernelArchX86;NOT KernelVerificationBuild"
)

config_option(
    KernelX86_64VTX64BitGuests X86_64_VTX_64BIT_GUESTS "Support 64-bit guests"
    DEFAULT OFF
    DEPENDS "KernelArchX86;KernelVTX;NOT KernelVerificationBuild"
)

config_option(
    KernelIOMMU IOMMU "IOMMU support for VT-d enabled chipset"
    DEFAULT ON
    DEPENDS "KernelPlatPC99; NOT KernelVerificationBuild"
    DEFAULT_DISABLED OFF
)

config_string(
    KernelMaxRMRREntries MAX_RMRR_ENTRIES
    "Setsthe maximum number of Reserved Memory Region Reporting structures we support \
    recording from the ACPI tables"
    DEFAULT 32
    DEPENDS "KernelIOMMU" DEFAULT_DISABLED 1
    UNQUOTE
)

config_string(
    KernelMaxVPIDs MAX_VPIDS
    "The kernel maintains a mapping of 16-bit VPIDs to VCPUs. This option should be \
    sized as small as possible to save memory, but be at least the number of VCPUs that \
    will be run for optimum performance."
    DEFAULT 1024
    DEPENDS "KernelVTX" DEFAULT_DISABLED 0
    UNQUOTE
)

config_option(
    KernelHugePage HUGE_PAGE
    "Add support for 1GB huge page. Not all recent processor models support this feature."
    DEFAULT ON
    DEPENDS "KernelSel4ArchX86_64"
    DEFAULT_DISABLED OFF
)
config_option(
    KernelSupportPCID SUPPORT_PCID
    "Add support for PCIDs (aka hardware ASIDs). Not all processor models support this feature."
    DEFAULT ON
    DEPENDS "KernelSel4ArchX86_64"
    DEFAULT_DISABLED OFF
)

config_choice(
    KernelSyscall
    KERNEL_X86_SYSCALL
    "The kernel only ever supports one method of performing syscalls at a time. This \
    config should be set to the most efficient one that is support by the hardware the \
    system will run on"
    "syscall;KernelX86SyscallSyscall;SYSCALL;KernelSel4ArchX86_64"
    "sysenter;KernelX86SyscallSysenter;SYSENTER;KernelArchX86"
)

config_choice(
    KernelFPU
    KERNEL_X86_FPU
    "Choose the method that FPU state is stored in. This \
    directly affects the method used to save and restore it. \
    FXSAVE -> This chooses the legacy 512-byte region used by the fxsave and fxrstor functions \
    XSAVE -> This chooses the variable xsave region, and enables the ability to use any \
    of the xsave variants to save and restore. The actual size of the region is dependent on \
    the features enabled."
    "XSAVE;KernelFPUXSave;XSAVE;KernelArchX86"
    "FXSAVE;KernelFPUFXSave;FXSAVE;KernelArchX86"
)

config_choice(
    KernelXSave
    KERNEL_XSAVE
    "The XSAVE area supports multiple instructions to save
        and restore to it. These instructions are dependent upon specific CPU support. See Chapter 13 of Volume \
        1 of the Intel Architectures SOftware Developers Manual for discussion on the init and modified \
        optimizations. \
        XSAVE -> Original XSAVE instruction. This is the only XSAVE instruction that is guaranteed to exist if \
            XSAVE is present \
        XSAVEC -> Save state with compaction. This compaction has to do with minimizing the total size of \
            XSAVE buffer, if using non contiguous features, XSAVEC will attempt to use the init optimization \
            when saving \
        XSAVEOPT -> Save state taking advantage of both the init optimization and modified optimization \
        XSAVES -> Save state taking advantage of the modified optimization. This instruction is only \
            available in OS code, and is the preferred save method if it exists."
    "XSAVEOPT;KernelXSaveXSaveOpt;XSAVE_XSAVEOPT;KernelFPUXSave"
    "XSAVE;KernelXSaveXSave;XSAVE_XSAVE;KernelFPUXSave"
    "XSAVEC;KernelXSaveXSaveC;XSAVE_XSAVEC;KernelFPUXSave"
)
config_string(
    KernelXSaveFeatureSet XSAVE_FEATURE_SET
    "XSAVE can save and restore the state for various features \
    through the use of the feature mask. This config option represents the feature mask that we want to \
    support. The CPU must support all bits in this feature mask. Current known bits are \
        0 - FPU \
        1 - SSE \
        2 - AVX \
        FPU and SSE is guaranteed to exist if XSAVE exists."
    DEFAULT 3
    DEPENDS "KernelFPUXSave" DEFAULT_DISABLED 0
    UNQUOTE
)

if(KernelFPUXSave)
    set(default_xsave_size 576)
else()
    set(default_xsave_size 512)
endif()

config_string(
    KernelXSaveSize XSAVE_SIZE
    "The size of the XSAVE region. This is dependent upon the features in \
    XSAVE_FEATURE_SET that have been requested. Default is 576 for the FPU and SSE
    state, unless XSAVE is not in use then it should be 512 for the legacy FXSAVE region."
    DEFAULT ${default_xsave_size}
    DEPENDS "KernelArchX86" DEFAULT_DISABLED 0
    UNQUOTE
)

config_choice(
    KernelFSGSBase
    KERNEL_FSGS_BASE
    "There are three ways to to set FS/GS base addresses: \
    IA32_FS/GS_GDT, IA32_FS/GS_BASE_MSR, and fsgsbase instructions. \
    IA32_FS/GS_GDT and IA32_FS/GS_BASE_MSR are availble for 32-bit. \
    IA32_FS/GS_BASE_MSR and fsgsbase instructions are available for 64-bit."
    "inst;KernelFSGSBaseInst;FSGSBASE_INST;KernelSel4ArchX86_64"
    "gdt;KernelFSGSBaseGDT;FSGSBASE_GDT;KernelSel4ArchIA32"
    "msr;KernelFSGSBaseMSR;FSGSBASE_MSR;KernelArchX86"
)

config_choice(
    KernelMultibootGFXMode
    KERNEL_MUTLTIBOOT_GFX_MODE
    "The type of graphics mode to request from the boot loader. This is encoded into the \
    multiboot header and is merely a hint, the boot loader is free to ignore or set some \
    other mode"
    "none;KernelMultibootGFXModeNone;MULTIBOOT_GRAPHICS_MODE_NONE;KernelArchX86"
    "text;KernelMultibootGFXModeText;MULTIBOOT_GRAPHICS_MODE_TEXT;KernelArchX86"
    "linear;KernelMultibootGFXModeLinear;MULTIBOOT_GRAPHICS_MODE_LINEAR;KernelArchX86"
)

config_string(
    KernelMultibootGFXDepth MULTIBOOT_GRAPHICS_MODE_DEPTH
    "The bits per pixel of the linear graphics mode ot request. Value of zero indicates \
    no preference."
    DEFAULT 32
    DEPENDS "KernelMultibootGFXModeLinear" UNDEF_DISABLED
    UNQUOTE
)

config_string(
    KernelMultibootGFXWidth MULTIBOOT_GRAPHICS_MODE_WIDTH
    "The width of the graphics mode to request. For a linear graphics mode this is the \
    number of pixels. For a text mode this is the number of characters, value of zero \
    indicates no preference."
    DEFAULT 0
    DEPENDS "KernelMultibootGFXModeText OR KernelMultibootGFXModeLinear" UNDEF_DISABLED
    UNQUOTE
)
config_string(
    KernelMultibootGFXHeight MULTIBOOT_GRAPHICS_MODE_HEIGHT
    "The height of the graphics mode to request. For a linear graphics mode this is the \
    number of pixels. For a text mode this is the number of characters, value of zero \
    indicates no preference."
    DEFAULT 0
    DEPENDS "KernelMultibootGFXModeText OR KernelMultibootGFXModeLinear" UNDEF_DISABLED
    UNQUOTE
)

config_option(
    KernelMultiboot1Header MULTIBOOT1_HEADER
    "Inserts a header that indicates to the bootloader that the kernel supports a multiboot 1 boot header"
    DEFAULT ON
    DEPENDS "KernelArchX86"
)

config_option(
    KernelMultiboot2Header MULTIBOOT2_HEADER
    "Inserts a header that indicates to the bootloader that the kernel supports a multiboot 2 boot header. \
    This is can be enabled together with a multiboot 1 header and the boot loader may use either one"
    DEFAULT ON
    DEPENDS "KernelArchX86"
)

config_option(
    KernelSkimWindow KERNEL_SKIM_WINDOW
    "Prevent against the Meltdown vulnerability by using a reduced Static Kernel
    Image and Micro-state window instead of having all kernel state in the kernel window.
    This only needs to be enabled if deploying to a vulnerable processor"
    DEFAULT ON
    DEPENDS "KernelSel4ArchX86_64"
    DEFAULT_DISABLED OFF
)

config_option(
    KernelExportPMCUser EXPORT_PMC_USER "Grant user access to the Performance Monitoring Counters.
    This allows the user to read performance counters, although
    not control what the counters are and whether or not they
    are counting. Nevertheless whilst this is useful for
    evalulating performance this option opens timing and covert
    channels."
    DEFAULT OFF
    DEPENDS "KernelArchX86;NOT KernelVerificationBuild"
)

config_option(
    KernelX86DangerousMSR KERNEL_X86_DANGEROUS_MSR
    "rdmsr/wrmsr kernel interface. Provides a syscall interface for reading and writing arbitrary MSRs.
    This is extremely dangerous as no checks are performed and exists
    to aid debugging and benchmarking."
    DEFAULT OFF
    DEPENDS "KernelArchX86;NOT KernelVerificationBuild"
)

if(KernelArchX86 AND (NOT "${KernelMaxNumNodes}" EQUAL 1))
    set(STIBDEP TRUE)
else()
    set(STIBDEP FALSE)
endif()

config_choice(
    KernelX86IBRSMode
    KERNEL_X86_IBRS
    "Indirect Branch Restricted Speculation mode
    Used to prevent a user from manipulating the branch predictor to manipulate speculative
    execution of other processes. On current processors IBRS has a prohibitive performance
    penalty and it is recommended that it be disabled such that software mitigations are
    used instead. Software mitigation is done by disabling jump tables (the only form of
    indirect jump in seL4 except for 'ret') and flushing the RSB on vmexit. Flushing the RSB
    at other times is not needed as seL4 does not switch kernel stacks and so is not
    vulnerable to RSB underflow. The STIBP is essentially software mitigation but enables
    the single thread isolation for branch predictions. This is only needed if attempting
    to protect user level process from each other in a multicore environment."
    "ibrs_none;KernelX86IBRSnone;KERNEL_X86_IBRS_NONE;KernelArchX86"
    "ibrs_stibp;KernelX86IBRSSTIBP;KERNEL_X86_IBRS_STIBP;STIBPDEP"
    "ibrs_basic;KernelX86IBRSBasic;KERNEL_X86_IBRS_BASIC;KernelArchX86"
    "ibrs_all;KernelX86IBRSAll;KERNEL_X86_IBRS_ALL;KernelArchX86"
)

if(KernelX86IBRSBasic OR KernelX86IBRSSTIBP)
    # As the kernel has no function pointers or other indirect jumps except those
    # as generated by the compiler through switch statements we can disable jump
    # tables in order to prevent Spectre Variant 2 style attacks.
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fno-jump-tables")
endif()

config_option(
    KernelX86IBPBOnContextSwitch KERNEL_X86_IBPB_ON_CONTEXT_SWITCH
    "Performs a IBPB on every context switch to prevent Spectre attacks between user
    processes. This is extremely expensive and is recommended you only turn this on
    if absolutely necessary.
    Note that in a multicore environment you should also enable STIBP to prevent
    other cores retraining the branch predictor even after context switch."
    DEFAULT OFF
    DEPENDS "KernelArchX86"
)

config_option(
    KernelX86RSBOnContextSwitch KERNEL_X86_RSB_ON_CONTEXT_SWITCH
    "Flushes the RSB on context switch to prevent Spectre attacks between user processes.
    Whilst not nearly as expensive as an IBPB it is not enabled by default as it is
    largely pointless to flush the RSB without also doing an IBPB as the RSB is already
    a harder attack vector."
    DEFAULT OFF
    DEPENDS "KernelArchX86"
)

if(KernelSel4ArchIA32)
    set(KernelSetTLSBaseSelf ON)
    math(EXPR KernelPaddrUserTop "0xffff0000")
else()
    math(EXPR KernelPaddrUserTop "1 << 47")
endif()
if(KernelSel4ArchX86_64 AND NOT KernelFSGSBaseInst)
    set(KernelSetTLSBaseSelf ON)
endif()

add_sources(
    DEP "KernelArchX86"
    PREFIX src/arch/x86
    CFILES
        c_traps.c
        idle.c
        api/faults.c
        object/interrupt.c
        object/ioport.c
        object/objecttype.c
        object/tcb.c
        object/iospace.c
        object/vcpu.c
        kernel/vspace.c
        kernel/apic.c
        kernel/xapic.c
        kernel/x2apic.c
        kernel/boot_sys.c
        kernel/smp_sys.c
        kernel/boot.c
        kernel/cmdline.c
        kernel/ept.c
        kernel/thread.c
        model/statedata.c
        machine/capdl.c
        machine/hardware.c
        machine/fpu.c
        machine/cpu_identification.c
        machine/breakpoint.c
        machine/registerset.c
        benchmark/benchmark.c
        smp/ipi.c
    ASMFILES multiboot.S
)

add_bf_source_old("KernelArchX86" "structures.bf" "include/arch/x86" "arch/object")

include(src/arch/x86/${KernelWordSize}/config.cmake)
