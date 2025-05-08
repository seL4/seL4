<!-- SPDX-License-Identifier: CC-BY-SA-4.0 -->

# Revision History for seL4

<!--
    Document maintainers: Wrap lines in this file at 120 characters.

    Kernel engineers: When making changes to code (rather than documentation or comments) in the kernel repository,
    include a change item entry here, at the end of the list for the upcoming release, describing the change, and
    evaluate whether the compatibility breakage level must be promoted as a consequence.  As some rules of thumb:
    * If the change affects only the sources of the kernel (`src/`, `/include`), it is a BINARY-COMPATIBLE change.
    * If the change adds visible C preprocessor or language symbols in `libsel4/`, it is a BINARY-COMPATIBLE change.
    * If the change alters existing symbol definitions, types, or implementations in `libsel4/`, it is a
      SOURCE-COMPATIBLE change.
    * Otherwise, it is BREAKING.
-->

The following is a high-level description of changes to the seL4 kernel project, grouped by release.  It is aimed at
engineers who desire a summary of changes in more coarse-grained form than the Git commit history.  Each release
description indicates whether it is SOURCE-COMPATIBLE, BINARY-COMPATIBLE, or BREAKING relative to the previous release.

---

## Upcoming release: BREAKING

### Changes

* Added `zynqmp` and `rpi4` to the set of verified AArch64 configs.
* riscv: Change default cmake options KernelRiscvExtF and KernelRiscvExtD from OFF to ON.
  Except for RISCV32 with LLVM clang enabled will default both to OFF.
* Change fault based FPU context switching to a TCB flag based approach:
  New system call `seL4_TCB_SetFlags` and new flag `seL4_TCBFlag_fpuDisabled`.
  See [RFC-18](https://sel4.github.io/rfcs/implemented/0180-fpu-switching.html).

### Platforms

* Removed the default settings `KernelArmVtimerUpdateVOffset` and `KernelArmDisableWFIWFETraps` from the platform
  `tqma8xqp1gb`, since they are project specific settings, not platform settings. Add
  `set(KernelArmVtimerUpdateVOffset OFF)` and
  `set(KernelArmDisableWFIWFETraps ON)`
  to your project settings to get the same configuration as before if you are using `tqma8xqp1gb`.

#### Arm

* Added config option for selecting which thread ID register is used for Kernel TLS syscalls and invocations.
  KernelArmTLSReg can be used to select either `tpidru` or `tpidruro` as the TLS register used for `seL4_TCB_SetTLSBase` and `seL4_SetTLSBase` operations.
  This config option's default value is `tpidru` which is what the register that the kernel currently uses for the TLS register for aarch32 and aarch64 platforms.

* Fixed: under some circumstances, writes by a VMM to VCPU timer registers could have been reverted by the kernel to
  their previous state. This was triggered when:

  * a VCPU thread was running,
  * the VCPU was then disabled but remained active by switching to a non-VCPU thread,
  * that VCPU thread had the VCPU cap and performed the timer register writes,
  * and execution then switched back to the VCPU thread.

  This was found by Alison Felizzi and independently by Ryan Barry during the integrity proofs for AArch64 hyp mode.

* Fixed: under some circumstances, `seL4_VCPUReg_CPACR` is saved twice to the current VCPU. The value of this
  register may change between saves, causing the latter save to unintentionally grant EL0/1 access to the FPU.

  1. A thread with an active current VCPU switches to a thread without a VCPU. The current VCPU is disabled.
    1.1. `seL4_VCPUReg_CPACR` is saved to the current VCPU.
    1.2. `enableFpuEL01` updates the register, enabling FPU access in EL0 and EL1.
  2. The thread without a VCPU switches to a thread with a different VCPU to the first
    2.1. All registers from `seL4_VCPUReg_TTBR0` to `seL4_VCPUReg_SPSR_EL1` are saved. This range includes
         `seL4_VCPUReg_CPACR`, which overwrites the previously saved value and grants FPU access at EL0 and EL1.

* Fixed: on aarch32 configurations with hypervisor support, `CNTKCTL` was not saved and restored alongside other virtual
  timer registers. `seL4_VCPUReg_CNTKCTL` has been introduced to mirror `seL4_VCPUReg_CNTKCTL_EL1` from aarch64.

#### X86

* FPU: Fixed XSAVES option. Now it's possible to enable this for newer x86 CPUs.
* Avoid use-after-free if a VCPU with active FPU state gets deleted.

### Upgrade Notes

Set `seL4_TCBFlag_fpuDisabled` for tasks not using the FPU to retain the old context switch performance.
The -mgeneral-regs-only option may be needed to stop the compiler from issuing SIMD instructions to speed
up integer operations.

---

## 13.0.0 2024-07-01: BREAKING

### Security-relevant Changes

* Fixed a kernel-crashing NULL pointer dereference when injecting an IRQ for a non-associated VCPU on SMP
  configurations. This can be triggered from user-level by any thread that has access to or can create non-associated
  VCPU objects. While HYP+SMP is not a verified configuration and is not thoroughly tested, it is generally assumed to
  be working. If you are using this configuration, it is strongly recommended to upgrade.

  * Affected configurations: only unverified HYP+SMP configurations on Arm platforms are affected.
  * Affected versions: seL4 versions 12.0.0 and 12.1.0.
  * Exploitability: Any thread that can create or that has access to an unassociated VCPU can cause the crash. In static
    systems, only the system initialiser thread can create VCPUs and the standard capDL system initialiser will not
    trigger the issue. VMMs could have the authority to dissociate an existing VCPU from a TCB if they have both
    capabilities. That is, a malicious VMM could cause a crash, but generally VMMs are trusted, albeit not verified
    code. Guest VMs generally do not have sufficient authority to exploit this vulnerability.
  * Severity: Critical. This crashes the entire system.

* Fixed a kernel-crashing cache maintenance operation on AArch64 (Armv8). On AArch64, when seL4 runs in EL1 the kernel
  would fault with a data abort in `seL4_ARM_Page_Invalidate_Data` and `seL4_ARM_VSpace_Invalidate_Data` when the user
  requested a `dc ivac` cache maintenance operation on a page that is not mapped writeable. If you are using seL4 in EL1
  on AArch64, it is strongly recommended to upgrade.

  * Affected configurations: unverified AArch64 configurations of seL4 with hypervisor extensions off (kernel runs in
    EL1). AArch32 configurations and configurations where seL4 runs in EL2 are not affected.
  * Affected versions: all previous versions since 5.0.0
  * Exploitability: Any thread that has a VSpace capability or page capability to a page that is not mapped writable can
    cause the data abort. Most Microkit and CAmkES systems do not give their component access to these capabilities, but any component with Untyped capabilities could create threads with enough capabilities to trigger the issue.
  * Severity: Critical. This crashes the system.

* Fixed a cache issue on Arm where cleared memory was not flushed to RAM, but only to the point of unification. This
  means that uncached access was able to still see old memory content.

  * Affected configurations: Arm platforms that distinguish flushing to PoU from flushing to RAM
  * Affected versions: all previous versions since 4.0.0
  * Exploitability: Low. The issue is trivially observable by mapping the same frame as cached and uncached.
    However, it is unlikely to be exploitable in a real system, because re-using memory over security boundaries
    is already excluded, so information leakage happens only within the same domain.
  * Severity: Medium. It breaks functional correctness in the sense that a cleared frame may not yet be cleared when
    viewed as uncached. It does not break any functional kernel behaviour.

### Platforms

* Added support for the ARM Cortex A55
* Added support for the imx8mp-evk platform
* Added support for additional RPI4 variants
* Added support for the Odroid C4
* Added support for the Avnet MaaXBoard
* Added support for arm_hyp on qemu-arm-virt platfrom with cortex-a15 CPU
* Added support for qemu-riscv-virt
* Added support for the Pine64 Star64
* Added support for the TQMa8XQP 1GiB module
* Remove imx31/kzm platform support. This platform is being removed as it is sufficiently old and unused.
* Remove ARM1136JF_S and ARMv6 support. This architecture version is being removed as it is sufficiently old and
  unused. See [RFC-8](https://sel4.github.io/rfcs/implemented/0080-remove-armv6-support.html).
* Remove ARMv6 specific configs: `KernelGlobalsFrame` and `KernelDangerousCodeInjectionOnUndefInstr`. This removes the
  constant `seL4_GlobalsFrame` from libsel4 as well as the IPC buffer in GlobalsFrame caveat from CAVEATS.md
* rpi3+rpi4: Mark first memory page as reserved

#### Arm

* Enabled access to `seL4_VCPUReg_VMPIDR` and `seL4_VCPUReg_VMPIDR_EL2` for all hypervisor configurations. Previously
  this register was only accessible for SMP kernel configurations. Non-SMP configurations can still require access when
  wanting to control the value of `MPIDR` that the guest reads. Note that the initial value for new seL4_ARM_VCPUs for
  this register is 0 which isn't a legal value for `MPIDR_EL1` on AArch64. It may be necessary for the register to be
  explicitly initialized by user level before launching a thread associated with the new seL4_ARM_VCPU.
* Allow changing the VCPU of active thread: call `vcpu_switch` in `associateVCPUTCB`. This guarantees that the correct
  VCPU will be activated when the kernel finishes execution. Previously, changing the VCPU of the current thread would
  result in no VCPU being active.
* benchmarking: use write-through kernel log buffer
* arm_hyp: Access `SPSR` via non-banked instructions
* `generic_timer`: force timer to de-assert IRQ.
* No special handling for edge-triggered IRQs.
  -  Clearing the pending state only has an effect if the IRQ state is active-and-pending, which happens for
    edge-triggered interrupts if another edge happens on the IRQ line for the currently active interrupt. This window is
    small enough to ignore, at worst user space will get another notification, which is harmless.

    If unnecessary notifications are unwanted, the pending state should be cleared during `seL4_IRQHandler_Ack()`, as
    that covers a much bigger window. However, edge-triggered interrupts are not expected to happen often. Making all
    interrupt handling slightly faster and the code simpler is the better trade-off.
* Make write-only mapping message consistent. There is a warning when creating a write-only mapping on AArch32/AArch64.
  This message is now the same in all variants.

##### AArch32

* Implement `KernelArmExportPTMRUser` and `KernelArmExportVTMRUser` options for Arm generic timer use access on AArch32.
* AArch32 VM fault messages now deliver original (untranslated) faulting IP in a hypervisor context, matching
  AArch64 behaviour.
* Fix single stepping on ARMv7
* TLB: only perform TLB lockdown for Cortex A8.
  - The code previously used the same instructions for Cortex A8 and A9, but the Cortex A8 instructions are undocumented
    for A9, and A9 provides a slightly different TLB interface. As far as we can tell, the instructions were simply
    ignored by the supported A8 platforms, so there was no current correctness issue. Since the instructions had no
    effect, we removed A9 TLB lockdown support. This potential issue was discovered and reported by the UK's National
    Cyber Security Centre (NCSC).
* TLB: guard TLB lockdown count.
  - `lockTLBEntry` uses the global `tlbLockCount` as input without checking bounds. This is fine, because the function
    is called at most 2 times per core, but this is only apparent when checking the entire possible calling context.
    Make this bound obvious locally by doing nothing if the function is called with values of `tlbLockCount` of 2 or
    greater. This is safe, because TLB lockdown is a performance change only. Also add an assert for debug mode, because
    we want to know if calling context ever changes. This potential issue was reported by The UK's National Cyber
    Security Centre (NCSC).

##### AArch64

* Add `AARCH64_verified.cmake` config for functional correctness on AArch64
* Rename libsel4 config option `AARCH64_VSPACE_S2_START_L1` to `CONFIG_AARCH64_VSPACE_S2_START_L1` to be namespace
  compliant.
* Added SMC Capability (`smc_cap`) and SMC forwarding for AArch64 platforms. See
  [RFC-9](https://sel4.github.io/rfcs/implemented/0090-smc-cap.html).
* Remove VSpace object types in AArch64: `seL4_ARM_PageDirectory` and `seL4_ARM_PageUpperDirectory`. See also the
  corresponding [RFC](https://sel4.github.io/rfcs/implemented/0100-refactor-aarch64-vspace.html). The functionality
  previously provided by these types will be provided by the existing `seL4_ARM_PageTable` object type. This allows for
  a simpler API and enables a smaller kernel implementation that will be easier to verify. libsel4 provides a source
  compatibility translation that maps the old libsel4 names and constants the new ones in
  `<sel4/sel4_arch/deprecated.h>`. There are some exceptional cases where kernel behavior has changed:
  - A Page directory and page table are now the same kind of object and can be mapped at any page table level.
    If the lookup for the provided address stops at a slot that can map a page directory, it will map the object as a
    page directory. If there already is a page directory mapped, the lookup will proceed to the next level and it will
    map as a page table instead of returning an error.
* Removed user address space reserved slots restriction on 40bit PA platforms when KernelArmHypervisorSupport is set.
  This change is reflected in the definition of the seL4_UserTop constant that holds the largest user virtual address.
* Added support for GICv3 virtualization, tested on iMX8QXP
* Implemented a signal fastpath on AArch64. Must be enabled explicitly with the `KernelSignalFastpath` config option.
* Implemented a virtual memory fault fastpath on AArch64. Must be enabled explicitly with the `KernelExceptionFastpath` config option.
* Add option `KernelAArch64UserCacheEnable` for user cache maintenance.
  - Enables user level access to `DC CVAU`, `DC CIVAC`, `DC CVAC`, and `IC IVAU` which are cache maintenance operations
    for the data caches and instruction caches underlying Normal memory and also access to the read-only cache-type
    register `CTR_EL0` that provides cache type information. The ArmV8-A architecture allows access from EL0 as fast
    cache maintenance operations improves DMA performance in user-level device drivers.

    These instructions are a subset of the available cache maintenance instructions as they can only address lines by
    virtual address (VA). They also require that the VA provided refers to a valid mapping with at least read
    permissions. This corresponds to lines that the EL0 could already affect via regular operation and so it's not
    expected to break any cache-partitioning scheme.

    The config option allows this policy to be selected for a particular kernel configuration, but it is default enabled
    as this has been the existing behavior for current aarch64,hyp configurations and have not been explicitly disabled
    in non-hyp configurations.
* make error reporting consistent: Report the VSpace cap (1) as invalid, instead of the frame cap (0) in
  `ARMFrameInvocation` to stay consistent with the other architectures.
* vcpu: only trap WFx instructions from VCPUs. When `KernelArmDisableWFIWFETraps` is disabled (trapping of WFI/WFE is
  enabled), the kernel traps WFx instructions from both native and vCPU threads. This change brings the code in line
  with the config description.

#### RISC-V

* Remove the ability for user-space on RISC-V platforms to access the core-local interrupt controller (CLINT). The
  CLINT contains memory-mapped registers that the kernel depends on for timer interrupts and hence should not be
  accessible by user-space.
* Add configuration option `KernelRiscvUseClintMtime` for faster access of hardware timestamp on RISC-V platforms.
  The configuration option is not enabled by default as it requires access to the CLINT which depends on the platform
  and whether the M-mode firmware allows S-mode to access the CLINT. For example, newer versions of OpenSBI (1.0 and above)
  do not allow direct access of the CLINT.
* Rename object interface files `include/interfaces/sel4.xml`, `arch_include/*/interfaces/sel4arch.xml`, and
  `sel4_arch_include/*/interfaces/sel4arch.xml` to `include/interfaces/object-api.xml`,
  `arch_include/*/interfaces/object-api-arch.xml`, and `sel4_arch_include/*/interfaces/object-api-sel4-arch.xml`,
  respectively.
* Improve PLIC driver API and documentation
* Fix `getMaxUsToTicks` function tor return time instead of ticks.
* Improve RISC-V PTE compliance: keep D, A, and U bits cleared.

#### Intel

* libsel4: add enum for EPT attributes.
* VTX: fix EPT cache attribute setting. Previously the only effectively possible value was `EPTWriteBack`.
* Add kernel support for 64-bit VMs
* Provide `CONFIG_X86_64_VTX_64BIT_GUESTS` for 64-bit VM support
* Only access real IOAPIC registers. IOAPICS can have varying numbers of lines attached. The actual number can be
  accessed in the top 16 bits of the version register. Rather than assuming fixed 24 lines per IRQ, read the actual
  number and use that.

#### MCS

* Rename `seL4_TimeoutMsg` to `seL4_Timeout_Msg` to make it consistent with the naming of other messages.
* Correct the minimum size of a scheduling context. This changes the value of `seL4_MinSchedContextBits`.
* Correct check for message length in `SchedControl_ConfigureFlags`
* Allow lazy SchedContext rebind. Before, binding a scheduling context to a TCB was not allowed if the SC was bound to a
  notification object. Also, binding an SC to a notification was not allowed if that scheduling context was already
  bound to a TCB. Without these restriction it is much easier to move scheduling contexts around: In effect having a SC
  bound on both the TCB and a notification acts as if the thread is running on a donated SC which will be returned when
  the tasks calls `Recv`/`Wait`, which is done by `maybeReturnSchedContext()`.
* Only charge budgets for non-idle thread SCs
* ARM+MCS: Introduce `TIMER_OVERHEAD_TICKS`. For ARM currently `TIMER_PRECISION` exists, but that is in microseconds and
  not fine-grained enough. `TIMER_OVERHEAD_TICKS` is needed to make periodic tasks synchronous with the system clock. If
  this value is non-zero every period will be extended with the overhead of taking an interrupt and reading the system
  clock. To avoid this drift, the configured value should be set to at least the average overhead.
* SMP: Do not use cross-node `ksCurTime` assuming they are in sync (which they are not), instead use
  `NODE_STATE(ksCurTime)`.
* SMP: Add clock synchronisation test on boot
* SMP: Fix scheduling context use-after-free

#### Other Changes

* boot: Introduce `seL4_BootInfoFrameSize` and `seL4_BootInfoFrameBits` for user land so there is no longer a need to
  hard-code a 4 KiByte assumption. Remove `BI_FRAME_SIZE_BITS`.
* Fix: Don't clobber msgInfo register in `PageGetAddress`, `ASIDControlInvocation`, `ARMCBInvocation`,
  `A32PageDirectoryGetStatusBits`, `X86PortIn`, `WriteVMCS`, `ReadVMCS`, `ConfigureSingleStepping`, and `GetBreakpoint`.
* `libsel4`: Make `bootinfo` consistent. Some slot positions in the rootnode would depend on configuration. However that
  makes it difficult to add new root caps, especially if multiple caps only exist based on configuration. Make all caps
  always there, but null if not configured.
* `libsel4`: Eliminate unnamed enums
* Improved consistency and completeness of user manual
* Added manual for bitfield generator
* `bitfield_gen`: allow non-contiguous tag fields. A tagged union can now optionally use multiple fields to indicate the
  tag. These are called "sliced" tags in the code. The tag fields have to be at the same position and width in each
  block of the tagged unions, and all tag fields have to be within the same word. See the manual for details.
* Make `CONFIG_PRINTING` and `CONFIG_DEBUG_BUILD` usable independently from each other
* Overall debug printing improvements
* Fix invisible chars due to ANSI escape codes. On terminals with black background some debug output was invisible due
  to black foreground colour. Use bold instead.
* Rename libsel4 config option `ENABLE_SMP_SUPPORT` to `CONFIG_ENABLE_SMP_SUPPORT` to be namespace compliant.
* Removed obsolete define `HAVE_AUTOCONF`
* Remove userError from `seL4_ReplyRecv` path, because it too often incorrectly warns about legitimate operations
* Update GDB macros
* Add support for `cmake --install <dir>` for final build and config artefacts
* `cmake`: support supplying custom device trees overrides
* `cmake`: provide `gen_config.json` with kernel config settings
* Provide `platform_gen.json` in addition to `platform_gen.yaml`
* Allow build with GNU `binutils` >= 2.38
* Allow compilation with clang 12
* `cmake`: detect x86 cross-compiler for Arm host (e.g. on Apple M1)
* Consistently use `/usr/bin/env` for bash/sh invocations
* Require Python 3 consistently everywhere
* Improved support for compiling on MacOS
* General minor build system improvements and clean-up
* Set up automated tests for CI and GitHub pull requests on seL4
* Add vulnerability disclosure policy

### Upgrade Notes

* The change in `seL4_MinSchedContextBits` can lead to failure where code previously created
  scheduling contexts with size `seL4_MinSchedContextBits` and expected more than the 2 minimum
  refills to be available for that size. Either use a larger size (the previous value 8 of
  `seL4_MinSchedContextBits`) in retyping, or request fewer refills.

---

## 12.1.0 2021-06-10: SOURCE COMPATIBLE

### Changes

* Moved kernel configuration header to libsel4.
* Improved benchmarking:
  - Made the kernel log buffer to be derived from cmake config.
  - Added x86_64 kernel log buffer.
  - Implemented RISC-V benchmark timestamping.
  - Implemented benchmark log buffer for RISC-V.
* Moved cap functions out of inline to make changing cap bitfields less noisy.
* Removed weak definition of the __sel4_ipc_buffer variable which was causing large thread local storages to be
  required.
* Prepared the bitfield generator for Isabelle 2021.
* Made a number of improvements to the CMake build scripts.
* Added pre-processor 'include guards' for auto-generated files.
* Added missing CONFIG_PLAT_IMX7 pre-processor '#define's.
* Added `#pragma once` to the autoconf headers.
* Removed `HAVE_AUTOCONF` guards in `sel4/config.h`.
* Improved the manual:
  - Corrected descriptions of CNode addressing.
  - Documented initial thread's SMMU caps.
* Improved libsel4:
  - Removed redundant `HAVE_AUTOCONF` header guards in libsel4.
  - Added missing `macros.h` #include in libsel4.
  - Cleaned-up `macros.h` in libsel4.
  - Added checks to use `_Static_assert()` in libsel4 if it is available.
  - Unified definitions in `simple_types.h` in libsel4.
  - Added `printf` format specifier `PRI_sel4_word` for printing word types.
  - Unified seL4 type definitions.
* Added specific `printf` formatting for seL4_Word.
* Changed some variables to use `BOOT_BSS` instead of `BOOT_DATA` to save space in the ELF file.
* Replaced the `capDL()` function with a generic `debug_capDL` function that is intended to be implemented by all
  architectures.
* Reduced `printf`s stack usage.
* Fixed `ksnprintf()` corner case handling.
* Fixed NULL `printf` output wrapper handling.
* Cleaned up the printing API implementation.
* Changed code to pass buffer to `printf` output channel.
* Refactored the kernel console handling.
* Added support for `PRIu64` and `SEL4_PRIu_word` in the kernel.
* Changed various `printf` conversion specifiers to use `SEL4_PRIx_word` specifiers.

#### MCS

* Fixed a physical counter access issue on MCS on EL2.
* Added MCS support for the ZynqMP.
* Changed invokeSchedControl_Configure to always produce a scheduling context that is active and has configured
  refills.
* Prevented the binding of scheduling contexts to blocked TCBs.
* Fixed conversions of ticks to microseconds on aarch64.
* Added an additional sporadic flag to `seL4_SchedControl_Configure` which allows the option to create a sporadic
  scheduling context.
* Added explicit checks to not unblock the current scheduling context.
* Fixed MCS and aarch64 VCPU interrupt interaction.
* Renamed MCS kernel configuration option `KernelStaticMaxBudgetUs` to `KernelStaticMaxPeriodUs`.
* Added check to make sure that the current thread will not yield to multiple threads.
* Added check to account for an inactive scheduling context at preemption.
* Deferred charging time budget in a preempted invocation.
* Added code to update `ksDomainTime` in `updateTimestamp`.
* Added call to `updateTimestamp` in a preemption point.
* Added code to clear ksConsume when charging time to a revoked scheduling context.
* Added code to cancel IPC when finalising reply caps.
* Fixed a dereference of a scheduling context after it's removed from the associated TCB.
* Added MCS to the preprocess check.

#### x86

* Removed a redundant de-reference for `seL4_X86DangerousRDMSR` in ia32.
* Added a config option to set the frequency of the TSC.
* Optimized the boot image size for x86_64.
* Removed the PT_PHDR segment from the linker script to work around an issue in a variant of syslinux that treats a
  PT_PHDR segment as distinct from a PT_LOAD segment.

#### Arm

* FPU ownership is now also given away on thread deletion instead of only on FPU exception.
* Added basic build support for A35 core.
* Fixed read/write of the VCPU CPACR register.
* Fixed invalidation of the VIPI I-cache in hypervisor mode.
* Removed duplicate interrupts for the zynqmp in its DTS.
* Updated device definitions for the exynos5.
* Added Raspberry Pi 4 support.
* Updated Ethernet interrupts in the ZynqMP.
* Added i.MX6 Nitrogen6_SoloX support.
* Fixed CMake configurations for the ZynqMP and the Ultra96.
* Fixed the platform `#define` for the i.MX6 Nitrogen6_SoloX.
* Fixed I-cache invalidation on aarch64 SMP.
* Fixed the usage of KernelPaddrUserTop on Arm platforms.
* Added support for the i.MX low-power UART.
* Added an option to ignore SErrors which is enabled on default for the TX2.

#### RISC-V

* Added PLIC driver and updated the DTS for the Ariane.
* Merged the PLIC drivers for the Ariane and the Hifive.
* Updated default timer frequency for the Ariane.
* Map devices with large pages on 32 and 64-bit kernel.
* Replaced mentions of BBL with OpenSBI.
* Added definitions of the KernelOpenSBIPlatform variable for RISC-V platforms.
* Removed instances of passing `extra_caps_t` by value for binary verification purposes.
* Removed `slot_range_t` for binary verification purposes.
* Removed `DONT_TRANSLATE` tag on 'read_sip' for binary verification purposes.
* Added more efficient clz and ctz implementations to substitute the lack of machine instructions to count leading and
  trailing zeroes.
* Updated kernel bootstrap message to be the same as the one on Arm.
* Added some fastpath improvements for RISC-V.
* Added extra snippets of code to track kernel entries for RISC-V.
* Added a configuration guard for fastpath on RISC-V.
* Reorganised `traps.S` so that syscalls and fastpath checks were done after interrupts and exceptions checks to avoid
  exceptions being interpreted as null-syscalls.
* Added support for `riscv64-elf-` toolchain.
* Fixed a register bug in the assembly entry point for SMP with regards to the elfloader passing HART and core IDs.

### Upgrade Notes

* Scheduling contexts can now be configured as constant-bandwidth or sporadic server.
  - Constant bandwidth observes a continuous constant bandwidth of budget/period.
  - Sporadic server behaves as described by Sprunt et. al.
  - In an overcommitted system, sporadic preserves accumulated time.
* There are new `PRIx` and `SEL4_PRIx` `printf` conversion specifiers that can now be used inside the kernel.
* x86_64 kernel binaries are now smaller and may be structured differently compared to previous kernel binaries.
* Kernel entry benchmarking can now be done on RISC-V.
* AUTOCONF_INCLUDED is no longer defined. The seL4 build system has stopped
  using autoconf a long time ago and this define has been kept for compatibility
  since then. It is no longer used anywhere by now, so it can be removed.

---

## 12.0.0 2020-10-30: BREAKING

### Changes


* Update licensing to reflect project transfer to seL4 Foundation. SPDX tags are now used to identify the licenses for
  each file in the project. Generally, kernel-level code is licensed under GPLv2 and user-level code under the 2-clause
  BSD license.
* Update contribution guidelines:
  * the seL4 foundation requires DCO process instead of a CLA
* Functional correctness verification for the RISC-V 64-bit HiFive Unleashed platform configuration
  (RISCV64_verified.cmake with no fastpath or FPU enabled)
* Update caveats file:
  - The recycle operation has been removed
  - More detail on what versions are verified
  - Update comments on real time use of seL4
* Improve seL4 manual.
  - Fix aarch64 seL4_ARM_PageDirectory object API docs:
    seL4_ARM_PageDirectory_Map is passed a vspace cap not an upper page directory cap.
  - Increase documentation coverage for Arm object invocations
  - Rework introduction of system calls in Kernel Services and Objects chapter.
  - Improve discussion of Receive and Wait syscall behaviour between MCS and non-MCS systems.
  - Explicitly mention grantreply rights in the exceptions section.
  - Document schedcontext size_bits meaning.
  - Remove metion of system criticality.
  - Add SchedContext to object size discussion.
  - Fix initial thread's CNode guard size.
  - Update BootInfo struct table.
  - Update padding field in UntypedDesc table
* Update seL4_DebugSnapshot to provide a CapDL dump of the capability layout of
  a running system for Arm, x86_64 and riscv32 configurations.
* KernelBenchmarksTrackUtilisation:
  - Add feature support for SMP configurations
  - For each thread also track number of times scheduled, number of kernel entries and amount of cycles spent inside the
    kernel and also add core-wide totals for each.
* Add 2 new benchmark utilization syscalls
  - seL4_BenchmarkDumpAllThreadsUtilisation: Prints a JSON formatted record of total and per-thread utilisation
    statistics about the system. This currently includes a thread's total cycles scheduled, total number of times
    scheduled, total cycles spent in the kernel and total number of times entering the kernel and then totals of each
    for all threads on the current core.
  - seL4_BenchmarkResetAllThreadsUtilisation: Resets the current counts of every user thread on the current core.
* Added seL4_DebugPutString libsel4 function for printing a null-terminated string via calling seL4_DebugPutChar().
* Introduced a new config flag, KernelInvocationReportErrorIPC, to enable userError format strings to be written to
  the IPC buffer. Another config bool has been introduced to toggle printing the error out and this can also be set at
  runtime. LibSel4PrintInvocationErrors is a libsel4 config used to print any kernel error messages reported in the IPC
  buffer.
* Repair barriers in clh_lock_acquire (SMP kernel lock). Strengthen the clh_lock_acquire to use release on the
  atomic_exchange that makes the node public. Otherwise, (on ARM & RISCV) the store to the node value which sets its
  state to CLHState_Pending can become visible some time after the node is visible. In that window of time, the next
  thread which attempts to acquire the lock will still see the old state (CLHState_Granted) and enters the critical
  section, leading to a mutual exclusion violation.
* Replace all #ifdef header guards with #pragma once directives in libsel4 header files
* gcc.cmake:
  - Add option for coloured gcc output. Setting GCC_COLORS in the environment will result in -fdiagnostics-color=always
    being provided to gcc. Ordinarily gcc would suppress coloured output when ninja redirects its stderr during normal
    builds.
  - Remember CROSS_COMPILER_PREFIX across CMake invocations. The variable would become unset in certain contexts.
  - Add support for Arm cross-compilers on Red Hat distros.
* Fastpath optimisation:
  - Reorganise the code layout on Arm.
  - bitfield_gen: explicit branch predictions.
  - Optimize instruction cache access for fastpath.
* Extend Clang support to all kernel configurations. Support targets LLVM versions between 9 and 11.
* hardware_gen.py: Add elfloader output target for hardware_gen script. This generates header files describing the
  platform's CPU configuration as well as device information such as compatibility strings and memory regions to the
  elfloader that are consistent with the kernel's own definitions.
* Fix bootinfo allocation bug when user image pushed against page directory boundary. The bootinfo is mapped in at the
  end of the user image in the initial thread's vspace. The kernel initialisation code wasn't calculating the
  bootinfo size correctly which could lead to a kernel fault when trying to map the bootinfo in when the parent page
  table object hadn't been allocated.
* Use autoconf definition for `RetypeMaxObjects` in <sel4/types.h>. This ensures that the definition stays consistent
  with what the kernel is configured with.
* Fix up timers and clock frequencies
  - Remove beaglebone kernel timer prescaling. Previously the timer frequency was incorrectly set to to half (12MHz) its
    configured frequency (24MHz).
  - Set TX1 kernel timer frequency config to 12MHz and not 19.2MHz as this is the standard frequency of the input clock
    source (m_clock).
  - Set KZM kernel timer frequency config to 35MHz and not 18.6MHz based on sampling the timer frequency.
  - imx31: add missing dts entry for the epit2 timer.
  - Set non-mcs i.MX6 kernel timer frequency config to 498MHz and not 400MHz as this is based on the frequency of the
    input clock source (PLL1)
  - Zynq7000: Set kernel timer frequency to 320MHz.
  - Qemu-arm-virt: Set kernel timer frequency to 6.25MHz.
* Do not generate data symbols for enums in libsel4 as they end up as bss symbols and cause linker errors on newer
  compiler versions.
* Update padding field definition in seL4_UntypedDesc to make the struct word aligned. Previously, this struct wasn't
  correctly word aligned on 64-bit platforms. This change removes the padding1 and padding2 fields and replaces them
  with a padding field that is a variable number of bytes depending on the platform.
* Add GitHub actions scripts. These scripts replicate internal CI checks directly on GitHub

#### MCS

* Stop scheduling contexts from being bound to tcb's that already have scheduling contexts.
* Fix x86 `KERNEL_TIMER_IRQ` definition. Previously, MCS preemption point handling would check the wrong interrupt on
  x86 platforms.
* smp: tcb affinity modification bug. When changing the affinity of a thread on a remote core, the reschedule
  operation wasn't being performed.
* Allow `replyGrant` for fault handlers. The MCS kernel so far insisted on full grant rights for fault handler caps,
  but replyGrant is sufficient and consistent with the default kernel config.
* All scheduling contexts compare their time with time in assigned core instead of currently executing core.
* Prevent recursion on timeout faults by suspending a passive server that receives a timeout fault.
* Add KernelStaticMaxBudgetUs to bound the time the user provides to configure scheduling contexts to avoid malicious
  or erroneous overflows of the scheduling math. Make the default max period/budget 1 hour.
* rockpro64: enable mcs configurations

#### Arm

* arm: Add seL4_BenchmarkFlushL1Caches syscall to manually flush L1 caches in benchmark configurations.
* New fault type when running in Arm hypervisor mode: seL4_Fault_VPPIEvent
  - The kernel can keep track of IRQ state for each VCPU for a reduced set of PPI IRQs and deliver IRQ events as
    VCPU faults for these interrupt numbers.
  - Additionally a new VCPU invocation is introduced: seL4_ARM_VCPU_AckVPPI.
    This is used to acknowledge a virtual PPI that was delivered as a fault.
* Virtualise Arm Timer and VTimer interrupts to support sharing across VCPUs.
  - A VCPU will now save and restore VTimer registers for the generic timer and also deliver a VTimer IRQ via a
    seL4_Fault_VPPIEvent fault. This enables multiple VCPUs bound to the same physical core to share this device.
* Build config option for whether WFE/WFI traps on VCPUs when running in Arm hypervisor mode
* Arm: Add VMPIDR and VMPIDR_EL2 registers to VCPU objects for programming a VCPU's 'Virtualization Multiprocessor
  ID Register' on aarch32 and aarch64.
* Arm, vcpu, smp: Remote IPI call support for VIRQS. Injecting a VIRQ into a vcpu running on a different core will
  IPI the remote core to perform the IRQ injection.
* zynqmp: Disable hardware debug APIs as the platform doesn't support kernel hardware debug API.
* zynqmp: Add support for aarch32 hyp
* Gicv3: include cluster id when sending ipis.
* qemu-arm-virt:
  - Generate platform dtb based on KernelMaxNumNodes config value.
  - Reserve the first 512MiB of Ram as device untyped for use in virtualization configurations.

##### Aarch32

* Moved TPIDRURO (PL0 Read-Only Thread ID register) to TCB register context from VCPU registers. This means
  changes to this register from user level have to go via seL4_TCB_Write Registers instead of seL4_ARM_VCPU_WriteRegs.
* aarch32: Restrict cache flush operation address range in hyp mode. It's required that cache flushing in hyp mode
  is performed through the kernel window mapping as the kernel is unable to flush addresses outside of this mapping
  without causing an access fault.
* arm_hyp: Move PGD definitions out of libsel4 as they don't correspond to any public interfaces and are only used
  internally by the kernel to manage its own address space.

##### Aarch64

* aarch64: Fix a bug where saving ELR_EL1 when managing a VCPU object was reading from ELR_EL1 instead of writing to it.
* aarch64: Fix a bug where saving FAR_EL1 when managing a VCPU object was only writing to the low 32 bits of the 64-bit
  FAR_EL1 register.
* aarch64: Add missing faults to seL4_getArchFault. seL4_getArchFault is a libsel4 helper that constructs fault messages
  out of the IPC buffer message registers but it wasn't aware of all possible fault types.
* aarch64,vcpu: Add CNTKCTL_EL1 register to `vcpu_t`. This register tracks timer delegation to EL0 from EL1 and
  needs to be switched for different VCPUs.
* aarch64: Adds missing vcpu cases for some aarch64-specific functions on capabilities.
* cortex-a53,hyp: Reduce seL4_UserTop when on a cortex-a53 platform and KernelArmHypervisorSupport is set.
  - This is because the kernel uses the last slot in the top level VSpace object for storing the assigned VMID and so
    any addresses that are addressed by the final slot are not accessible. This would apply to any CPU that have 40bit
    stage 2 translation input address.
* Arm SMMUv2 kernel API and TX2 smmuv2 driver. This supports using an SMMU to provide address translation and memory
  protection for device memory access to platforms that implement a compatible Arm SMMUv2 System mmu. The kernel
  implementation supports using an SMMU to restrict memory access of VM guest pass-through devices, or for isolating
  devices and their drivers' memory accesses to the rest of a running system.

#### x86

* Fix printf typo in `apic_init`.
* x86_64: Fix PCID feature constant to use the correct bit.
* Fix interrupt flag reset upon nested interrupt resume, `c_nested_interrupt`. This fixes an issue where ia32 kernels
  would crash if receiving a nested interrupt.

#### RISC-V

* Functional correctness of seL4/RISCV now formally verified at the C level.
* Hifive: Enable seL4_IRQControl_GetTrigger object method. This method allocates an IRQ handler by ID and whether it is
  level or edge triggered. Note: HiFive PLIC interrupts are all positive-level triggered.
* Add search for additonal gcc riscv toolchains if the first one cannot be found.
* Add support for rocketchip soc. Support Rocketchip SoC maps to Xilinx ZC706 board and ZCU102 board
* Add support for polarfire soc.
* Clear reservation state on slowpath exit as the RISC-V ISA manual requires supervisor code to execute a dummy sc
  instruction to clear reservations "during a preemptive context switch".
* Pass DTB through to userlevel in extra bootinfo fields similar to on Arm.
* Use full width of scause to prevent large exception numbers to be misinterpreted as syscalls.
* Fix page map bug.
  - Previously, it was possible in decodeRISCVFrameInvocation for the rwx rights of the new PTE to become 000 after
    masking with cap rights. This would turn the frame PTE into a page table PTE instead, and allow the user to create
    almost arbitrary mappings, including to kernel data and code. The defect was discovered in the C verification of the
    RISC-V port.
* Remove seL4_UserException_FLAGS. This field was unused and was never set to anything by the kernel.
* Add FPU config options for RISCV
  - Two options, KernelRiscvExtD and KernelRiscvExtF, are added to represent the D and F floating-point extensions.
    KernelHaveFPU is enabled when the floating-point extensions are enabled. The compiler ABI will also be changed to
    lp64d for hardfloat builds.
* Add RISCV64_MCS_verified.cmake config for in-progress MCS verification
* riscv32: Remove incorrectly provided constants for 512MiB 'huge pages' which is not part of the specification.
* riscv: Lower .boot alignment to 4KiB. This makes the final kernel image more compact.

### Upgrade Notes

* The project's licensing updates don't change the general licensing availability of the sources.
  More information can be found: <https://github.com/seL4/seL4/blob/master/LICENSE.md>
* Any references to the padding1 or padding2 fields in seL4_UntypedDesc require updating to
  the new padding field. It is expected that these fields are unused.
* Any platforms that have had changed kernel timer frequencies will see different scheduling
  behavior as kernel timer ticks will occur at different times as kernel ticks are configured
  in microseconds but then converted to timer ticks via the timer frequency. Any time-sensitive
  programs may need to be re-calibrated.
* Any riscv32 programs that were using the constants RISCVGigaPageBits or seL4_HugePageBits will
  see a compilation error as the constants have been deleted as they aren't supported in the riscv32 spec.
* Any riscv programs that refer to the seL4_UserException_FLAGS field will need to remove this reference. This field was
  never initialised by the kernel previously and has now been removed.
* Any riscv programs using the LR/SC atomics instructions will see reservations invalidated after kernel entries. It is
  expected that this will not require any changes as reservations becoming invalid is normal behavior.
* On cortex-a53 platforms when KernelArmHypervisorSupport is set have 1 less GiB of virtual memory addresses available
  or 2GiB less on TX2 with the SMMU feature enabled. This is captured by the change in definition of the seL4_UserTop
  constant that holds the larges virtual address accessible by user level.
* On aarch32 the TPIDRURO register(PL0 Read-Only Thread ID register) has been removed from the VCPU object and added to
  the TCB object. A VCPU is typically bound to a TCB so after updating the access, a thread with a VCPU attached will
  still support having a TPIDRURO managed.
* On Arm hypervisor configurations, PPI virtual timer interrupts are now delivered via seL4_Fault_VPPIEvent faults and
  it is not possible to allocate an interrupt handler for these interrupts using the normal interrupt APIs. A VPPI
  interrupt is received via receiving a fault message on a VCPU fault handler, and acknowledged by an invocation on
  the VCPU object. VPPI interrupts that target a particular VCPU can only be generated while the VCPU thread is
  executing.

---

## 11.0.0 2019-11-19: BREAKING

### Changes

* Add GrantReply access right for endpoint capabilities.
  - seL4_Call is permitted on endpoints with either the Grant or the GrantReply access rights.
  - Capabilities can only be transferred in a reply message if receiver's endpoint capability has the Grant right.
* `seL4_CapRights_new` now takes 4 parameters
* seL4_CapRightsBits added to libsel4. seL4_CapRightsBits is the number of bits to encode seL4_CapRights.
* `seL4_UserTop` added
  - a new constant in libsel4 that contains the first virtual address unavailable to
    user level.
* Add Kernel log buffer to aarch64
* Support added for Aarch64 hypervisor mode (EL2) for Nvidia TX1 and TX2. This is not verified.
* Support for generating ARM machine header files (memory regions and interrupts) based on a device tree.
* Support added for ARM kernel serial driver to be linked in at build time based on the device tree compatibility string.
* Support added for compiling verified configurations of the kernel with Clang 7.
* RISC-V: handle all faults
  - Pass all non-VM faults as user exceptions.
* arm-hyp: pass ESR in handleUserLevelFault
* aarch64: return ESR as part of user level fault
* Created new seL4_nbASIDPoolsBits constant to keep track of max nb of ASID pools.
* Support added for Hardkernel ODROID-C2.
* Added extended bootinfo header for device tree (SEL4_BOOTINFO_HEADER_FDT).
* Support added for passing a device tree from the bootloader to the root task on ARM.
* Add seL4_VSpaceBits, the size of the top level page table.
* The root cnode size is now a minimum of 4K.
* Hifive board support and RISC-V external interrupt support via a PLIC driver.
* Update seL4_FaultType size to 4bits.
* Fix seL4_MappingFailedLookupLevel() for EPTs on x86.
  - add SEL4_MAPING_LOOKUP_NO_[EPTPDPT, EPTPD, EPTPT] which now correspond to
    the value returned by seL4_MappingFailedLookupLevel on X86 EPT mapping calls.
* BeagleBone Black renamed from am335x to am335x-boneblack.
* Supported added for BeagleBone Blue (am335x-boneblue).
* Remove IPC Buffer register in user space on all platforms
* Add managed TLS register for all platforms
* Add configurable system call allowing userspace to set TLS register without capability on all platforms.
* Non-hyp support added for Arm GICv3 interrupt controller.
* Add initial support for i.MX8M boards.
  - Support for i.MX8M Quad evk AArch64 EL2 and EL1, AArch32 smode only is accessible via the imx8mq-evk platform.
  - Support for i.MX8M Mini evk AArch64 EL2 and EL1, AArch32 smode only is accessible via the imx8mm-evk platform.
* Add FVP platform with fixed configuration. This currently assumes A57 configuration described in tools/dts/fvp.dts.
* Arm SMP invocation IRQControl_GetTriggerCore added
  - Used to route a specify which core an IRQ should be delivered on.
* Kernel log buffer: Specify on which core an IRQ was delivered.
* Add new seL4_DebugSendIPI syscall to send arbitrary SGIs on ARM when SMP and DEBUG_BUILD are activated.
* Support for aarch64-hyp configurations with 40-bit physical addresses (PA) added.
  - The aarch64 api now refers to VSpaces rather than PageGlobalDirectories,
    as depending on the PA the top level translation structure can change.
  - all `seL4_ARM_PageGlobalDirectory` invocations are now `seL4_ARM_VSpace` invocations.
  - new constants 'seL4_ARM_VSpaceObject` and `seL4_VSpaceIndexBits`.
* Merged MCS kernel feature.
  - this is not verified and is under active verification.
  - The goals of the MCS kernel is to provide strong temporal isolation and a basis for reasoning about time.
* Moved aarch64 kernel window
  - aarch64 kernel window is now placed at 0, meaning the kernel can access memory
    below where the kernel image is mapped.
* aarch64: Moved TPIDRRO_EL0 (EL0 Read-Only Thread ID register) to TCB register context from VCPU registers. This means
  changes to this register from user level have to go via seL4_TCB_Write Registers instead of seL4_ARM_VCPU_WriteRegs.
* Merge ARCH_Page_Remap functionality into ARCH_Page_Map. Remap was used for updating the mapping attributes of a page
  without changing its virtual address. Now ARCH_Page_Map can be performed on an existing mapping to achieve the same
  result. The ARCH_Page_Remap invocation has been removed from all configurations.
* riscv64: Experimental SMP support for RISCV64 on HiFive.
* Support added for QEMU ARM virt platform, with 3 CPUs: cortex-a15, cortex-a53 and cortex-a57
  - PLATFORM=qemu-arm-virt
  - ARM_CPU={cortex-a15, cortex-a53, cortex-a57}
  - QEMU_MEMORY=1024 (default)
* Support added for rockpro64.
* RISCV: Add support for Ariane SoC
* Unify device untyped initialisation across x86, Arm and RISC-V
  - Access to the entire physical address range is made available via untypes.
  - The kernel reserves regions that user level is not able to access and doesn't hand out untypeds for it.
  - Ram memory is part of this reservation and is instead handed out as regular Untypeds.
  - Memory reserved for use by the kernel or other reserved regions are not accessible via any untypeds.
  - Devices used by the kernel are also not accessible via any untypeds.

### Upgrade Notes

* Usages of Endpoints can now use seL4_Call without providing Grant rights by downgrading the Grant to GrantReply
* The kernel no longer reserves a register for holding the address of a thread's IPC buffer. It is now expected that the
  location of the IPC buffer is stored in a __thread local variable and a thread register is used to refer to each
  thread's thread local variables. The sel4runtime is an seL4 runtime that provides program entry points that setup
  the IPC buffer address and serves as a reference for how the IPC buffer is expected to be accessed.
* All `seL4_ARM_PageGlobalDirectory` invocations need to be replaced with `seL4_ARM_VSpace`.
* Usages of ARCH_Page_Remap can be replaced with ARCH_Page_Map and require the original mapping address to be provided.
* Device untypeds are provided to user level in different sizes which may require more initial processing to break them
  down for each device they refer to.

---

## 10.1.1 2018-11-12: BINARY COMPATIBLE

### Changes

* Remove theoretical uninitialised variable use in infer_cpu_gic_id for binary translation validation

### Upgrade Notes

* 10.1.0 has a known broken test in the proofs. 10.1.1 fixes this test.

---

## 10.1.0 2018-11-07: SOURCE COMPATIBLE

### Changes

* structures in the boot info are not declared 'packed'
  - these were previously packed (in the GCC attribute sense)
  - some field lengths are tweaked to avoid padding
  - this is a source-compatible change
* ARM platforms can now set the trigger of an IRQ Handler capability
  - seL4_IRQControl_GetTrigger allows users to obtain an IRQ Handler capability
    and set the trigger (edge or level) in the interrupt controller.
* Initial support for NVIDIA Jetson TX2 (ARMv8a, Cortex A57)
* AARCH64 support added for raspberry pi 3 platform.
* Code generation now use jinja2 instead of tempita.
* AARCH32 HYP support added for running multiple ARM VMs
* AARCH32 HYP VCPU registers updated.
* A new invocation for setting TLSBase on all platforms.
  - seL4_TCB_SetTLSBase
* Kbuild/Kconfig/Makefile build system removed.

---

## 10.0.0 2018-05-28: BREAKING

* Final version of the kernel  which supports integration with Kbuild based projects
* Future versions, including this one, provide a CMake based build system

For more information see <https://docs.sel4.systems/Developing/Building>.

### Changes

* x86 IO ports now have an explicit IOPortControl capability to gate their creation. IOPort capabilities  may now only
  be created through the IOPortControl capability that is passed to the rootserver. Additionally IOPort capabilities
  may not be derived to have smaller ranges and the IOPortControl will not issue overlapping IOPorts
* 32-bit support added for the initial prototype RISC-V architecture port

### Upgrade Notes

* A rootserver must now create IOPort capabilities from the provided IOPortControl capability. As IOPorts can not
  have their ranges further restricted after creation it must create capabilities with the final desired granularity,
  remembering that since ranges cannot overlap you cannot issue a larger and smaller range that have any IO ports
  in common.

---

## 9.0.1 2018-04-18: BINARY COMPATIBLE

### Changes

* On 64-bit architectures, the `label` field of `seL4_MessageInfo` is now 52 bits wide. User-level programs
  which use any of the following functions may break, if the program relies on these functions to mask the
  `label` field to the previous width of 20 bits.
  - `seL4_MessageInfo_new`
  - `seL4_MessageInfo_get_label`
  - `seL4_MessageInfo_set_label`
* Initial prototype RISC-V architecture port. This port currently only supports running in 64-bit mode without FPU or
  or multicore support on the Spike simulation platform. There is *no verification* for this platform.

---

## 9.0.0 2018-04-11: BREAKING

### Changes

* Debugging option on x86 for syscall interface to read/write MSRs (this is an, equally dangerous, alternative to
  dangerous code injection)
* Mitigation for Meltdown (<https://meltdownattack.com>) on x86-64 implemented. Mitigation is via a form of kernel
  page table isolation through the use of a Static Kernel Image with Microstate (SKIM) window that is used for
  trapping to and from the kernel address space. This can be enabled/disabled through the build configuration
  depending on whether you are running on vulnerable hardware or not.
* Mitigation for Spectre (<https://spectreattack.com>) on x86 against the kernel implemented. Default is software
  mitigation and is the best performing so users need to do nothing. This does *not* prevent user processes from
  exploiting each other.
* x86 configuration option for performing branch prediction barrier on context switch to prevent Spectre style
  attacks between user processes using the indirect branch predictor
* x86 configuration option for flushing the RSB on context switch to prevent Spectre style attacks between user
  processes using the RSB
* Define extended bootinfo header for the x86 TSC frequency
* x86 TSC frequency exported in extended bootinfo header
* `archInfo` is no longer a member of the bootinfo struct. Its only use was for TSC frequency on x86, which
  can now be retrieved through the extended bootinfo
* Invocations to set thread priority and maximum control priority (MCP) have changed.
  - For both invocations, users must now provide a TCB capability `auth`
  - The requested MCP/priority is checked against the MCP of the `auth` capability.
  - Previous behavior checked against the invoked TCB, which could be subject to the confused deputy
   problem.
* seL4_TCB_Configure no longer takes prio, mcp as an argument. Instead these fields must be set separately
  with seL4_TCB_SetPriority and seL4_TCB_SetMCPriority.
* seL4_TCB_SetPriority and seL4_TCB_SetMCPriority now take seL4_Word instead of seL4_Uint8.
  - seL4_MaxPrio remains at 255.
* seL4_TCB_SetSchedParams is a new method where MCP and priority can be set in the same sytsem call.
* Size of the TCB object is increased for some build configurations

### Upgrade notes

* seL4_TCB_Configure calls that set priority should be changed to explicitly call seL4_TCB_SetSchedParams
  or SetPriority
* seL4_TCB_Configure calls that set MCP should be changed to explicitly call seL4_TCB_SetSchedParams
  or seL4_TCB_SetMCPriority

---

## 8.0.0 2018-01-17

### Changes

* Support for additional zynq platform Zynq UltraScale+ MPSoC (Xilinx ZCU102, ARMv8a, Cortex A53)
* Support for multiboot2 bootloaders on x86 (contributed change from Genode Labs)
* Deprecate seL4_CapData_t type and functions related to it
* A fastpath improvement means that when there are two runnable threads and the target thread is the highest priority in
  the scheduler, the fastpath will be hit. Previously the fastpath would not be used on IPC from a high priority thread
  to a low priority thread.
* As a consequence of the above change, scheduling behaviour has changed in the case where a non-blocking IPC is sent
  between two same priority threads: the sender will be scheduled, rather than the destination.
* Benchmarking support for armv8/aarch64 is now available.
* Additional x86 extra bootinfo type for retrieving frame buffer information from multiboot 2
* Debugging option to export x86 Performance-Monitoring Counters to user level

### Upgrade notes

* seL4_CapData_t should be replaced with just seL4_Word. Construction of badges should just be `x` instead of
  `seL4_CapData_Badge_new(x)` and guards should be `seL4_CNode_CapData_new(x, y)` instead of
  `seL4_CapData_Guard_new(x, y)`
* Code that relied on non-blocking IPC to switch between threads of the same priority may break.

---

## 7.0.0 2017-09-05

### Changes

* Support for building standalone ia32 kernel added
* ia32: Set sensible defaults for FS and GS selectors
* aarch64: Use tpidrro_el0 for IPC buffer instead of tpidr_el0
* More seL4 manual documentation added for aarch64 object invocations
* Default NUM_DOMAINS set to 16 for x86-64 standalone builds
* libsel4: Return seL4_Error in invocation stubs in 8fb06eecff9 ''' This is a source code level breaking change '''
* Add a CMake based build system
* x86: Increase TCB size for debug builds
* libsel4: x86: Remove nested struct declarations ''' This is a source code level breaking change '''
* Bugfix: x86: Unmap pages when delete non final frame caps

### Upgrade notes

* This release is not source compatible with previous releases.
* seL4 invocations that previously returned long now return seL4_Error which is an enum. Our libraries have already been
  updated to reflect this change, but in other places where seL4 invocations are used directly, the return types will
  need to be updated to reflect this change.
* On x86 some structs in the Bootinfo have been rearranged. This only affects seL4_VBEModeInfoBlock_t which is used if
  VESA BIOS Extensions (VBE) information is being used.

### Known issues

* One of our tests is non-deterministically becoming unresponsive on the SMP release build on the Sabre IMX.6 platform,
  which is a non verified configuration of the kernel.  We are working on fixing this problem, and will likely do a
  point release once it is fixed.

---
For previous releases see <https://docs.sel4.systems/releases/seL4.html>
