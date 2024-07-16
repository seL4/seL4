<!--
    Copyright 2014, General Dynamics C4 Systems

    SPDX-License-Identifier: GPL-2.0-only
-->

# Known caveats in the seL4 API and implementation

## Implementation Correctness

The following seL4 architectures have platforms with a C-level functional
correctness proof. Proof support for further platforms within these
architectures is on the roadmap and expected in 2025.

- AArch32: Armv7-a with and without hypervisor extensions, no SMMU, with
  fast path
  - Platforms (non-hyp): `sabre` (no FPU), `imx8mm-evk` (with FPU)
  - Platforms (hyp, no FPU): `tk1`, `exynos5`
- AArch64: Armv8-a with hypervisor extensions only, no SMMU, with fast path
  - Platforms: `tx2`, `zynqmp`, `bcm2711` (rpi4)
- RISC-V: 64-bit only, no fast path
  - Platforms: `hifive`
- x64: without VT-x and VT-d, no fast path
  - Platforms: `pc99`

This proof covers the functional behaviour of the C code of the kernel. It does
not cover machine code, compiler, linker, boot code, cache or TLB management.
The compiler and linker can be removed from this list by additionally running the
binary verification tool chain for seL4 for AArch32 or RISC-V.

Overall, the functional correctness proof shows that the seL4 C code implements
the formal [abstract API specification][ASpec] of seL4 and is free from standard
C implementation defects such as buffer overruns or NULL pointer dereferences.

For AArch32 without hypervisor extensions and without FPU, and for RISC-V, there
are additional proofs that this specification satisfies the following high-level
security properties:

- integrity (no write without authority),
- confidentiality (no read without authority), and
- intransitive non-interference (isolation, modulo timing channels, between
  adequately configured user-level components).

The security property proofs depend on additional assumptions on the correct
configuration of the system. See the [l4v] repository on GitHub for more
details.

Similar proofs for AArch64 with hypervisor extensions are in progress.

For AArch32, there additionally exist proofs for correct user-level system
initialisation. See the [l4v] repository for details.

Note that seL4 currently performs lazy FPU and VCPU switching, which can
introduce information flow timing channels. An API-change proposal ([RFC]) to
improve this behaviour is currently in progress.

## Verified Configurations

For the precise configuration of the verified platforms above, see the
corresponding files in the seL4 `configs/` directory.

The proofs are generally sensitive to changes in configuration parameters, and
will break if these are changed. For some parameters, the proofs are explicitly
set up to be robust, such as the number of domains `NUM_DOMAINS`, and the domain
schedule. More such parameters are on the roadmap to be added and documented
here.

If in doubt, edit the corresponding `_verified` config files and re-run the
proofs as specified in the [l4v] repository.

## Real Time

The default version of seL4 must be configured carefully for use in real-time
requirements. It has a small number of potentially long-running kernel
operations that are not preemptible (e.g., endpoint deletion, certain
scheduling states, frame and CNode initialisation). These can (and must) be
avoided by careful system configuration if low latency is required.

## MCS

The MCS configuration of the kernel addresses many of these real-time problems
and provides principled access control for execution time, but its formal
verification is currently still in progress. For RISC-V, design-level proofs
have completed, and C-level proofs are in progress. Similar proofs for AArch64
are planned.

The MCS configuration is supported by the seL4 foundation and should generally
be stable, with small API changes to be expected while verification is ongoing
and the configuration is deployed in more systems. See open [requests for
comments][RFC] (RFCs) for MCS for what is currently being discussed.

Note that the kernel worst-case execution time (WCET) configuration values in
the kernel platform definitions are defaults only and need to be determined
based on the specific use case -- for instance, static systems can be set up to
have low latency and WCET, whereas dynamic systems or systems where untrusted
code has authority to perform longer-running kernel operations may need higher
values.

## SMP

A symmetric multi-processor (SMP) configuration for seL4 exists and is supported
by the seL4 Foundation, but currently without formal verification. While
generally stable, there are a small number of known open issues, in particular
when the kernel is compiled with `clang`. We recommend `gcc` for working with
SMP configurations of seL4.

The combination of SMP and hypervisor extensions is supported and should be
generally stable, but like the plain SMP configuration it is not formally
verified.

The combination of SMP and MCS is supported and is receiving active development,
but it is less explored and less tested. It should still be considered
experimental. There are no supported Armv7-a boards for SMP+MCS, only Armv8-a,
RISC-V, and Intel. It is tested with `gcc` on `hifive`, `tqma8xqp1gb`,
`odroidc4`, `zynqmp`, `tx1`, `tx2`, `pc99-32`, and `pc99-64`.

The combination of SMP, MCS, and hypervisor extensions is currently supported on
AArch64 only. It is less tested with lower code coverage; currently with `gcc`
only, on `odroidc4`, `tx1`, and `tx2`.

The combination of SMP and domain scheduler is not supported. The SMP
configuration is not expected to satisfy strong intransitive non-interference
for information flow.

See the [seL4 issue tracker][issues] and the [sel4test issue tracker][sel4test
issues] for details using the labels `MCS` and `SMP` for finding issues on these
configurations.

As these are unverified configurations, standard C implementation defects are
possible and not excluded as in verified seL4 configurations.

An intermediate step towards higher assurance for seL4-based multicore systems
is a static multi-kernel configuration of seL4. Formal verification for this
configuration is on the roadmap for the AArch64 architecture, with initial work
begun. Even without verification complete, we expect multi-kernel configurations
to be more robust, because they are simpler and closer to the current sequential
seL4 proofs.

In a multi-kernel configuration, each CPU core runs a separate instance of seL4,
with each kernel instance getting access to disjoint subsets of memory of the
machine. User-level memory can be shared as device-untyped memory, which the
kernel manages but does not access. These configurations can already be set up
without kernel changes by providing suitable device tree overlays to each kernel
instance. Further work is planned to make such configurations easier to use and
more robust against unsafe use/configurations, e.g. by managing IRQ controller
access for each instance.

## Re-using Address Spaces

Before a VSpace can be safely reused in a new security context, all frame caps
previously installed in it should be deleted. The kernel will not do this
automatically for the user.

If not deleted, old frame capabilities retain some authority in the new security
context: the authority to perform cache maintenance operations and the unmap
operation for mappings to the same frame at the same virtual address.

Capabilities to mapped frames store the seL4 ASID and virtual address under
which the frame is mapped to be able find the corresponding mapping slot. This
information can become stale when, for instance, the page table object where the
mapping resides is deleted, and a new page table object is created and used
there instead. The kernel guards against obvious mistakes such as attempting an
unmap operation for a mapping slot that now points to another frame, but cannot
distinguish a cap with correct mapping information for the old VSpace from a cap
with correct mapping information for the new VSpace.

## Intel VT-d (IOMMU)

### Support

Intel VT-d support in seL4 was tested for the following chipsets:

- Intel Q35 Express
- Intel 5500

On other chipsets with Intel VT-d support, seL4 might:

- complain and disable IOMMU support
- hang during bootstrapping
- have some weird behaviour during runtime

In any of these cases, the workaround is to disable VT-d support, either:

- in the BIOS, or
- by including `disable_iommu` into the MultiBoot (e.g. GRUB) command line
  as described in the seL4 documentation

### MSI Remapping

This release does not yet provide support for IOMMU interrupt remapping, which
means devices cannot be securely passed through to untrusted virtual machines,
because they could then be used to trigger [arbitrary MSIs with arbitrary
payload][MSI remap].

Work on a MSI remapping feature is underway.

## Information flow for x86

While no configuration in this release of seL4 provides timing or other
micro-architectural channel guarantees, timing channel exploits on the x86
architecture are more widespread and relevant, especially for dynamic systems.

The kernel does provide configuration options for mitigating [Meltdown] and
[Spectre]-style attacks, but there are no specific mitigations for more recent
such attacks such as [Zenbleed] and [Inception].

Note that the kernel does not depend on secrets, cryptographic or otherwise, so
most kernel targets for such attacks are not present, but timing channel
exploits *can* enable one user-level thread to extract secrets from another.

For many constrained systems, e.g. static embedded systems with known code, the
existing mitigations may be sufficient. For more dynamic systems they are
unlikely to be. Further mitigations could be added in the future with dedicated
funding.

## Rowhammer

seL4 does not offer specific protection against hardware-based memory attacks
such as Rowhammer, but it does provide primitives for user-level systems to, for
instance, only map physical memory in such a way that Rowhammer is ineffective
at crossing protection boundaries.

[l4v]: https://github.com/seL4/l4v
[RFC]: https://github.com/seL4/rfcs
[issues]: https://github.com/seL4/seL4/issues/
[sel4test issues]: https://github.com/seL4/sel4test/issues/
[ASpec]: https://github.com/seL4/l4v/blob/master/spec/abstract

[Meltdown]: https://meltdownattack.com
[Spectre]: https://meltdownattack.com
[Zenbleed]: https://lock.cmpxchg8b.com/zenbleed.html
[Inception]: https://comsec.ethz.ch/research/microarch/inception/
[MSI remap]: http://theinvisiblethings.blogspot.com/2011/05/following-white-rabbit-software-attacks.html
