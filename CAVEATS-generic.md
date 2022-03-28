<!--
    Copyright 2014, General Dynamics C4 Systems

    SPDX-License-Identifier: GPL-2.0-only
-->

# Known caveats in the seL4 API and implementation

## Implementation Correctness

Only the ARMv7 version on the imx6 platform of seL4 has the full stack of
correctness proofs. This proof covers the functional behaviour of the C code of
the kernel. It does not cover machine code, compiler, linker, boot code, cache
and TLB management. Compiler and linker can be removed from this list by
additionally running the binary verification tool chain for seL4. The proof
shows that the seL4 C code implements the abstract API specification of seL4,
and that this specification satisfies the following high-level security
properties:

  * integrity (no write without authority),
  * confidentiality (no read without authority), and
  * intransitive non-interference (isolation between adequately
    configured user-level components).

The security property proofs depend on additional assumptions on the correct
configuration of the system. See the `l4v` repository on github for more
details.

The x64 port of the kernel without VT-x and VT-d support has a functional
correctness proof between abstract specification and C code, but without
security theorems, and the ARMv7 version of the kernel with hypervisor
extensions also has a functional correctness proofs, but without the security
theorems. For the precise configuration of these three verified platforms, see
the corresponding files in the `config/` directory.

Proofs for the MCS version (mixed-criticality systems) and for seL4 on the
RISC-V architecture are in progress.


## Real Time

The default version of seL4 must be configured carefully for use in real-time
requirements. It has a small number of potentially long-running kernel
operations that are not preemptible (e.g., endpoint deletion, certain
scheduling states, frame and CNode initialisation). These can (and must) be
avoided by careful system configuration if low latency is required.

The MCS configuration of the kernel addresses many of these problems and
provides principled access control for execution time, but its formal
verification is currently still in progress.


## Re-using Address Spaces

Before an ASID/page directory/page table can be reused, all frame caps
installed in it should be revoked. The kernel will not do this automatically
for the user.

If, for instance, page cap c is installed in the address space denoted by a
page directory under ASID A, and the page directory is subsequently revoked or
deleted, and then a new page directory is installed under that same ASID A,
the page cap c will still retain some authority in the new page directory,
even though the user intention might be to run the new page directory under a
new security context. The authority retained is to perform the unmap operation
on the page the cap c refers to.
