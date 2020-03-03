<!--
     Copyright 2014, General Dynamics C4 Systems

     SPDX-License-Identifier: GPL-2.0-only
-->

The seL4 Repository
===================

This repository contains the source code of seL4 microkernel.

For details about the seL4 microkernel, including details about its formal
correctness proof, please see the [`sel4.systems`][1] website and associated
[FAQ][2].

DOIs for citing recent releases of this repository:
  * [![DOI][4]](https://doi.org/10.5281/zenodo.591727)

We welcome contributions to seL4. Please see the website for information
on [how to contribute][3].

This repository is usually not used in isolation, but as part of the build
system in a larger project.

  [1]: http://sel4.systems/
  [2]: http://sel4.systems/Info/FAQ/
  [3]: http://sel4.systems/Community/Contributing/
  [4]: https://zenodo.org/badge/DOI/10.5281/zenodo.591727.svg
  [5]: https://sel4.systems/Info/Docs/seL4-manual-latest.pdf
  [6]: http://sel4.systems/Info/GettingStarted/

Manual
------

A hosted version of the [manual](manual/) for the most recent release can be found [here][5].

Repository Overview
-------------------

  * `include` and `src`: C and ASM source code of seL4
  * `tools`: build tools
  * `libsel4`: C bindings for the seL4 ABI
  * `manual`: LaTeX sources of the seL4 reference manual


Build Instructions
------------------

See the seL4 website for [build instructions][6].

License
=======

The files in this repository are released under standard open source
licenses, identified by [SPDX license tags][7]. Generally, kernel-level
code is licensed under GPLv2 and user-level code under the 2-clause BSD
license. See the individual file headers for details, or use one of the
publicly available SPDX tools to generate a bill of materials. The
directory `LICENSES` contains the text for all licenses that are
mentioned by files in this repository.

### GPL syscall note
Note that, as in the [Linux syscall note for the GPL][8], the seL4
kernel GPL license does *not* cover user-level code that uses kernel
services by normal system calls - this is merely considered normal use
of the kernel, and does *not* fall under the heading of "derived work".
Syscall headers are provided under BSD.

[7]: https://spdx.org
[8]: https://spdx.org/licenses/Linux-syscall-note.html
