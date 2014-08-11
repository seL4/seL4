<!--
  Copyright 2014, General Dynamics C4 Systems

  This software may be distributed and modified according to the terms of
  the GNU General Public License version 2. Note that NO WARRANTY is provided.
  See "LICENSE_GPLv2.txt" for details.

  @TAG(GD_GPL)
-->

The seL4 Repository
===================

This repository contains the source code of seL4 microkernel.

For details about the seL4 microkernel, including details about its formal
correctness proof, please see the [`sel4.systems`][1] website and associated
[FAQ][2].

DOIs for citing recent releases of this repository:
  * [![DOI][4]](http://dx.doi.org/10.5281/zenodo.11247)

We welcome contributions to seL4. Please see the website for information
on [how to contribute][3].

This repository is usually not used in isolation, but as part of the build
system in a larger project.

  [1]: http://sel4.systems/
  [2]: http://sel4.systems/FAQ/
  [3]: http://sel4.systems/Contributing/
  [4]: https://zenodo.org/badge/doi/10.5281/zenodo.11247.png

Repository Overview
-------------------

  * `include` and `src`: C and ASM source code of seL4
  * `tools`: build tools
  * `haskell`: Haskell model of the seL4 kernel,
               kept in sync with the C version.
  * `libsel4`: C bindings for the seL4 ABI
  * `manual`: LaTeX sources of the seL4 reference manual


Build Instructions
------------------

tl;dr:

    TOOLPREFIX=arm-none-eabi- ARCH=arm PLAT=imx31 ARMV=armv6 CPU=arm1136jf-s \
 	make

The kernel source requires a cross-compiler for the target architecture. To
build using `make`, follow these instructions:

 * Ensure that the appropriate cross-compiler for your target
   architecture is installed.

 * Set the `TOOLPREFIX` environment variable to your cross-compiler's
   prefix. E.g. `arm-none-eabi-`.

 * Set the `ARCH`, `PLAT`, `ARMV` and `CPU` variables for the intended target
   architecture and platform, chosen from the following lists:

    ARCH | PLAT   | ARMV    | CPU
    -----|--------|---------|-----------
    arm  | imx31  | armv6   | arm1136jf-s
    arm  | omap3  | armv7-a | cortex-a8
    arm  | am335x | armv7-a | cortex-a8
    ia32 | pc99   |         |

 * For a debug build, append `DEBUG=y`.

See the seL4 website for more [comprehensive build instructions][5].

 [5]: http://sel4.systems/Download/


License
=======

The files in this repository are released under standard open source licenses.
Please see the individual file headers and `LICENSE_GPLv2.txt` and
`LICENSE_BSD2.txt` files for details.
