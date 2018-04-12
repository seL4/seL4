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

The files in this repository are released under standard open source licenses.
Please see the individual file headers and `LICENSE_GPLv2.txt` and
`LICENSE_BSD2.txt` files for details.
