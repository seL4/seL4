<!--
     Copyright 2014, General Dynamics C4 Systems

     SPDX-License-Identifier: GPL-2.0-only
-->

The seL4 microkernel
====================

This project contains the source code of seL4 microkernel.

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
  [2]: https://docs.sel4.systems/projects/sel4/frequently-asked-questions
  [3]: https://docs.sel4.systems/processes/contributing.html
  [4]: https://zenodo.org/badge/DOI/10.5281/zenodo.591727.svg
  [5]: https://sel4.systems/Info/Docs/seL4-manual-latest.pdf
  [6]: https://docs.sel4.systems/GettingStarted
  [7]: https://docs.sel4.systems/releases/sel4
  [8]: https://docs.sel4.systems/projects/sel4/api-doc.html

seL4 Basics
---------------

- [Tutorials](https://docs.sel4.systems/Tutorials)
- [Documentation](https://docs.sel4.systems/projects/sel4/documentation)
- [seL4 libraries](https://docs.sel4.systems/projects/user_libs)
- [seL4Test](https://docs.sel4.systems/project/sel4test)
- [Debugging guide](https://docs.sel4.systems/projects/sel4-tutorials/debugging-guide)
- [Benchmarking guide](https://docs.sel4.systems/projects/sel4-tutorials/benchmarking-guide.html)
- [Virtualization on seL4](https://docs.sel4.systems/projects/virtualization/)
- [Host Build Dependencies](https://docs.sel4.systems/projects/buildsystem/host-dependencies.html)
- [Porting seL4](https://docs.sel4.systems/projects/sel4/porting)


Community
---------

- Mailing lists
  - [seL4 Announce](https://sel4.systems/lists/listinfo/announce)
  - [seL4 Devel](https://sel4.systems/lists/listinfo/devel)
- [Suggested projects](/SuggestedProjects)
- [CommunityProjects](/CommunityProjects)


Manual
------

A hosted version of the [manual](manual/) for the most recent release can be found [here][5].

A web version of the API can be found [here][8]

Repository Overview
-------------------

  * `include` and `src`: C and ASM source code of seL4
  * `tools`: build tools
  * `libsel4`: C bindings for the seL4 ABI
  * `manual`: LaTeX sources of the seL4 reference manual


Build Instructions
------------------

See the seL4 website for [build instructions][6].


Status
------

A list of releases and current project status can be found under [seL4 releases][7].

- [Roadmap](https://docs.sel4.systems/projects/roadmap): new features in development
- [Hardware Support](https://docs.sel4.systems/Hardware): information about hardware platform ports
- [Kernel Features]((https://docs.sel4.systems/projects/sel4/status)): information about available kernel features
- [Userland Components and Drivers](https://docs.sel4.systems/projects/available-user-components.html): available device drivers and
      userland components

