<!--
     Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)

     SPDX-License-Identifier: CC-BY-SA-4.0
-->

# License

The files in this repository are released under standard open source
licenses, identified by [SPDX license tags][1]. Generally, kernel-level
code is licensed under GPLv2 and user-level code under the 2-clause BSD
license. See the individual file headers for details, or use one of the
publicly available SPDX tools to generate a bill of materials. The
directory `LICENSES` contains the text for all licenses that are
mentioned by files in this repository.


## GPL syscall note

Note that, as in the [Linux syscall note for the GPL][2], the seL4
kernel GPL license does *not* cover user-level code that uses kernel
services by normal system calls - this is merely considered normal use
of the kernel, and does *not* fall under the heading of "derived work".
Syscall headers are provided under BSD.

For a longer explanation of how the seL4 license does or does not affect
your own code see also [this blog post][3].

[1]: https://spdx.org
[2]: https://spdx.org/licenses/Linux-syscall-note.html
[3]: https://microkerneldude.wordpress.com/2019/12/09/what-does-sel4s-license-imply/
