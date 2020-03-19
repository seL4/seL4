<!--
     Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)

     SPDX-License-Identifier: CC-BY-SA-4.0
-->

# Contributions Welcome!

Contributions to the seL4 kernel repository are welcome!

## Kernel Development Process

In addition to our general [contribution guidelines][1], the kernel has additional git history requirements:

* Please try to make sure every commit is in a working state to facilitate bisecting.
    + unless there is a concrete reason, if so please state that reason in the commit message.
* Try to keep commits small for ease of reviewing.

[1]: https://docs.sel4.systems/Contributing

## Build/Test

Generally, any contributions should pass the tests in the project
<https://github.com/seL4/sel4test>. If new features or platforms are added,
they should add corresponding tests in `sel4test`.

Contributions to `master` should additionally either be invisible to the proof
in <https://github.com/seL4/l4v>, such as comments, documentation, style,
unverified platform, etc, or they should come with proof updates to `l4v`.

## Contact

If you have larger changes or additions, it is a good idea to get in contact
with us as <devel@sel4.systems>, so we can help you get started.

## Developer Certificate of Origin (DCO)

This repository uses the same sign-off process as the Linux kernel. For every
commit, use

    git commit -s

to add a sign-off line to your commit message, which will come out as:

    Signed-off-by: name <email>

By adding this line, you make the declaration that you have the right to make
this contribution under the open source license the files use that you changed
or contributed.

The full text of the declaration is at <https://developercertificate.org>.
