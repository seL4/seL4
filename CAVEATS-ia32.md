<!--
  Copyright 2014, General Dynamics C4 Systems

  SPDX-License-Identifier: GPL-2.0-only
-->

# Caveats specific to seL4 on ia32 and ia64

## Intel VT-d (I/O MMU) support

Intel VT-d support in seL4 was tested for the following chipsets:

 - Intel Q35 Express
 - Intel 5500

On other chipsets with Intel VT-d support, seL4 might:

 - complain and disable IOMMU support
 - hang during bootstrapping
 - have some weird behaviour during runtime

In any case, the workaround is to disable VT-d support, either:

 - in the BIOS, or
 - by including `disable_iommu` into the MultiBoot (e.g. GRUB) command line
   as described in the seL4 documentation
