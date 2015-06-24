/*
 * Copyright 2014, NICTA
 *
 * This software may be distributed and modified according to the terms of
 * the BSD 2-Clause license. Note that NO WARRANTY is provided.
 * See "LICENSE_BSD2.txt" for details.
 *
 * @TAG(NICTA_BSD)
 */
/*
 * A default _libsel4_start. It initalizes sel4_BootInfo and invokes
 * int main(void). The return value is ignored and then this hangs.
 */

#define __ASM__
#include <autoconf.h>

#include <sel4/arch/constants.h>


    .global _libsel4_start

    .text

_libsel4_start:
    /* Setup a stack for ourselves. */
    ldr     sp, =_stack_top

    /* Setup bootinfo. The pointer to the bootinfo struct starts in 'r0'. */
    bl      seL4_InitBootInfo

    /* Invoke 'int main(void);' */
    bl      main

    /* If main returns ignore result and die */
1:  jmp     1b


    .bss
    .align  8

_stack_bottom:
    .space  16384
_stack_top:

#endif /* CONFIG_LIB_SEL4_PLAT_SUPPORT_SEL4_START */
