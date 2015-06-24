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

#ifdef X86_64

.align 0x1000
_libsel4_start:
    leaq    _stack_top, %esp

    /* Setup segment selector for IPC buffer access. */
    movw    $IPCBUF_GDT_SELECTOR, %ax
    movw    %ax, %gs

    /* Setup the global "bootinfo" structure. */
    /* Invoke 'seL4_BootInfo* seL4_GetBootInfo();' to get address */
    movq    %rbx, %rdi
    call    seL4_InitBootInfo

#else

_libsel4_start:
    leal    _stack_top, %esp

    /* Setup segment selector for IPC buffer access. */
    movw    $IPCBUF_GDT_SELECTOR, %ax
    movw    %ax, %gs

    /* Setup the global "bootinfo" structure. */
    /* Invoke 'seL4_BootInfo* seL4_GetBootInfo();' to get address */
    pushl   %ebx
    call    seL4_InitBootInfo

#endif

    /* Invoke 'int main(void);' */
    call    main

    /* If main returns ignore result and die */
1:  jmp     1b


    .bss
    .align  8

_stack_bottom:
    .space  16384
_stack_top:

