/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

/* Configuration for MultiBoot, see MultiBoot Specification:
   www.gnu.org/software/grub/manual/multiboot
   We use a flags field of 3, indicating that we want modules loaded on page
   boundaries and access to the memory map information. We do not set bit 16,
   indicating that the structure of the image should be taken from its ELF
   headers. */

#include <config.h>
#include <machine/assembler.h>

#define MULTIBOOT_HEADER_MAGIC 0x1BADB002

#ifdef CONFIG_MULTIBOOT_GRAPHICS_MODE_NONE
    #define MULTIBOOT_HEADER_FLAGS 3
    #define MULTIBOOT_GRAPHICS_TEXT 0
#else /* CONFIG_MULTIBOOT_GRAPHICS_MODE_NONE */
    #define MULTIBOOT_HEADER_FLAGS 7
    #ifdef CONFIG_MULTIBOOT_GRAPHICS_MODE_TEXT
        #define MULTIBOOT_GRAPHICS_TEXT 1
    #else /* CONFIG_MULTIBOOT_GRAPHICS_MODE_TEXT */
        #define MULTIBOOT_GRAPHICS_TEXT 0
    #endif /* CONFIG_MULTIBOOT_GRAPHICS_MODE_TEXT */
#endif /* CONFIG_MULTIBOOT_GRAPHICS_MODE_NONE */

/* These configs will be set if a graphics mode is set. if they aren't
 * then we can put whatever in the multiboot header and it will be ignored.
 * This seems to be the easiest way to make things compile */
#ifndef CONFIG_MULTIBOOT_GRAPHICS_MODE_HEIGHT
#define CONFIG_MULTIBOOT_GRAPHICS_MODE_HEIGHT 0
#endif
#ifndef CONFIG_MULTIBOOT_GRAPHICS_MODE_WIDTH
#define CONFIG_MULTIBOOT_GRAPHICS_MODE_WIDTH 0
#endif
#ifndef CONFIG_MULTIBOOT_GRAPHICS_MODE_DEPTH
#define CONFIG_MULTIBOOT_GRAPHICS_MODE_DEPTH 0
#endif

.section .mbh
#ifdef CONFIG_MULTIBOOT1_HEADER
    /* MultiBoot header */
    .align  4
    .long   MULTIBOOT_HEADER_MAGIC; /*magic*/
    .long   MULTIBOOT_HEADER_FLAGS; /*flags*/
    .long   - MULTIBOOT_HEADER_FLAGS - MULTIBOOT_HEADER_MAGIC; /*checksum*/
    .long   0 /*header_addr*/
    .long   0 /*load_addr*/
    .long   0 /*load_end_addr*/
    .long   0 /*bss_end_addr*/
    .long   0 /*entry_addr*/
    .long   MULTIBOOT_GRAPHICS_TEXT /*mode_type*/
    .long   CONFIG_MULTIBOOT_GRAPHICS_MODE_WIDTH /*width*/
    .long   CONFIG_MULTIBOOT_GRAPHICS_MODE_HEIGHT /*height*/
    .long   CONFIG_MULTIBOOT_GRAPHICS_MODE_DEPTH /*depth*/
#endif
#ifdef CONFIG_MULTIBOOT2_HEADER
    .align  8
    __mbi2_start:
    /* magic multi-boot 2 header */
    .long   0xe85250d6
    .long   0x0
    .long   (__mbi2_end - __mbi2_start)
    .long  -(0xe85250d6 + (__mbi2_end - __mbi2_start))
    /* end tag - type, flags, size */
    .word   0x0
    .word   0x0
    .long   0x8
    __mbi2_end:
#endif
