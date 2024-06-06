/*
 * Copyright 2024, DornerWorks
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */
#include <config.h>
#include <arch/kernel/boot_sys.h>
#include <arch/model/statedata.h>
#include <machine/io.h>
#include <plat/machine/io.h>
#include <drivers/uart.h>

#define VGA_PHYS 0xb8000 // Text buffer address

//black background, light grey text, no blinking
#define VGA_COLOR 0x07
#define CLEAR_SCREEN_CHAR 32
static uint64_t vga_addr = 0;
static uint64_t vga_size = 0;
// assume 80x25 console
#define MAX_X_POS 80
#define MAX_Y_POS 25

/* Name kept 'serial_init' since other code expects to call this */
void serial_init(uint16_t port)
{
    set_vga_addr(VGA_PHYS);
}

void set_vga_addr(uint64_t addr)
{
    vga_addr = addr;
    // multiply by 2 since each slot in the VGA buffer has a char byte and 'style' byte
    vga_size = MAX_X_POS * MAX_Y_POS * 2;
}

uint64_t get_vga_addr(void)
{
    return vga_addr;
}

static int verify_vga_offset(uint64_t offset)
{
    if (offset > vga_size) {
        return -1;
    }
    return 0;
}

static void vga_write(uint16_t data, uint64_t offset)
{
    if (!verify_vga_offset(offset)) {
        *(uint16_t *)(vga_addr + offset) = data;
    }
}

static uint16_t vga_read(uint64_t offset)
{
    if (!verify_vga_offset(offset)) {
        return *(uint16_t *)(vga_addr + offset);
    }
    return 0;
}

static int current_ypos = MAX_Y_POS, current_xpos = 0;

/***
 * Slight rework of function 'early_vga_write' from
 * the linux kernel at arch/x86/kernel/early_printk.c
 ***/
static void vga_putchar(char c)
{
    int  row, prev_column, column;
    if (current_ypos >= MAX_Y_POS) { // do we need to scroll
        /* scroll 1 line up */
        for (prev_column = 1, column = 0; prev_column < MAX_Y_POS; prev_column++, column++) {
            for (row = 0; row < MAX_X_POS; row++) {
                vga_write(vga_read(2 * (MAX_X_POS * prev_column + row)),
                          2 * (MAX_X_POS * column + row));
            }
        }
        for (row = 0; row < MAX_X_POS; row++) { // clear last line so we can write too it
            vga_write((VGA_COLOR << 8) | CLEAR_SCREEN_CHAR, 2 * (MAX_X_POS * column + row));
        }
        current_ypos = MAX_Y_POS - 1;
    }

    if (c == '\n') {
        current_xpos = 0;
        current_ypos++;
    } else if (c != '\r')  {
        vga_write(((VGA_COLOR << 8) | (unsigned short) c),
                  2 * (MAX_X_POS * current_ypos +
                       current_xpos++));
        if (current_xpos >= MAX_X_POS) {
            current_xpos = 0;
            current_ypos++;
        }
    }
}

void kernel_putDebugChar(unsigned char c)
{
    vga_putchar(c);
}
