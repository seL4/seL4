/* Copyright (c) 2010-2017, The Regents of the University of California
 * (Regents).  All Rights Reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the Regents nor the
 * names of its contributors may be used to endorse or promote products
 * derived from this software without specific prior written permission.
 *
 * IN NO EVENT SHALL REGENTS BE LIABLE TO ANY PARTY FOR DIRECT, INDIRECT,
 * SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, INCLUDING LOST PROFITS, ARISING
 * OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF REGENTS HAS
 * BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * REGENTS SPECIFICALLY DISCLAIMS ANY WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
 * THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE. THE SOFTWARE AND ACCOMPANYING DOCUMENTATION, IF ANY, PROVIDED
 * HEREUNDER IS PROVIDED "AS IS". REGENTS HAS NO OBLIGATION TO PROVIDE
 * MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
 */

/* This file is copied from RISC-V tools. It's modified to work with seL4 */

#include <stdint.h>
#include <string.h>
#include <stdint.h>
#include <plat/machine/fdt.h>
#include <config.h>
#include <util.h>

// RVTODO: this code needs to updated to not have function pointers or string literals "like this"
// and any of these appropriate string functions should be moved to common utils
static word_t strlen(const char *s)
{
    const char *p = s;
    while (*p) {
        p++;
    }
    return p - s;
}

static int strcmp(const char* s1, const char* s2)
{
    unsigned char c1, c2;

    do {
        c1 = *s1++;
        c2 = *s2++;
    } while (c1 != 0 && c1 == c2);

    return c1 - c2;
}

static inline uint32_t bswap(uint32_t x)
{
    uint32_t y = (x & 0x00FF00FF) <<  8 | (x & 0xFF00FF00) >>  8;
    uint32_t z = (y & 0x0000FFFF) << 16 | (y & 0xFFFF0000) >> 16;
    return z;
}

static uint32_t *fdt_scan_helper(
    uint32_t *lex,
    const char *strings,
    struct fdt_scan_node *node,
    const struct fdt_cb *cb)
{
    struct fdt_scan_node child;
    struct fdt_scan_prop prop;
    int last = 0;

    child.parent = node;
    // these are the default cell counts, as per the FDT spec
    child.address_cells = 2;
    child.size_cells = 1;
    prop.node = node;

    while (1) {
        switch (bswap(lex[0])) {
        case FDT_NOP: {
            lex += 1;
            break;
        }
        case FDT_PROP: {
            assert (!last);
            prop.name  = strings + bswap(lex[2]);
            prop.len   = bswap(lex[1]);
            prop.value = lex + 3;
            if (node && !strcmp(prop.name, "#address-cells")) {
                node->address_cells = bswap(lex[3]);
            }
            if (node && !strcmp(prop.name, "#size-cells"))    {
                node->size_cells    = bswap(lex[3]);
            }
            lex += 3 + (prop.len + 3) / 4;
            cb->prop(&prop, cb->extra);
            break;
        }
        case FDT_BEGIN_NODE: {
            uint32_t *lex_next;
            if (!last && node && cb->done) {
                cb->done(node, cb->extra);
            }
            last = 1;
            child.name = (const char *)(lex + 1);
            if (cb->open) {
                cb->open(&child, cb->extra);
            }
            lex_next = fdt_scan_helper(
                           lex + 2 + strlen(child.name) / 4,
                           strings, &child, cb);
            if (cb->close && cb->close(&child, cb->extra) == -1)
                while (lex != lex_next) {
                    *lex++ = bswap(FDT_NOP);
                }
            lex = lex_next;
            break;
        }
        case FDT_END_NODE: {
            if (!last && node && cb->done) {
                cb->done(node, cb->extra);
            }
            return lex + 1;
        }
        default: { // FDT_END
            if (!last && node && cb->done) {
                cb->done(node, cb->extra);
            }
            return lex;
        }
        }
    }
}

void fdt_scan(uintptr_t fdt, const struct fdt_cb *cb)
{
    struct fdt_header *header = (struct fdt_header *)fdt;

    // Only process FDT that we understand
    if (bswap(header->magic) != FDT_MAGIC ||
            bswap(header->last_comp_version) > FDT_VERSION) {
        return;
    }

    const char *strings = (const char *)(fdt + bswap(header->off_dt_strings));
    uint32_t *lex = (uint32_t *)(fdt + bswap(header->off_dt_struct));

    fdt_scan_helper(lex, strings, 0, cb);
}

uint32_t fdt_size(uintptr_t fdt)
{
    struct fdt_header *header = (struct fdt_header *)fdt;

    // Only process FDT that we understand
    if (bswap(header->magic) != FDT_MAGIC ||
            bswap(header->last_comp_version) > FDT_VERSION) {
        return 0;
    }
    return bswap(header->totalsize);
}

const uint32_t *fdt_get_address(const struct fdt_scan_node *node, const uint32_t *value, uint64_t *result)
{
    *result = 0;
    for (int cells = node->address_cells; cells > 0; --cells) {
        *result = (*result << 32) + bswap(*value++);
    }
    return value;
}

const uint32_t *fdt_get_size(const struct fdt_scan_node *node, const uint32_t *value, uint64_t *result)
{
    *result = 0;
    for (int cells = node->size_cells; cells > 0; --cells) {
        *result = (*result << 32) + bswap(*value++);
    }
    return value;
}

int fdt_string_list_index(const struct fdt_scan_prop *prop, const char *str)
{
    const char *list = (const char *)prop->value;
    const char *end = list + prop->len;
    int index = 0;
    while (end - list > 0) {
        if (!strcmp(list, str)) {
            return index;
        }
        ++index;
        list += strlen(list) + 1;
    }
    return -1;
}

//////////////////////////////////////////// MEMORY SCAN /////////////////////////////////////////

// This code is left as an example for the future, but functionally it is useless as it has
// no actual interface or relevant side affects
#if 0

struct mem_scan {
    int memory;
    const uint32_t *reg_value;
    int reg_len;
};

static void mem_open(const struct fdt_scan_node *node, void *extra)
{
    struct mem_scan *scan = (struct mem_scan *)extra;
    memset(scan, 0, sizeof(*scan));
}

static void mem_prop(const struct fdt_scan_prop *prop, void *extra)
{
    struct mem_scan *scan = (struct mem_scan *)extra;
    if (!strcmp(prop->name, "device_type") && !strcmp((const char*)prop->value, "memory")) {
        scan->memory = 1;
    } else if (!strcmp(prop->name, "reg")) {
        scan->reg_value = prop->value;
        scan->reg_len = prop->len;
    }
}

static void mem_done(const struct fdt_scan_node *node, void *extra)
{
    struct mem_scan *scan = (struct mem_scan *)extra;
    const uint32_t *value = scan->reg_value;
    const uint32_t *end = value + scan->reg_len / 4;
    uintptr_t self = (uintptr_t)mem_done;

    if (!scan->memory) {
        return;
    }
    assert (scan->reg_value && scan->reg_len % 4 == 0);

    while (end - value > 0) {
        uint64_t base, size;
        value = fdt_get_address(node->parent, value, &base);
        value = fdt_get_size   (node->parent, value, &size);
    }
    assert (end == value);
}

void query_mem(uintptr_t fdt)
{
    struct fdt_cb cb;
    struct mem_scan scan;

    memset(&cb, 0, sizeof(cb));
    cb.open = mem_open;
    cb.prop = mem_prop;
    cb.done = mem_done;
    cb.extra = &scan;

    fdt_scan(fdt, &cb);
}

#endif

#ifdef CONFIG_PRINTING

#define FDT_PRINT_MAX_DEPTH 32

struct fdt_print_info {
    int depth;
    const struct fdt_scan_node *stack[FDT_PRINT_MAX_DEPTH];
};

static inline int isstring(char c)
{
    if (c >= 'A' && c <= 'Z') {
        return 1;
    }
    if (c >= 'a' && c <= 'z') {
        return 1;
    }
    if (c >= '0' && c <= '9') {
        return 1;
    }
    if (c == '\0' || c == ' ' || c == ',' || c == '-') {
        return 1;
    }
    return 0;
}

static void fdt_print_printm(struct fdt_print_info *info, const char *format, ...)
{
    va_list vl;

    for (int i = 0; i < info->depth; ++i) {
        printf("  ");
    }

    va_start(vl, format);
    vprintf(format, vl);
    va_end(vl);
}

static void fdt_print_open(const struct fdt_scan_node *node, void *extra)
{
    struct fdt_print_info *info = (struct fdt_print_info *)extra;

    while (node->parent != NULL && info->stack[info->depth - 1] != node->parent) {
        info->depth--;
        fdt_print_printm(info, "}\r\n");
    }

    fdt_print_printm(info, "%s {\r\n", node->name);
    info->stack[info->depth] = node;
    info->depth++;
}

static void fdt_print_prop(const struct fdt_scan_prop *prop, void *extra)
{
    struct fdt_print_info *info = (struct fdt_print_info *)extra;
    int asstring = 1;
    char *char_data = (char *)(prop->value);

    fdt_print_printm(info, "%s", prop->name);

    if (prop->len == 0) {
        printf(";\r\n");
        return;
    } else {
        printf(" = ");
    }

    /* It appears that dtc uses a hueristic to detect strings so I'm using a
     * similar one here. */
    for (int i = 0; i < prop->len; ++i) {
        if (!isstring(char_data[i])) {
            asstring = 0;
        }
        if (i > 0 && char_data[i] == '\0' && char_data[i - 1] == '\0') {
            asstring = 0;
        }
    }

    if (asstring) {
        for (word_t i = 0; i < prop->len; i += strlen(char_data + i) + 1) {
            if (i != 0) {
                printf(", ");
            }
            printf("\"%s\"", char_data + i);
        }
    } else {
        printf("<");
        for (word_t i = 0; i < prop->len / 4; ++i) {
            if (i != 0) {
                printf(" ");
            }
            printf("0x%08x", bswap(prop->value[i]));
        }
        printf(">");
    }

    printf(";\r\n");
}

static void fdt_print_done(const struct fdt_scan_node *node, void *extra)
{
    struct fdt_print_info * UNUSED info = (struct fdt_print_info *)extra;
}

static int fdt_print_close(const struct fdt_scan_node *node, void *extra)
{
    struct fdt_print_info * UNUSED info = (struct fdt_print_info *)extra;
    return 0;
}

void fdt_print(uintptr_t fdt)
{
    struct fdt_print_info info;
    struct fdt_cb cb;

    info.depth = 0;

    memset(&cb, 0, sizeof(cb));
    cb.open = fdt_print_open;
    cb.prop = fdt_print_prop;
    cb.done = fdt_print_done;
    cb.close = fdt_print_close;
    cb.extra = &info;

    fdt_scan(fdt, &cb);

    while (info.depth > 0) {
        info.depth--;
        fdt_print_printm(&info, "}\r\n");
    }
}

#endif /* CONFIG_PRINTING */
