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
#include <string.h>
#include <plat/machine/fdt.h>
#include <config.h>
#include <util.h>

#define FDT_MAGIC   0xd00dfeed
#define FDT_VERSION 17

#define FDT_BEGIN_NODE  1
#define FDT_END_NODE    2
#define FDT_PROP    3
#define FDT_NOP     4
#define FDT_END     9

struct fdt_header {
    uint32_t magic;
    uint32_t totalsize;
    uint32_t off_dt_struct;
    uint32_t off_dt_strings;
    uint32_t off_mem_rsvmap;
    uint32_t version;
    uint32_t last_comp_version; /* <= 17 */
    uint32_t boot_cpuid_phys;
    uint32_t size_dt_strings;
    uint32_t size_dt_struct;
};

struct fdt_scan_node {
    const struct fdt_scan_node *parent;
    const char *name;
    int address_cells;
    int size_cells;
};

struct fdt_scan_prop {
    const struct fdt_scan_node *node;
    const char *name;
    uint32_t *value;
    int len; // in bytes of value
};

/* workaround because string literals are not supported by the C parser */
const char fdt_address_cells[] = {'#', 'a', 'd', 'd', 'r', 'e', 's', 's', '-', 'c', 'e', 'l', 'l', 's', 0};
const char fdt_size_cells[] = {'#', 's', 'i', 'z', 'e', '-', 'c', 'e', 'l', 'l', 's', 0};
const char fdt_reg[] = {'r', 'e', 'g', 0};
const char fdt_device_type[] = {'d', 'e', 'v', 'i', 'c', 'e', '_', 't', 'y', 'p', 'e', 0};
const char fdt_memory[] = {'m', 'e', 'm', 'o', 'r', 'y', 0};

static inline uint32_t bswap(uint32_t x)
{
    uint32_t y = (x & 0x00FF00FF) <<  8 | (x & 0xFF00FF00) >>  8;
    uint32_t z = (y & 0x0000FFFF) << 16 | (y & 0xFFFF0000) >> 16;
    return z;
}

struct scan_state {
    int found_memory;
    const uint32_t *reg_value;
    int reg_len;
};

static const uint32_t *fdt_get_address(const struct fdt_scan_node *node, const uint32_t *value, uint64_t *result)
{
    *result = 0;
    for (int cells = node->address_cells; cells > 0; --cells) {
        *result = (*result << 32) + bswap(*value++);
    }
    return value;
}

static const uint32_t *fdt_get_size(const struct fdt_scan_node *node, const uint32_t *value, uint64_t *result)
{
    *result = 0;
    for (int cells = node->size_cells; cells > 0; --cells) {
        *result = (*result << 32) + bswap(*value++);
    }
    return value;
}

static uint32_t *fdt_scan_helper(
    uint32_t *lex,
    const char *strings,
    struct fdt_scan_node *node,
    struct scan_state *state)
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
            if (node && !strncmp(prop.name, fdt_address_cells, 14)) {
                node->address_cells = bswap(lex[3]);
            }
            if (node && !strncmp(prop.name, fdt_size_cells, 11))    {
                node->size_cells    = bswap(lex[3]);
            }
            lex += 3 + (prop.len + 3) / 4;
            if (state->found_memory && strncmp(prop.name, fdt_reg, 3) == 0) {
                state->reg_value = prop.value;
                state->reg_len = prop.len;
            }
            if (strncmp(prop.name, fdt_device_type, 11) == 0 && strncmp((const char*)prop.value, fdt_memory, 6) == 0) {
                state->found_memory = 1;
            }
            break;
        }
        case FDT_BEGIN_NODE: {
            uint32_t *lex_next;
            last = 1;
            child.name = (const char *)(lex + 1);
            lex_next = fdt_scan_helper(
                           lex + 2 + strnlen(child.name, 1024) / 4,
                           strings, &child, state);
            lex = lex_next;
            break;
        }
        case FDT_END_NODE: {
            if (state->found_memory) {
                const uint32_t *value = state->reg_value;
                const uint32_t *end = value + state->reg_len / 4;

                assert (state->reg_value && state->reg_len % 4 == 0);

                while (end - value > 0) {
                    uint64_t base, size;
                    value = fdt_get_address(node->parent, value, &base);
                    value = fdt_get_size   (node->parent, value, &size);
                    if (!add_avail_p_reg((p_region_t) {
                    base, base + size
                })) {
                        printf("Failed to add physical memory region %llu-%llu\n", (unsigned long long)base, (unsigned long long)(base + size));
                    }
                }
                state->found_memory = 0;
            }
            return lex + 1;
        }
        default: { // FDT_END
            return lex;
        }
        }
    }
}

void parseFDT(void *fdt)
{
    struct fdt_header *header = (struct fdt_header *)fdt;

    // Only process FDT that we understand
    if (bswap(header->magic) != FDT_MAGIC ||
            bswap(header->last_comp_version) > FDT_VERSION) {
        return;
    }

    const char *strings = (const char *)((word_t)fdt + bswap(header->off_dt_strings));
    uint32_t *lex = (uint32_t *)((word_t)fdt + bswap(header->off_dt_struct));

    struct scan_state state;
    state.found_memory = 0;

    fdt_scan_helper(lex, strings, 0, &state);
}

uint32_t fdt_size(void *fdt)
{
    struct fdt_header *header = (struct fdt_header *)fdt;

    // Only process FDT that we understand
    if (bswap(header->magic) != FDT_MAGIC ||
            bswap(header->last_comp_version) > FDT_VERSION) {
        return 0;
    }
    return bswap(header->totalsize);
}
