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

#ifndef FDT_H
#define FDT_H

#define FDT_MAGIC	0xd00dfeed
#define FDT_VERSION	17

typedef unsigned long int uintptr_t;

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

#define FDT_BEGIN_NODE	1
#define FDT_END_NODE	2
#define FDT_PROP	3
#define FDT_NOP		4
#define FDT_END		9

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

struct fdt_cb {
    void (*open)(const struct fdt_scan_node *node, void *extra);
    void (*prop)(const struct fdt_scan_prop *prop, void *extra);
    void (*done)(const struct fdt_scan_node *node, void *extra); // last property was seen
    int  (*close)(const struct fdt_scan_node *node, void *extra); // -1 => delete the node + children
    void *extra;
};

// Scan the contents of FDT
void fdt_scan(uintptr_t fdt, const struct fdt_cb *cb);
uint32_t fdt_size(uintptr_t fdt);

// Extract fields
const uint32_t *fdt_get_address(const struct fdt_scan_node *node, const uint32_t *base, uint64_t *value);
const uint32_t *fdt_get_size(const struct fdt_scan_node *node, const uint32_t *base, uint64_t *value);
int fdt_string_list_index(const struct fdt_scan_prop *prop, const char *str); // -1 if not found

// Setup memory+clint+plic
void query_mem(uintptr_t fdt);
void query_harts(uintptr_t fdt);
void query_plic(uintptr_t fdt);
void query_clint(uintptr_t fdt);

// Remove information from FDT
void filter_harts(uintptr_t fdt, unsigned long hart_mask);
void filter_plic(uintptr_t fdt);
void filter_compat(uintptr_t fdt, const char *compat);

// The hartids of available harts
extern uint64_t hart_mask;

void fdt_print(uintptr_t fdt);

#endif
