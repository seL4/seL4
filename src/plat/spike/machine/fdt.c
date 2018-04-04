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

#define MAX_HARTS CONFIG_MAX_NUM_NODES
#define RISCV_PGSIZE 4096

typedef word_t size_t;

unsigned int mem_size = BIT(CONFIG_KERNEL_WINDOW_SIZE_BIT);
volatile uint64_t mtime;
volatile uint32_t* plic_priorities;
size_t plic_ndevs;

typedef struct {
    volatile uint32_t* ipi;
    volatile int mipi_pending;

    volatile uint64_t* timecmp;

    volatile uint32_t* plic_m_thresh;
    volatile uintptr_t* plic_m_ie;
    volatile uint32_t* plic_s_thresh;
    volatile uintptr_t* plic_s_ie;
} hls_t;

/*
#define MACHINE_STACK_TOP() ({ \
  register unsigned long sp asm ("sp"); \
  (void*)((sp + RISCV_PGSIZE) & -RISCV_PGSIZE); })
*/
#define HLS_SIZE 64
#define MACHINE_STACK_TOP() 0x0000000080008000ull

// hart-local storage, at top of stack
#define HLS() ((hls_t*)(MACHINE_STACK_TOP() - HLS_SIZE))
#define OTHER_HLS(id) 0

hls_t* hls_init(uintptr_t id)
{
    hls_t* hls = OTHER_HLS(id);
    memset(hls, 0, sizeof(*hls));
    return hls;
}

size_t strlen(const char *s)
{
    const char *p = s;
    while (*p) {
        p++;
    }
    return p - s;
}

int strcmp(const char* s1, const char* s2)
{
    unsigned char c1, c2;

    do {
        c1 = *s1++;
        c2 = *s2++;
    } while (c1 != 0 && c1 == c2);

    return c1 - c2;
}

char* strcpy(char* dest, const char* src)
{
    char* d = dest;
    while ((*d++ = *src++))
        ;
    return dest;
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
        if (base <= self && self <= base + size) {
            mem_size = size;
        }
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

    mem_size = 0;
    fdt_scan(fdt, &cb);
    assert (mem_size > 0);
}

///////////////////////////////////////////// HART SCAN //////////////////////////////////////////

static uint32_t hart_phandles[MAX_HARTS];
uint64_t hart_mask;

struct hart_scan {
    const struct fdt_scan_node *cpu;
    int hart;
    const struct fdt_scan_node *controller;
    int cells;
    uint32_t phandle;
};

static void hart_open(const struct fdt_scan_node *node, void *extra)
{
    struct hart_scan *scan = (struct hart_scan *)extra;
    if (!scan->cpu) {
        scan->hart = -1;
    }
    if (!scan->controller) {
        scan->cells = 0;
        scan->phandle = 0;
    }
}

static void hart_prop(const struct fdt_scan_prop *prop, void *extra)
{
    struct hart_scan *scan = (struct hart_scan *)extra;
    if (!strcmp(prop->name, "device_type") && !strcmp((const char*)prop->value, "cpu")) {
        assert (!scan->cpu);
        scan->cpu = prop->node;
    } else if (!strcmp(prop->name, "interrupt-controller")) {
        assert (!scan->controller);
        scan->controller = prop->node;
    } else if (!strcmp(prop->name, "#interrupt-cells")) {
        scan->cells = bswap(prop->value[0]);
    } else if (!strcmp(prop->name, "phandle")) {
        scan->phandle = bswap(prop->value[0]);
    } else if (!strcmp(prop->name, "reg")) {
        uint64_t reg;
        fdt_get_address(prop->node->parent, prop->value, &reg);
        scan->hart = reg;
    }
}

static void hart_done(const struct fdt_scan_node *node, void *extra)
{
    struct hart_scan *scan = (struct hart_scan *)extra;

    if (scan->cpu == node) {
        assert (scan->hart >= 0);
    }

    if (scan->controller == node && scan->cpu) {
        assert (scan->phandle > 0);
        assert (scan->cells == 1);

        if (scan->hart < MAX_HARTS) {
            hart_phandles[scan->hart] = scan->phandle;
            hart_mask |= 1 << scan->hart;
            hls_init(scan->hart);
        }
    }
}

static int hart_close(const struct fdt_scan_node *node, void *extra)
{
    struct hart_scan *scan = (struct hart_scan *)extra;
    if (scan->cpu == node) {
        scan->cpu = 0;
    }
    if (scan->controller == node) {
        scan->controller = 0;
    }
    return 0;
}

void query_harts(uintptr_t fdt)
{
    struct fdt_cb cb;
    struct hart_scan scan;

    memset(&cb, 0, sizeof(cb));
    memset(&scan, 0, sizeof(scan));
    cb.open = hart_open;
    cb.prop = hart_prop;
    cb.done = hart_done;
    cb.close = hart_close;
    cb.extra = &scan;

    fdt_scan(fdt, &cb);

    // The current hart should have been detected
    assert ((hart_mask >> read_csr(mhartid)) != 0);
}

///////////////////////////////////////////// CLINT SCAN /////////////////////////////////////////

struct clint_scan {
    int compat;
    uint64_t reg;
    const uint32_t *int_value;
    int int_len;
    int done;
};

static void clint_open(const struct fdt_scan_node *node, void *extra)
{
    struct clint_scan *scan = (struct clint_scan *)extra;
    scan->compat = 0;
    scan->reg = 0;
    scan->int_value = 0;
}

static void clint_prop(const struct fdt_scan_prop *prop, void *extra)
{
    struct clint_scan *scan = (struct clint_scan *)extra;
    if (!strcmp(prop->name, "compatible") && fdt_string_list_index(prop, "riscv,clint0") >= 0) {
        scan->compat = 1;
    } else if (!strcmp(prop->name, "reg")) {
        fdt_get_address(prop->node->parent, prop->value, &scan->reg);
    } else if (!strcmp(prop->name, "interrupts-extended")) {
        scan->int_value = prop->value;
        scan->int_len = prop->len;
    }
}

static void clint_done(const struct fdt_scan_node *node, void *extra)
{
    struct clint_scan *scan = (struct clint_scan *)extra;
    const uint32_t *value = scan->int_value;
    const uint32_t *end = value + scan->int_len / 4;

    if (!scan->compat) {
        return;
    }
    assert (scan->reg != 0);
    assert (scan->int_value && scan->int_len % 16 == 0);
    assert (!scan->done); // only one clint

    scan->done = 1;
    mtime = (void*)((uintptr_t)scan->reg + 0xbff8);
    printf("mtime = %p\n", mtime);

    for (int index = 0; end - value > 0; ++index) {
        uint32_t phandle = bswap(value[0]);
        int hart;
        for (hart = 0; hart < MAX_HARTS; ++hart)
            if (hart_phandles[hart] == phandle) {
                break;
            }
        if (hart < MAX_HARTS) {
            hls_t *hls = OTHER_HLS(hart);
            hls->ipi = (void*)((uintptr_t)scan->reg + index * 4);
            hls->timecmp = (void*)((uintptr_t)scan->reg + 0x4000 + (index * 8));
            printf("mtimecmp = %p\n", hls->timecmp);
        }
        value += 4;
    }
}

void query_clint(uintptr_t fdt)
{
    struct fdt_cb cb;
    struct clint_scan scan;

    memset(&cb, 0, sizeof(cb));
    cb.open = clint_open;
    cb.prop = clint_prop;
    cb.done = clint_done;
    cb.extra = &scan;

    scan.done = 0;
    fdt_scan(fdt, &cb);
    assert (scan.done);
}

///////////////////////////////////////////// PLIC SCAN /////////////////////////////////////////

struct plic_scan {
    int compat;
    uint64_t reg;
    uint32_t *int_value;
    int int_len;
    int done;
    int ndev;
};

static void plic_open(const struct fdt_scan_node *node, void *extra)
{
    struct plic_scan *scan = (struct plic_scan *)extra;
    scan->compat = 0;
    scan->reg = 0;
    scan->int_value = 0;
}

static void plic_prop(const struct fdt_scan_prop *prop, void *extra)
{
    struct plic_scan *scan = (struct plic_scan *)extra;
    if (!strcmp(prop->name, "compatible") && fdt_string_list_index(prop, "riscv,plic0") >= 0) {
        scan->compat = 1;
    } else if (!strcmp(prop->name, "reg")) {
        fdt_get_address(prop->node->parent, prop->value, &scan->reg);
    } else if (!strcmp(prop->name, "interrupts-extended")) {
        scan->int_value = prop->value;
        scan->int_len = prop->len;
    } else if (!strcmp(prop->name, "riscv,ndev")) {
        scan->ndev = bswap(prop->value[0]);
    }
}

#define HART_BASE	0x200000
#define HART_SIZE	0x1000
#define ENABLE_BASE	0x2000
#define ENABLE_SIZE	0x80

static void plic_done(const struct fdt_scan_node *node, void *extra)
{
    struct plic_scan *scan = (struct plic_scan *)extra;
    const uint32_t *value = scan->int_value;
    const uint32_t *end = value + scan->int_len / 4;

    if (!scan->compat) {
        return;
    }
    assert (scan->reg != 0);
    assert (scan->int_value && scan->int_len % 8 == 0);
    assert (scan->ndev >= 0 && scan->ndev < 1024);
    assert (!scan->done); // only one plic

    scan->done = 1;
    plic_priorities = (uint32_t*)(uintptr_t)scan->reg;
    plic_ndevs = scan->ndev;

    for (int index = 0; end - value > 0; ++index) {
        uint32_t phandle = bswap(value[0]);
        uint32_t cpu_int = bswap(value[1]);
        int hart;
        for (hart = 0; hart < MAX_HARTS; ++hart)
            if (hart_phandles[hart] == phandle) {
                break;
            }
        if (hart < MAX_HARTS) {
            hls_t *hls = OTHER_HLS(hart);
            if (cpu_int == IRQ_M_EXT) {
                hls->plic_m_ie     = (uintptr_t*)((uintptr_t)scan->reg + ENABLE_BASE + ENABLE_SIZE * index);
                hls->plic_m_thresh = (uint32_t*) ((uintptr_t)scan->reg + HART_BASE   + HART_SIZE   * index);
            } else if (cpu_int == IRQ_S_EXT) {
                hls->plic_s_ie     = (uintptr_t*)((uintptr_t)scan->reg + ENABLE_BASE + ENABLE_SIZE * index);
                hls->plic_s_thresh = (uint32_t*) ((uintptr_t)scan->reg + HART_BASE   + HART_SIZE   * index);
            } else {
                printf("PLIC wired hart %d to wrong interrupt %d", hart, cpu_int);
            }
        }
        value += 2;
    }
#if 0
    printm("PLIC: prio %x devs %d\r\n", (uint32_t)(uintptr_t)plic_priorities, plic_ndevs);
    for (int i = 0; i < MAX_HARTS; ++i) {
        hls_t *hls = OTHER_HLS(i);
        printm("CPU %d: %x %x %x %x\r\n", i, (uint32_t)(uintptr_t)hls->plic_m_ie, (uint32_t)(uintptr_t)hls->plic_m_thresh, (uint32_t)(uintptr_t)hls->plic_s_ie, (uint32_t)(uintptr_t)hls->plic_s_thresh);
    }
#endif
}

void query_plic(uintptr_t fdt)
{
    struct fdt_cb cb;
    struct plic_scan scan;

    memset(&cb, 0, sizeof(cb));
    cb.open = plic_open;
    cb.prop = plic_prop;
    cb.done = plic_done;
    cb.extra = &scan;

    scan.done = 0;
    fdt_scan(fdt, &cb);
}

static void plic_redact(const struct fdt_scan_node *node, void *extra)
{
    struct plic_scan *scan = (struct plic_scan *)extra;
    uint32_t *value = scan->int_value;
    uint32_t *end = value + scan->int_len / 4;

    if (!scan->compat) {
        return;
    }
    scan->done = 1;

    while (end - value > 0) {
        if (bswap(value[1]) == IRQ_M_EXT) {
            value[1] = bswap(-1);
        }
        value += 2;
    }
}

void filter_plic(uintptr_t fdt)
{
    struct fdt_cb cb;
    struct plic_scan scan;

    memset(&cb, 0, sizeof(cb));
    cb.open = plic_open;
    cb.prop = plic_prop;
    cb.done = plic_redact;
    cb.extra = &scan;

    scan.done = 0;
    fdt_scan(fdt, &cb);
}

//////////////////////////////////////////// COMPAT SCAN ////////////////////////////////////////

struct compat_scan {
    const char *compat;
    int depth;
    int kill;
};

static void compat_open(const struct fdt_scan_node *node, void *extra)
{
    struct compat_scan *scan = (struct compat_scan *)extra;
    ++scan->depth;
}

static void compat_prop(const struct fdt_scan_prop *prop, void *extra)
{
    struct compat_scan *scan = (struct compat_scan *)extra;
    if (!strcmp(prop->name, "compatible") && fdt_string_list_index(prop, scan->compat) >= 0)
        if (scan->depth < scan->kill) {
            scan->kill = scan->depth;
        }
}

static int compat_close(const struct fdt_scan_node *node, void *extra)
{
    struct compat_scan *scan = (struct compat_scan *)extra;
    if (scan->kill == scan->depth--) {
        scan->kill = 999;
        return -1;
    } else {
        return 0;
    }
}

void filter_compat(uintptr_t fdt, const char *compat)
{
    struct fdt_cb cb;
    struct compat_scan scan;

    memset(&cb, 0, sizeof(cb));
    cb.open = compat_open;
    cb.prop = compat_prop;
    cb.close = compat_close;
    cb.extra = &scan;

    scan.compat = compat;
    scan.depth = 0;
    scan.kill = 999;
    fdt_scan(fdt, &cb);
}

//////////////////////////////////////////// HART FILTER ////////////////////////////////////////

struct hart_filter {
    int compat;
    int hart;
    char *status;
    unsigned long mask;
};

static void hart_filter_open(const struct fdt_scan_node *node, void *extra)
{
    struct hart_filter *filter = (struct hart_filter *)extra;
    filter->status = 0;
    filter->compat = 0;
    filter->hart = -1;
}

static void hart_filter_prop(const struct fdt_scan_prop *prop, void *extra)
{
    struct hart_filter *filter = (struct hart_filter *)extra;
    if (!strcmp(prop->name, "device_type") && !strcmp((const char*)prop->value, "cpu")) {
        filter->compat = 1;
    } else if (!strcmp(prop->name, "reg")) {
        uint64_t reg;
        fdt_get_address(prop->node->parent, prop->value, &reg);
        filter->hart = reg;
    } else if (!strcmp(prop->name, "status")) {
        filter->status = (char*)prop->value;
    }
}

static void hart_filter_done(const struct fdt_scan_node *node, void *extra)
{
    struct hart_filter *filter = (struct hart_filter *)extra;

    if (!filter->compat) {
        return;
    }
    assert (filter->status);
    assert (filter->hart >= 0);

    if (((filter->mask >> filter->hart) & 1) && !strcmp(filter->status, "okay")) {
        strcpy(filter->status, "masked");
        uint32_t *len = (uint32_t*)filter->status;
        len[-2] = bswap(strlen("masked") + 1);
    }
}

void filter_harts(uintptr_t fdt, unsigned long hart_mask)
{
    struct fdt_cb cb;
    struct hart_filter filter;

    memset(&cb, 0, sizeof(cb));
    cb.open = hart_filter_open;
    cb.prop = hart_filter_prop;
    cb.done = hart_filter_done;
    cb.extra = &filter;

    filter.mask = hart_mask;
    fdt_scan(fdt, &cb);
}

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

void fdt_print_printm(struct fdt_print_info *info, const char *format, ...)
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
        for (size_t i = 0; i < prop->len; i += strlen(char_data + i) + 1) {
            if (i != 0) {
                printf(", ");
            }
            printf("\"%s\"", char_data + i);
        }
    } else {
        printf("<");
        for (size_t i = 0; i < prop->len / 4; ++i) {
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
    struct fdt_print_info *info = (struct fdt_print_info *)extra;
}

static int fdt_print_close(const struct fdt_scan_node *node, void *extra)
{
    struct fdt_print_info *info = (struct fdt_print_info *)extra;
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
