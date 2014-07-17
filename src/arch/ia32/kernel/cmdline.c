/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#include <config.h>
#include <util.h>
#include <machine/io.h>
#include <arch/kernel/cmdline.h>
#include <arch/kernel/boot_sys.h>
#include <arch/linker.h>

/* 'cmdline_val' is declared globally because of a C-subset restriction.
 * It is only used in cmdline_parse(), which therefore is non-reentrant.
 */
#define MAX_CMDLINE_VAL_LEN 1000
BOOT_DATA_GLOB
char cmdline_val[MAX_CMDLINE_VAL_LEN];

/* workaround because string literals are not supported by C parser */
const char cmdline_str_max_num_nodes[]  = {'m', 'a', 'x', '_', 'n', 'u', 'm', '_', 'n', 'o', 'd', 'e', 's', 0};
const char cmdline_str_num_sh_frames[]  = {'n', 'u', 'm', '_', 's', 'h', '_', 'f', 'r', 'a', 'm', 'e', 's', 0};
const char cmdline_str_disable_iommu[]  = {'d', 'i', 's', 'a', 'b', 'l', 'e', '_', 'i', 'o', 'm', 'm', 'u', 0};

static int is_space(char c)
{
    return c <= ' ';
}

static int parse_opt(const char *cmdline, const char *opt, char *value, int bufsize)
{
    int len = -1;
    const char *optptr = NULL;

    while (true) {
        for (; is_space(*cmdline) && (*cmdline != 0); cmdline++);
        if (*cmdline == 0) {
            break;
        }

        for (optptr = opt; *optptr && *cmdline && (*cmdline != '=') && !is_space(*cmdline) && (*optptr == *cmdline); optptr++, cmdline++);

        if (*optptr == '\0' && *cmdline == '=') {
            cmdline++;

            for (len = 0; !is_space(*cmdline) && (len < bufsize - 1); cmdline++, len++) {
                value[len] = *cmdline;
            }
            if (bufsize) {
                value[len] = '\0';
            }
        }
        for (; !is_space(*cmdline); cmdline++);
    }

    return len;
}

#ifdef CONFIG_IOMMU
static int parse_bool(const char *cmdline, const char *opt)
{
    const char *optptr = NULL;

    while (1) {
        for (; is_space(*cmdline) && (*cmdline != 0); cmdline++);
        if (*cmdline == 0) {
            return 0;
        }

        for (optptr = opt; *optptr && *cmdline && !is_space(*cmdline) && (*optptr == *cmdline); optptr++, cmdline++);

        if (*optptr == '\0' && is_space(*cmdline)) {
            return 1;
        } else {
            for (; !is_space(*cmdline); cmdline++);
        }
    }
}
#endif

#ifdef DEBUG
static void parse_uint16_array(char* str, uint16_t* array, int array_size)
{
    char* last;
    int   i = 0;
    int   v;

    while (str && i < array_size) {
        for (last = str; *str && *str != ','; str++);
        if (*str == 0) {
            str = 0;
        } else {
            *str = 0;
            str++;
        }
        v = str_to_int(last);
        if (v == -1) {
            array[i] = 0;
        } else {
            array[i] = v;
        }
        i++;
    }
}
#endif

void cmdline_parse(const char *cmdline, cmdline_opt_t* cmdline_opt)
{
    int  i;

#ifdef DEBUG
    /* initialise to default */
    for (i = 0; i < CONFIG_MAX_NUM_NODES; i++) {
        cmdline_opt->console_port[i] = 0;
        cmdline_opt->debug_port[i] = 0;
    }
    cmdline_opt->console_port[0] = 0x3f8;
    cmdline_opt->debug_port[0] = 0x3f8;

    if (parse_opt(cmdline, "console_port", cmdline_val, MAX_CMDLINE_VAL_LEN) != -1) {
        parse_uint16_array(cmdline_val, cmdline_opt->console_port, CONFIG_MAX_NUM_NODES);
    }

    /* initialise console ports to enable debug output */
    for (i = 0; i < CONFIG_MAX_NUM_NODES; i++) {
        if (cmdline_opt->console_port[i]) {
            serial_init(cmdline_opt->console_port[i]);
        }
    }

    /* only start printing here after having parsed/set/initialised the console_port */
    printf("\nBoot config: parsing cmdline '%s'\n", cmdline);

    for (i = 0; i < CONFIG_MAX_NUM_NODES; i++)
        if (cmdline_opt->console_port[i]) {
            printf("Boot config: console_port of node #%d = 0x%x\n", i, cmdline_opt->console_port[i]);
        }

    if (parse_opt(cmdline, "debug_port", cmdline_val, MAX_CMDLINE_VAL_LEN) != -1) {
        parse_uint16_array(cmdline_val, cmdline_opt->debug_port, CONFIG_MAX_NUM_NODES);
    }

    /* initialise debug ports */
    for (i = 0; i < CONFIG_MAX_NUM_NODES; i++) {
        if (cmdline_opt->debug_port[i]) {
            serial_init(cmdline_opt->debug_port[i]);
            printf("Boot config: debug_port of node #%d = 0x%x\n", i, cmdline_opt->debug_port[i]);
        }
    }
#endif

#ifdef CONFIG_IOMMU
    cmdline_opt->disable_iommu = parse_bool(cmdline, cmdline_str_disable_iommu);
    printf("Boot config: disable_iommu = %s\n", cmdline_opt->disable_iommu ? "true" : "false");
#endif

    /* parse max_num_nodes option */
    cmdline_opt->max_num_nodes = 1; /* default */
    if (parse_opt(cmdline, cmdline_str_max_num_nodes, cmdline_val, MAX_CMDLINE_VAL_LEN) != -1) {
        i = str_to_int(cmdline_val);
        if (i > 0 && i <= CONFIG_MAX_NUM_NODES) {
            cmdline_opt->max_num_nodes = i;
        }
    }
    printf("Boot config: max_num_nodes = %d\n", cmdline_opt->max_num_nodes);

    /* parse num_sh_frames option */
    cmdline_opt->num_sh_frames = 0; /* default */
    if (parse_opt(cmdline, cmdline_str_num_sh_frames, cmdline_val, MAX_CMDLINE_VAL_LEN) != -1) {
        i = str_to_int(cmdline_val);
        if (i >= 0 && i < BIT(32 - PAGE_BITS)) {
            cmdline_opt->num_sh_frames = i;
        }
    }
    printf("Boot config: num_sh_frames = 0x%x\n", cmdline_opt->num_sh_frames);
}
