/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#include <stdint.h>
#include <util.h>
#include <arch/machine/debug.h>
#include <machine/io.h>
#include <model/statedata.h>

typedef struct {
    uint32_t va;
    break_handler_t handler;
} breakpoint_t;

breakpoint_t breakpoints[MAX_BREAKPOINTS] VISIBLE;
int n_breakpoints;

void
debug_init(void)
{
    uint32_t didr;
    int version, variant, revision;
    unsigned int i;

    didr = getDIDR();
    n_breakpoints = ((didr >> DIDR_BRP_OFFSET) & MASK(DIDR_BRP_SIZE)) + 1;
    version = (didr >> DIDR_VERSION_OFFSET) & MASK(DIDR_VERSION_SIZE);
    variant = (didr >> DIDR_VARIANT_OFFSET) & MASK(DIDR_VARIANT_SIZE);
    revision = (didr >> DIDR_REVISION_OFFSET) & MASK(DIDR_REVISION_SIZE);

    printf("debug_init:  variant %d  revision %d  debug version %d\n",
           variant, revision, version);
    printf("debug_init:  breakpoint registers %d\n", n_breakpoints);

    /* Enable monitor mode debugging */
    setDSCR(BIT(DSCR_MONITOR_MODE_ENABLE));

    /* Disable all breakpoints and vector catch */
    for (i = 0; i < n_breakpoints; i++) {
        breakpoints[i].handler = 0;
        setBCR(i, 0);
    }
    setVCR(0);
}

void
software_breakpoint(uint32_t va, user_context_t *context)
{
    unsigned int i;

    printf("Software breakpoint at %x, context:\n", (unsigned int)va);
    for (i = 0; i < 10; i++) {
        printf("r%d  %x\n", i, (unsigned int)context->registers[i]);
    }
    for (i = 10; i < 15; i++) {
        printf("r%d %x\n", i, (unsigned int)context->registers[i]);
    }
    printf("LR_abt %x\n", (unsigned int)context->registers[15]);
    printf("CPSR %x\n", (unsigned int)context->registers[16]);

    printf("ksCurThread context:\n");
    for (i = 0; i < 10; i++) {
        printf("r%d  %x\n", i,
               (unsigned int)ksCurThread->tcbContext.registers[i]);
    }
    for (i = 10; i < 15; i++) {
        printf("r%d %x\n", i,
               (unsigned int)ksCurThread->tcbContext.registers[i]);
    }
    printf("LR_abt %x\n", (unsigned int)ksCurThread->tcbContext.registers[15]);
    printf("CPSR %x\n", (unsigned int)ksCurThread->tcbContext.registers[16]);
}

void
breakpoint_multiplexer(uint32_t va, user_context_t *context)
{
    unsigned int i;

    for (i = 0; i < n_breakpoints && (breakpoints[i].va != va ||
                                      !breakpoints[i].handler); i++);

    if (i == n_breakpoints) {
        printf("Unhandled breakpoint @ %x\n", (unsigned int)va);
    } else {
        breakpoints[i].handler(context);
    }
}

int
set_breakpoint(uint32_t va, break_handler_t handler)
{
    unsigned int i;

    for (i = 0; i < n_breakpoints && breakpoints[i].handler; i++);

    if (i == n_breakpoints) {
        return -1;
    }

    breakpoints[i].va = va;
    breakpoints[i].handler = handler;

    /* Set breakpoint address */
    setBVR(i, va);

    /* Set breakpoint control for full word, user and supervisor and enabled */
    setBCR(i, (0xf << BCR_BYTE_SELECT) |
           (0x3 << BCR_SUPERVISOR) |
           BIT(BCR_ENABLE));

    return i;
}

void
clear_breakpoint(uint32_t va)
{
    unsigned int i;

    for (i = 0; i < n_breakpoints; i++) {
        if (breakpoints[i].va == va && breakpoints[i].handler) {
            /* Disable breakpoint */
            setBCR(i, 0);
            breakpoints[i].handler = 0;
        }
    }
}

catch_handler_t catch_handler VISIBLE;

void
set_catch_handler(catch_handler_t handler)
{
    catch_handler = handler;
}

void
catch_vector(vector_t vector)
{
    uint32_t vcr;

    vcr = getVCR();
    vcr |=  BIT(vector);
    setVCR(vcr);
}

void
uncatch_vector(vector_t vector)
{
    uint32_t vcr;

    vcr = getVCR();
    vcr &=  ~BIT(vector);
    setVCR(vcr);
}
