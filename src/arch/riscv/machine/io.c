/*
 * Copyright 2018, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(DATA61_GPL)
 */

/*
 * Copyright (c) 2018, Hesham Almatary <Hesham.Almatary@cl.cam.ac.uk>
 * All rights reserved.
 *
 * This software was was developed in part by SRI International and the University of
 * Cambridge Computer Laboratory (Department of Computer Science and
 * Technology) under DARPA contract HR0011-18-C-0016 ("ECATS"), as part of the
 * DARPA SSITH research programme.
 */

/*
 *
 * Copyright 2016, 2017 Hesham Almatary, Data61/CSIRO <hesham.almatary@data61.csiro.au>
 * Copyright 2015, 2016 Hesham Almatary <heshamelmatary@gmail.com>
 */

#include <stdint.h>
#include <util.h>
#include <machine/io.h>
#include <plat/machine/devices.h>
#include <arch/machine.h>
#include <arch/sbi.h>
#include <arch/encoding.h>
#include <arch/kernel/traps.h>

static inline void print_format_cause(int cause_num)
{
    switch (cause_num) {
    case RISCVInstructionMisaligned:
        printf("Instruction address misaligned\n");
        break;
    case RISCVInstructionAccessFault:
        printf("Instruction access fault\n");
        break;
    case RISCVInstructionIllegal:
        printf("Illegal instruction\n");
        break;
    case RISCVBreakpoint:
        printf("Breakpoint\n");
        break;
    case RISCVLoadAccessFault:
        printf("Load access fault\n");
        break;
    case RISCVAddressMisaligned:
        printf("AMO address misaligned\n");
        break;
    case RISCVStoreAccessFault:
        printf("Store/AMO access fault\n");
        break;
    case RISCVEnvCall:
        printf("Environment call\n");
        break;
    case RISCVInstructionPageFault:
        printf("Instruction page fault\n");
        break;
    case RISCVLoadPageFault:
        printf("Load page fault\n");
        break;
    case RISCVStorePageFault:
        printf("Store page fault\n");
        break;
    default:
        printf("Reserved cause %d\n", cause_num);
        break;
    }
}

void handle_exception(void)
{
    word_t cause = read_csr_env(cause);

    /* handleVMFaultEvent
     * */
    switch (cause) {
    case RISCVInstructionAccessFault:
    case RISCVLoadAccessFault:
    case RISCVStoreAccessFault:
    case RISCVInstructionPageFault:
    case RISCVLoadPageFault:
    case RISCVStorePageFault:
        handleVMFaultEvent(cause);
        break;
    case RISCVInstructionIllegal:
        handleUserLevelFault(0, 0);

        break;
    default:
        print_format_cause(read_csr_env(cause));
        printf("epc = %p\n", (void*)(word_t)read_csr_env(epc));
        printf("badaddr = %p\n", (void*)(word_t)read_csr_env(badaddr));
        printf("status = %p\n", (void*)(word_t)read_csr_env(status));
        printf("Halt!\n");

        printf("Register Context Dump \n");

        register word_t thread_context asm("t0");
        for (int i = 0; i < 32; i++) {
            printf("x%d = %p\n", i, (void*) * (((word_t *) thread_context) + i));
        }

        halt();

    }
}

#ifdef CONFIG_PRINTING
void
putConsoleChar(unsigned char c)
{
    sbi_console_putchar(c);
}
#endif
