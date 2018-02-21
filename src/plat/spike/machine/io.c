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
#include <arch/kernel/traps.h>
#include <machine/fpu.h>
#include <arch/machine/fpu.h>

#define KNRM  "\x1B[0m"
#define KRED  "\x1B[31m"
#define KGRN  "\x1B[32m"
#define KYEL  "\x1B[33m"
#define KBLU  "\x1B[34m"
#define KMAG  "\x1B[35m"
#define KCYN  "\x1B[36m"
#define KWHT  "\x1B[37m"

static inline void print_format_cause(int cause_num)
{
    switch (cause_num) {
    case 0:
        printf("Instruction address misaligned\n");
        break;
    case 1:
        printf("Instruction access fault\n");
        break;
    case 2:
        printf("Illegal instruction\n");
        break;
    case 3:
        printf("Breakpoint\n");
        break;
    case 5:
        printf("Load access fault\n");
        break;
    case 6:
        printf("AMO address misaligned\n");
        break;
    case 7:
        printf("Store/AMO access fault\n");
        break;
    case 8:
        printf("Environment call\n");
        break;
    default:
        printf("Reserved cause %d\n", cause_num);
        break;
    }
}

void handle_exception(void)
{
    word_t scause = read_csr(scause);

    /* handleVMFaultEvent
     * 1 -> Instruction access fault
     * 5 -> Load access fault
     * 7 -> Store/AMO access fault
     * 12 -> Instruction page fault
     * 13 -> Load page fault
     * 15 -> Store/AMO page fault
     * */
    switch (scause) {
    case 1:
    case 5:
    case 7:
    case 12:
    case 13:
    case 15:
        handleVMFaultEvent(scause);
        break;
    case 2:
#if defined(CONFIG_HAVE_FPU)
        /* We assume the first fault is a FP exception and enable FPU, if not already enabled */
        if (!isFpuEnable()) {
            handleFPUFault();

            /* Restart the FP instruction that caused the fault */
            setNextPC(NODE_STATE(ksCurThread), getRestartPC(NODE_STATE(ksCurThread)));
        } else {
            handleUserLevelFault(0, 0);
        }
#else
        handleUserLevelFault(0, 0);
#endif

        break;
    default:
        print_format_cause(read_csr(scause));
        printf("sepc = %p\n", read_csr(sepc));
        printf("sbadaddr = %p\n", read_csr(sbadaddr));
        printf("sstatus = %p\n", read_csr(sstatus));
        printf(KRED "Halt!\n");

        printf(KBLU"Register Context Dump \n");

        register word_t thread_context asm("t0");
        for (int i = 0; i < 32; i++) {
            printf("x%d = %p\n", i, *(((uint64_t *) thread_context) + i));
        }

        halt();

    }
}

#ifdef CONFIG_PRINTING
void
qemu_uart_putchar(char c)
{
}

unsigned char getDebugChar(void)
{
}

void putDebugChar(unsigned char c)
{
    sbi_console_putchar(c);
}

void
putConsoleChar(unsigned char c)
{
    putDebugChar(c);
}
#endif
