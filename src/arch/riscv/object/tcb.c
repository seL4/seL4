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

#include <types.h>
#include <api/failures.h>
#include <machine/registerset.h>
#include <object/structures.h>
#include <arch/machine.h>
#include <object/tcb.h>

word_t CONST
Arch_decodeTransfer(word_t flags)
{
    return 0;
}

exception_t CONST
Arch_performTransfer(word_t arch, tcb_t *tcb_src, tcb_t *tcb_dest)
{
    return EXCEPTION_NONE;
}

void
Arch_setTCBIPCBuffer(tcb_t *thread, word_t bufferAddr)
{
    setRegister(thread, tp, bufferAddr);
}
