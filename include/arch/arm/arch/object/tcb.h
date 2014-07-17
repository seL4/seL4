/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __ARCH_OBJECT_TCB_H
#define __ARCH_OBJECT_TCB_H

#include <types.h>
#include <object/structures.h>

unsigned int setMRs_fault(tcb_t *sender, tcb_t* receiver,
                          word_t *receiveIPCBuffer);
unsigned int setMRs_syscall_error(tcb_t *thread, word_t *receiveIPCBuffer);
word_t CONST Arch_decodeTransfer(word_t flags);
exception_t CONST Arch_performTransfer(word_t arch, tcb_t *tcb_src,
                                       tcb_t *tcb_dest);

#endif
