/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __API_FAULTS_H
#define __API_FAULTS_H

#include <api/failures.h>
#include <object.h>
#include <types.h>

static inline void
copyMessageToRegisters(tcb_t *sender, tcb_t *receiver, const register_t message[], word_t length)
{
    /* set mrs that are passed in registers */
    int i;
    for (i = 0; i < MIN(length, n_msgRegisters); i++) {
        register_t r = message[i];
        word_t v = getRegister(sender, msgRegisters[i]);
        setRegister(receiver, r, sanitiseRegister(r, v));
    }

    /* set mrs that are passed in ipc buffer */
    word_t *sendBuf = lookupIPCBuffer(false, sender);
    if (i < length && sendBuf) {
        for (; i < length; i++) {
            register_t r = message[i];
            word_t v = sendBuf[i + 1];
            setRegister(receiver, r, sanitiseRegister(r, v));
        }
    }
}

bool_t handleFaultReply(tcb_t *receiver, tcb_t *sender);

#endif
