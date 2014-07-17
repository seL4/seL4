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

bool_t handleFaultReply(tcb_t *receiver, tcb_t *sender);

#endif
