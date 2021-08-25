/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <object.h>

void Arch_switchToThread(tcb_t *tcb);
void Arch_switchToIdleThread(void);
void Arch_configureIdleThread(tcb_t *tcb);
void Arch_activateIdleThread(tcb_t *tcb);
word_t sanitiseRegister(register_t reg, word_t v, bool_t archInfo);

static inline bool_t CONST Arch_getSanitiseRegisterInfo(tcb_t *thread)
{
    return 0;
}

void Mode_postModifyRegisters(tcb_t *tptr);


