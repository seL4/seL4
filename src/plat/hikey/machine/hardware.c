/*
 * Copyright 2016, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#include <config.h>
#include <types.h>
#include <arch/machine.h>
#include <arch/machine/timer.h>
#include <arch/linker.h>

/**
   DONT_TRANSLATE
 */
void
resetTimer(void)
{
    resetGenericTimer();
}

/**
   DONT_TRANSLATE
 */
BOOT_CODE void
initTimer(void)
{
    initGenericTimer();
}

void
initL2Cache(void)
{

}

void plat_cleanL2Range(paddr_t start, paddr_t end) {}
void plat_invalidateL2Range(paddr_t start, paddr_t end) {}
void plat_cleanInvalidateL2Range(paddr_t start, paddr_t end) {}
void plat_cleanInvalidateCache(void) {}
