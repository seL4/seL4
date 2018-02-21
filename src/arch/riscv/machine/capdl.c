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

#ifdef DEBUG

#include <object/structures.h>
#include <object/tcb.h>
#include <model/statedata.h>
#include <machine/capdl.h>
#include <arch/machine/capdl.h>
#include <plat/machine/debug_helpers.h>
#include <plat/machine/hardware.h>

static int getDecodedChar(unsigned char *result)
{
}

static void putEncodedChar(unsigned char c)
{
}

static int getArg32(unsigned int *res)
{
}

static void sendWord(unsigned int word)
{
}

static cte_t *getMDBParent(cte_t *slot)
{
}

static void sendPD(unsigned int address)
{
}

static void sendPT(unsigned int address)
{
}

static void sendASIDPool(unsigned int address)
{
}

static void sendRunqueues(void)
{
}

static void sendEPQueue(unsigned int epptr)
{
}

static void sendCNode(unsigned int address, unsigned int sizebits)
{
}

static void sendIRQNode(void)
{
}

static void sendVersion(void)
{
}

void capDL(void)
{
}

#endif
