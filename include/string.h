/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __STRING_H
#define __STRING_H

#ifdef DEBUG

#include <stdint.h>

unsigned int strnlen(const char *s, unsigned int maxlen);
unsigned int strlcpy(char *dest, const char *src, unsigned int size);
unsigned int strlcat(char *dest, const char *src, unsigned int size);

#endif

#endif
