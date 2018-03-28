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

// RVTODO: what is this? why is nothing implemented?

#include <stdint.h>
#include <util.h>
#include <machine/io.h>
#include <model/statedata.h>

typedef uint32_t vector_t;
typedef void (*break_handler_t)(user_context_t *context);
typedef void (*catch_handler_t)(user_context_t *context, vector_t vector);

void
debug_init(void)
{
}

void
software_breakpoint(uint32_t va, user_context_t *context)
{
}

void
breakpoint_multiplexer(uint32_t va, user_context_t *context)
{
}

int
set_breakpoint(uint32_t va, break_handler_t handler)
{
    return 0;
}

void
clear_breakpoint(uint32_t va)
{
}

catch_handler_t catch_handler VISIBLE;

void
set_catch_handler(catch_handler_t handler)
{
}

void
catch_vector(vector_t vector)
{
}

void
uncatch_vector(vector_t vector)
{
}
