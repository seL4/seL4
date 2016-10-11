/*
 * Copyright 2016, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(D61_GPL)
 */

#pragma once

#include <config.h>

#if CONFIG_MAX_NUM_NODES > 1
#define NODE_STATE_BEGIN(_name)                 typedef struct _name {
#define NODE_STATE_END(_name)                   } _name ## _t
#define NODE_STATE_TYPE_DECLARE(_name, _state)  _name ## _t _state
#define NODE_STATE_DECLARE(_type, _state)       _type _state

#define SMP_STATE_DEFINE(_type, _state)         _type _state
#define UP_STATE_DEFINE(_type, _state)

#define SMP_COND_STATEMENT(_st)                 _st
#else

#define NODE_STATE_BEGIN(_name)
#define NODE_STATE_END(_name)
#define NODE_STATE_TYPE_DECLARE(_name, _state)
/* UP states are declared as VISIBLE so that they are accessible in assembly */ 
#define NODE_STATE_DECLARE(_type, _state)       extern _type _state VISIBLE

#define SMP_STATE_DEFINE(_name, _state)
#define UP_STATE_DEFINE(_type, _state)          _type _state

#define SMP_COND_STATEMENT(_st)

#endif /* CONFIG_MAX_NUM_NODES */
