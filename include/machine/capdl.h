/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#ifdef CONFIG_DEBUG_BUILD

/* helpers */
void add_to_seen(cap_t c);
void reset_seen_list(void);
bool_t seen(cap_t c);
bool_t same_cap(cap_t a, cap_t b);
bool_t root_or_idle_tcb(tcb_t *tcb);
word_t get_tcb_sp(tcb_t *tcb);

/* common */
void debug_capDL(void);

#endif /* CONFIG_DEBUG_BUILD */

#if defined(CONFIG_DEBUG_BUILD) && defined(CONFIG_PRINTING)

void obj_tcb_print_cnodes(cap_t cnode, tcb_t *tcb);
void print_caps(void);
void print_objects(void);
void print_cap(cap_t cap);
void print_object(cap_t cap);

void obj_tcb_print_attrs(tcb_t *tcb);
void obj_sc_print_attrs(cap_t sc);
void obj_cnode_print_attrs(cap_t cnode);
void obj_ut_print_attrs(cte_t *slot, tcb_t *tcb);

void obj_tcb_print_slots(tcb_t *tcb);
void obj_cnode_print_slots(tcb_t *tcb);
void obj_irq_print_slots(cap_t irq_cap);
void obj_irq_print_maps(void);

void cap_ep_print_attrs(cap_t ep);
void cap_ntfn_print_attrs(cap_t ntfn);
void cap_cnode_print_attrs(cap_t cnode);

/* arch specific functions */
void print_ipc_buffer_slot(tcb_t *tcb);
/* TBD: currently the capDL extractor declaring an object for every entry in the vspace.
 * However, frames can be mapped into multiple locations but sould only be declared once.
 */
void obj_vtable_print_slots(tcb_t *tcb);

void print_cap_arch(cap_t cap);
void print_object_arch(cap_t cap);
void obj_tcb_print_vtable(tcb_t *tcb);

#endif /* defined(CONFIG_DEBUG_BUILD) && defined(CONFIG_PRINTING) */
