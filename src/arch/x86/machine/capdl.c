/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <config.h>
#include <object/structures.h>
#include <object/tcb.h>
#include <model/statedata.h>
#include <machine/capdl.h>
#include <arch/machine/capdl.h>
#include <machine/io.h>

#ifdef CONFIG_DEBUG_BUILD

#define ARCH 0xe1

#define PD_READ_SIZE         BIT(PD_INDEX_BITS)
#define PT_READ_SIZE         BIT(PT_INDEX_BITS)
#define ASID_POOL_READ_SIZE  BIT(ASID_POOL_INDEX_BITS)
#define IO_PT_READ_SIZE      BIT(VTD_PT_INDEX_BITS)

static int getDecodedChar(unsigned char *result)
{
    unsigned char c;
    c = getDebugChar();
    if (c == START) {
        return 1;
    }
    if (c == ESCAPE) {
        c = getDebugChar();
        if (c == START) {
            return 1;
        }
        switch (c) {
        case ESCAPE_ESCAPE:
            *result = ESCAPE;
            break;
        case START_ESCAPE:
            *result = START;
            break;
        case END_ESCAPE:
            *result = END;
            break;
        default:
            if (c >= 20 && c < 40) {
                *result = c - 20;
            } else {
                return 1;
            }
        }
        return 0;
    } else {
        *result = c;
        return 0;
    }
}

static void putEncodedChar(unsigned char c)
{
    switch (c) {
    case ESCAPE:
        putDebugChar(ESCAPE);
        putDebugChar(ESCAPE_ESCAPE);
        break;
    case START:
        putDebugChar(ESCAPE);
        putDebugChar(START_ESCAPE);
        break;
    case END:
        putDebugChar(ESCAPE);
        putDebugChar(END_ESCAPE);
        break;
    default:
        if (c < 20) {
            putDebugChar(ESCAPE);
            putDebugChar(c + 20);
        } else {
            putDebugChar(c);
        }
    }
}

static int getArg(unsigned long *res)
{
    unsigned long i;
    unsigned char byte;
    *res = 0;
    for (i = 0; i < sizeof(unsigned long); i++) {
        if (getDecodedChar(&byte)) {
            return 1;
        }
        (*res) = ((*res) << 8) | byte;
    }
    return 0;
}

static void sendWord(unsigned long word)
{
    unsigned long i;
    for (i = 0; i < sizeof(unsigned long); i++) {
        putEncodedChar((word >> (i * 8)) & 0xff);
    }
}

static cte_t *getMDBParent(cte_t *slot)
{
    cte_t *oldSlot = CTE_PTR(mdb_node_get_mdbPrev(slot->cteMDBNode));

    while (oldSlot != 0 && !isMDBParentOf(oldSlot, slot)) {
        oldSlot = CTE_PTR(mdb_node_get_mdbPrev(oldSlot->cteMDBNode));
    }

    return oldSlot;
}

static void sendPD(unsigned long address)
{
    unsigned long i;
    unsigned int exists;
    pde_t *start = (pde_t *)address;
    for (i = 0; i < PD_READ_SIZE; i++) {
        pde_t pde = start[i];
        exists = 1;
        if (pde_get_page_size(pde) == pde_pde_pt && (pde_pde_pt_get_pt_base_address(pde) == 0 ||
                                                     !pde_pde_pt_get_present(pde) || !pde_pde_pt_get_super_user(pde))) {
            exists = 0;
        } else if (pde_get_page_size(pde) == pde_pde_large && (pde_pde_large_get_page_base_address(pde) == 0 ||
                                                               !pde_pde_large_get_present(pde) || !pde_pde_large_get_super_user(pde))) {
            exists = 0;
        }
        if (exists != 0 && i < PPTR_BASE >> pageBitsForSize(X86_LargePage)) {
            sendWord(i);
            sendWord(pde.words[0]);
        }
    }
}

static void sendPT(unsigned long address)
{
    unsigned long i;
    pte_t *start = (pte_t *)address;
    for (i = 0; i < PT_READ_SIZE; i++) {
        pte_t pte = start[i];
        if (pte_get_page_base_address(pte) != 0 && pte_get_present(pte) && pte_get_super_user(pte)) {
            sendWord(i);
            sendWord(pte.words[0]);
        }
    }
}

static void sendASIDPool(unsigned long address)
{
    unsigned long i;
    pde_t **start = (pde_t **)address;
    for (i = 0; i < ASID_POOL_READ_SIZE; i++) {
        pde_t *pde = start[i];
        if (pde != 0) {
            sendWord(i);
            sendWord((unsigned long)pde);
        }
    }
}

#ifdef CONFIG_IOMMU
static void sendIOPT(unsigned long address, unsigned int level)
{
    unsigned long i;
    vtd_pte_t *start = (vtd_pte_t *)address;
    for (i = 0; i < IO_PT_READ_SIZE; i++) {
        vtd_pte_t vtd_pte = start[i];
        if (vtd_pte_get_addr(vtd_pte) != 0) {
            sendWord(i);
            sendWord(vtd_pte.words[0]);
#ifdef CONFIG_ARCH_IA32
            sendWord(vtd_pte.words[1]);
#endif
            if (level == x86KSnumIOPTLevels) {
                sendWord(1);
            } else {
                sendWord(0);
            }
        }
    }
}

static void sendIOSpace(uint32_t pci_request_id)
{
    uint32_t   vtd_root_index;
    uint32_t   vtd_context_index;
    vtd_rte_t *vtd_root_slot;
    vtd_cte_t *vtd_context;
    vtd_cte_t *vtd_context_slot;

    vtd_root_index = get_pci_bus(pci_request_id);
    vtd_root_slot = x86KSvtdRootTable + vtd_root_index;

    vtd_context = (vtd_cte_t *)paddr_to_pptr(vtd_rte_ptr_get_ctp(vtd_root_slot));
    vtd_context_index = (get_pci_dev(pci_request_id) << 3) | get_pci_fun(pci_request_id);
    vtd_context_slot = &vtd_context[vtd_context_index];

    if (vtd_cte_ptr_get_present(vtd_context_slot)) {
        sendWord(vtd_cte_ptr_get_asr(vtd_context_slot));
    } else {
        sendWord(0);
    }
}
#endif

static void sendRunqueues(void)
{
    word_t i;
    sendWord((unsigned long)NODE_STATE(ksCurThread));
    for (i = 0; i < NUM_READY_QUEUES; i++) {
        tcb_t *current = NODE_STATE(ksReadyQueues[i]).head;
        if (current != 0) {
            while (current != NODE_STATE(ksReadyQueues[i]).end) {
                sendWord((unsigned long)current);
                current = current -> tcbSchedNext;
            }
            sendWord((unsigned long)current);
        }
    }
}

static void sendEPQueue(unsigned long epptr)
{
    tcb_t *current = (tcb_t *)endpoint_ptr_get_epQueue_head((endpoint_t *)epptr);
    tcb_t *tail = (tcb_t *)endpoint_ptr_get_epQueue_tail((endpoint_t *)epptr);
    if (current == 0) {
        return;
    }
    while (current != tail) {
        sendWord((unsigned long)current);
        current = current->tcbEPNext;
    }
    sendWord((unsigned long)current);
}

static void sendCNode(unsigned long address, unsigned int sizebits)
{
    unsigned long i;
    cte_t *start = (cte_t *)address;
    for (i = 0; i < (1 << sizebits); i++) {
        cap_t cap = start[i].cap;
        if (cap_get_capType(cap) != cap_null_cap) {
            cte_t *parent = getMDBParent(&start[i]);
            sendWord(i);
            sendWord(cap.words[0]);
            sendWord(cap.words[1]);
            sendWord((unsigned long)parent);
        }
    }
}

static void sendIRQNode(void)
{
    sendCNode((unsigned long)intStateIRQNode, 8);
}

static void sendVersion(void)
{
    sendWord(ARCH);
    sendWord(CAPDL_VERSION);
}

void capDL(void)
{
    int result;
    int done = 0;
    while (done == 0) {
        unsigned char c;
        do {
            c = getDebugChar();
        } while (c != START);
        do {
            result = getDecodedChar(&c);
            if (result) {
                continue;
            }
            switch (c) {
            case PD_COMMAND: {
                /*pgdir */
                unsigned long arg;
                result = getArg(&arg);
                if (result) {
                    continue;
                }
                sendPD(arg);
                putDebugChar(END);
            }
            break;
            case PT_COMMAND: {
                /*pg table */
                unsigned long arg;
                result = getArg(&arg);
                if (result) {
                    continue;
                }
                sendPT(arg);
                putDebugChar(END);
            }
            break;
            case ASID_POOL_COMMAND: {
                /*asid pool */
                unsigned long arg;
                result = getArg(&arg);
                if (result) {
                    continue;
                }
                sendASIDPool(arg);
                putDebugChar(END);
            }
            break;
#ifdef CONFIG_IOMMU
            case IO_PT_COMMAND: {
                /*io pt table */
                unsigned long address, level;
                result = getArg(&address);
                if (result) {
                    continue;
                }
                result = getArg(&level);
                if (result) {
                    continue;
                }
                sendIOPT(address, level);
                putDebugChar(END);
            }
            break;
            case IO_SPACE_COMMAND: {
                /*io space */
                unsigned long arg;
                result = getArg(&arg);
                if (result) {
                    continue;
                }
                sendIOSpace(arg);
                putDebugChar(END);
            }
#endif
            break;
            case RQ_COMMAND: {
                /*runqueues */
                sendRunqueues();
                putDebugChar(END);
                result = 0;
            }
            break;
            case EP_COMMAND: {
                /*endpoint waiters */
                unsigned long arg;
                result = getArg(&arg);
                if (result) {
                    continue;
                }
                sendEPQueue(arg);
                putDebugChar(END);
            }
            break;
            case CN_COMMAND: {
                /*cnode */
                unsigned long address, sizebits;
                result = getArg(&address);
                if (result) {
                    continue;
                }
                result = getArg(&sizebits);
                if (result) {
                    continue;
                }

                sendCNode(address, sizebits);
                putDebugChar(END);
            }
            break;
            case IRQ_COMMAND: {
                sendIRQNode();
                putDebugChar(END);
                result = 0;
            }
            break;
            case VERSION_COMMAND: {
                sendVersion();
                putDebugChar(END);
            }
            break;
            case DONE: {
                done = 1;
                putDebugChar(END);
            }
            default:
                result = 0;
                break;
            }
        } while (result);
    }
}

#endif
