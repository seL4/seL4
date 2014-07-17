/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __ARCH_OBJECT_STRUCTURES_H
#define __ARCH_OBJECT_STRUCTURES_H

#include <assert.h>
#include <config.h>
#include <util.h>
#include <api/types.h>
#include <arch/types.h>
#include <arch/object/structures_gen.h>
#include <arch/machine/hardware.h>
#include <arch/machine/registerset.h>


/* Arch-independent object types */

enum endpoint_state {
    EPState_Idle = 0,
    EPState_Send = 1,
    EPState_Recv = 2
};
typedef uint32_t endpoint_state_t;

#define EP_SIZE_BITS 4
#define EP_PTR(r) ((endpoint_t *)r)
#define EP_REF(p) ((unsigned int)p)

enum async_endpoint_state {
    AEPState_Idle    = 0,
    AEPState_Waiting = 1,
    AEPState_Active  = 2
};
typedef uint32_t async_endpoint_state_t;

#define AEP_SIZE_BITS 4
#define AEP_PTR(r) ((async_endpoint_t *)r)
#define AEP_REF(p) ((unsigned int)p)

#define CTE_SIZE_BITS 4
#define CTE_PTR(r) ((cte_t *)(r))
#define CTE_REF(p) ((unsigned int)p)

#define CNODE_MIN_BITS 1
#define CNODE_PTR(r) (CTE_PTR(r))
#define CNODE_REF(p) (CTE_REF(p)>>CNODE_MIN_BITS)

#define TCB_CNODE_RADIX     4
#define TCB_SIZE_BITS       (TCB_CNODE_RADIX + CTE_SIZE_BITS)
#define TCB_BLOCK_SIZE_BITS 10
#define TCB_OFFSET          (1 << TCB_SIZE_BITS)

/* Generate a tcb_t or cte_t pointer from a tcb block reference */
#define TCB_PTR(r)       ((tcb_t *)(r))
#define TCB_CTE_PTR(r,i) (((cte_t *)(r))+i)
#define TCB_REF(p)       ((unsigned int)p)

/* Generate a cte_t pointer from a tcb_t pointer */
#define TCB_PTR_CTE_PTR(p,i) (((cte_t *)((unsigned int)p&~MASK(TCB_BLOCK_SIZE_BITS)))+i)

#define ZombieType_ZombieTCB      BIT(5)
#define ZombieType_ZombieCNode(n) (n & MASK(5))

static inline cap_t CONST
Zombie_new(uint32_t number, uint32_t type, uint32_t ptr)
{
    uint32_t mask;

    if (type == ZombieType_ZombieTCB) {
        mask = MASK(TCB_CNODE_RADIX + 1);
    } else {
        mask = MASK(type + 1);
    }

    return cap_zombie_cap_new((ptr & ~mask) | (number & mask), type);
}

static inline uint32_t CONST
cap_zombie_cap_get_capZombieBits(cap_t cap)
{
    uint32_t type = cap_zombie_cap_get_capZombieType(cap);
    if (type == ZombieType_ZombieTCB) {
        return 3;
    }
    return type;
}

static inline uint32_t CONST
cap_zombie_cap_get_capZombieNumber(cap_t cap)
{
    uint32_t radix = cap_zombie_cap_get_capZombieBits(cap);
    return cap_zombie_cap_get_capZombieID(cap) & MASK(radix + 1);
}

static inline uint32_t CONST
cap_zombie_cap_get_capZombiePtr(cap_t cap)
{
    uint32_t radix = cap_zombie_cap_get_capZombieBits(cap);
    return cap_zombie_cap_get_capZombieID(cap) & ~MASK(radix + 1);
}

static inline cap_t CONST
cap_zombie_cap_set_capZombieNumber(cap_t cap, uint32_t n)
{
    uint32_t radix = cap_zombie_cap_get_capZombieBits(cap);
    uint32_t ptr = cap_zombie_cap_get_capZombieID(cap) & ~MASK(radix + 1);
    return cap_zombie_cap_set_capZombieID(cap, ptr | (n & MASK(radix + 1)));
}

/* Capability table entry (CTE): size = 16 bytes */
struct cte {
    cap_t cap;
    mdb_node_t cteMDBNode;
};
typedef struct cte cte_t;

#define nullMDBNode mdb_node_new(0, false, false, 0)

/* Thread state */
enum _thread_state {
    ThreadState_Inactive = 0,
    ThreadState_Running,
    ThreadState_Restart,
    ThreadState_BlockedOnReceive,
    ThreadState_BlockedOnSend,
    ThreadState_BlockedOnReply,
    ThreadState_BlockedOnAsyncEvent,
    ThreadState_IdleThreadState
};
typedef uint32_t _thread_state_t;

/* A TCB CNode and a TCB are always allocated together, and adjacently,
 * such that they fill a 1024-byte aligned block. The CNode comes first. */
enum tcb_cnode_index {
    /* CSpace root, 16 bytes */
    tcbCTable = 0,

    /* VSpace root, 16 bytes */
    tcbVTable = 1,

    /* Reply cap slot, 16 bytes */
    tcbReply = 2,

    /* TCB of most recent IPC sender, 16 bytes */
    tcbCaller = 3,

    /* IPC buffer cap slot, 16 bytes */
    tcbBuffer = 4,

    tcbCNodeEntries
};
typedef uint32_t tcb_cnode_index_t;

/* TCB CNode: size = 256 bytes */
/* typedef cte_t[16] tcb_cnode; */

/* TCB: size = 652 bytes, alignment = 256 bytes */
struct tcb {
    /* Saved user-level context of thread, 592 bytes */
    user_context_t tcbContext;

    /* Thread state, 12 bytes */
    thread_state_t tcbState;

    /* Current fault, 8 bytes */
    fault_t tcbFault;

    /* Current lookup failure, 8 bytes */
    lookup_fault_t tcbLookupFailure;

    /* Domain, 1 byte (packed to 4) */
    uint32_t tcbDomain;

    /* Priority, 1 byte (packed to 4) */
    uint32_t tcbPriority;

    /* Timeslice remaining, 4 bytes */
    word_t tcbTimeSlice;

    /* Capability pointer to thread fault handler, 4 bytes */
    cptr_t tcbFaultHandler;

    /* userland virtual address of thread IPC buffer, 4 bytes */
    word_t tcbIPCBuffer;

    /* Previous and next pointers for endpoint & scheduler queues, 16 bytes */
    struct tcb* tcbSchedNext;
    struct tcb* tcbSchedPrev;
    struct tcb* tcbEPNext;
    struct tcb* tcbEPPrev;
};
typedef struct tcb tcb_t;

/* Ensure TCB size is sane. */
compile_assert(tcb_size_sane,
               (1 << TCB_SIZE_BITS) + sizeof(tcb_t) <= (1 << TCB_BLOCK_SIZE_BITS))

/* IA32-specific object types */

#define GDT_NULL    0
#define GDT_CS_0    1
#define GDT_DS_0    2
#define GDT_CS_3    3
#define GDT_DS_3    4
#define GDT_TSS     5
#define GDT_TLS     6
#define GDT_IPCBUF  7
#define GDT_ENTRIES 8

#define SEL_NULL    GDT_NULL
#define SEL_CS_0    (GDT_CS_0 << 3)
#define SEL_DS_0    (GDT_DS_0 << 3)
#define SEL_CS_3    ((GDT_CS_3 << 3) | 3)
#define SEL_DS_3    ((GDT_DS_3 << 3) | 3)
#define SEL_TSS     (GDT_TSS << 3)
#define SEL_TLS     ((GDT_TLS << 3) | 3)
#define SEL_IPCBUF  ((GDT_IPCBUF << 3) | 3)

#define IDT_ENTRIES 256

#define PDE_SIZE_BITS  2
#define PDE_PTR(r)     ((pde_t *)(r))
#define PDE_PTR_PTR(r) ((pde_t **)r)
#define PDE_REF(p)     ((unsigned int)p)

#define PD_BITS      10
#define PD_SIZE_BITS (PD_BITS + PDE_SIZE_BITS)
#define PD_PTR(r)    ((pde_t *)(r))
#define PD_REF(p)    ((unsigned int)p)

#define PTE_SIZE_BITS 2
#define PTE_PTR(r)    ((pte_t *)r)
#define PTE_REF(p)    ((unsigned int)p)

#define PT_BITS      10
#define PT_SIZE_BITS (PT_BITS + PTE_SIZE_BITS)
#define PT_PTR(r)    ((pte_t *)r)
#define PT_REF(p)    ((unsigned int)p)

#ifdef CONFIG_IOMMU

#define VTD_RT_SIZE_BITS  12

#define VTD_CTE_SIZE_BITS 3
#define VTD_CTE_PTR(r)    ((vtd_cte_t*)r)
#define VTD_CT_BITS       9
#define VTD_CT_SIZE_BITS  (VTD_CT_BITS + VTD_CTE_SIZE_BITS)

#define VTD_PTE_SIZE_BITS 3
#define VTD_PTE_PTR(r)    ((vtd_pte_t*)r)
#define VTD_PT_BITS       9
#define VTD_PT_SIZE_BITS  (VTD_PT_BITS + VTD_PTE_SIZE_BITS)

#endif

/* helper structure for filling descriptor registers */
typedef struct gdt_idt_ptr {
    uint16_t limit;
    uint16_t basel;
    uint16_t baseh;
} gdt_idt_ptr_t;

compile_assert(gdt_idt_ptr_packed,
               sizeof(gdt_idt_ptr_t) == sizeof(uint16_t) * 3)

#define WORD_SIZE_BITS 2
#define WORD_BITS   (8 * sizeof(word_t))
#define WORD_PTR(r) ((word_t *)(r))
#define WORD_REF(p) ((unsigned int)p)

enum vm_rights {
    VMKernelOnly = 1,
    VMReadOnly = 2,
    VMReadWrite = 3
};
typedef uint32_t vm_rights_t;

static inline word_t CONST
wordFromVMRights(vm_rights_t vm_rights)
{
    return (word_t)vm_rights;
}

static inline vm_rights_t CONST
vmRightsFromWord(word_t w)
{
    return (vm_rights_t)w;
}

enum asidSizeConstants {
    asidHighBits = 6,
    asidLowBits = 10
};

struct asid_pool {
    pde_t* array[BIT(asidLowBits)];
};

typedef struct asid_pool asid_pool_t;

#define ASID_POOL_BITS      asidLowBits
#define ASID_POOL_SIZE_BITS (ASID_POOL_BITS + WORD_SIZE_BITS)
#define ASID_POOL_PTR(r)    ((asid_pool_t*)r)
#define ASID_POOL_REF(p)    ((unsigned int)p)
#define ASID_BITS           (asidHighBits + asidLowBits)
#define nASIDPools          BIT(asidHighBits)
#define ASID_LOW(a)         (a & MASK(asidLowBits))
#define ASID_HIGH(a)        ((a >> asidLowBits) & MASK(asidHighBits))

static inline asid_t CONST
cap_frame_cap_get_capFMappedASID(cap_t cap)
{
    return
        (cap_frame_cap_get_capFMappedASIDHigh(cap) << asidLowBits) +
        cap_frame_cap_get_capFMappedASIDLow(cap);
}

static inline cap_t CONST
cap_frame_cap_set_capFMappedASID(cap_t cap, word_t asid)
{
    cap = cap_frame_cap_set_capFMappedASIDLow(cap, ASID_LOW(asid));
    return cap_frame_cap_set_capFMappedASIDHigh(cap, ASID_HIGH(asid));
}

static inline asid_t PURE
cap_frame_cap_ptr_get_capFMappedASID(cap_t* cap)
{
    return cap_frame_cap_get_capFMappedASID(*cap);
}

static inline void
cap_frame_cap_ptr_set_capFMappedASID(cap_t* cap, asid_t asid)
{
    *cap = cap_frame_cap_set_capFMappedASID(*cap, asid);
}


static inline unsigned int CONST
cap_get_capSizeBits(cap_t cap)
{
    cap_tag_t ctag;
    uint32_t  type;

    ctag = cap_get_capType(cap);

    switch (ctag) {
    case cap_untyped_cap:
        return cap_untyped_cap_get_capBlockSize(cap);

    case cap_endpoint_cap:
        return EP_SIZE_BITS;

    case cap_async_endpoint_cap:
        return AEP_SIZE_BITS;

    case cap_cnode_cap:
        return cap_cnode_cap_get_capCNodeRadix(cap) + CTE_SIZE_BITS;

    case cap_thread_cap:
        return TCB_BLOCK_SIZE_BITS;

    case cap_frame_cap:
        return pageBitsForSize(cap_frame_cap_get_capFSize(cap));

    case cap_page_table_cap:
        return PT_SIZE_BITS;

    case cap_page_directory_cap:
        return PD_SIZE_BITS;

    case cap_zombie_cap:
        type = cap_zombie_cap_get_capZombieType(cap);
        if (type == ZombieType_ZombieTCB) {
            return TCB_BLOCK_SIZE_BITS;
        }
        return ZombieType_ZombieCNode(type) + CTE_SIZE_BITS;

    case cap_null_cap:
        return 0;

    case cap_domain_cap:
        return 0;

    case cap_reply_cap:
        return 0;

    case cap_irq_control_cap:
        return 0;

    case cap_irq_handler_cap:
        return 0;

    case cap_io_port_cap:
        return 0;
#ifdef CONFIG_IOMMU
    case cap_io_space_cap:
        return 0;

    case cap_io_page_table_cap:
        return VTD_PT_SIZE_BITS;
#endif
    case cap_asid_control_cap:
        return 0;

    case cap_asid_pool_cap:
        return ASID_POOL_SIZE_BITS;

    default:
        fail("Invalid arch cap type");
    }
}

static inline void * CONST
cap_get_capPtr(cap_t cap)
{
    cap_tag_t ctag;

    ctag = cap_get_capType(cap);

    switch (ctag) {
    case cap_untyped_cap:
        return WORD_PTR(cap_untyped_cap_get_capPtr(cap));

    case cap_endpoint_cap:
        return EP_PTR(cap_endpoint_cap_get_capEPPtr(cap));

    case cap_async_endpoint_cap:
        return AEP_PTR(cap_async_endpoint_cap_get_capAEPPtr(cap));

    case cap_cnode_cap:
        return CTE_PTR(cap_cnode_cap_get_capCNodePtr(cap));

    case cap_thread_cap:
        return TCB_PTR_CTE_PTR(cap_thread_cap_get_capTCBPtr(cap), 0);

    case cap_frame_cap:
        return (void *)(cap_frame_cap_get_capFBasePtr(cap));

    case cap_page_table_cap:
        return PD_PTR(cap_page_table_cap_get_capPTBasePtr(cap));

    case cap_page_directory_cap:
        return PT_PTR(cap_page_directory_cap_get_capPDBasePtr(cap));

    case cap_zombie_cap:
        return CTE_PTR(cap_zombie_cap_get_capZombiePtr(cap));

    case cap_null_cap:
        return NULL;

    case cap_domain_cap:
        return NULL;

    case cap_reply_cap:
        return NULL;

    case cap_irq_control_cap:
        return NULL;

    case cap_irq_handler_cap:
        return NULL;

    case cap_io_port_cap:
        return NULL;
#ifdef CONFIG_IOMMU
    case cap_io_space_cap:
        return NULL;

    case cap_io_page_table_cap:
        return (void *)(cap_io_page_table_cap_get_capIOPTBasePtr(cap));
#endif
    case cap_asid_control_cap:
        return NULL;

    case cap_asid_pool_cap:
        return ASID_POOL_PTR(cap_asid_pool_cap_get_capASIDPool(cap));

    default:
        fail("Invalid arch cap type");
    }
}

static inline vm_attributes_t CONST
vmAttributesFromWord(word_t w)
{
    vm_attributes_t attr;

    attr.words[0] = w;
    return attr;
}

static inline word_t CONST
isArchCap(cap_t cap)
{
    return (cap_get_capType(cap) % 2);
}

#endif
