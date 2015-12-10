/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#include <machine.h>
#include <model/statedata.h>
#include <object/structures.h>
#include <object/objecttype.h>

#define GT  ( 1)
#define EQ  ( 0)
#define LT  (-1)

void printCTE(char *msg, cte_t *cte);

static cte_t *aaInsert(cte_t *rootSlot, cte_t *newSlot);
static cte_t *aaRemove(bool_t isSwapped, cte_t *rootSlot, cte_t *targetSlot);
static cte_t *aaTraverseBackward(cte_t *slot);
static cte_t *aaTraverseForward(cte_t *slot);

typedef int (*comp_t)(cte_t *, cte_t *);
typedef int (*tie_comp_t)(cte_t *, cte_t *, comp_t);
typedef int (*type_comp_t)(cte_t *, cte_t *, tie_comp_t);

#define compare(a, b) \
    ({ typeof(a) _a = (a); \
       typeof(b) _b = (b); \
       _a == _b ? EQ : (_a > _b ? GT : LT); \
    })

static inline bool_t
capsEqual(cap_t a, cap_t b)
{
    return (cap_get_capSpaceType(a) == cap_get_capSpaceType(b)) &&
           ((word_t)cap_get_capSpacePtr(a) == (word_t)cap_get_capSpacePtr(b)) &&
           (cap_get_capSpaceSize(a) == cap_get_capSpaceSize(b)) &&
           (cap_get_capBadge(a)   == cap_get_capBadge(b)) &&
           (cap_get_capExtraComp(a) == cap_get_capExtraComp(b));
}

static inline int
tie_break_comparator(cte_t *a, cte_t *b, comp_t pre_slot)
{
    int cmp;
    /* Check the depth */
    cmp = compare(mdb_node_get_cdtDepth(a->cteMDBNode), mdb_node_get_cdtDepth(b->cteMDBNode));
    if (cmp != EQ) {
        return cmp;
    }
    if (pre_slot) {
        cmp = pre_slot(a, b);
        if (cmp != EQ) {
            return cmp;
        }
    }
    /* compare on the slot as a last resort */
    return compare(a, b);
}

static inline int
untyped_comparator(cte_t *a, cte_t *b, tie_comp_t tie_break)
{
    int cmp;
    /* Compare base address and size of the untyped object */
    cmp = compare(cap_untyped_cap_get_capPtr(a->cap), cap_untyped_cap_get_capPtr(b->cap));
    if (cmp != EQ) {
        return cmp;
    }
    cmp = - compare(cap_untyped_cap_get_capBlockSize(a->cap), cap_untyped_cap_get_capBlockSize(b->cap));
    if (cmp != EQ) {
        return cmp;
    }
    /* Do common late comparisons */
    return tie_break(a, b, NULL);
}

static inline int
endpoint_cap_comparator(cte_t *a, cte_t *b, tie_comp_t tie_break)
{
    int cmp;
    /* compare on the badge */
    cmp = compare(cap_endpoint_cap_get_capEPBadge(a->cap), cap_endpoint_cap_get_capEPBadge(b->cap));
    if (cmp != EQ) {
        return cmp;
    }
    /* tiebreak as normal */
    return tie_break(a, b, NULL);
}

static inline int
notification_cap_comparator(cte_t *a, cte_t *b, tie_comp_t tie_break)
{
    int cmp;
    /* compare on the badge */
    cmp = compare(cap_notification_cap_get_capNtfnBadge(a->cap), cap_notification_cap_get_capNtfnBadge(b->cap));
    if (cmp != EQ) {
        return cmp;
    }
    /* tiebreak as normal */
    return tie_break(a, b, NULL);
}

static inline int cap_extra_comp(cte_t *a, cte_t *b)
{
    return compare(cap_get_capExtraComp(a->cap), cap_get_capExtraComp(b->cap));
}

static inline int
frame_cap_comparator(cte_t *a, cte_t *b, tie_comp_t tie_break)
{
    return tie_break(a, b, cap_extra_comp);
}

static inline int
page_table_comparator(cte_t *a, cte_t *b, tie_comp_t tie_break)
{
    return tie_break(a, b, cap_extra_comp);
}

static inline int
page_directory_comparator(cte_t *a, cte_t *b, tie_comp_t tie_break)
{
    return tie_break(a, b, cap_extra_comp);
}

static inline int
pdpt_comparator(cte_t *a, cte_t *b, tie_comp_t tie_break)
{
    return tie_break(a, b, cap_extra_comp);
}

static inline int
io_page_table_comparator(cte_t *a, cte_t *b, tie_comp_t tie_break)
{
    return tie_break(a, b, cap_extra_comp);
}

static inline int
io_space_comparator(cte_t *a, cte_t *b, tie_comp_t tie_break)
{
    return tie_break(a, b, cap_extra_comp);
}

static inline int
ept_pdpt_comparator(cte_t *a, cte_t *b, tie_comp_t tie_break)
{
    return tie_break(a, b, cap_extra_comp);
}

static inline int
ept_page_directory_comparator(cte_t *a, cte_t *b, tie_comp_t tie_break)
{
    return tie_break(a, b, cap_extra_comp);
}

static inline int
ept_page_table_comparator(cte_t *a, cte_t *b, tie_comp_t tie_break)
{
    return tie_break(a, b, cap_extra_comp);
}

static inline int
just_tie_break(cte_t *a, cte_t *b, tie_comp_t tie_break)
{
    return tie_break(a, b, NULL);
}

static inline int
typed_comparator(cte_t *a, cte_t *b, tie_comp_t tie_break)
{
    int cmp;
    cap_tag_t type;
    type_comp_t comp;
    static type_comp_t comparator[] = {
        [cap_endpoint_cap]       = endpoint_cap_comparator,
        [cap_notification_cap] = notification_cap_comparator,
        [cap_cnode_cap]          = just_tie_break,
        [cap_thread_cap]         = just_tie_break,
        [cap_frame_cap]          = frame_cap_comparator,
        [cap_page_table_cap]     = page_table_comparator,
        [cap_page_directory_cap] = page_directory_comparator,
#ifdef ARCH_IA32
        [cap_pdpt_cap]           = pdpt_comparator,
#endif
        [cap_zombie_cap]         = just_tie_break,
#ifdef CONFIG_IOMMU
        [cap_io_page_table_cap]  = io_page_table_comparator,
        [cap_io_space_cap]       = io_space_comparator,
#endif
#ifdef CONFIG_VTX
        [cap_vcpu_cap]           = just_tie_break,
        [cap_ept_page_directory_pointer_table_cap] = ept_pdpt_comparator,
        [cap_ept_page_directory_cap]               = ept_page_directory_comparator,
        [cap_ept_page_table_cap]                   = ept_page_table_comparator,
#endif
    };
    /* Typed objects do not overlap, so sufficient to compare base address */
    cmp = compare(cap_get_capPtr(a->cap), cap_get_capPtr(b->cap));
    if (cmp != EQ) {
        return cmp;
    }
    /* at this point we *know* the types must be equal, so call the
     * per cap type comparator, if it needs one. */
    type = cap_get_capType(a->cap);
    assert(type < ARRAY_SIZE(comparator));
    comp = comparator[type];
    assert(comp);
    return comp(a, b, tie_break);
}

static inline int
irq_comparator(cte_t *a, cte_t *b, tie_comp_t tie_break)
{
    int cmp;
    cap_tag_t typeA, typeB;
    /* The IRQ control cap can be thought of as having an 'address' of 0 and a 'size' of
     * the entire IRQ space. IRQ handlers then have an address that is their irq and a size
     * of 1. Since IRQ control caps cannot be subdivided this is equivalent to putting
     * all IRQ control caps first, then sorting IRQ handlers by their IRQ */
    typeA = cap_get_capType(a->cap);
    typeB = cap_get_capType(b->cap);
    if (typeA == typeB) {
        if (typeA == cap_irq_control_cap) {
            /* both control caps, tie break */
            return tie_break(a, b, NULL);
        } else {
            /* both irq handlers, compare on irq */
            assert(typeA == cap_irq_handler_cap);
            cmp = compare(cap_irq_handler_cap_get_capIRQ(a->cap), cap_irq_handler_cap_get_capIRQ(b->cap));
            if (cmp != EQ) {
                return cmp;
            }
            return tie_break(a, b, NULL);
        }
    } else if (typeA == cap_irq_control_cap) {
        return LT;
    } else {
        assert(typeA == cap_irq_handler_cap);
        return GT;
    }
}

#ifdef ARCH_IA32
static inline int
ioport_comparator(cte_t *a, cte_t *b, tie_comp_t tie_break)
{
    int cmp;
    uint32_t firstA, firstB, lastA, lastB;
    /* ioports have a base address and size that is defined by their start port and end port */
    firstA = cap_io_port_cap_get_capIOPortFirstPort(a->cap);
    firstB = cap_io_port_cap_get_capIOPortFirstPort(b->cap);
    cmp = compare(firstA, firstB);
    if (cmp != EQ) {
        return cmp;
    }
    lastA = cap_io_port_cap_get_capIOPortLastPort(a->cap);
    lastB = cap_io_port_cap_get_capIOPortLastPort(b->cap);
    cmp = - compare(lastA - firstA, lastB - firstB);
    if (cmp != EQ) {
        return cmp;
    }
    return tie_break(a, b, NULL);
}
#endif

#ifdef CONFIG_IOMMU
static inline int
iospace_comparator(cte_t *a, cte_t *b, tie_comp_t tie_break)
{
    int cmp;
    /* order by pci device this is assigned to and then by domain ID */
    cmp = compare(cap_io_space_cap_get_capPCIDevice(a->cap), cap_io_space_cap_get_capPCIDevice(b->cap));
    if (cmp != EQ) {
        return cmp;
    }
    cmp = compare(cap_io_space_cap_get_capDomainID(a->cap), cap_io_space_cap_get_capDomainID(b->cap));
    if (cmp != EQ) {
        return cmp;
    }
    return tie_break(a, b, NULL);
}
#endif

static inline int
compare_space(int space, cte_t *a, cte_t *b, tie_comp_t tie_break)
{
    type_comp_t comp;
    static type_comp_t comparator[] = {
        [capSpaceUntypedMemory] = untyped_comparator,
        [capSpaceTypedMemory] = typed_comparator,
        [capSpaceDomain] = just_tie_break,
        [capSpaceIRQ] = irq_comparator,
#ifdef CONFIG_IOMMU
        [capSpaceIOSpace] = iospace_comparator,
#endif
#ifdef ARCH_IA32
        [capSpaceIOPort] = ioport_comparator,
        [capSpaceIPI] = just_tie_break,
#endif
    };
    assert(space < ARRAY_SIZE(comparator));
    comp = comparator[space];
    assert(comp);
    return comp(a, b, tie_break);
}

static inline int
compSlotWith(cte_t *a, cte_t *b, tie_comp_t tie_break)
{
    /* check space */
    int spaceA = cap_get_capSpaceType(a->cap);
    int spaceB = cap_get_capSpaceType(b->cap);
    int cmp = compare(spaceA, spaceB);
    if (cmp != EQ) {
        return cmp;
    }
    /* now call the space specific comparator */
    return compare_space(spaceA, a, b, tie_break);
}

static inline int
compSlot(cte_t *a, cte_t *b)
{
    /* We know nothing, call general comparator for caps and tie break on slots */
    return compSlotWith(a, b, tie_break_comparator);
}

static inline int has_extra_comparator(cte_t *a, cte_t *b, comp_t pre_slot)
{
    int cmp;
    /* Check depth as per normal */
    cmp = compare(mdb_node_get_cdtDepth(a->cteMDBNode), mdb_node_get_cdtDepth(b->cteMDBNode));
    if (cmp != EQ) {
        return cmp;
    }
    assert(pre_slot);
    cmp = pre_slot(a, b);
    /* if the extra comparison was not equal then we found something, so we will claim that we found equality,
     * otherwise return a psudo-random result */
    if (cmp != EQ) {
        return EQ;
    }
    return LT;
}

cte_t *
cdtFindWithExtra(cap_t hypothetical)
{
    uint32_t i;
    unsigned int depth_bits = cte_depth_bits_cap(hypothetical);
    for (i = 0; i < BIT(depth_bits); i++) {
        cte_t *current;
        cte_t *next;

        cte_t slot = (cte_t) {
            .cap = hypothetical,
             .cteMDBNode = mdb_node_new(0, i, 0, 0)
        };

        next = ksRootCTE;
        do {
            int cmp;
            current = next;
            /* we are searching for a slot that is mostly equal to this node,
             * except that it has a non zero extra component */
            cmp = compSlotWith(current, &slot, has_extra_comparator);
            switch (cmp) {
            case LT:
                next = CTE_PTR(mdb_node_get_cdtRight(current->cteMDBNode));
                break;
            case GT:
                next = CTE_PTR(mdb_node_get_cdtLeft(current->cteMDBNode));
                break;
            case EQ:
                return current;
            }
        } while (next);
    }
    return NULL;
}

static inline int slot_eq_comparator(cte_t *a, cte_t *b, comp_t pre_slot)
{
    int cmp;
    /* Check depth and pre_slot as per normal */
    cmp = compare(mdb_node_get_cdtDepth(a->cteMDBNode), mdb_node_get_cdtDepth(b->cteMDBNode));
    if (cmp != EQ) {
        return cmp;
    }
    if (pre_slot) {
        cmp = pre_slot(a, b);
        if (cmp != EQ) {
            return cmp;
        }
    }
    /* Slot is always EQ */
    return EQ;
}

cte_t *
cdtFindAtDepth(cap_t hypothetical, uint32_t depth)
{
    cte_t *current;
    cte_t *next;

    cte_t slot = (cte_t) {
        .cap = hypothetical,
         .cteMDBNode = mdb_node_new(0, depth, 0, 0)
    };

    next = ksRootCTE;
    /* we want to find the entry in the tree that is equal to this node
     * in every way except that it will have a different slot. So we will
     * do a search with a comparator that always returns equality on slots */
    do {
        current = next;
        switch (compSlotWith(current, &slot, slot_eq_comparator)) {
        case LT:
            next = CTE_PTR(mdb_node_get_cdtRight(current->cteMDBNode));
            break;
        case GT:
            next = CTE_PTR(mdb_node_get_cdtLeft(current->cteMDBNode));
            break;
        case EQ:
            return current;
        }
    } while (next);
    return NULL;
}

cte_t *
cdtFind(cap_t hypothetical)
{
    uint32_t i;
    cte_t *ret;
    unsigned int depth_bits = cte_depth_bits_cap(hypothetical);
    for (i = 0; i < BIT(depth_bits); i++) {
        ret = cdtFindAtDepth(hypothetical, i);
        if (ret) {
            return ret;
        }
    }
    return NULL;
}

bool_t
cdtIsFinal(cte_t *slot)
{
    cte_t *closest;

    /* For finality testing it is sufficient to check the objects immediately
     * before and after us in cdt ordering. This is because we are only
     * interested in equivalent objects, not whether something is actually
     * a parent or not */
    closest = aaTraverseForward(slot);
    if (closest && sameObjectAs(closest->cap, slot->cap)) {
        return false;
    }
    closest = aaTraverseBackward(slot);
    if (closest && sameObjectAs(closest->cap, slot->cap)) {
        return false;
    }
    return true;
}

static inline cap_t
build_largest_child(cap_t cap)
{
    switch (cap_get_capType(cap)) {
    case cap_domain_cap:
#ifdef ARCH_IA32
    case cap_ipi_cap:
#endif
    case cap_irq_handler_cap:
    case cap_cnode_cap:
    case cap_thread_cap:
#ifdef CONFIG_VTX
    case cap_vcpu_cap:
#endif
    case cap_zombie_cap:
        return cap;
#ifdef CONFIG_IA32
    case cap_io_port_cap:
        /* We order on base address first, so set it has high as possible, size doesn't matter then.
           But size shouldn't be zero */
        return cap_io_port_cap_new(cap_io_port_cap_get_capIOPortLastPort(cap) - 1, 1);
#endif
    case cap_irq_control_cap:
        /* Largest child is a irq handler with biggest irq */
        return cap_irq_handler_cap_new(0xff);
    case cap_untyped_cap:
        /* untyped cap of smallest size at the end of this region */
        return cap_untyped_cap_new(0, 4, cap_untyped_cap_get_capPtr(cap) + BIT(cap_untyped_cap_get_capBlockSize(cap)) - BIT(4));
    case cap_endpoint_cap:
        if (cap_endpoint_cap_get_capEPBadge(cap) == 0) {
            return cap_endpoint_cap_new(BIT(28) - 1, 0, 0, 0, cap_endpoint_cap_get_capEPPtr(cap));
        }
        return cap;
    case cap_notification_cap:
        if (cap_notification_cap_get_capNtfnBadge(cap) == 0) {
            return cap_notification_cap_new(BIT(28) - 1, 0, 0, cap_notification_cap_get_capNtfnPtr(cap));
        }
        return cap;
        /* We get away with not setting the extra higher as we will always be comparing
         * with an infinite depth, hence any 'extra' is not relevant */
    case cap_frame_cap:
    case cap_page_table_cap:
    case cap_page_directory_cap:
#ifdef ARCH_IA32
    case cap_pdpt_cap:
#endif
#ifdef CONFIG_VTX
    case cap_ept_page_directory_pointer_table_cap:
    case cap_ept_page_directory_cap:
    case cap_ept_page_table_cap:
#endif
#ifdef CONFIG_IOMMU
    case cap_io_page_table_cap:
    case cap_io_space_cap:
#endif
        return cap;
    default:
        fail("Unknown cap type");
    }
}

static inline int largest_child_comparator(cte_t *a, cte_t *b, comp_t pre_slot)
{
    /* Tie breaking for largest child is easy. Their depth is always less than ours */
    return LT;
}

static inline int slot_lt_comparator(cte_t *a, cte_t *b, comp_t pre_slot)
{
    int cmp;
    /* Check depth and pre_slot as per normal */
    cmp = compare(mdb_node_get_cdtDepth(a->cteMDBNode), mdb_node_get_cdtDepth(b->cteMDBNode));
    if (cmp != EQ) {
        return cmp;
    }
    if (pre_slot) {
        cmp = pre_slot(a, b);
        if (cmp != EQ) {
            return cmp;
        }
    }
    /* Slot is always LT */
    return LT;
}

static inline cte_t *
aaFindFromBelow(cte_t *hypothetical, tie_comp_t tie_break)
{
    cte_t *current;
    cte_t *largest;
    cte_t *next;
    next = ksRootCTE;
    largest = NULL;
    do {
        int cmp;
        current = next;
        cmp = compSlotWith(current, hypothetical, tie_break);
        if (cmp == LT) {
            next = CTE_PTR(mdb_node_get_cdtRight(current->cteMDBNode));
            if (!largest || compSlot(current, largest) == GT) {
                largest = current;
            }
        } else {
            next = CTE_PTR(mdb_node_get_cdtLeft(current->cteMDBNode));
        }
    } while (next);
    return largest;
}

/* Finding a child is complicated because your child may not
 * live directly after you in cdt order. That is, if you take
 * ever node in the tree and squash it into a list, directly
 * after you may be some N number of siblings, then your
 * children. This is why we need to do a creative search
 * where as cdtIsFinal was able to get away with checking
 * neighbouring nodes */
static cte_t *
_cdtFindChild(cte_t *parentSlot)
{
    cte_t *child;
    /* Construct a hypothetical child. This needs to be the largest
     * possible child such that anything greater than it would no
     * longer be our child and anything less than it is either
     * our sibling or our child. We do not worry about the depth
     * as we will use a fake comparator that assumes our node
     * is of infinite depth */
    cte_t hypothetical = {
        .cap = build_largest_child(parentSlot->cap),
    };

    /* Search for hypothetical cap from below. */
    child = aaFindFromBelow(&hypothetical, largest_child_comparator);

    /* Verify that this is in fact a child (we could have none). To ensure
     * we did not find ourself or a sibling we ensure that we are strictly
     * greater than ignoring slot tie breaks */
    if (!child || compSlotWith(child, parentSlot, slot_lt_comparator) != GT) {
        return NULL;
    }
    return child;
}

cte_t *
cdtFindTypedInRange(word_t base, unsigned int size_bits)
{
    cte_t *child;
    /* Construct the smallest typed object we know about at the top
     * of the memory range and search for it */
    cte_t hypothetical = {
        .cap = cap_endpoint_cap_new(0, 0, 0, 0, base + BIT(size_bits) - BIT(EP_SIZE_BITS)),
    };
    /* Search for it from below */
    child = aaFindFromBelow(&hypothetical, largest_child_comparator);
    /* Check we found something in the right range. Construct a fake untyped
     * to reuse existing range checking */
    if (child && sameRegionAs(cap_untyped_cap_new(0, size_bits, base), child->cap)) {
        return child;
    }
    return NULL;
}

cte_t *
cdtFindChild(cte_t *parentSlot)
{
    if (cap_get_capSpaceType(parentSlot->cap) == capSpaceUntypedMemory) {
        /* Find anything in this range that is typed */
        cte_t *result = cdtFindTypedInRange(cap_untyped_cap_get_capPtr(parentSlot->cap), cap_untyped_cap_get_capBlockSize(parentSlot->cap));
        if (result) {
            return result;
        }
    }
    return _cdtFindChild(parentSlot);
}

static inline void
cdtInsertTree(cte_t *slot)
{
    ksRootCTE = aaInsert(ksRootCTE, slot);
}

void
cdtInsert(cte_t *parentSlot, cte_t *newSlot)
{
    word_t depth;
    assert(cap_get_capType(newSlot->cap) != cap_null_cap);
    assert(!parentSlot || cap_get_capType(parentSlot->cap) != cap_null_cap);
    if (!parentSlot || (cap_get_capSpaceType(parentSlot->cap) != cap_get_capSpaceType(newSlot->cap))) {
        depth = 0;
    } else {
        depth = mdb_node_get_cdtDepth(parentSlot->cteMDBNode) + 1;
        if (depth == BIT(cte_depth_bits_cap(newSlot->cap))) {
            depth--;
        }
    }
    newSlot->cteMDBNode = mdb_node_new(0, depth, 0, 0);
    cdtInsertTree(newSlot);
}

void
cdtRemove(cte_t *slot)
{
    assert(cap_get_capType(slot->cap) != cap_null_cap);
    ksRootCTE = aaRemove(false, ksRootCTE, slot);
    slot->cteMDBNode = nullMDBNode;
}

void
cdtMove(cte_t *oldSlot, cte_t *newSlot)
{
    assert(cap_get_capType(oldSlot->cap) != cap_null_cap);
    assert(cap_get_capType(newSlot->cap) != cap_null_cap);
    ksRootCTE = aaRemove(false, ksRootCTE, oldSlot);

    newSlot->cteMDBNode = mdb_node_new(0, mdb_node_get_cdtDepth(oldSlot->cteMDBNode), 0, 0);
    oldSlot->cteMDBNode = mdb_node_new(0, 0, 0, 0);

    ksRootCTE = aaInsert(ksRootCTE, newSlot);
}

void
cdtUpdate(cte_t *slot, cap_t newCap)
{
    if (capsEqual(slot->cap, newCap)) {
        slot->cap = newCap;
    } else {
        ksRootCTE = aaRemove(false, ksRootCTE, slot);
        slot->cteMDBNode = mdb_node_new(0, mdb_node_get_cdtDepth(slot->cteMDBNode), 0, 0);
        slot->cap = newCap;
        ksRootCTE = aaInsert(ksRootCTE, slot);
    }
}

void
cdtSwap(cap_t cap1, cte_t *slot1, cap_t cap2, cte_t *slot2)
{
    word_t depth1, depth2;
    assert(slot1 != slot2);
    if (cap_get_capType(slot1->cap) != cap_null_cap) {
        ksRootCTE = aaRemove(false, ksRootCTE, slot1);
    }
    if (cap_get_capType(slot2->cap) != cap_null_cap) {
        ksRootCTE = aaRemove(false, ksRootCTE, slot2);
    }
    depth1 = mdb_node_get_cdtDepth(slot1->cteMDBNode);
    depth2 = mdb_node_get_cdtDepth(slot2->cteMDBNode);
    slot1->cteMDBNode = mdb_node_new(0, depth2, 0, 0);
    slot2->cteMDBNode = mdb_node_new(0, depth1, 0, 0);

    slot1->cap = cap2;
    slot2->cap = cap1;

    if (cap_get_capType(slot1->cap) != cap_null_cap) {
        ksRootCTE = aaInsert(ksRootCTE, slot1);
    }
    if (cap_get_capType(slot2->cap) != cap_null_cap) {
        ksRootCTE = aaInsert(ksRootCTE, slot2);
    }
}

/*****************************************************************************
 * AA Tree implementation
 *****************************************************************************/

/* AA Tree rebalancing functions */
static cte_t *aaRemoveNode(bool_t isSwapped, cte_t *rootSlot);
static cte_t *aaRebalance(cte_t *slot);
static cte_t *aaDecLevel(cte_t *slot);
static cte_t *aaSkew(cte_t *slot);
static cte_t *aaSplit(cte_t *slot);

static cte_t * aaSucc(cte_t *slot)
{
    cte_t *left;

    left = CTE_PTR(mdb_node_get_cdtLeft(slot->cteMDBNode));
    while (left) {
        slot = left;
        left = CTE_PTR(mdb_node_get_cdtLeft(slot->cteMDBNode));
    }
    return slot;
}

static cte_t * aaPred(cte_t *slot)
{
    cte_t *right;

    right = CTE_PTR(mdb_node_get_cdtRight(slot->cteMDBNode));
    while (right) {
        slot = right;
        right = CTE_PTR(mdb_node_get_cdtRight(slot->cteMDBNode));
    }
    return slot;
}

static cte_t *aaParent(cte_t *slot)
{
    cte_t *current = NULL;
    cte_t *next;

    next = ksRootCTE;
    while (next != slot) {
        current = next;
        switch (compSlot(current, slot)) {
        case LT:
            next = CTE_PTR(mdb_node_get_cdtRight(current->cteMDBNode));
            break;
        case GT:
            next = CTE_PTR(mdb_node_get_cdtLeft(current->cteMDBNode));
            break;
        case EQ:
            return current;
        }
    }
    return current;
}

static cte_t *aaTraverseBackward(cte_t *slot)
{
    cte_t *parent;
    cte_t *left;
    /* Optimistically see if we our predecessor is a child */
    left = CTE_PTR(mdb_node_get_cdtLeft(slot->cteMDBNode));
    if (left) {
        return aaPred(left);
    }
    /* We need to find our parent. This is actually hard so we
     * need to find ourselves and perform a trace as we do so */

    /* search upwards until we find an ancestor on a right link,
     * we have then found something before us */
    parent = aaParent(slot);
    while (parent && CTE_PTR(mdb_node_get_cdtRight(parent->cteMDBNode)) != slot) {
        slot = parent;
        parent = aaParent(parent);
    }
    return parent;
}

static cte_t *aaTraverseForward(cte_t *slot)
{
    cte_t *parent;
    cte_t *right;
    /* Optimistically see if we our successor is a child */
    right = CTE_PTR(mdb_node_get_cdtRight(slot->cteMDBNode));
    if (right) {
        return aaSucc(right);
    }
    /* We need to find our parent. This is actually hard so we
     * need to find ourselves and perform a trace as we do so */


    /* search upwards until we find an ancestor on a left link,
     * we have then found something before us */
    parent = aaParent(slot);
    while (parent && CTE_PTR(mdb_node_get_cdtLeft(parent->cteMDBNode)) != slot) {
        slot = parent;
        parent = aaParent(parent);
    }
    return parent;
}

static inline int
aaLevel(cte_t *slot)
{
    if (!slot) {
        return 0;
    }
    return mdb_node_get_cdtLevel(slot->cteMDBNode);
}

static inline int CONST min(int a, int b)
{
    return (a < b) ? a : b;
}

static cte_t *aaInsert(cte_t *rootSlot, cte_t *newSlot)
{
    cte_t *left, *right;

    if (!newSlot) {
        fail("inserting null CTE");
    }
    assert(newSlot != rootSlot);

    if (!rootSlot) {

        mdb_node_ptr_set_cdtLevel(&newSlot->cteMDBNode, 1);
        return newSlot;

    } else {

        switch (compSlot(newSlot, rootSlot)) {
        case GT:
            right = CTE_PTR(mdb_node_get_cdtRight(rootSlot->cteMDBNode));
            right = aaInsert(right, newSlot);
            mdb_node_ptr_set_cdtRight(&rootSlot->cteMDBNode, CTE_REF(right));
            break;

        case LT:
            left = CTE_PTR(mdb_node_get_cdtLeft(rootSlot->cteMDBNode));
            left = aaInsert(left, newSlot);
            mdb_node_ptr_set_cdtLeft(&rootSlot->cteMDBNode, CTE_REF(left));
            break;

        default:
            fail("Inserting duplicate");
        }

        rootSlot = aaSkew(rootSlot);
        rootSlot = aaSplit(rootSlot);

        return rootSlot;
    }
}

static cte_t *aaRemove(bool_t isSwapped, cte_t *rootSlot, cte_t *targetSlot)
{
    cte_t *left, *right;

    if (!targetSlot) {
        fail("removing null");
    }
    if (!rootSlot) {
        fail("removing from null");
    }

    switch (compSlot(targetSlot, rootSlot)) {
    case GT:
        right = CTE_PTR(mdb_node_get_cdtRight(rootSlot->cteMDBNode));
        right = aaRemove(isSwapped, right, targetSlot);
        mdb_node_ptr_set_cdtRight(&rootSlot->cteMDBNode, CTE_REF(right));
        break;
    case LT:
        left = CTE_PTR(mdb_node_get_cdtLeft(rootSlot->cteMDBNode));
        left = aaRemove(isSwapped, left, targetSlot);
        mdb_node_ptr_set_cdtLeft(&rootSlot->cteMDBNode, CTE_REF(left));
        break;
    default:
        rootSlot = aaRemoveNode(isSwapped, rootSlot);
    }
    rootSlot = aaRebalance(rootSlot);
    return rootSlot;
}

/* AA Tree rebalancing functions */

static cte_t *aaRemoveNode(bool_t isSwapped, cte_t *rootSlot)
{
    cte_t *left, *right, *pred, *succ;
    mdb_node_t mdb;

    mdb = rootSlot->cteMDBNode;

    left = CTE_PTR(mdb_node_get_cdtLeft(mdb));
    right = CTE_PTR(mdb_node_get_cdtRight(mdb));
    if (left) {
        pred = aaPred(left);
        left = aaRemove(true, left, pred);

        mdb_node_ptr_set_cdtLevel(&pred->cteMDBNode, mdb_node_get_cdtLevel(mdb));
        mdb_node_ptr_set_cdtRight(&pred->cteMDBNode, mdb_node_get_cdtRight(mdb));
        mdb_node_ptr_set_cdtLeft(&pred->cteMDBNode, CTE_REF(left));

        return pred;

    } else if (right) {
        succ = aaSucc(right);
        right = aaRemove(true, right, succ);

        mdb_node_ptr_set_cdtLevel(&succ->cteMDBNode, mdb_node_get_cdtLevel(mdb));
        mdb_node_ptr_set_cdtRight(&succ->cteMDBNode, CTE_REF(right));
        mdb_node_ptr_set_cdtLeft(&succ->cteMDBNode, CTE_REF(NULL));

        return succ;

    } else {
        return NULL;
    }
}

static cte_t *aaRebalance(cte_t *slot)
{
    cte_t *right, *right_right;

    if (!slot) {
        return NULL;
    }

    slot = aaDecLevel(slot);
    slot = aaSkew(slot);

    right = aaSkew(CTE_PTR(mdb_node_get_cdtRight(slot->cteMDBNode)));
    mdb_node_ptr_set_cdtRight(&slot->cteMDBNode, CTE_REF(right));

    if (right) {
        right_right = aaSkew(CTE_PTR(mdb_node_get_cdtRight(right->cteMDBNode)));
        mdb_node_ptr_set_cdtRight(&right->cteMDBNode, CTE_REF(right_right));
    }

    slot = aaSplit(slot);

    right = aaSplit(CTE_PTR(mdb_node_get_cdtRight(slot->cteMDBNode)));
    mdb_node_ptr_set_cdtRight(&slot->cteMDBNode, CTE_REF(right));

    return slot;
}

static cte_t *aaDecLevel(cte_t *slot)
{
    cte_t *left, *right;
    int should_be;

    if (!slot) {
        return NULL;
    }

    left = CTE_PTR(mdb_node_get_cdtLeft(slot->cteMDBNode));
    right = CTE_PTR(mdb_node_get_cdtRight(slot->cteMDBNode));

    should_be = min(aaLevel(left), aaLevel(right)) + 1;

    if (should_be < mdb_node_get_cdtLevel(slot->cteMDBNode)) {
        mdb_node_ptr_set_cdtLevel(&slot->cteMDBNode, should_be);

        if (right && should_be < mdb_node_get_cdtLevel(right->cteMDBNode)) {
            mdb_node_ptr_set_cdtLevel(&right->cteMDBNode, should_be);
        }
    }

    return slot;
}

static cte_t *aaSplit(cte_t *slot)
{
    cte_t *right, *right_right;
    int level;

    /*
     *                             |
     *     |                      |R|
     *    |T|->|R|->|X|   =>     /   \
     *   /    /                |T|   |X|
     * |A|  |B|               /   \
     *                      |A|   |B|
     */

    if (!slot) {
        return NULL;
    }

    right = CTE_PTR(mdb_node_get_cdtRight(slot->cteMDBNode));
    if (right) {

        right_right = CTE_PTR(mdb_node_get_cdtRight(right->cteMDBNode));
        if (right_right && mdb_node_get_cdtLevel(slot->cteMDBNode)
                == mdb_node_get_cdtLevel(right_right->cteMDBNode)) {

            mdb_node_ptr_set_cdtRight(&slot->cteMDBNode,
                                      mdb_node_get_cdtLeft(right->cteMDBNode));

            level = mdb_node_get_cdtLevel(right->cteMDBNode) + 1;
            mdb_node_ptr_set_cdtLevel(&right->cteMDBNode, level);

            /* check level dosn't overflow */
            assert(mdb_node_get_cdtLevel(right->cteMDBNode) == level);

            mdb_node_ptr_set_cdtLeft(&right->cteMDBNode, CTE_REF(slot));

            return right;
        }
    }

    return slot;
}

static cte_t *aaSkew(cte_t *slot)
{
    cte_t *left;

    /*
     *          |              |
     *    |L|<-|T|     =>     |L|->|T|
     *   /   \    \          /    /   \
     * |A|   |B|  |R|      |A|  |B|   |R|
     */

    if (!slot) {
        return NULL;
    }

    left = CTE_PTR(mdb_node_get_cdtLeft(slot->cteMDBNode));
    if (left && mdb_node_get_cdtLevel(left->cteMDBNode)
            == mdb_node_get_cdtLevel(slot->cteMDBNode)) {

        mdb_node_ptr_set_cdtLeft(&slot->cteMDBNode,
                                 mdb_node_get_cdtRight(left->cteMDBNode));
        mdb_node_ptr_set_cdtRight(&left->cteMDBNode, CTE_REF(slot));

        return left;
    }

    return slot;
}

/*****************************************************************************
 * AA Tree Debug Functions
 *****************************************************************************/

static char *
printCap(cap_t cap)
{
    switch (cap_get_capType(cap)) {
    case cap_null_cap:
        return "NullCap";
    case cap_untyped_cap:
        return "Untyped";
    case cap_endpoint_cap:
        return "Endpoint";
    case cap_notification_cap:
        return "Notification";
    case cap_reply_cap:
        return "Reply";
    case cap_cnode_cap:
        return "CNode";
    case cap_thread_cap:
        return "Thread";
    default:
        return "?";
    }
}

void
printCTE(char *msg, cte_t *cte)
{
    (void)printCap;
    if (!cte) {
        printf("%s [NULL]@0x%p", msg, (void *) cte);
    } else  {
        printf("%s [%d %s(%d) { addr = 0x%lx, size = 0x%x } left: 0x%lx right: 0x%lx badge: %d depth: %d extra: 0x%x]@0x%p\n",
               msg,
               mdb_node_get_cdtLevel(cte->cteMDBNode),
               printCap(cte->cap),
               cap_get_capType(cte->cap),
               cap_get_capType(cte->cap) == cap_null_cap ? 0 : (word_t)cap_get_capSpacePtr(cte->cap),
               cap_get_capType(cte->cap) == cap_null_cap ? 0 : cap_get_capSpaceSize(cte->cap),
               mdb_node_get_cdtLeft(cte->cteMDBNode),
               mdb_node_get_cdtRight(cte->cteMDBNode),
               cap_get_capBadge(cte->cap),
               mdb_node_get_cdtDepth(cte->cteMDBNode),
               cap_get_capType(cte->cap) == cap_null_cap ? 0 : cap_get_capExtraComp(cte->cap),
               cte);
    }
}
