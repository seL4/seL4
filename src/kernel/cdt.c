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

static inline int CONST compare(int a, int b)
{
    return a == b ? EQ : (a > b ? GT : LT);
}

static inline bool_t
capsEqual(cap_t a, cap_t b)
{
    return (cap_get_capSpaceType(a) == cap_get_capSpaceType(b)) &&
           ((word_t)cap_get_capSpacePtr(a) == (word_t)cap_get_capSpacePtr(b)) &&
           (cap_get_capSpaceSize(a) == cap_get_capSpaceSize(b)) &&
           (cap_get_capExtraComp(a) == cap_get_capExtraComp(b));
}

static inline int _compSpace(cte_t *a, int bSpaceType, word_t bSpacePtr, unsigned int bSpaceSize)
{
    int cmp;
    cmp = compare(cap_get_capSpaceType(a->cap), bSpaceType);
    if (cmp != EQ) {
        return cmp;
    }
    cmp = compare((word_t)cap_get_capSpacePtr(a->cap), bSpacePtr);
    if (cmp != EQ) {
        return cmp;
    }
    return - compare(cap_get_capSpaceSize(a->cap), bSpaceSize);
}

static inline int
_compDepth(cte_t *a, int bSpaceType, word_t bSpacePtr, unsigned int bSpaceSize, uint32_t bDepth)
{
    int cmp;
    cmp = _compSpace(a, bSpaceType, bSpacePtr, bSpaceSize);
    if (cmp != EQ) {
        return cmp;
    }
    return compare(mdb_node_get_cdtDepth(a->cteMDBNode), bDepth);
}

static inline int
_compExtra(cte_t *a, int bSpaceType, word_t bSpacePtr, unsigned int bSpaceSize, uint32_t bDepth, uint32_t bExtraComp)
{
    int cmp;
    cmp = _compDepth(a, bSpaceType, bSpacePtr, bSpaceSize, bDepth);
    if (cmp != EQ) {
        return cmp;
    }
    return compare(cap_get_capExtraComp(a->cap), bExtraComp);
}

static inline int
compExtra(cte_t *a, cte_t *b)
{
    return _compExtra(a, cap_get_capSpaceType(b->cap), (word_t)cap_get_capSpacePtr(b->cap), cap_get_capSpaceSize(b->cap), mdb_node_get_cdtDepth(b->cteMDBNode), cap_get_capExtraComp(b->cap));
}

static inline int
_compSlot(cte_t *a, int bSpaceType, word_t bSpacePtr, unsigned int bSpaceSize, uint32_t bDepth, uint32_t bExtraComp, cte_t *b)
{
    int cmp;
    cmp = _compExtra(a, bSpaceType, bSpacePtr, bSpaceSize, bDepth, bExtraComp);
    if (cmp != EQ) {
        return cmp;
    }
    return compare((word_t)a, (word_t)b);
}

static inline int
compSlot(cte_t *a, cte_t *b)
{
    return _compSlot(a, cap_get_capSpaceType(b->cap), (word_t)cap_get_capSpacePtr(b->cap), cap_get_capSpaceSize(b->cap), mdb_node_get_cdtDepth(b->cteMDBNode), cap_get_capExtraComp(b->cap), b);
}

cte_t *
cdtCapFindWithExtra(cap_t cap)
{
    return cdtFindWithExtra(cap_get_capSpaceType(cap), (word_t)cap_get_capSpacePtr(cap), cap_get_capSpaceSize(cap), cte_depth_bits_cap(cap));
}

cte_t *
cdtFindWithExtra(int spaceType, word_t paddr, unsigned int size, unsigned int depth_bits)
{
    uint32_t i;
    for (i = 0; i < BIT(depth_bits); i++) {
        cte_t *current;
        cte_t *next;

        next = ksRootCTE;
        do {
            int cmp;
            current = next;
            cmp = _compSpace(current, spaceType, paddr, size);
            if (cmp == EQ) {
                if (cap_get_capExtraComp(current->cap) != 0) {
                    return current;
                }
                cmp = _compExtra(current, spaceType, paddr, size, i, -1);
            }
            switch (cmp) {
            case LT:
                next = CTE_PTR(mdb_node_get_cdtRight(current->cteMDBNode));
                break;
            case GT:
                next = CTE_PTR(mdb_node_get_cdtLeft(current->cteMDBNode));
                break;
            case EQ:
                fail("Cannot be equal here");
            }
        } while (next);
    }
    return NULL;
}

cte_t *
cdtFindAtDepth(int spaceType, word_t paddr, unsigned int size, unsigned int extra, uint32_t depth)
{
    cte_t *current;
    cte_t *next;

    next = ksRootCTE;
    do {
        current = next;
        switch (_compExtra(current, spaceType, paddr, size, depth, extra)) {
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
cdtFind(int spaceType, word_t paddr, unsigned int size, unsigned int extra, unsigned int depth_bits)
{
    uint32_t i;
    cte_t *ret;
    for (i = 0; i < BIT(depth_bits); i++) {
        ret = cdtFindAtDepth(spaceType, paddr, size, extra, i);
        if (ret) {
            return ret;
        }
    }
    return NULL;
}

bool_t
cdtIsFinal(cte_t *slot)
{
    cte_t *current;
    cte_t *next;
    cte_t *closest;
    cte_t *search_for;
    word_t addr, size;
    int spaceType;
    uint32_t depth, extra;

    spaceType = cap_get_capSpaceType(slot->cap);
    addr = (word_t)cap_get_capSpacePtr(slot->cap);
    size = cap_get_capSpaceSize(slot->cap);
    extra = cap_get_capExtraComp(slot->cap);
    depth = mdb_node_get_cdtDepth(slot->cteMDBNode);

    /* We know that any capability to this same object will occur just 'next to'
       this object (if you consider the cdt tree as a flattened list). If we search
       for two hypothetical nodes, one position just before this node and one position
       justafter, then we can observe the closest nodes we find in either case */

    /* First search for one just before */
    search_for = slot - 1;
    next = ksRootCTE;
    closest = NULL;
    do {
        int cmp;
        current = next;
        cmp = _compDepth(current, spaceType, addr, size, depth);
        if (cmp == EQ) {
            /* If we are equal check the extra. if the extra is different then
               we already know we have found to things that are the same object */
            if (cap_get_capExtraComp(current->cap) != extra) {
                assert(sameObjectAs(current->cap, slot->cap));
                return false;
            }
            /* We break ties at this point on slot addresses */
            cmp = compare((word_t)current, (word_t)search_for);
        }
        if (cmp == LT) {
            if (!closest || compExtra(current, closest) == GT) {
                closest = current;
            }
            next = CTE_PTR(mdb_node_get_cdtRight(current->cteMDBNode));
        } else if (cmp == GT) {
            next = CTE_PTR(mdb_node_get_cdtLeft(current->cteMDBNode));
        } else {
            /* There is a remote chance that the slot we crafted does actually exist.
               In this case just return it early */
            return false;
        }
    } while (next);
    if (closest && sameObjectAs(closest->cap, slot->cap)) {
        return false;
    }

    /* Now search for one just after */
    search_for = slot + 1;
    next = ksRootCTE;
    closest = NULL;
    do {
        int cmp;
        current = next;
        cmp = _compDepth(current, spaceType, addr, size, depth);
        if (cmp == EQ) {
            /* If we are equal check the extra. if the extra is different then
               we already know we have found to things that are the same object */
            if (cap_get_capExtraComp(current->cap) != extra) {
                assert(sameObjectAs(current->cap, slot->cap));
                return false;
            }
            /* We break ties at this point on slot address */
            cmp = compare((word_t)current, (word_t)search_for);
        }
        if (cmp == GT) {
            if (!closest || compExtra(current, closest) == LT) {
                closest = current;
            }
            next = CTE_PTR(mdb_node_get_cdtLeft(current->cteMDBNode));
        } else if (cmp == LT) {
            next = CTE_PTR(mdb_node_get_cdtRight(current->cteMDBNode));
        } else {
            /* There is a remote chance that the slot we crafted does actually exist.
               In this case just return it early */
            return false;
        }
    } while (next);
    if (closest && sameObjectAs(slot->cap, closest->cap)) {
        return false;
    }
    return true;
}

cte_t *
cdtFindInRange(int spaceType, word_t addr, unsigned int size)
{
    cte_t *current;
    cte_t *next;
    /* We are searching for a hypothetical node that is at paddr+size */
    next = ksRootCTE;
    do {
        int cmp;
        current = next;
        cmp = _compSpace(current, spaceType, addr + size - 1, 0);
        if (cmp == LT) {
            if (cap_get_capSpaceType(current->cap) == spaceType &&
                    (word_t)cap_get_capSpacePtr(current->cap) < addr + size &&
                    (word_t)cap_get_capSpacePtr(current->cap) + cap_get_capSpaceSize(current->cap) > addr) {
                return current;
            }
            next = CTE_PTR(mdb_node_get_cdtRight(current->cteMDBNode));
        } else if (cmp == GT) {
            next = CTE_PTR(mdb_node_get_cdtLeft(current->cteMDBNode));
        } else {
            assert(!"Should never actually find this node as it has zero size");
        }
    } while (next);
    return NULL;
}

static cte_t *
_cdtFindChild(cte_t *parentSlot)
{
    cte_t *current;
    cte_t *largest;
    cte_t *next;
    int spaceType = cap_get_capSpaceType(parentSlot->cap);
    word_t paddr = (word_t)cap_get_capSpacePtr(parentSlot->cap);
    unsigned int size = cap_get_capSpaceSize(parentSlot->cap);
    /* We are searching for a hypothetical node in the cdt that is at paddr+size and of zero size */
    next = ksRootCTE;
    largest = NULL;
    do {
        int cmp;
        current = next;
        cmp = _compSpace(current, spaceType, paddr + size - 1, 0);
        if (cmp == LT) {
            if (!largest || compExtra(current, largest) == GT) {
                largest = current;
            }
            next = CTE_PTR(mdb_node_get_cdtRight(current->cteMDBNode));
        } else if (cmp == GT) {
            next = CTE_PTR(mdb_node_get_cdtLeft(current->cteMDBNode));
        } else {
            assert(!"Should never actually find this node as it has zero size");
        }
    } while (next);
    /* Verify what we found is actually a child */
    if (!largest || compExtra(largest, parentSlot) != GT || (word_t)cap_get_capSpacePtr(largest->cap) + cap_get_capSpaceSize(largest->cap) <= paddr || (word_t)cap_get_capSpacePtr(largest->cap) + cap_get_capSpaceSize(largest->cap) > paddr + size) {
        return NULL;
    }
    return largest;
}

cte_t *
cdtFindChild(cte_t *parentSlot)
{
    if (cap_get_capSpaceType(parentSlot->cap) == capSpaceUntypedMemory) {
        /* Find anything in this range that is typed */
        cte_t *result = cdtFindInRange(capSpaceTypedMemory, (word_t)cap_get_capSpacePtr(parentSlot->cap), cap_get_capSpaceSize(parentSlot->cap));
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

static inline cte_t * aaSucc(cte_t *slot)
{
    cte_t *left;

    left = CTE_PTR(mdb_node_get_cdtLeft(slot->cteMDBNode));
    while (left) {
        slot = left;
        left = CTE_PTR(mdb_node_get_cdtLeft(slot->cteMDBNode));
    }
    return slot;
}

static inline cte_t * aaPred(cte_t *slot)
{
    cte_t *right;

    right = CTE_PTR(mdb_node_get_cdtRight(slot->cteMDBNode));
    while (right) {
        slot = right;
        right = CTE_PTR(mdb_node_get_cdtRight(slot->cteMDBNode));
    }
    return slot;
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
    case cap_async_endpoint_cap:
        return "AsyncEndpoint";
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
        printf("%s [NULL]@0x%x", msg, cte);
    } else  {
        printf("%s [%d %s(%d) { addr = 0x%x, size = 0x%x } left: 0x%x right: 0x%x depth: %d extra: 0x%x]@0x%x\n",
               msg,
               mdb_node_get_cdtLevel(cte->cteMDBNode),
               printCap(cte->cap),
               cap_get_capType(cte->cap),
               cap_get_capType(cte->cap) == cap_null_cap ? 0 : (word_t)cap_get_capSpacePtr(cte->cap),
               cap_get_capType(cte->cap) == cap_null_cap ? 0 : cap_get_capSpaceSize(cte->cap),
               mdb_node_get_cdtLeft(cte->cteMDBNode),
               mdb_node_get_cdtRight(cte->cteMDBNode),
               mdb_node_get_cdtDepth(cte->cteMDBNode),
               cap_get_capType(cte->cap) == cap_null_cap ? 0 : cap_get_capExtraComp(cte->cap),
               cte);
    }
}
