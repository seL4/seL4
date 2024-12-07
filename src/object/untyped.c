/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <config.h>
#include <types.h>
#include <api/failures.h>
#include <api/syscall.h>
#include <api/invocation.h>
#include <machine/io.h>
#include <object/structures.h>
#include <object/untyped.h>
#include <object/objecttype.h>
#include <object/cnode.h>
#include <kernel/cspace.h>
#include <kernel/thread.h>
#include <util.h>

static word_t alignUp(word_t baseValue, word_t alignment)
{
    /* Must be power-of-two */
    assert((alignment & (alignment - 1)) == 0);
    return (baseValue + alignment - 1ul) & ~(alignment - 1ul);
}

exception_t decodeUntypedInvocation(word_t invLabel, word_t length, cte_t *slot,
                                    cap_t cap, bool_t call, word_t *buffer)
{
    word_t newType, userObjSize, nodeIndex;
    word_t nodeDepth, nodeOffset, nodeWindow;
    cte_t *rootSlot UNUSED;
    exception_t status;
    cap_t nodeCap;
    lookupSlot_ret_t lu_ret;
    word_t nodeSize;
    word_t i;
    cte_t *destCNode;
    word_t freeRef, alignedFreeRef, objectSize, totalSize, untypedFreeBytes;
    bool_t deviceMemory;

    /* Ensure operation is valid. */
    if (invLabel != UntypedRetype) {
        userError("Untyped cap: Illegal operation attempted.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    /* Ensure message length valid. */
    if (length < 6 || current_extra_caps.excaprefs[0] == NULL) {
        userError("Untyped invocation: Truncated message.");
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

    /* Fetch arguments. */
    newType     = getSyscallArg(0, buffer);
    userObjSize = getSyscallArg(1, buffer);
    nodeIndex   = getSyscallArg(2, buffer);
    nodeDepth   = getSyscallArg(3, buffer);
    nodeOffset  = getSyscallArg(4, buffer);
    nodeWindow  = getSyscallArg(5, buffer);

    rootSlot = current_extra_caps.excaprefs[0];

    if (newType == seL4_UntypedObject) {
        userObjSize = BIT(userObjSize); //TODO: Remove when changing API
    }

    /* Is the requested object type valid? */
    if (newType >= seL4_ObjectTypeCount) {
        userError("Untyped Retype: Invalid object type.");
        current_syscall_error.type = seL4_InvalidArgument;
        current_syscall_error.invalidArgumentNumber = 0;
        return EXCEPTION_SYSCALL_ERROR;
    }

    objectSize = getObjectSize(newType, userObjSize);
    //TODO: Prevent overflow of nodeWindow * objectSize
    totalSize = nodeWindow * objectSize;

    /* Exclude impossibly large object sizes. getObjectSize can overflow if userObjSize
       is close to 2^wordBits, which is nonsensical in any case, so we check that this
       did not happen. userObjSize will always need to be less than wordBits. */
    if ((newType != seL4_UntypedObject && userObjSize >= wordBits) ||
            objectSize > BIT(seL4_MaxUntypedBits)) {
        userError("Untyped Retype: Invalid object size.");
        current_syscall_error.type = seL4_RangeError;
        current_syscall_error.rangeErrorMin = 0;
        current_syscall_error.rangeErrorMax = seL4_MaxUntypedBits;
        return EXCEPTION_SYSCALL_ERROR;
    }

    /* If the target object is a CNode, is it at least size 1? */
    if (newType == seL4_CapTableObject && userObjSize == 0) {
        userError("Untyped Retype: Requested CapTable size too small.");
        current_syscall_error.type = seL4_InvalidArgument;
        current_syscall_error.invalidArgumentNumber = 1;
        return EXCEPTION_SYSCALL_ERROR;
    }

#ifdef CONFIG_KERNEL_MCS
    if (newType == seL4_SchedContextObject && userObjSize < seL4_MinSchedContextBits) {
        userError("Untyped retype: Requested a scheduling context too small.");
        current_syscall_error.type = seL4_InvalidArgument;
        current_syscall_error.invalidArgumentNumber = 1;
        return EXCEPTION_SYSCALL_ERROR;
    }
#endif

    /* Lookup the destination CNode (where our caps will be placed in). */
    if (nodeDepth == 0) {
        nodeCap = current_extra_caps.excaprefs[0]->cap;
    } else {
        cap_t rootCap = current_extra_caps.excaprefs[0]->cap;
        lu_ret = lookupTargetSlot(rootCap, nodeIndex, nodeDepth);
        if (lu_ret.status != EXCEPTION_NONE) {
            userError("Untyped Retype: Invalid destination address.");
            return lu_ret.status;
        }
        nodeCap = lu_ret.slot->cap;
    }

    /* Is the destination actually a CNode? */
    if (cap_get_capType(nodeCap) != cap_cnode_cap) {
        userError("Untyped Retype: Destination cap invalid or read-only.");
        current_syscall_error.type = seL4_FailedLookup;
        current_syscall_error.failedLookupWasSource = 0;
        current_lookup_fault = lookup_fault_missing_capability_new(nodeDepth);
        return EXCEPTION_SYSCALL_ERROR;
    }

    /* Is the region where the user wants to put the caps valid? */
    nodeSize = 1ul << cap_cnode_cap_get_capCNodeRadix(nodeCap);
    if (nodeOffset > nodeSize - 1) {
        userError("Untyped Retype: Destination node offset #%d too large.",
                  (int)nodeOffset);
        current_syscall_error.type = seL4_RangeError;
        current_syscall_error.rangeErrorMin = 0;
        current_syscall_error.rangeErrorMax = nodeSize - 1;
        return EXCEPTION_SYSCALL_ERROR;
    }
    if (nodeWindow < 1 || nodeWindow > CONFIG_RETYPE_FAN_OUT_LIMIT) {
        userError("Untyped Retype: Number of requested objects (%d) too small or large.",
                  (int)nodeWindow);
        current_syscall_error.type = seL4_RangeError;
        current_syscall_error.rangeErrorMin = 1;
        current_syscall_error.rangeErrorMax = CONFIG_RETYPE_FAN_OUT_LIMIT;
        return EXCEPTION_SYSCALL_ERROR;
    }
    if (nodeWindow > nodeSize - nodeOffset) {
        userError("Untyped Retype: Requested destination window overruns size of node.");
        current_syscall_error.type = seL4_RangeError;
        current_syscall_error.rangeErrorMin = 1;
        current_syscall_error.rangeErrorMax = nodeSize - nodeOffset;
        return EXCEPTION_SYSCALL_ERROR;
    }

    /* Ensure that the destination slots are all empty. */
    destCNode = CTE_PTR(cap_cnode_cap_get_capCNodePtr(nodeCap));
    for (i = nodeOffset; i < nodeOffset + nodeWindow; i++) {
        status = ensureEmptySlot(destCNode + i);
        if (status != EXCEPTION_NONE) {
            userError("Untyped Retype: Slot #%d in destination window non-empty.",
                      (int)i);
            return status;
        }
    }

    deviceMemory = cap_untyped_cap_get_capIsDevice(cap);
    if ((deviceMemory && !Arch_isFrameType(newType))
        && newType != seL4_UntypedObject) {
        userError("Untyped Retype: Creating kernel objects with device untyped");
        current_syscall_error.type = seL4_InvalidArgument;
        current_syscall_error.invalidArgumentNumber = 1;
        return EXCEPTION_SYSCALL_ERROR;
    }

    /*
     * Determine the maximum number of objects we can create, and return an
     * error if we don't have enough space.
     */
    word_t base = cap_untyped_cap_get_capPtr(cap);
    alignedFreeRef = freeRef = base;
    /*
     * If there are child objects, the oldest one will be first.
     * Use its ptr addres + object size to find the first free location:
     */
    cte_t *next = CTE_PTR(mdb_node_get_mdbNext(slot->cteMDBNode));
    if (next && sameRegionAs(cap, next->cap)) {
        cap_t child = next->cap;
        alignedFreeRef = freeRef = (word_t)cap_get_capPtr(child) + cap_get_capSize(child);
    }
    if (newType != seL4_UntypedObject) {
        /* Align the free region to the target object's size. */
        alignedFreeRef = alignUp(freeRef, objectSize);
        word_t end = base + cap_untyped_cap_get_capSize(cap);
        if (alignedFreeRef < freeRef || alignedFreeRef > end) {
            userError("Untyped Retype: Insufficient memory "
                    "(%lu * %lu bytes needed, %lu bytes available).",
                    nodeWindow, objectSize, end - freeRef);
            current_syscall_error.type = seL4_NotEnoughMemory;
            current_syscall_error.memoryLeft = end - freeRef;
            return EXCEPTION_SYSCALL_ERROR;
        }
    }
    untypedFreeBytes = base + cap_untyped_cap_get_capSize(cap) - alignedFreeRef;

    if (untypedFreeBytes < totalSize) {
        userError("Untyped Retype: Insufficient memory "
                  "(%lu * %lu bytes needed, %lu bytes available).",
                  nodeWindow, objectSize, untypedFreeBytes);
        current_syscall_error.type = seL4_NotEnoughMemory;
        current_syscall_error.memoryLeft = untypedFreeBytes;
        return EXCEPTION_SYSCALL_ERROR;
    }

    /* Perform the retype. */
    setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);
    return invokeUntyped_Retype(slot, (void *)freeRef, newType, userObjSize, totalSize,
                                destCNode, nodeOffset, nodeWindow, deviceMemory);
}

exception_t invokeUntyped_Retype(cte_t *srcSlot, void *retypeBase,
                                 object_t newType, word_t userSize, word_t total_size,
                                 cte_t *destCNode, word_t destOffset, word_t destLength,
                                 bool_t deviceMemory)
{
    if (newType != seL4_UntypedObject && newType != seL4_CapTableObject && !deviceMemory) {
        memzero(retypeBase, total_size);
    }
    /* Create new objects and caps. */
    createNewObjects(newType, srcSlot, destCNode, destOffset, destLength,
                     retypeBase, userSize, deviceMemory);

    return EXCEPTION_NONE;
}
