%
% Copyright 2014, General Dynamics C4 Systems
%
% This software may be distributed and modified according to the terms of
% the GNU General Public License version 2. Note that NO WARRANTY is provided.
% See "LICENSE_GPLv2.txt" for details.
%
% @TAG(GD_GPL)
%

This module defines the types and functions related to the kernel
objects used to represent a capability space. It contains
implementations of the operations that user-level threads can request
by invoking a capability table node. It is also responsible for
creating the "Capability" objects used at higher levels of the kernel.

> module SEL4.Object.CNode (
>         cteRevoke, cteDelete, cteInsert, cteDeleteOne,
>         ensureNoChildren, ensureEmptySlot, slotCapLongRunningDelete,
>         getSlotCap, locateSlot, getReceiveSlots,
>         getCTE, setupReplyMaster,
>         insertInitCap, decodeCNodeInvocation, invokeCNode,
>         updateCap, isFinalCapability, createNewObjects
>     ) where

\begin{impdetails}

> {-# BOOT-IMPORTS: SEL4.Machine SEL4.API.Types SEL4.API.Failures SEL4.Model SEL4.Object.Structures SEL4.API.Invocation #-}
> {-# BOOT-EXPORTS: ensureNoChildren getSlotCap locateSlot ensureEmptySlot insertInitCap cteInsert cteDelete cteDeleteOne decodeCNodeInvocation invokeCNode getCTE updateCap isFinalCapability createNewObjects #-}

> import SEL4.API.Types
> import SEL4.API.Failures
> import SEL4.API.Invocation
> import SEL4.Machine
> import SEL4.Model
> import SEL4.Object.Structures
> import SEL4.Object.Interrupt
> import SEL4.Object.Instances()
> import SEL4.Object.ObjectType
> import {-# SOURCE #-} SEL4.Object.TCB
> import {-# SOURCE #-} SEL4.Kernel.CSpace

> import Data.Bits

\end{impdetails}

\subsection{Capability Node Object Invocations}

The following function decodes a CNode invocation message, and checks for any error conditions.

> decodeCNodeInvocation :: Word -> [Word] -> Capability -> [Capability] ->
>         KernelF SyscallError CNodeInvocation
> decodeCNodeInvocation label (index:bits:args)
>         cap@(CNodeCap {}) extraCaps
>   = do

The first check is that the invocation type requested is a CNode operation.

>     let inv = invocationType label
>     unless (inv `elem` [CNodeRevoke .. CNodeSaveCaller]) $
>         throw IllegalOperation

All CNode operations require the caller to specify a slot in the capability space mapped by the tree whose root is the invoked node, by providing a pointer in the space defined by the tree and the depth of the tree.

The lookup can be terminated early, and therefore operate on a capability at one of the intermediate levels, by reducing the specified depth and shifting the address to the right by the corresponding number of bits. However, the depth \emph{must} be that of a CNode existing in the specified region of the tree; if not, the operation will fail with a "MissingCapabilityError".

>     destSlot <- lookupTargetSlot cap (CPtr index) (fromIntegral bits)

>     case (inv `elem` [CNodeCopy .. CNodeMutate], inv, args, extraCaps) of

The "Copy", "Mint", "Move" and "Mutate" operations are similar. Each creates a new capability, and must specify a source slot, in the same manner as the destination.

>         (True, _, srcIndex:srcDepth:args, srcRootCap:_) -> do

For these operations, the destination slot must be empty, so a new capability can be created in it.

>             ensureEmptySlot destSlot

The source slot is located in a similar way to the destination slot, but starting from an additional root capability provided to the system call.

>             srcSlot <- lookupSourceSlot srcRootCap
>                 (CPtr srcIndex) (fromIntegral srcDepth)

The source slot must contain a valid capability.

>             srcCTE <- withoutFailure $ getCTE srcSlot
>             when (isNullCap $ cteCap srcCTE) $
>                 throw $ FailedLookup True $ MissingCapability {
>                     missingCapBitsLeft = fromIntegral srcDepth }

Rights may be masked for the "Copy" and "Mint" operations. The capability data is adjusted on the "Mint" and "Mutate" operations.

>             (rights, capData) <-
>               case (inv, args) of
>                 (CNodeCopy, rights:_) ->
>                     return $! (rightsFromWord rights, Nothing)
>                 (CNodeMint, rights:newData:_) ->
>                     return $! (rightsFromWord rights, Just newData)
>                 (CNodeMove, _) ->
>                     return $! (allRights, Nothing)
>                 (CNodeMutate, newData:_) ->
>                     return $! (allRights, Just newData)
>                 _ -> throw TruncatedMessage

The moving system calls, "Move" and "Mutate", are differentiated from the copying system calls, "Copy" and "Mint".

>             let isMove = inv `elem` [CNodeMove, CNodeMutate]

The rights and capability data word are applied to the source capability to create a new capability.
             
>             let srcCap = maskCapRights rights $ cteCap srcCTE
>             newCap <- (if isMove then return else deriveCap srcSlot) $
>               case capData of
>                 Just w -> updateCapData isMove w srcCap
>                 Nothing -> srcCap
>             when (isNullCap newCap) $ throw IllegalOperation
>             
>             return $!
>                 (if isMove then Move else Insert) newCap srcSlot destSlot

The "Revoke", "Delete", "SaveCaller" and "Recycle" operations have no additional arguments. The "SaveCaller" call requires the target slot to be empty.

>         (_, CNodeRevoke, _, _) -> return $ Revoke destSlot
>         (_, CNodeDelete, _, _) -> return $ Delete destSlot
>         (_, CNodeSaveCaller, _, _) -> do
>             ensureEmptySlot destSlot
>             return $ SaveCaller destSlot

For "Recycle", the slot must contain a valid capability. 

>         (_, CNodeRecycle, _, _) -> do
>             cte <- withoutFailure $ getCTE destSlot
>             unless (hasRecycleRights $ cteCap cte) $ throw IllegalOperation
>             return $ Recycle destSlot

The "Rotate" operation atomically moves two capabilities.

>         (_, CNodeRotate,
>          pivotNewData:pivotIndex:pivotDepth:srcNewData:srcIndex:srcDepth:_,
>          pivotRootCap:srcRootCap:_) -> do

The source and pivot slots are located in a similar way to the destination slot, but starting from two additional root capabilities provided to the system call.

>             srcSlot <- lookupSourceSlot srcRootCap
>                     (CPtr srcIndex) (fromIntegral srcDepth)
>             pivotSlot <- lookupPivotSlot pivotRootCap
>                     (CPtr pivotIndex) (fromIntegral pivotDepth)

The pivot slot must be distinct from the source and destination slots.

>             when (pivotSlot == srcSlot || pivotSlot == destSlot) $
>                 throw IllegalOperation

The destination slot must be empty, unless it is the same as the source slot (in which case its contents will be swapped with those of the pivot slot).

>             unless (srcSlot == destSlot) $ ensureEmptySlot destSlot

The source and pivot slots must contain valid capabilities.

>             srcCap <- withoutFailure $ liftM cteCap $ getCTE srcSlot
>             when (isNullCap srcCap) $
>                 throw $ FailedLookup True $ MissingCapability {
>                     missingCapBitsLeft = fromIntegral srcDepth }
>             pivotCap <- withoutFailure $ liftM cteCap $ getCTE pivotSlot
>             when (isNullCap pivotCap) $
>                 throw $ FailedLookup False $ MissingCapability {
>                     missingCapBitsLeft = fromIntegral pivotDepth }

The two moved capabilities are updated with the provided data words.

>             let newSrcCap = updateCapData True srcNewData srcCap
>             let newPivotCap = updateCapData True pivotNewData pivotCap

The moved capabilities must not be null.

>             when (isNullCap newSrcCap) $ throw IllegalOperation
>             when (isNullCap newPivotCap) $ throw IllegalOperation

>             return $! Rotate newSrcCap newPivotCap srcSlot pivotSlot destSlot

Otherwise, the message was too short.

>         _ -> throw TruncatedMessage

> decodeCNodeInvocation label _ (CNodeCap {}) _
>     = throw $ if invocationType label `elem` [CNodeRevoke .. CNodeSaveCaller]
>         then TruncatedMessage
>         else IllegalOperation

> decodeCNodeInvocation _ _ _ _ = fail "decodeCNodeInvocation: invalid cap"

The function "invokeCNode" dispatches an invocation to one of the handlers defined below, given a "CNodeInvocation" object.

> invokeCNode :: CNodeInvocation -> KernelP ()
>
> invokeCNode (Revoke destSlot) = cteRevoke destSlot
>
> invokeCNode (Delete destSlot) = cteDelete destSlot True
> 
> invokeCNode (Recycle destSlot) = cteRecycle destSlot
> 
> invokeCNode (Insert cap srcSlot destSlot) =
>     withoutPreemption $ cteInsert cap srcSlot destSlot
>     
> invokeCNode (Move cap srcSlot destSlot) =
>     withoutPreemption $ cteMove cap srcSlot destSlot
>     
> invokeCNode (Rotate cap1 cap2 slot1 slot2 slot3) = withoutPreemption $
>     if (slot1 == slot3)
>       then cteSwap cap1 slot1 cap2 slot2
>       else do 
>              cteMove cap2 slot2 slot3
>              cteMove cap1 slot1 slot2

> invokeCNode (SaveCaller destSlot) = withoutPreemption $ do
>     thread <- getCurThread
>     srcSlot <- getThreadCallerSlot thread
>     cap <- getSlotCap srcSlot
>     case cap of
>         NullCap -> return ()
>         ReplyCap { capReplyMaster = False } -> cteMove cap srcSlot destSlot
>         _ -> fail "caller capability must be null or reply"

\subsection{CNode Operations}

The following functions define the operations that can be performed by a CNode invocation.

\subsubsection{Inserting New Capabilities}

> setUntypedCapAsFull :: Capability -> Capability -> PPtr CTE -> Kernel ()
> setUntypedCapAsFull srcCap newCap srcSlot = do
>         if (isUntypedCap srcCap && isUntypedCap newCap &&
>            capPtr srcCap == capPtr newCap && capBlockSize srcCap == capBlockSize newCap)
>            then updateCap srcSlot (srcCap { capFreeIndex = maxFreeIndex (capBlockSize srcCap) }) else return ()

Insertion of new capabilities copied from existing capabilities is performed by "cteInsert". The parameters are the physical addresses of the source and destination slots, and the capability to be inserted in the destination slot. This function requires the destination slot to be empty, and assumes that the provided capability is one that may be derived from the contents of the source slot.

> cteInsert :: Capability -> PPtr CTE -> PPtr CTE -> Kernel ()
> cteInsert newCap srcSlot destSlot = do

First, fetch the capability table entry for the source.

>         srcCTE <- getCTE srcSlot
>         let srcMDB = cteMDBNode srcCTE
>         let srcCap = cteCap srcCTE

If the newly created capability is allowed to become a parent of other capabilities in the MDB, it must be marked \emph{revocable}.

>         let newCapIsRevocable = case newCap of

If the new capability is an endpoint capability, then it can be an MDB parent if and only if its badge is being changed by this operation.

>                 EndpointCap {} ->
>                     capEPBadge newCap /= capEPBadge srcCap
>                 AsyncEndpointCap {} ->
>                     capAEPBadge newCap /= capAEPBadge srcCap

If the new capability is the first IRQ handler for a given IRQ, then it can be an MDB parent.

>                 IRQHandlerCap {} -> isIRQControlCap srcCap

Untyped capabilities can always be MDB parents.

>                 UntypedCap {} -> True

Any other capability created by this function is a leaf of the derivation tree, and cannot be used to revoke other capabilities.

>                 _ -> False

Create the new capability table entry. Its "MDBNode" is inserted in the mapping database immediately after the existing capability.

The "mdbRevocable" bit is set if the capability is revocable, as determined by the test above. There is a second bit in the MDB, "mdbFirstBadged", that is effective only for endpoint capabilities; it is used to distinguish between capabilities badged by separate "Mint" operations, and marks the leftmost capability in the MDB that was created by the present "Mint" operation. If this capability is deleted, the bit's value will be moved to the next capability to the right (see "cteDelete").

>         let newMDB = srcMDB {
>                 mdbPrev = srcSlot,
>                 mdbRevocable = newCapIsRevocable,
>                 mdbFirstBadged = newCapIsRevocable }

The destination slot must be empty.

>         oldCTE <- getCTE destSlot
>         assert (isNullCap $ cteCap oldCTE) 
>                 "cteInsert to non-empty destination"
>         assert (mdbPrev (cteMDBNode oldCTE) == nullPointer && 
>                 mdbNext (cteMDBNode oldCTE) == nullPointer) 
>                 "cteInsert: mdb entry must be empty"
>         setUntypedCapAsFull srcCap newCap srcSlot 
 
Store the new entry in the destination slot and update the mapping database.

>         updateCap destSlot newCap
>         updateMDB destSlot (const newMDB)
>         updateMDB srcSlot (\m -> m { mdbNext = destSlot })
>         updateMDB (mdbNext newMDB) (\m -> m { mdbPrev = destSlot })

\subsubsection{Moving Capabilities}

> cteMove :: Capability -> PPtr CTE -> PPtr CTE -> Kernel ()
> cteMove newCap srcSlot destSlot = do

The destination slot must be empty.

>         oldCTE <- getCTE destSlot
>         assert (isNullCap $ cteCap oldCTE) 
>                 "cteMove to non-empty destination"
>         assert (mdbPrev (cteMDBNode oldCTE) == nullPointer && 
>                 mdbNext (cteMDBNode oldCTE) == nullPointer) 
>                 "cteMove: mdb entry must be empty"

Move the "CTE" into the new slot and update the mapping database.

>         cte <- getCTE srcSlot
>         let mdb = cteMDBNode cte
>         updateCap destSlot newCap
>         updateCap srcSlot NullCap
>         updateMDB destSlot (const mdb)
>         updateMDB srcSlot (const nullMDBNode)
>         updateMDB (mdbPrev mdb) (\m -> m { mdbNext = destSlot })
>         updateMDB (mdbNext mdb) (\m -> m { mdbPrev = destSlot })

\subsubsection{Swapping Capabilities}

To avoid long-running recursive "Delete" operations, it is sometimes necessary to swap the contents of two slots (which are never visible in the capability space at that time). See the definition of "reduceZombie" for details about when and why this is done. The function "capSwapForDelete" is used to perform these swaps.

> capSwapForDelete :: PPtr CTE -> PPtr CTE -> Kernel ()
> capSwapForDelete slot1 slot2 = when (slot1 /= slot2) $ do
>     cap1 <- liftM cteCap $ getCTE slot1
>     cap2 <- liftM cteCap $ getCTE slot2
>     cteSwap cap1 slot1 cap2 slot2

The following function is used to atomically swap the contents of two slots, which must be distinct and contain valid capabilities. It is used by "capSwapForDelete", and by the "Rotate" system call if the source and destination slots are the same.

Note the order of the MDB updates; this is necessary in case the two capabilities are MDB siblings.

> cteSwap :: Capability -> PPtr CTE -> Capability -> PPtr CTE -> Kernel ()
> cteSwap cap1 slot1 cap2 slot2 = do
>     cte1 <- getCTE slot1
>     updateCap slot1 cap2
>     updateCap slot2 cap1
>     let mdb1 = cteMDBNode cte1
>     updateMDB (mdbPrev mdb1) (\m -> m { mdbNext = slot2 })
>     updateMDB (mdbNext mdb1) (\m -> m { mdbPrev = slot2 })
>     cte2 <- getCTE slot2
>     let mdb2 = cteMDBNode cte2
>     updateMDB slot1 (const mdb2)
>     updateMDB slot2 (const mdb1)
>     updateMDB (mdbPrev mdb2) (\m -> m { mdbNext = slot1 })
>     updateMDB (mdbNext mdb2) (\m -> m { mdbPrev = slot1 })

\subsubsection{Revoking Capabilities}

The work for the revoke operation is done by "cteRevoke", which revokes all capabilities which are children of the invoked capability in the derivation tree.

> cteRevoke :: PPtr CTE -> KernelP ()
> cteRevoke slot = do

Load the CTE being revoked.

>     cte <- withoutPreemption $ getCTE slot
>     let nextPtr = mdbNext $ cteMDBNode cte

Determine whether the CTE is valid, and has children to be deleted. If a child is found, then it is deleted, and we look for another; otherwise the revocation is complete. There is a preemption point immediately after each capability deletion.

>     unless ((isNullCap $ cteCap cte) || (nextPtr == nullPointer)) $ do
>         nextCTE <- withoutPreemption $ getCTE nextPtr
>         when (cte `isMDBParentOf` nextCTE) $ do
>             cteDelete nextPtr True
>             preemptionPoint
>             cteRevoke slot

\subsubsection{Deleting Capabilities}
\label{sec:object.cnode.ops.delete}

This function deletes the capability in a given slot. If it is the last remaining capability for the given object, the object will be destroyed.

The run time of "cteDelete" is unbounded under certain circumstances, so the operation must include preemption points. Specifically, deleting the last capability to a CNode or TCB may recursively call "cteDelete". Keeping the kernel's state consistent during such operations is a complex task.

> cteDelete :: PPtr CTE -> Bool -> KernelP ()
> cteDelete slot exposed = do
>     (success, irq) <- finaliseSlot slot exposed
>     when (exposed || success) $ withoutPreemption $ emptySlot slot irq

This helper routine empties a slot. The routine may be called on the same
slot twice as a result of recursion between "cteDelete" and "reduceZombie",
thus a check is made as to whether the slot is already empty.

The deletion of the final IRQHandlerCap to any IRQ should be followed by
certain operations handled by deletedIRQHandler, including the clearing of
the bitmask bit that will allow the reissue of an IRQHandlerCap to this IRQ.

> emptySlot :: PPtr CTE -> Maybe IRQ -> Kernel ()
> emptySlot slot irq = do
>     newCTE <- getCTE slot
>     let mdbNode = cteMDBNode newCTE
>     let prev = mdbPrev mdbNode
>     let next = mdbNext mdbNode

>     case (cteCap newCTE) of
>         NullCap   -> return ()
>         _         -> do
>             updateMDB prev (\mdb -> mdb { mdbNext = next })
>             updateMDB next (\mdb -> mdb {
>                     mdbPrev = prev,
>                     mdbFirstBadged =
>                         mdbFirstBadged mdb || mdbFirstBadged mdbNode })
>             updateCap slot NullCap
>             updateMDB slot (const nullMDBNode)

>             case irq of
>                 Just irq  -> deletedIRQHandler irq
>                 Nothing   -> return ()

If the deleted capability is marked as being the leftmost endpoint capability in the tree with a given badge, the next capability to the right in the tree will inherit that property. This is significant only if the next capability is a sibling of the deleted one; otherwise the bit either has no effect or is already set.

This helper routine prepares a slot to be emptied. In most cases, this simply involves taking a finalisation action relevant to the contained capability; that action is implemented in "finaliseCap", in \autoref{sec:object.objecttype.finalise}. However, if the capability contains references to other slots, a Zombie will be returned from "finaliseCap" to indicate this, and the resulting recursive problem must be handled carefully.

The boolean in the return value indicates whether the slot was successfully finalised. When the "exposed" flag is set, this will always be true. The unexposed operation may abort when it encounters a zombie capability that is already pointing at the slot it is in. Zombies are always the final reference to any given object, thus this Zombie must also be the capability that the exposed deletion operation (which is the recursive parent) used to recurse on. This case can be handled by returning control to the recursive parent without taking any action.

The optional IRQ in the return value indicates any IRQ lines whose final IRQ handling capability will be freed by the deletion of the capability in the slot. The 'deletedIRQHandler' function should be called once the slot is empty.

> finaliseSlot :: PPtr CTE -> Bool -> KernelP (Bool, Maybe IRQ)
> finaliseSlot slot exposed = do

Load the contents of the slot.

>     cte <- withoutPreemption $ getCTE slot

>     if isNullCap $ cteCap cte then return (True, Nothing) else do

Determine whether this is a final capability.

>         final <- withoutPreemption $ isFinalCapability cte

Perform any type-specific finalisation actions associated with the capability.

>         (remainder, irq) <- withoutPreemption $
>                                 finaliseCap (cteCap cte) final False

At this point, the capability can safely be replaced with the returned
remainder, which is either a "NullCap" or a "Zombie". If this cap is removable, we
are finished finalising the slot. If not, we should save the returned cap as a
potential resume point in the event of preemption, and then attempt to reduce
further. Once a reduction step is taken we will allow preemption, and then
continue finalising in this slot.

>         if capRemovable remainder slot then return (True, irq) else
>             if not exposed && capCyclicZombie remainder slot
>             then do
>                 withoutPreemption $ updateCap slot remainder
>                 return (False, Nothing)
>             else do
>                 withoutPreemption $ updateCap slot remainder
>                 reduceZombie remainder slot exposed
>                 preemptionPoint
>                 finaliseSlot slot exposed

This helper determines if a capability returned by "finaliseCap" can be removed from its slot. "Zombie" capabilities are only removable if they point to zero slots or only to the slot they are in. For all other object types, "finaliseCap" returns "NullCap", to indicate that the capability is removable.

> capRemovable :: Capability -> PPtr CTE -> Bool
> capRemovable NullCap _ = True
> capRemovable (Zombie { capZombiePtr = slot', capZombieNumber = n }) slot =
>     (n == 0) || (n == 1 && slot == slot')
> capRemovable _ _ = error "finaliseCap should only return Zombie or NullCap"

This helper determines if a capability returned by "finaliseCap" is a cyclic Zombie. If so, unexposed deletion operations should abort.

> capCyclicZombie :: Capability -> PPtr CTE -> Bool
> capCyclicZombie NullCap _ = False
> capCyclicZombie (Zombie { capZombiePtr = slot' }) slot =
>     slot == slot'
> capCyclicZombie _ _ = False

This helper takes a reduction step on an unremovable "Zombie" capability.

> reduceZombie :: Capability -> PPtr CTE -> Bool -> KernelP ()

Unremovable "Zombie" capabilities must point to at least one slot.

> reduceZombie (Zombie { capZombieNumber = 0 }) _ _ =
>     fail "reduceZombie expected unremovable Zombie"

When the "Zombie" is not exposed, a swap operation is performed, moving the "Zombie" capability into the location it points to. This cannot already be the case, as if it were, the "Zombie" capability itself would be the only exposed capability to itself, and thus the target of the exposed deletion calling for the unexposed one. This in turn requires the "Zombie" to be of size 1, thus removable.

> reduceZombie (Zombie { capZombiePtr = ptr }) slot False = do
>     assert (ptr /= slot) "Cyclic zombie passed to unexposed reduceZombie."
>     capAtPtr <- withoutPreemption $ liftM cteCap $ getCTE ptr
>     case capAtPtr of
>         (Zombie { capZombiePtr = ptr2 }) -> assert (ptr2 /= ptr)
>                 "Moving self-referential Zombie aside."
>         _ -> return ()
>     withoutPreemption $ capSwapForDelete ptr slot

When the Zombie is exposed, the reduction operation deletes the contents of one of the slots the Zombie points to. The Zombie is then reduced in size. A corner case exists when the deletion operation, which is unexposed, swaps a Zombie capability discovered back into the slot in which our Zombie was or even clears our slot entirely. In this case the deletion operation may even fail to clear the slot. For this reason, we check whether the Zombie is unchanged before shrinking it.

> reduceZombie z@(Zombie { capZombiePtr = ptr, capZombieNumber = n }) slot True = do
>     endSlot <- withoutPreemption $ locateSlot ptr (fromIntegral (n - 1))
>     cteDelete endSlot False
>     ourCTE  <- withoutPreemption $ getCTE slot
>     case (cteCap ourCTE) of
>         NullCap -> return ()
>         c2@(Zombie { capZombiePtr = ptr2 }) ->
>             if (ptr == ptr2 && capZombieNumber c2 == n
>                     && capZombieType z == capZombieType c2)
>             then withoutPreemption $ do
>                 endCTE <- getCTE endSlot
>                 assert (isNullCap $ cteCap endCTE)
>                     "Expected cteDelete to clear slot or overwrite existing."
>                 let newCap = z { capZombieNumber = n-1 }
>                 updateCap slot newCap
>             else assert (ptr2 == slot && ptr /= slot)
>                     "Expected new Zombie to be self-referential."
>         _ -> fail "Expected recursion to result in Zombie."

> reduceZombie _ _ _ = fail "reduceZombie expected Zombie"

In some cases we call for the deletion of a capability which we know can be deleted without recursion. For this case we have a non-preemptible way of calling the delete operation. For technical reasons this is done here by unfolding the definitions of "cteDelete" and "finaliseSlot".

> cteDeleteOne :: PPtr CTE -> Kernel ()
> cteDeleteOne slot = do
>     cte <- getCTE slot
>     unless (isNullCap $ cteCap cte) $ do
>         final <- isFinalCapability cte
>         (remainder, irq) <- finaliseCap (cteCap cte) final True
>         assert (capRemovable remainder slot && irq == Nothing) $
>             "cteDeleteOne: cap should be removable"
>         emptySlot slot Nothing

\subsection{Recycling Objects}

Objects can only be created by invoking "Retype" with an \emph{empty} untyped memory region. It is therefore impractical to recycle a kernel object by deleting it and creating a new object in its place, if it was originally created as part of a larger pool of objects --- the entire pool must be destroyed and re-created at the same time. The "Recycle" operation is used instead; it returns a single kernel object to its original state.

> cteRecycle :: PPtr CTE -> KernelP ()
> cteRecycle slot = do

We begin by revoking the capability. This ensures that no other copy of the capability exists.

>     cteRevoke slot

Call "finaliseSlot". This will clean up the object, assuming that the "cteRevoke" removed every other capability pointing to it.

>     finaliseSlot slot True

Finally, reconstruct the capability and the object in their initial states. The appropriate actions depend on the capability type, and are implemented in "recycleCap" --- see \autoref{sec:object.objecttype.recycle}. Note that there is a rare situation, unlikely in practice, in which the capability will have been deleted by "finaliseSlot". In this case, the calling thread has just been suspended, cannot be restarted, and is the only thread that can reach this slot; so we can safely leave the slot empty.

>     withoutPreemption $ do
>         cte <- getCTE slot
>         unless (isNullCap $ cteCap cte) $ do
>             is_final <- isFinalCapability cte
>             cap <- recycleCap is_final $ cteCap cte
>             updateCap slot cap

\subsection{Object Creation}

Create a set of new capabilities (and possibly the objects backing them) and
insert them in given empty slots. The required parameters are an object type;
a pointer to the source capability's slot; a list of pointers to empty slots;
the region of memory where the objects will be created; and an integer
repesenting the size of the objects to be created.

> createNewObjects :: ObjectType -> PPtr CTE -> [PPtr CTE] -> PPtr () -> Int -> Kernel ()
> createNewObjects newType srcSlot destSlots regionBase userSizeBits = do
>     let objectSizeBits = getObjectSize newType userSizeBits
>     zipWithM_ (\num slot -> do
>       cap <- createObject newType
>               (PPtr (num `shiftL` objectSizeBits) + regionBase) userSizeBits
>       insertNewCap srcSlot slot cap)
>       [0 .. fromIntegral (length destSlots - 1)] destSlots

The following function inserts a new revokable cap as a child of another.

> insertNewCap :: PPtr CTE -> PPtr CTE -> Capability -> Kernel ()
> insertNewCap parent slot cap = do
>     next <- liftM (mdbNext . cteMDBNode) $ getCTE parent
>     oldCTE <- getCTE slot
>     assert (isNullCap (cteCap oldCTE)
>             && mdbNext (cteMDBNode oldCTE) == nullPointer
>             && mdbPrev (cteMDBNode oldCTE) == nullPointer)
>             "insertNewCap: slot and mdb entry must be empty"
>     setCTE slot $ CTE cap (MDB next parent True True)
>     updateMDB next   $ (\m -> m { mdbPrev = slot })
>     updateMDB parent $ (\m -> m { mdbNext = slot })

The following function is used by the bootstrap code to create the initial set of capabilities.

> insertInitCap :: PPtr CTE -> Capability -> Kernel ()
> insertInitCap slot cap = do
>     oldCTE <- getCTE slot
>     assert (isNullCap $ cteCap oldCTE) "insertInitCap: slot must be empty"
>     assert (not $ isNullCap cap) "insertInitCap: cannot insert null"
>     assert (mdbPrev (cteMDBNode oldCTE) == nullPointer && 
>             mdbNext (cteMDBNode oldCTE) == nullPointer) 
>            "insertInitCap: mdb entry must be empty"
>     updateCap slot cap
>     updateMDB slot (const (nullMDBNode {
>         mdbRevocable = True,
>         mdbFirstBadged = True }))

The following function is called when a thread is restarted, to ensure that the thread's master reply capability exists. This is a special capability that exists only to be the MDB parent of a real reply capability generated by an IPC "Call" operation. It can never be invoked.

> setupReplyMaster :: PPtr TCB -> Kernel ()
> setupReplyMaster thread = do
>     slot <- locateSlot (PPtr $ fromPPtr thread) tcbReplySlot
>     oldCTE <- getCTE slot
>     when (isNullCap $ cteCap oldCTE) $ do
>         stateAssert (noReplyCapsFor thread)
>             "setupReplyMaster: reply master must not exist"
>         let cap = ReplyCap { capTCBPtr = thread, capReplyMaster = True }
>         let mdb = nullMDBNode { mdbRevocable = True, mdbFirstBadged = True }
>         setCTE slot $ CTE cap mdb

This function is used in the assertion above; it returns "True" if no reply capabilities (masters or otherwise) currently exist for the given thread. In the Haskell model, it always returns "True"; in the Isabelle formalisation of this model, it is strengthened to return "False" if a reply capability for the thread does exist.

> noReplyCapsFor :: PPtr TCB -> KernelState -> Bool
> noReplyCapsFor _ _ = True

\subsection{MDB Operations}
\label{sec:object.cnode.mdb}

The Mapping Database (MDB) is used to keep track of the derivation hierachy of seL4 capabilities, so all existing capabilities to an object can be revoked before that object is reused or deleted. A similar structure is used in L4Ka::Pistachio\cite{Pistachio:URL} to support that kernel's Unmap operation.

The MDB is a double-linked list that is equivalent to a prefix traversal of the derivation tree. It is possible to compare two capabilities to determine whether one is an ancestor of the other in the derivation tree.

> isMDBParentOf :: CTE -> CTE -> Bool

To be the parent of "b", "a" must have the "mdbRevocable" bit set and must have authority over the same physical resources as "b".

> isMDBParentOf (CTE a mdbA) (CTE b mdbB)
>     | not $ mdbRevocable mdbA  = False
>     | not $ a `sameRegionAs` b = False
>     | otherwise                = case a of

If "a" is an endpoint capability with a badge set, then it is the parent of "b" if and only if "b" has the same badge as "a" and has the "mdbFirstBadged" bit clear.

>         EndpointCap { capEPBadge = badge } | badge /= 0 ->
>             (badge == capEPBadge b) && (not $ mdbFirstBadged mdbB)
>         AsyncEndpointCap { capAEPBadge = badge } | badge /= 0 ->
>             (badge == capAEPBadge b) && (not $ mdbFirstBadged mdbB)

Otherwise, the object is not an endpoint, and "a" is the parent of "b".

>         _ -> True

% XXX diagram to illustrate this

In several of the functions in this module, it is necessary to modify the MDB node in a CTE without changing anything else in the CTE, and to skip the operation if the CTE pointer is null. The following function is a helper function used to do so.

> updateMDB :: PPtr CTE -> (MDBNode -> MDBNode) -> Kernel ()
> updateMDB 0 _ = return ()
> updateMDB slot f = do
>         cte <- getCTE slot
>         let mdb = cteMDBNode cte
>         let mdb' = f mdb
>         let cte' = cte { cteMDBNode = mdb' }
>         setCTE slot cte'

\subsection{Error Checking}

Before retyping an untyped memory object, it is necessary to check that the object contains no existing objects.

> ensureNoChildren :: PPtr CTE -> KernelF SyscallError ()
> ensureNoChildren slot = do
>         cte <- withoutFailure $ getCTE slot
>         when (mdbNext (cteMDBNode cte) /= nullPointer) $ do
>             next <- withoutFailure $ getCTE (mdbNext $ cteMDBNode cte)
>             when (cte `isMDBParentOf` next) $ throw RevokeFirst

When creating or deriving new capabilities, the destination slot must be empty.

> ensureEmptySlot :: PPtr CTE -> KernelF SyscallError ()
> ensureEmptySlot slot = do
>         cte <- withoutFailure $ getCTE slot
>         unless (isNullCap $ cteCap cte) $ throw DeleteFirst

\subsection{Accessing Capabilities}

\subsubsection{Locating Slots}

This function is used for locating a slot at a given offset in a CNode.

> locateSlot :: PPtr CTE -> Word -> Kernel (PPtr CTE)
> locateSlot cnode offset = do
>         let slotSize = 1 `shiftL` objBits (undefined::CTE)
>         return $ PPtr $ fromPPtr $ cnode + PPtr (slotSize * offset)

\subsubsection{Loading and Storing Entries}

The following two functions are specialisations of "getObject" and
"setObject" for the capability table entry object and pointer types.

> getCTE :: PPtr CTE -> Kernel CTE
> getCTE = getObject

> setCTE :: PPtr CTE -> CTE -> Kernel ()
> setCTE = setObject

Often, only the capability slot of the CTE needs to be modified.

> updateCap :: PPtr CTE -> Capability -> Kernel ()
> updateCap slot newCap = do
>         cte <- getCTE slot
>         setCTE slot (cte { cteCap = newCap })

\subsubsection{Reading Capabilities}

At higher levels of the kernel model ("SEL4.Kernel" and "SEL4.API"),
capabilities are represented by the abstract "Capability" type, rather
than the "CTE" type that is used to store them in memory.

The following functions are used to extract capabilities, in the form
of "Capability" objects, from the "CTE"s that store them.

> getSlotCap :: PPtr CTE -> Kernel Capability
> getSlotCap ptr = do
>     cte <- getCTE ptr
>     return $ cteCap cte

\subsubsection{Testing Capabilities}

When deleting a capability, it is necessary to determine whether it is the only existing capability to access a typed object it refers to. When an object's last remaining capability is deleted, the kernel must clean up any internal references it has to the object (in the scheduler's ready queues, the mapping database, and so on). The following function tests a capability to determine whether it is the last.

> isFinalCapability :: CTE -> Kernel Bool
> isFinalCapability cte@(CTE { cteMDBNode = mdb }) = do
>     prevIsSameObject <- if mdbPrev mdb == nullPointer
>         then return False
>         else do
>             prev <- getCTE (mdbPrev mdb)
>             return $! sameObjectAs (cteCap prev) (cteCap cte)
>     if prevIsSameObject
>         then return False
>         else if mdbNext mdb == nullPointer
>             then return True
>             else do
>                 next <- getCTE (mdbNext mdb)
>                 return $ not $ sameObjectAs (cteCap cte) (cteCap next)

The "SetSpace" method of the "TCB" object type will refuse to change the address space roots of a thread if removing the existing root capability might cause a long running delete --- see \autoref{sec:object.tcb.decode.setspace} for details. These functions are called to perform that check.

> longRunningDelete :: Capability -> Bool
> longRunningDelete (ThreadCap {}) = True
> longRunningDelete (CNodeCap {}) = True
> longRunningDelete (Zombie {}) = True
> longRunningDelete _ = False

> slotCapLongRunningDelete :: PPtr CTE -> Kernel Bool
> slotCapLongRunningDelete slot = do
>     cte <- getCTE slot
>     case cteCap cte of
>         NullCap -> return False
>         _ -> do
>             final <- isFinalCapability cte
>             return $ final && longRunningDelete (cteCap cte)

\subsection{Capability Transfers}

The following function locates the available receive slots for a capability transfer. If a specified slot is non-empty, the message will be truncated rather than using that slot. As with truncation of untyped data, the receiver is expected to either detect this at user level based on a protocol violation, or not care.

Note that the kernel API currently only allows one receive slot to be specified, so the list returned by "getReceiveSlots" only contains one item. However, this may change in future. The code calling this function does not assume that there is only one receive slot.

> getReceiveSlots :: PPtr TCB -> Maybe (PPtr Word) ->
>     Kernel [PPtr CTE]
> getReceiveSlots thread (Just buffer) = do

Load the receive parameters from the receiving thread's IPC buffer.

>         ct <- loadCapTransfer buffer

Look up the specified root CNode, and then the slot at the given index and depth in that CNode's tree. Fail if there is already a valid capability in it, to avoid a potentially slow revocation during the IPC operation. Any faults or errors occurring during the lookup operations are caught here.

>         emptyOnFailure $ do
>             let cptr = ctReceiveRoot ct
>             cnode <- unifyFailure $ lookupCap thread cptr
>             slot <- unifyFailure $ lookupTargetSlot
>                 cnode (ctReceiveIndex ct) (ctReceiveDepth ct)
>             cte <- withoutFailure $ getCTE slot
>             unless (isNullCap $ cteCap cte) $ throw ()
>             return [slot]
> getReceiveSlots _ Nothing = return []

This helper function is used to load the capability transfer data from an IPC buffer.

> loadCapTransfer :: PPtr Word -> Kernel CapTransfer
> loadCapTransfer buffer = do
>         let intSize = fromIntegral $ bitSize (undefined::Word) `div` 8
>         let offset = msgMaxLength + msgMaxExtraCaps + 2
>         capTransferFromWords (buffer + PPtr (offset*intSize))

> capTransferFromWords :: PPtr Word -> Kernel CapTransfer
> capTransferFromWords ptr = do
>         let intSize = fromIntegral $ bitSize (undefined::Word) `div` 8
>         w0 <- loadWordUser ptr
>         w1 <- loadWordUser $ ptr + PPtr intSize
>         w2 <- loadWordUser $ ptr + PPtr (2 * intSize)
>         return CT {
>             ctReceiveRoot = CPtr w0,
>             ctReceiveIndex = CPtr w1,
>             ctReceiveDepth = fromIntegral w2 }


