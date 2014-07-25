%
% Copyright 2014, General Dynamics C4 Systems
%
% This software may be distributed and modified according to the terms of
% the GNU General Public License version 2. Note that NO WARRANTY is provided.
% See "LICENSE_GPLv2.txt" for details.
%
% @TAG(GD_GPL)
%

Each thread has a capability space, possibly shared with other
threads. This is a mapping between addresses and kernel objects.

The capability space is represented by a guarded page table. Each
level of the table contains $2^n$ capability table entries, which
are represented by the Haskell type "CTE". These entries each contain
a physical pointer to a kernel object, a set of permissions, a word
with a meaning depending on the object type, and a mapping database
entry.

This module implements the functions that look up a capability or
virtual address in a user-level thread's capability space, as well as
the function that assigns a new address space root for a thread.

> module SEL4.Kernel.CSpace (
>     resolveAddressBits,
>     lookupCap, lookupCapAndSlot,
>     lookupSlotForThread,
>     lookupSourceSlot, lookupPivotSlot, lookupTargetSlot,
>     ) where

\begin{impdetails}

% {-# BOOT-IMPORTS: SEL4.Machine SEL4.Model SEL4.Object.Structures SEL4.Object.Instances() SEL4.API.Types SEL4.API.Failures #-}
% {-# BOOT-EXPORTS: lookupCap lookupCapAndSlot lookupSlotForThread  lookupSourceSlot lookupTargetSlot lookupPivotSlot #-}

> import SEL4.Machine
> import SEL4.Object
> import SEL4.Model
> import SEL4.API.Types
> import SEL4.API.Failures

> import Data.Bits

\end{impdetails}

\subsection{Capability Lookups}

These functions are used when invoking a capability, to determine the
location and type of the object being invoked, and what rights the
caller possesses for it.

These functions simply call the slot lookup functions defined below to locate
the slot containing the requested capability, and then load the capability from
it.

> lookupCap :: PPtr TCB -> CPtr -> KernelF LookupFailure Capability
> lookupCap thread cPtr = liftM fst $ lookupCapAndSlot thread cPtr

> lookupCapAndSlot :: PPtr TCB -> CPtr -> 
>                     KernelF LookupFailure (Capability, PPtr CTE)
> lookupCapAndSlot thread cPtr = do
>         slot <- lookupSlotForThread thread cPtr
>         cap <- withoutFailure $ getSlotCap slot
>         return (cap, slot)

\subsection{Locating a Capability Table Entry}

The following functions are used in two different situations in
which the kernel must locate a capability table entry: invocation of a
capability in a given address space, and modification of a specific
entry in an invoked capability table.

When a capability is being invoked, the root can be determined from
the identity of the thread making the call, and any failures are
reported as faults.

> lookupSlotForThread :: PPtr TCB -> CPtr ->
>         KernelF LookupFailure (PPtr CTE)
> lookupSlotForThread thread capptr = do
>         threadRootSlot <- withoutFailure $ getThreadCSpaceRoot thread
>         threadRoot <- withoutFailure $ getSlotCap threadRootSlot
>         let bits = bitSize $ fromCPtr capptr
>         (s, _) <- resolveAddressBits threadRoot capptr bits
>         return s

When a pager is manipulating a capability space using a system call, the root and depth have been explicitly specified, and any failures encountered during the lookup should be turned into a system call error. The specified depth must be correct (with no bits remaining after the lookup).

> lookupSlotForCNodeOp ::
>         Bool -> Capability -> CPtr -> Int ->
>         KernelF SyscallError (PPtr CTE)
> lookupSlotForCNodeOp isSource root@(CNodeCap {}) capptr depth = do
>     rangeCheck depth 1 $ bitSize capptr
>     lookupErrorOnFailure isSource $ do
>         result <- resolveAddressBits root capptr depth
>         case result of
>             (slot, 0) -> return slot
>             (_, bitsLeft) -> throw $ DepthMismatch {
>                 depthMismatchBitsLeft = bitsLeft,
>                 depthMismatchBitsFound = 0 }
> lookupSlotForCNodeOp isSource _ _ _ =
>     throw $ FailedLookup isSource InvalidRoot

The "lookupSlotForCNodeOp" function is not exported directly; instead, we export specific versions of it for locating the slots that are the source, destination, and pivot (that is, both source and destination) when manipulating capabilities.

> lookupSourceSlot :: Capability -> CPtr -> Int ->
>         KernelF SyscallError (PPtr CTE)
> lookupSourceSlot = lookupSlotForCNodeOp True

> lookupTargetSlot :: Capability -> CPtr -> Int ->
>         KernelF SyscallError (PPtr CTE)
> lookupTargetSlot = lookupSlotForCNodeOp False

> lookupPivotSlot :: Capability -> CPtr -> Int ->
>         KernelF SyscallError (PPtr CTE)
> lookupPivotSlot = lookupSlotForCNodeOp True

\subsubsection{Internal Functions}

The capability table lookup is performed by the function
"resolveAddressBits".

The arguments to this function are a capability to access a CNode, the capability space address being looked up, and the number of bits remaining to be resolved in the address. 

It returns a pointer to a slot and the number of bits still unresolved in the address when a valid non-CNode capability was found.

> resolveAddressBits :: Capability -> CPtr -> Int ->
>         KernelF LookupFailure (PPtr CTE, Int)

The following definition is used when a CNode capability is encountered in the cap table.

> resolveAddressBits nodeCap@(CNodeCap {}) capptr bits = do

Determine the number of bits that this CNode can resolve. 

>         let radixBits = capCNodeBits nodeCap
>         let guardBits = capCNodeGuardSize nodeCap
>         let levelBits = radixBits + guardBits
>         assert (levelBits /= 0) "All CNodes must resolve bits"

Check that the guard does not resolve too many bits, and that it matches the expected value.

>         let guard = (fromCPtr capptr `shiftR` (bits-guardBits)) .&. 
>                    (mask guardBits)
>         unless (guardBits <= bits && guard == capCNodeGuard nodeCap)
>             $ throw $ GuardMismatch {
>                 guardMismatchBitsLeft = bits,
>                 guardMismatchGuardFound = capCNodeGuard nodeCap,
>                 guardMismatchGuardSize = guardBits }

If the lookup has exceeded the expected depth, then the table is badly formed. In this case, throw a fault. Note that the depth mismatch fault has lower priority than the guard mismatch fault; if the guard alone resolves too many bits, this code is not reached.

>         when (levelBits > bits) $ throw $ DepthMismatch {
>             depthMismatchBitsLeft = bits,
>             depthMismatchBitsFound = levelBits }

If the current CNode is valid, then locate the slot in it that
contains the next capability to be visited.

>         let offset = (fromCPtr capptr `shiftR` (bits-levelBits)) .&. 
>                    (mask radixBits)
>         slot <- withoutFailure $ locateSlot (capCNodePtr nodeCap) offset

If all of the remaining bits in the address have been resolved, then
"slot" is the final result.

>         let bitsLeft = bits - levelBits
>         if (bitsLeft == 0)
>           then return (slot, 0)

Otherwise, there are more address bits left to resolve. Fetch the next capability.

>           else do
>             nextCap <- withoutFailure $ getSlotCap slot

Determine the type of the next capability.

>             case nextCap of

If the next capability is a CNode, then perform a recursive call to look at the next level of the table.

>                 CNodeCap {} -> do
>                     resolveAddressBits nextCap capptr bitsLeft

Otherwise, return the new slot and the number of unresolved address bits.

>                 _ -> return (slot, bitsLeft)

The following definition will be used if the top level CNode is not valid.

> resolveAddressBits _ _ _ = throw InvalidRoot


