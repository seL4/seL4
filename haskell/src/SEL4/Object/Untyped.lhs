%
% Copyright 2014, General Dynamics C4 Systems
%
% This software may be distributed and modified according to the terms of
% the GNU General Public License version 2. Note that NO WARRANTY is provided.
% See "LICENSE_GPLv2.txt" for details.
%
% @TAG(GD_GPL)
%

This module defines the behavior of untyped objects. 

> module SEL4.Object.Untyped (
>         decodeUntypedInvocation, invokeUntyped
>     ) where

\begin{impdetails}

> import SEL4.Config
> import SEL4.API.Types
> import SEL4.API.Failures
> import SEL4.API.Invocation
> import SEL4.Machine
> import SEL4.Model
> import SEL4.Object.Structures
> import SEL4.Object.Instances()
> import {-# SOURCE #-} SEL4.Object.CNode

> import {-# SOURCE #-} SEL4.Kernel.CSpace

> import Data.Bits

\end{impdetails}

> getFreeRef :: PPtr () -> Int -> PPtr ()
> getFreeRef base freeIndex = base + (fromIntegral freeIndex)

> getFreeIndex :: PPtr () -> PPtr () -> Int
> getFreeIndex base free = fromIntegral $ fromPPtr (free - base)

\subsection{Invocation}

Invocation of an untyped object retypes the memory region, possibly creating
new typed kernel objects. As shown in \autoref{fig:derive}, the
retype operation will generate one or more new capabilities, which are inserted in the mapping database as children of the initial capability. These newly created capabilities will have all access rights, and other object specific fields will be initialised to some sensible value. 

\begin{figure}[htp]
\centering \includegraphics{figures/derive}
\caption{Invoking an Untyped Object}\label{fig:derive}
\end{figure}

We start by defining a simple function to align one value to a power-of-two boundard. In particular, this function aligns its first argument up to the next power-of-two specified by the second argument.

> alignUp :: Word -> Int -> Word
> alignUp baseValue alignment =
>   (baseValue + (1 `shiftL` alignment) - 1) .&. complement (mask alignment)

The expected parameters are the type of the new objects, the size of the requested objects (for those with variable size), a capability to a capability space, index and depth used to locate the destination for the new capabilities, and the maximum number of new capabilities to be created. When successful, it returns the number of new objects or regions created.

> decodeUntypedInvocation :: Word -> [Word] -> PPtr CTE -> Capability ->
>         [Capability] -> KernelF SyscallError UntypedInvocation
> decodeUntypedInvocation label
>         (newTypeW:userObjSizeW:nodeIndexW:nodeDepthW:nodeOffset:nodeWindow:_)
>         slot cap (rootCap:_) =
>   do

The only supported operation on Untyped capabilities is Retype.

>     unless (invocationType label == UntypedRetype) $ throw IllegalOperation

The first argument must be a valid object type.

>     when (fromIntegral newTypeW > fromEnum (maxBound :: ObjectType)) $
>         throw $ InvalidArgument 0
>     let newType = toEnum (fromIntegral newTypeW) :: ObjectType

The second argument specifies the size of the object, for the types for which the object size may vary --- untyped memory, data frames, and CNodes, and possibly architecture-defined types. For any other type, it is not used. It must be a positive integer less than the number of bits in a physical address.

The value of this argument is the base 2 logarithm of the actual required size. The unit depends on the object type: one byte for untyped memory objects, the architecture's minimum page size for data frames, and one capability slot for CNodes.

>     let userObjSize = fromIntegral userObjSizeW
>     rangeCheck userObjSize 0 $ bitSize nullPointer - 2 

The kernel does not allow creation of CNodes containing only one entry; this is done to avoid non-terminating loops in capability lookup. Note that it is possible for a single entry CNode to translate bits using its guard; this is not allowed, however, to avoid having to check for it during capability lookups.

>     when (newType == fromAPIType CapTableObject && userObjSize == 0) $
>         throw $ InvalidArgument 1

Because of capability size limitations, the kernel does not allow creation of objects smaller than 16 bytes.

>     when (newType == fromAPIType Untyped && userObjSize < 4) $
>         throw $ InvalidArgument 1

The node index and depth arguments, and the root capability, specify a CNode to place newly created capabilities in. This is similar to the source argument of the Insert and Move operations. However, unlike those operations, the specified slot must contain a CNode capability, and the new capabilities will be placed in \emph{that} CNode. % XXX should have either a diagram, or a more intuitive way to specify the destination.

>     let nodeIndex = CPtr nodeIndexW
>     let nodeDepth = fromIntegral nodeDepthW
>     
>     nodeCap <- if nodeDepth == 0
>         then return rootCap
>         else do
>             nodeSlot <- lookupTargetSlot rootCap nodeIndex nodeDepth
>             withoutFailure $ getSlotCap nodeSlot

If the destination capability is not a CNode, an error is returned.

>     case nodeCap of 
>         CNodeCap {} -> return ()
>         _ -> throw $ FailedLookup False $ MissingCapability {
>             missingCapBitsLeft = nodeDepth }

The node offset and window arguments specify the start and the length of a contiguous block of empty capability slots in the destination CNode.

>     let nodeSize = 1 `shiftL` (capCNodeBits nodeCap)
>     rangeCheck nodeOffset 0 $ nodeSize - 1
>     rangeCheck nodeWindow 1 retypeFanOutLimit
>     rangeCheck nodeWindow 1 $ nodeSize - nodeOffset
>     
>     slots <- withoutFailure $
>         mapM (locateSlot $ capCNodePtr nodeCap)
>             [nodeOffset .. nodeOffset+nodeWindow - 1]

The destination slots must all be empty. If any of them contains a capability, the operation will fail with a "DeleteFirst" error.

>     mapM_ ensureEmptySlot slots

Find out how much free room is available in the Untyped. If we discover that we don't have any children (for instance, they have all been manually deleted), we can just start allocating from the beginning of region again.

>     freeIndex <- withoutFailure $ constOnFailure (capFreeIndex cap) $ do
>             ensureNoChildren slot
>             return 0
>     let freeRef = getFreeRef (capPtr cap) freeIndex

Ensure that sufficient space is available in the region of memory.

>     let objectSize = getObjectSize newType userObjSize
>     let untypedFreeBytes = (bit (capBlockSize cap)) - freeIndex 
>     let maxCount = untypedFreeBytes `shiftR` objectSize
>     when (fromIntegral maxCount < nodeWindow) $
>         throw $ NotEnoughMemory $ fromIntegral untypedFreeBytes

Align up the free region pointer to ensure that created objects are aligned to their size.

>     let alignedFreeRef = PPtr $ alignUp (fromPPtr freeRef) objectSize

>     return $! Retype {
>         retypeSource = slot,
>         retypeRegionBase = capPtr cap,
>         retypeFreeRegionBase = alignedFreeRef,
>         retypeNewType = newType,
>         retypeNewSizeBits = userObjSize,
>         retypeSlots = slots }

> decodeUntypedInvocation label _ _ _ _ = throw $
>     if invocationType label == UntypedRetype
>         then TruncatedMessage
>         else IllegalOperation

> invokeUntyped :: UntypedInvocation -> Kernel ()
> invokeUntyped (Retype srcSlot base freeRegionBase newType userSize destSlots) = do

>     cap <- getSlotCap srcSlot

\begin{impdetails}
The following code removes any existing objects in the physical memory region. This operation is specific to the Haskell physical memory model, in which memory objects are typed; it is not necessary (or possible) when running on real hardware.

>     when (base == freeRegionBase) $
>         deleteObjects base (capBlockSize cap)

\end{impdetails}

Update the untyped capability we are using to create these objects to record that this space now has objects in it.

>     let totalObjectSize = (length destSlots) `shiftL` (getObjectSize newType userSize)
>     let freeRef = freeRegionBase + PPtr (fromIntegral totalObjectSize)
>     updateCap srcSlot (cap {capFreeIndex = getFreeIndex base freeRef})

Create the new objects and insert caps to these objects into the destination slots.

>     createNewObjects newType srcSlot destSlots freeRegionBase userSize

