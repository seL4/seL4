%
% Copyright 2014, General Dynamics C4 Systems
%
% This software may be distributed and modified according to the terms of
% the GNU General Public License version 2. Note that NO WARRANTY is provided.
% See "LICENSE_GPLv2.txt" for details.
%
% @TAG(GD_GPL)
%

This module contains functions that create a new kernel state and set up the address space and context of the initial user-level task.

> module SEL4.Kernel.Init(
>         initKernel, newKernelState,
>         KernelInit, allocRegion, allocFrame, provideCap, provideRegion,
>         doKernelOp
>     ) where

\begin{impdetails}

% {-# BOOT-IMPORTS: SEL4.Machine SEL4.Model SEL4.Object.Structures SEL4.API.Types Control.Monad.State #-}
% {-# BOOT-EXPORTS: #InitData KernelInit allocRegion allocFrame provideCap provideRegion doKernelOp #-}

> import SEL4.Config
> import SEL4.API.Types
> import SEL4.Model
> import SEL4.Object
> import SEL4.Machine
> import SEL4.Kernel.Thread
> import SEL4.Kernel.VSpace
> import SEL4.Machine.Hardware.ARM (pageBitsForSize)
> import SEL4.Object.Structures (maxPriority)

> import Data.Bits
> import Data.List
> import Control.Monad.State(StateT, runStateT, lift)

\end{impdetails}

\subsection{Overview}

The first thread in the system is created by the kernel; its address space contains capabilities to change thread domains and to use all of the machine's physical resources, minus any physical memory which is used by the kernel's static data and code.

\subsubsection{Initial Address Space}

The initial address space layout needs to be balanced between using superpages to cover as much memory as possible, to allow greater flexibility for creating large mappings; and providing a reasonable number of small objects, to make the root server's address space easy to manage reliably.

So, the initial address space is constructed as follows:

\begin{itemize}
\item The kernel creates a root CNode, of a user-configurable size. It is given a guard of whatever size is necessary to make each slot in the root CNode cover the same amount of the address space as the architecture's virtual page size. The address space covered by this node is recorded as a "BRNodeL1" region in the boot region list.
\item The kernel creates a second-level CNode from a page-sized block of memory, and give it the necessary guard to allow it to be mapped in the first slot of the root node. The slots in this CNode are used for capabilities created in the following steps. If the kernel runs out of slots, a new CNode will be created and mapped the same way in an unused root node slot. For each such CNode, a "BRNodeL2" region is added to the boot region list.
\item The initial task's thread capability, address space root capabilities, interrupt control capability, architecture-specific initial capabilities and domain capability are mapped into its address space at well-known locations starting at capability address 1. The capability at address 0 is always invalid. These capabilities' locations are recorded as a "BRInitCaps" region in the boot region list.
\item Architecture-specific frame capabilities are created for the code and data of the initial task, and are placed in the corresponding slots in the root CNode. These are then mapped into the virtual address space using an architecture-defined mechanism. "BRRootTask" regions are created to specify the locations of the mapped pages.
\item Two extra virtual memory pages are allocated and mapped, using the same mechanism as the root task's code and data. The boot region list is copied into one of the pages; its location will be passed to the initial thread in a register. The second page will become the initial thread's IPC buffer.
\item From the remaining physical memory, the kernel creates a configurable number of untyped \emph{small blocks} of size equal to the machine's smallest virtual page size, and records their locations with one or more "BRSmallBlocks" regions.
\item Virtual memory capabilities are created for each memory-mapped IO device in the system. For each device, a "BRDeviceCaps" region is created to locate the capabilities used by the device.
\item The kernel ensures that the number of free slots that will remain after the following step is not less than a configurable minimum. If necessary, a new CNode is allocated.
\item All remaining physical memory is divided into aligned power-of-two-sized untyped blocks. The kernel creates capabilities for these blocks, arranged in groups. Each group has no more than one capability of each size, arranged in order of increasing size; for each group, a "BRLargeBlocks" region is created, and the region's data word used as a bit field that indicates which sizes are present in the group.
\item Finally, the kernel creates "BRFreeSlots" regions for any unused slots in the second-level CNodes, "BREmpty" regions for any unused areas of the CSpace, and "BRCapsOnly" regions for any areas of the virtual address space that cannot be allocated to user-level tasks.
\end{itemize}

Implementations that require separate CSpace and VSpace structures will provide caps in the initial CSpace for all frames used to back the initial VSpace. Otherwise, the initial VSpace root will be a copy of the initial CSpace root. In both cases, the CSpace address of each mapped frame capability will be equal to the virtual address of the page it backs.

\subsection{Framework}

\subsubsection{Kernel Initialisation Monad}

The various kernel initialisation functions run in a monad that extends the usual kernel state structures with additional information about the bootstrapping process. In particular, they are able to allocate memory from a list of free frames, and provide the user with capabilities and metadata about the capabilities.

> data InitData = InitData {
>     initFreeSlotsL1 :: [(CPtr, PPtr CTE)],
>     initFreeSlotsL2 :: [(CPtr, PPtr CTE)],
>     initFreeMemory :: [(PAddr, PAddr)],
>     initBootMemory :: [(PAddr, PAddr)],
>     initRegions :: [BootRegion] }

> type KernelInit = StateT InitData Kernel

> doKernelOp :: Kernel a -> KernelInit a
> doKernelOp = lift

> runInit :: [(PAddr, PAddr)] ->
>         [(PAddr, PAddr)] ->
>         KernelInit (VPtr, (VPtr, PPtr Word), Capability) ->
>         Kernel (BootInfo, VPtr, PPtr Word, Capability)
> runInit freeMemory bootMemory doInit = do
>     let initData = InitData {
>             initFreeSlotsL1 = [],
>             initFreeSlotsL2 = [],
>             initFreeMemory = freeMemory,
>             initBootMemory = bootMemory,
>             initRegions = [] }
>     ((bufferPtr, (infoVPtr, infoPPtr), tcbCap), initData') <-
>         runStateT doInit initData
>     let bootInfo = BootInfo bufferPtr $ initRegions initData'
>     return (bootInfo, infoVPtr, infoPPtr, tcbCap)

\subsubsection{Allocating Frames and Pages}

The following function allocates a region of physical memory. The region size is specified as a power of two, and the allocated region must be size-aligned.

> allocRegion :: Int -> KernelInit PAddr
> allocRegion bits = do
>     freeMem <- gets initFreeMemory
>     case isUsable `break` freeMem of
>         (small, (b, t):rest) -> do
>             let result = align b
>             let below = if result == b then [] else [(b, result - 1)]
>             let above = if result + s - 1 == t then [] else [(result + s, t)]
>             modify (\st -> st { initFreeMemory = small++below++above++rest })
>             return result
>         _ -> fail "Unable to allocate memory"
>     where
>         s = 1 `shiftL` bits
>         align b = (((b - 1) `shiftR` bits) + 1) `shiftL` bits
>         isUsable (b, t) = let r = align b in r >= b && r + s - 1 <= t

Most allocations are frame-sized, so for convenience there is an "allocFrame" function.

> allocFrame :: KernelInit PAddr
> allocFrame = allocRegion pageBits

The following function allocates and maps a virtual memory page.

> allocPage :: KernelInit (VPtr, PPtr Word, PPtr CTE)
> allocPage = do
>     frame <- allocFrame
>     let clr = (fromPPtr (ptrFromPAddr frame) `shiftR` pageBits)
>             .&. mask pageColourBits
>     (cptr, slot) <- allocRootSlotWithColour clr
>     doKernelOp $ do
>         cap <- createInitPage frame
>         let byteLength = 1 `shiftL` pageBits
>         doMachineOp $ clearMemory (ptrFromPAddr frame) byteLength
>         insertInitCap slot cap
>     return $ (VPtr $ fromCPtr cptr, ptrFromPAddr frame, slot)

The following function creates a capability for a frame, without modifying the contents of the frame. The frame is to be mapped at a given virtual address, and will be inserted into the initial capability space in the corresponding location. Depending on the architecture, this frame capability will either represent the mapping directly, or be copied into an architecture-specific page table structure after that structure is created later in the boot sequence.

> mapForInitTask :: PAddr -> VPtr -> KernelInit ()
> mapForInitTask frame page = do
>     slot <- requestRootSlot $ CPtr $ fromVPtr page
>     doKernelOp $ do
>         cap <- createInitPage frame
>         insertInitCap slot cap

\subsubsection{Creating Root CNode Slots}

This function is called once to allocate a root-level CNode and make its slots available.

> makeRootCNode :: KernelInit Capability
> makeRootCNode = do
>     let slotBits = objBits (undefined::CTE)
>     let levelBits = rootCNodeSize
>     let levelSize = 1 `shiftL` levelBits
>     frame <- liftM ptrFromPAddr $ allocRegion (levelBits + slotBits)
>     rootCNCap <- doKernelOp $
>         createObject (fromAPIType CapTableObject) frame levelBits
>     let rootCNCap' = rootCNCap {
>             capCNodeGuardSize = bitSize frame - pageBits - levelBits }
>     rootSlots <- mapM (\n -> doKernelOp $ do
>         slot <- locateSlot (capCNodePtr rootCNCap') n
>         let cptr = CPtr $ n `shiftL` pageBits
>         return (cptr, slot)) [0..levelSize - 1]
>     modify (\st -> st { initFreeSlotsL1 = rootSlots })
>     provideRegion $ BootRegion
>         (CPtr 0) (CPtr $ (1 `shiftL` (pageBits+levelBits)) - 1)
>         BRNodeL1 $ fromIntegral levelBits
>     return rootCNCap'

\subsubsection{Allocating Root CNode Slots}

The slots in the root CNode are shared by second-level CNodes and the root task's virtual memory mappings. Other than the first second-level CNode, which must start at virtual address 0, the CNodes can be placed in any available slot, but the memory mappings for the initial task's executable require specific addresses.

Slots for CNodes are allocated with "allocRootSlot", which simply returns the first available slot.

> allocRootSlot :: KernelInit (CPtr, PPtr CTE)
> allocRootSlot = do
>     freeSlots <- gets initFreeSlotsL1
>     case freeSlots of
>       result:rest -> do
>         modify (\st -> st { initFreeSlotsL1 = rest })
>         return result
>       _ -> fail "No free root CNode slots remaining"

Virtual memory pages that are not part of the initial executable can be mapped anywhere, as long as the user-level mapping and the kernel's mapping have the same colour (to prevent cache aliasing issues). The "allocRootSlotWithColour" function allocates the first slot of the requested colour. The number of possible colours depends on the platform-defined constant "pageColourBits".

> allocRootSlotWithColour :: Word -> KernelInit (CPtr, PPtr CTE)
> allocRootSlotWithColour c = do
>     freeSlots <- gets initFreeSlotsL1
>     let matchingSlots = filter (\slot ->
>                 fromCPtr (fst slot) `shiftR` pageBits
>                     .&. mask pageColourBits == c) freeSlots
>     case matchingSlots of
>       result:_ -> do
>         modify (\st -> st { initFreeSlotsL1 = delete result freeSlots })
>         return result
>       _ -> fail $ "No free root slots of colour " ++ show c ++ " remaining"

The initial task's executable must be mapped at the address it was linked at; the "requestRootSlot" function is used to allocate the corresponding root slots. It will fail if the requested slot has already been used or does not exist.

> requestRootSlot :: CPtr -> KernelInit (PPtr CTE)
> requestRootSlot addr = do
>     freeSlots <- gets initFreeSlotsL1
>     let (requested,others) = partition (\x -> fst x == addr) freeSlots
>     case requested of
>       [(_,result)] -> do
>         modify (\st -> st { initFreeSlotsL1 = others })
>         return result
>       [] -> fail $ "Requested root CNode slot is not free: " ++ show addr
>       _ -> fail "Unreachable: multiple matches for requested slot"

\subsubsection{Creating CNodes}

This function creates a frame full of new capability slots and adds them to the free list.

> makeSlots :: KernelInit ()
> makeSlots = do
>     let slotBits = objBits (undefined::CTE)
>     let slotSize = 1 `shiftL` slotBits
>     let levelBits = pageBits - slotBits
>     let levelSize = 1 `shiftL` levelBits
>     frame <- liftM ptrFromPAddr allocFrame
>     (cptr,slot) <- allocRootSlot
>     node <- doKernelOp $ do
>         cap <- createObject (fromAPIType CapTableObject)
>             frame levelBits
>         let cap' = cap { capCNodeGuardSize = pageBits - levelBits }
>         insertInitCap slot cap'
>         return $ capCNodePtr cap'
>     let newSlots = map
>             (\n -> (cptr + CPtr n, node + (slotSize * PPtr n)))
>             [0..levelSize - 1]
>     oldSlots <- gets initFreeSlotsL2
>     modify (\st -> st { initFreeSlotsL2 = oldSlots ++ newSlots })
>     provideRegion $ BootRegion
>         cptr (cptr - 1 + (CPtr $ 1 `shiftL` levelBits))
>         BRNodeL2 $ fromIntegral levelBits

\subsubsection{Allocating Slots}

This function allocates a slot and writes an initial capability into it (with no MDB parents or children). If there are no free slots remaining, it creates more. It returns a CSpace pointer to the created capability.

> provideCap :: Capability -> KernelInit (CPtr, PPtr CTE)
> provideCap cap = do
>     freeSlots <- gets initFreeSlotsL2
>     when (freeSlots == []) makeSlots
>     freeSlots <- gets initFreeSlotsL2
>     case freeSlots of
>       (cptr,slot):rest -> do
>         modify (\st -> st { initFreeSlotsL2 = rest })
>         unless (isNullCap cap) $ doKernelOp $ insertInitCap slot cap
>         return (cptr, slot)
>       _ -> fail "No free slots, even after makeSlots"

\subsubsection{Creating Boot Regions}

This function adds a boot region to the initial boot info. In a real implementation it would be written to the boot info page at this point; to keep this code simple we add regions to a list and write them all at once.

> provideRegion :: BootRegion -> KernelInit ()
> provideRegion r = addRegionWithMerge r 0

These functions also add a boot region, but check for the case where the new boot region can be merged with the last existing region to avoid consuming any additional space. The condition for merging of large block mappings is complex - it requires that the all bits in the size bitmap in the second region are at higher locations than those in the first. For convenience, an integer is supplied and the check made is whether the bits lie respectively below and above this cutoff point.

> mergeBRs :: BootRegion -> BootRegion -> Int -> Maybe BootRegion
> mergeBRs (BootRegion a b BRLargeBlocks s) (BootRegion c d BRLargeBlocks t) n
>     | (b == c - 1) && (s .&. mask n == s) && (t .&. mask n == 0)
>         = Just (BootRegion a d BRLargeBlocks (s .|. t))
>     | otherwise
>         = Nothing
> mergeBRs (BootRegion a b BRSmallBlocks s) (BootRegion c d BRSmallBlocks t) _
>     | (b == c - 1) && (s == t)
>         = Just (BootRegion a d BRSmallBlocks s)
>     | otherwise
>         = Nothing
> mergeBRs (BootRegion a b BRRootTask s) (BootRegion c d BRRootTask t) _
>     | (b == c - 1) && (s == t)
>         = Just (BootRegion a d BRRootTask s)
>     | otherwise
>         = Nothing
> mergeBRs
>   (BootRegion b1 t1 BRDeviceCaps d1) (BootRegion b2 t2 BRDeviceCaps d2) _
>     | (t1 == b2 - 1) && (s1 == s2) &&
>       (d1 `shiftR` s1 == (d2 `shiftR` s1) - n1)
>         = Just (BootRegion b1 t2 BRDeviceCaps d1)
>     | otherwise
>         = Nothing
>     where
>         n1 = fromCPtr $ t1 - b1 + 1
>         s1 = fromIntegral $ d1 .&. mask 8
>         s2 = fromIntegral $ d2 .&. mask 8
> mergeBRs _ _ _ = Nothing

> addRegionWithMerge :: BootRegion -> Int -> KernelInit ()
> addRegionWithMerge r n = do
>     regions <- gets initRegions
>     modify (\st -> st { initRegions = tryMergeList regions })
>     where
>         tryMergeList xs = case xs of
>             [] -> [r]
>             _  -> case mergeBRs (last xs) r n of
>                 Nothing -> xs ++ [r]
>                 Just r' -> init xs ++ [r']

\subsection{Bootstrapping the Kernel}

The kernel is bootstrapped by calling "initKernel". The arguments are the address of the initial thread's entry point, a list of frames to be mapped in the initial thread's address space, the offset to be subtracted from the initial frame addresses to obtain the corresponding virtual addresses, a list of frames reserved by the kernel, and a list of frames that are used by the kernel's bootstrapping code.

> initKernel :: VPtr -> [PAddr] -> VPtr -> [PAddr] -> [PAddr] ->
>         Kernel ()
> initKernel entry initFrames initOffset kernelFrames bootFrames = do

Define some useful constants.

>         let pageSize = bit pageBits
>         let wordSize = bitSize entry

Determine the available memory regions.

>         allMemory <- doMachineOp getMemoryRegions
>         let allFrames = concat $
>               map (\(s, e) -> [s, s+pageSize .. s+(e-s) - 1])
>                   allMemory

\begin{impdetails}
Configure the physical address space model. This is an implementation detail specific to the Haskell model, and is not relevant on real hardware.

>         initPSpace $ map (\(s, e) -> (ptrFromPAddr s, ptrFromPAddr e))
>                          allMemory
> --FIXME

Reserve the PSpace regions used by the kernel and the root task. This must be done first, since some of these reserved regions will be turned into kernel objects by "initKernelVM", below. Again, this is specific to the Haskell model's implementation and is not relevant on real hardware.

>         mapM (\p -> reserveFrame (ptrFromPAddr p) True) kernelFrames

\end{impdetails}
Set up the kernel's VM environment.

>         initKernelVM

Make sure the lists of initial task and kernel frames are valid.

>         when (null initFrames)
>             $ fail "initFrames must not be empty"
>         when (null kernelFrames)
>             $ fail "kernelFrames must not be empty"

>         unless (distinct initFrames)
>             $ fail "initFrames must be distinct"
>         unless (distinct kernelFrames)
>             $ fail "kernelFrames must be distinct"

>         when (0 `elem` initFrames)
>             $ fail "initFrames must not contain 0"
>         when (0 `elem` kernelFrames)
>             $ fail "kernelFrames must not contain 0"

>         unless (and $ map (flip elem allFrames) initFrames)
>             $ fail "initFrames must only use available physical memory"
>         unless (and $ map (flip elem allFrames) kernelFrames)
>             $ fail "kernelFrames must only use available physical memory"
>         when (or $ map (flip elem initFrames) kernelFrames)
>             $ fail "kernelFrames and initFrames must not overlap"
>         unless (and $ map (flip elem kernelFrames) bootFrames)
>             $ fail "bootFrames must all appear in kernelFrames"
>         unless (distinct allFrames)
>             $ fail "memory regions must not overlap"

>         unless (isAligned (fromVPtr initOffset) pageBits)
>             $ fail "initOffset not aligned"
>         unless (and $ map (\b -> isAligned b pageBits) initFrames)
>             $ fail "initFrames not aligned"
>         unless (and $ map (\b -> isAligned b pageBits) kernelFrames)
>             $ fail "kernelFrames not aligned"
>         unless (distinct bootFrames)
>             $ fail "boot regions must not overlap"
>         unless (and $ map (\(s, e) -> (e-s) < bit wordSize
>                                       && bit pageBits <= (e-s))
>                           allMemory) $ fail "memory regions wrong sizes"

Reserve the frames used by the kernel and the root task.

>         let emptyFrames =
>                 filter (flip notElem ([0] ++ initFrames ++ kernelFrames)) allFrames

Create a list of all remaining free memory regions.

>         let freeGroups = rangesBy (\a b -> a + pageSize == b)
>                 (sort emptyFrames)
>         let freeMemory =
>                 map (\fs -> (head fs, last fs + pageSize - 1)) freeGroups
>         let bootGroups = rangesBy (\a b -> a + pageSize == b) bootFrames
>         let bootMemory =
>                 map (\fs -> (head fs, last fs + pageSize - 1)) bootGroups

Move into the "KernelInit" monad.

>         (bootInfo, infoVPtr, infoPPtr, tcbCap) <-
>           runInit freeMemory bootMemory $ do

Create the root cnode.

>             rootCNCap <- makeRootCNode

Create a second-level CNode. It is not needed yet, but must be created at this point to avoid allocating the first root CNode slot (with address 0) to a virtual memory page.

>             makeSlots

Map the initial task's frames and create boot regions for them.

>             let initMappings = map
>                     (\f -> (f, (VPtr$fromIntegral f) - initOffset)) initFrames
>             (buffer, infoPtrs) <- mapTaskRegions initMappings

Create the basic set of initial capabilities.

>             tcbCap <- createInitCaps rootCNCap buffer

Create capabilities for the hardware devices and a small number of frame-sized untyped objects.

>             createSmallBlockCaps
>             createDeviceCaps

If there are not enough free slots remaining for both the capabilities to the remaining unallocated memory and a minimal number of initial free slots, then reserve another page full of slots.

>             untypedCount <- countUntypedCaps
>             freeSlotCount <- liftM length $ gets initFreeSlotsL2
>             when (freeSlotCount < untypedCount + minFreeSlots) $ do
>                 makeSlots

Reserving more slots may, in rare circumstances, increase the untyped cap count; this will happen if the free blocks are all more than double the size of a frame. Also, calling "makeSlots" may not produce enough free slots if "minFreeSlots" is too large. We assert that there are sufficient free slots to avoid these situations.

>                 untypedCount' <- countUntypedCaps
>                 freeSlotCount' <- liftM length $ gets initFreeSlotsL2
>                 assert (freeSlotCount' >= untypedCount' + minFreeSlots) $
>                     "Couldn't reserve enough free slots"

Create untyped capabilities to the remainder of available unallocated memory.

>             createUntypedCaps

Create boot regions for any remaining ranges of free slots. After this, it is no longer possible to create new capabilities, as there are no free slots, and no frames left to create more slots.

>             createFreeSlotRegions

Create empty regions corresponding to any free L1 frames.

>             createEmptyRegions
>             return (fst buffer, infoPtrs, tcbCap)

The boot data page is filled in.

>         let bootInfoWords = wordsFromBootInfo bootInfo
>         let intSize = bitSize (undefined::Word) `div` 8
>         assert (length bootInfoWords * intSize < 1 `shiftL` pageBits) $
>             "Boot info must fit in one page: " ++ show bootInfo
>         zipWithM_ storeWordUser
>             [infoPPtr, infoPPtr+(PPtr $ fromIntegral intSize)..] bootInfoWords

Finally, the initial thread's context is set up with the entry point and the location of the boot information page, and then activated.

>         activateInitialThread (capTCBPtr tcbCap) entry infoVPtr

\subsubsection{Initial Virtual Memory Mappings}

The initial thread's virtual address space contains a set of frames loaded by the boot loader, and two additional pages allocated by the kernel. Those frames are used for the initial thread's IPC buffer and for storing the "BootInfo" structure, respectively.

> mapTaskRegions :: [(PAddr, VPtr)] ->
>     KernelInit ((VPtr, PPtr CTE), (VPtr, PPtr Word))
> mapTaskRegions taskMappings = do
>     mapM (uncurry mapForInitTask) taskMappings
>     (bufferPtr,_,bufferSlot) <- allocPage
>     (infoVPtr,infoPPtr,_) <- allocPage
>     let pages = sort $ map snd taskMappings
>     let pageSize = 1 `shiftL` pageBits
>     let pageGroups = [bufferPtr]:[infoVPtr]:
>             rangesBy (\a b -> a + pageSize == b) pages
>     let rootTaskRegions =
>             map (\ps -> BootRegion
>                     (CPtr $ fromVPtr $ head ps)
>                     (CPtr $ fromVPtr $ last ps + pageSize - 1)
>                     BRRootTask (fromIntegral pageBits))
>                 $ pageGroups
>     mapM_ provideRegion rootTaskRegions
>     return ((bufferPtr, bufferSlot), (infoVPtr, infoPPtr))

\subsubsection{Initial Capabilities}

The first seven addresses in the initial thread capability space always contain a set of basic capabilities: a null capability at address 0, followed by the initial thread's TCB, CSpace and VSpace roots, and reply endpoint, the interrupt control capability, and the domain capability.

This function is responsible for placing those seven capabilities in the initial CSpace, and for creating the corresponding objects (other than the CSpace root, which already exists). It returns the TCB capability, which is needed later on to activate the thread.

> createInitCaps :: Capability -> (VPtr, PPtr CTE) -> KernelInit Capability
> createInitCaps rootCNCap (bufferPtr, bufferSlot) = do

Place a null capability in slot 0. The slot's contents should already be null; this is really only done to ensure that slot 0 isn't allocated to something else.

>     (nullCapPtr,_) <- provideCap NullCap
>     assert (nullCapPtr == CPtr 0) "Null cap must be at CPtr 0"

Create the root task and idle thread TCBs.

>     let tcbBits = objBits (undefined :: TCB)
>     tcbFrame <- liftM ptrFromPAddr $ allocRegion $ tcbBits
>     idleFrame <- liftM ptrFromPAddr $ allocRegion $ tcbBits
>     tcbCap <- doKernelOp $ createObject (fromAPIType TCBObject) tcbFrame 0
>     idleCap <- doKernelOp $ createObject (fromAPIType TCBObject) idleFrame 0

Place the initial TCB in the root thread.

>     provideCap tcbCap

Place the root CNode and VNode capabilities in the CSpace.

>     (_,rootCNSlot) <- provideCap rootCNCap
>     (_,rootVNSlot) <- provideCap NullCap

Set up the interrupt controller.

>     irqCap <- initInterruptController
>     provideCap irqCap

Grant the right to set domains.

>     provideCap DomainCap

Set up the VSpace. After this it is no longer safe to create new VSpace mappings (as this function may have searched the root CNode for mapped pages, and copied the mappings to a separate VSpace structure).

>     initVSpace rootCNSlot rootVNSlot

Insert the CSpace and VSpace roots in the initial TCB, and set its IPC buffer pointer and priority.

>     doKernelOp $ do
>         threadCRoot <- getThreadCSpaceRoot (capTCBPtr tcbCap)
>         cteInsert rootCNCap rootCNSlot threadCRoot
>         threadVRoot <- getThreadVSpaceRoot (capTCBPtr tcbCap)
>         rootVNCap <- getSlotCap rootVNSlot
>         cteInsert rootVNCap rootVNSlot threadVRoot
>         threadSet (\t -> t { tcbIPCBuffer = bufferPtr })
>             (capTCBPtr tcbCap)
>         threadBuffer <- getThreadBufferSlot (capTCBPtr tcbCap)
>         bufferCap <- getSlotCap bufferSlot
>         bufferCap' <- nullCapOnFailure $ deriveCap bufferSlot bufferCap
>         cteInsert bufferCap' bufferSlot threadBuffer
>         setPriority (capTCBPtr tcbCap) maxPriority

Configure and set the idle thread.

>     configureIdleThread (capTCBPtr idleCap)
>     doKernelOp $ setIdleThread (capTCBPtr idleCap)

The task now has its six initial capabilities: null, initial TCB, initial CSpace and VSpace roots, reply endpoint, and interrupt controller, in that order. There may also be additional architecture-specific capabilities created during "initVSpace". Create a boot region to point at them.

>     firstFreeSlot <- liftM (fst . head) $ gets initFreeSlotsL2
>     provideRegion $ BootRegion (CPtr 0) (firstFreeSlot - 1) BRInitCaps 0
>     return tcbCap

\subsubsection{Large Untyped Objects}

The large untyped objects are created from any physical memory that remains above the small block area, which generally occupies the first 1MB. This function may be called multiple times if the physical memory is not contiguous.

> createUntypedCaps :: KernelInit ()
> createUntypedCaps = do
>     freeRegions <- getUntypedRegions
>     modify (\st -> st { initFreeMemory = [], initBootMemory = [] })
>     let blocks = concat (map (uncurry makeBlockList) freeRegions)
>     mapM_ storeLargeBlock blocks

> storeLargeBlock :: (PAddr, Int) -> KernelInit ()
> storeLargeBlock (addr, bits) = do
>    let ptr = ptrFromPAddr addr
>    (cptr, _) <- provideCap (UntypedCap ptr bits 0)
>    addRegionWithMerge (BootRegion cptr cptr BRLargeBlocks (bit bits)) bits

Given the base and top addresses of a contiguous region of unallocated memory, this function generates a list of size-aligned blocks of memory.

> makeBlockList :: PAddr -> PAddr -> [(PAddr, Int)]
> makeBlockList s e = returnVal result
>     where
>         n = bitSize s
>         sizes = [0 .. n - 1]
>         makeLowBlock ((start, end), xs, ys) b =
>             if start `testBit` b && start <= end
>             then ((start + bit b, end), (start, b) : xs, ys)
>             else ((start, end), xs, ys)
>         makeHighBlock ((start, end), xs, ys) b =
>             if (end + 1) `testBit` b && start <= end
>             then ((start, end - bit b), xs, (end - bit b + 1, b) : ys)
>             else ((start, end), xs, ys)
>         makeBlocks v b = makeLowBlock (makeHighBlock v b) b
>         result = foldl makeBlocks ((s, e), [], []) sizes
>         returnVal (_, low, high) = reverse high ++ reverse low

In order to reserve enough capability slots for the initial free slots and the untyped object capabilities, the number of untyped capabilities that will be generated must be predicted. If the result is too large, then more slots will be allocated before calling "createUntypedCaps".

> countUntypedCaps :: KernelInit Int
> countUntypedCaps = do
>     freeRegions <- getUntypedRegions
>     return $ sum $ map (\(b,t) -> length (makeBlockList b t)) freeRegions

The available untyped regions include all free memory, and also all kernel memory that is used by the bootstrapping code. The latter cannot be allocated during kernel initialisation, but is usable by user-level code once the system has started.

> getUntypedRegions :: KernelInit [(PAddr, PAddr)]
> getUntypedRegions = gets (\st -> initFreeMemory st ++ initBootMemory st)

\subsubsection{Device Capabilities}

For each memory-mapped device in the system, the kernel provides the necessary capabilities to access the device's memory region, and creates a boot region structure pointing at those capabilities. The boot region's data word contains the base address and size of the device region, to allow user level device drivers to identify the devices (based on some platform-specific memory layout, or addresses provided by other devices).

> createDeviceCaps :: KernelInit ()
> createDeviceCaps = do
>     devices <- doKernelOp $ doMachineOp getDeviceRegions
>     forM_ devices $ \(base, end) -> do
>             cap <- doKernelOp $ createDeviceCap (base, end)
>             (cptr,_) <- provideCap cap
>             let rawsize = end - base
>             let sz = find (\sz -> rawsize == bit (pageBitsForSize sz))
>                           [minBound .. maxBound]
>             size <- case sz of
>                 Just size -> return $ pageBitsForSize size
>                 Nothing -> fail "Couldn't find appropriate size for device"
>             provideRegion $ BootRegion cptr cptr BRDeviceCaps
>                 (fromIntegral base .|. fromIntegral size)

\subsubsection{Small Untyped Objects}

A small fixed number of page-sized untyped objects is created from the free memory left over after kernel initialisation. These are intended for use by the initial task during bootstrapping.

> createSmallBlockCaps :: KernelInit ()
> createSmallBlockCaps = do
>     caps <- replicateM minSmallBlocks $ do
>         frame <- liftM ptrFromPAddr allocFrame
>         return $ UntypedCap frame pageBits 0
>     storeSmallBlockCaps caps

> storeSmallBlockCaps :: [Capability] -> KernelInit ()
> storeSmallBlockCaps = mapM_ storeSmallBlockCap

> storeSmallBlockCap :: Capability -> KernelInit ()
> storeSmallBlockCap cap = do
>     (cptr, _) <- provideCap cap
>     let dWord = fromIntegral pageBits
>     provideRegion $ BootRegion cptr cptr BRSmallBlocks dWord

\subsubsection{Free Slots}

After all of the initial capabilities have been created, there are some free slots left over. Boot regions are created for these free slots, to inform the user of their location. There must be a minimal number of free slots available, to allow the initial thread to bootstrap its capability management.

> createFreeSlotRegions :: KernelInit ()
> createFreeSlotRegions = do
>     slots <- gets initFreeSlotsL2
>     modify (\st -> st { initFreeSlotsL2 = [] })
>     assert (length slots >= minFreeSlots) $
>         "There are " ++ show (length slots) ++
>         " slots remaining, but there must be at least " ++ show minFreeSlots
>     let ranges = rangesBy (\a b -> a == b - 1) $ sort $ map fst slots
>     mapM_ (\slots -> provideRegion $
>             BootRegion (head slots) (last slots) BRFreeSlots 0
>         ) ranges

\subsubsection{Empty Regions}

Any regions of the capability space that are not allocated to the initial thread are marked as empty regions. This includes any unallocated slots in the root CNode, and the remainder of the valid address space.

> createEmptyRegions :: KernelInit ()
> createEmptyRegions = do
>     let l1size = bit (pageBits + rootCNodeSize)
>     slots <- gets initFreeSlotsL1
>     modify (\st -> st { initFreeSlotsL1 = [] })
>     let pageSize = 1 `shiftL` pageBits
>     let ranges = rangesBy (\a b -> a + pageSize == b) $ sort $ map fst slots
>     mapM_ (\slots -> provideRegion $
>             BootRegion (head slots) (last slots + pageSize - 1) BREmpty 0
>         ) ranges
>     provideRegion $ BootRegion l1size maxBound BREmpty 0

\subsection{Helper Functions}

Various functions in this module use "rangesBy" to split a sorted list into contiguous ranges, given a function that determines whether two adjacent items are contiguous. This is similar to "Data.List.groupBy", but does not assume that the comparison function "adj" is transitive; instead, the comparison function's arguments are always adjacent items in the input list.

> rangesBy :: (a -> a -> Bool) -> [a] -> [[a]]
> rangesBy _ [] = []
> rangesBy _ [x] = [[x]]
> rangesBy adj (x:y:xs)
>     | x `adj` y = (x:head r):tail r
>     | otherwise = [x]:r
>     where
>         r = rangesBy adj (y:xs)

> distinct :: Eq a => [a] -> Bool
> distinct [] = True
> distinct (x:xs) = (notElem x xs && distinct xs)

> isAligned :: (Num a, Bits a) => a -> Int -> Bool
> isAligned x n = x .&. mask n == 0


