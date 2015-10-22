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
>         initKernel, newKernelState, noInitFailure,
>         KernelInit, KernelInitState, doKernelOp, allocRegion,allocFrame,provideCap
>     ) where

\begin{impdetails}

% {-# BOOT-IMPORTS: SEL4.Machine SEL4.Model SEL4.API.Failures SEL4.Object.Structures SEL4.API.Types Control.Monad.State Control.Monad.Error #-}
% {-# BOOT-EXPORTS: #InitData KernelInitState KernelInit allocRegion allocFrame provideCap doKernelOp noInitFailure #-}

> import SEL4.Config
> import SEL4.API.Types
> import SEL4.API.Failures
> import SEL4.Model
> import SEL4.Object
> import SEL4.Object.Structures
> import SEL4.Machine
> import SEL4.Kernel.Thread
> import SEL4.Kernel.VSpace
> import SEL4.Kernel.BootInfo

> import Data.Bits
> import Control.Monad.State(StateT, runStateT)
> import Control.Monad.Error
> import Data.Word(Word8)

\end{impdetails}

\subsection{Overview}

The first thread in the system is created by the kernel; its address space contains capabilities to change thread domains and to use all of the machine's physical resources, minus any physical memory which is used by the kernel's static data and code.

\subsection{Framework}

\subsubsection{Kernel Initialisation Monad}

The various kernel initialisation functions run in a monad that extends the usual kernel state structures with additional information about the bootstrapping process. In particular, they are able to allocate memory from a list of free frames, and provide the user with capabilities and metadata about the capabilities.

The KernelInit monad can fail - however, we do not care what type of failure occurred, only that a failure has happened.

> type KernelInitState = StateT InitData Kernel
> type KernelInit = ErrorT InitFailure KernelInitState

> doKernelOp :: Kernel a -> KernelInit a
> doKernelOp = lift . lift

> noInitFailure :: KernelInitState a -> KernelInit a
> noInitFailure = lift

\subsubsection{Allocating Frames and Pages}

> minNum4kUntypedObj :: Int
> minNum4kUntypedObj = 12

> maxNumFreememRegions :: Int
> maxNumFreememRegions = 2

> getAPRegion :: PAddr -> KernelInit [Region]
> getAPRegion kernelFrameEnd = do
>    memRegions <- doKernelOp $ doMachineOp getMemoryRegions
>    subRegions <- return $ (flip map) memRegions (\x ->
>         let (s,e) = x in
>         if s < kernelFrameEnd then (kernelFrameEnd,e) else (s,e)
>       )
>    return $ map ptrFromPAddrRegion subRegions

> initFreemem :: PAddr -> Region -> KernelInit ()
> initFreemem kernelFrameEnd uiRegion = do
>    memRegions <- getAPRegion kernelFrameEnd
>    let region = fromRegion uiRegion
>    let fst' = fst . fromRegion
>    let snd' = snd . fromRegion
>    let subUI = \r  ->
>            if fst region >= fst' r  && snd region <= snd' r --assumes uiRegion within one region
>            then [(fst' r, fst region), (snd region, snd' r)]
>            else [(fst' r, snd' r)]
>    let freeRegions = concat $ map subUI memRegions
>    let freeRegions' = take maxNumFreememRegions $ freeRegions ++
>                       (if (length freeRegions < maxNumFreememRegions)
>                        then replicate (maxNumFreememRegions - length freeRegions) (PPtr 0, PPtr 0)
>                        else [])
>    noInitFailure $ modify (\st -> st { initFreeMemory = map Region freeRegions' })

> allocRegion :: Int -> KernelInit PAddr
> allocRegion bits = do
>     freeMem <- noInitFailure $ gets initFreeMemory
>     case isAlignedUsable `break` freeMem of
>         (small, r:rest) -> do
>             let (b, t) = fromRegion r
>             let (result, region) = if align b == b then (b, Region (b + s, t)) else (t - s, Region (b, t - s))
>             noInitFailure $ modify (\st -> st { initFreeMemory = small++ [region] ++rest })
>             return $ addrFromPPtr result
>         (_, []) ->
>             case isUsable `break` freeMem of
>                 (small, r':rest) -> do
>                     let (b, t) = fromRegion r'
>                     let result = align b
>                     let below = if result == b then [] else [Region (b, result)]
>                     let above = if result + s == t then [] else [Region (result + s, t)]
>                     noInitFailure $ modify (\st -> st { initFreeMemory = small++below++above++rest })
>                     return $ addrFromPPtr result
>                 _ -> fail "Unable to allocate memory"
>     where
>         s = 1 `shiftL` bits
>         align b = (((b - 1) `shiftR` bits) + 1) `shiftL` bits
>         isUsable reg = let r = (align . fst . fromRegion) reg in
>                            r >= (fst $ fromRegion reg) && r + s - 1 <= (snd $ fromRegion reg)
>         isAlignedUsable reg =
>             let
>                 b = fst $ fromRegion reg;
>                 t = snd $ fromRegion reg;
>                 (r, q) = (align b, align t)
>             in (r == b || q == t) && t - b >= s


\subsection{Bootstrapping the Kernel}

The kernel is bootstrapped by calling "initKernel". The arguments are the address of the initial thread's entry point, a list of frames to be mapped in the initial thread's address space, the offset to be subtracted from the initial frame addresses to obtain the corresponding virtual addresses, a list of frames reserved by the kernel, and a list of frames that are used by the kernel's bootstrapping code.

> initKernel :: VPtr -> [PAddr] -> VPtr -> [PAddr] -> [PAddr] -> Kernel ()
> initKernel entry initFrames initOffset kernelFrames bootFrames = do

Define some useful constants.

>         let uiRegion = coverOf $ map (\x -> Region (ptrFromPAddr x, (ptrFromPAddr x) + bit (pageBits))) initFrames
>         let kernelRegion = coverOf $ map (\x -> Region (ptrFromPAddr x, (ptrFromPAddr x) + bit (pageBits))) kernelFrames
>         let kePPtr = fst $ fromRegion $ uiRegion
>         let kfEndPAddr = addrFromPPtr kePPtr 
>         (startPPtr,endPPtr) <- return $ fromRegion uiRegion 
>         let vptrStart = (VPtr (fromPAddr $ addrFromPPtr $ startPPtr )) + initOffset
>         let vptrEnd = (VPtr (fromPAddr $ addrFromPPtr $ endPPtr )) + initOffset

Determine the available memory regions.

>         allMemory <- doMachineOp getMemoryRegions

\begin{impdetails}
Configure the physical address space model. This is an implementation detail specific to the Haskell model, and is not relevant on real hardware.

>         initPSpace $ map (\(s, e) -> (ptrFromPAddr s, ptrFromPAddr e))
>                          allMemory

Warning: Currently we assume that the bootFrames (probably get from qemu) is always []

Warning: We used to reserve the PSpace regions used by the kernel and the root task. This must be done first, since some of these reserved regions will be turned into kernel objects by "initKernelVM", below. Again, this is specific to the Haskell model's implementation and is not relevant on real hardware.

>         mapM (\p -> reserveFrame (ptrFromPAddr p) True) $ kernelFrames ++ bootFrames

\end{impdetails}

\begin{impdetails}
Set up the kernel's VM environment.

>         initKernelVM

\end{impdetails}

\begin{impdetails}
FIXME: is the following still necessary in haskell?

>         initCPU
>         initPlatform

\end{impdetails}

Move into the "KernelInit" monad. Arguments in nopInitData are not correct, BIFrameInfo will get initialized later.

>         runInit $ do
>                 initFreemem kfEndPAddr uiRegion
>                 rootCNCap <- makeRootCNode
>                 initInterruptController rootCNCap biCapIRQControl
>                 let ipcBufferVPtr = vptrEnd
>                 ipcBufferCap <- createIPCBufferFrame rootCNCap ipcBufferVPtr 
>                 let biFrameVPtr = vptrEnd + (1 `shiftL` pageBits)
>                 createBIFrame rootCNCap biFrameVPtr 0 1
>                 createFramesOfRegion rootCNCap uiRegion True initOffset
>                 itPDCap <- createITPDPTs rootCNCap vptrStart biFrameVPtr 
>                 writeITPDPTs rootCNCap itPDCap

Create and Init initial thread's ASID pool 

>                 itAPCap <- createITASIDPool rootCNCap
>                 doKernelOp $ writeITASIDPool itAPCap itPDCap

>                 createIdleThread

Create InitialThread and switch to it

>                 createInitialThread rootCNCap itPDCap ipcBufferCap entry ipcBufferVPtr biFrameVPtr

>                 createUntypedObject rootCNCap (Region (0,0))

Create Device Frames

>                 createDeviceFrames rootCNCap

Set the NullCaps in BIFrame

>                 finaliseBIFrame

Serialize BIFrame into memory 

>                 syncBIFrame

We should clean cache, but we did not have a good interface so far.

>--               doKernelOp $ doMachineOp $ cleanCache

> finaliseBIFrame :: KernelInit ()
> finaliseBIFrame = do
>   cur <- noInitFailure $ gets $ initSlotPosCur
>   max <- noInitFailure $ gets $ initSlotPosMax
>   noInitFailure $ modify (\s -> s { initBootInfo = (initBootInfo s) {bifNullCaps = [cur .. max - 1]}})

> runInit :: KernelInit () -> Kernel ()
> runInit oper = do
>     let initData = InitData {
>                       initFreeMemory = [],
>                       initSlotPosCur = 0,
>                       initSlotPosMax = bit (pageBits), -- The CNode Size is pageBits + (objSize cte)
>                       initBootInfo = nopBIFrameData,
>                       initBootInfoFrame = 0 }
>     (flip runStateT) initData $ do
>         result <- runErrorT oper
>         either (\_ -> fail $ "initFailure") return result
>     return ()

createInitalThread, setup caps in initial thread, set idleThread to be the currentThread and switch to the initialThread.

> createInitialThread :: Capability -> Capability -> Capability -> VPtr -> VPtr -> VPtr-> KernelInit ()
> createInitialThread rootCNCap itPDCap ipcBufferCap entry ipcBufferVPtr biFrameVPtr = do
>       let tcbBits = objBits (makeObject::TCB)
>       tcb' <- allocRegion tcbBits 
>       let tcbPPtr = ptrFromPAddr tcb'
>       doKernelOp $ do
>          placeNewObject tcbPPtr (makeObject::TCB) 0
>          srcSlot <- locateSlot (capCNodePtr rootCNCap) biCapITCNode
>          destSlot <- getThreadCSpaceRoot tcbPPtr 
>          cteInsert rootCNCap srcSlot destSlot
>          srcSlot <- locateSlot (capCNodePtr rootCNCap) biCapITPD
>          destSlot <- getThreadVSpaceRoot tcbPPtr
>          cteInsert itPDCap srcSlot destSlot
>          srcSlot <- locateSlot (capCNodePtr rootCNCap) biCapITIPCBuf
>          destSlot <- getThreadBufferSlot tcbPPtr
>          cteInsert ipcBufferCap srcSlot destSlot
>          threadSet (\t-> t{tcbIPCBuffer = ipcBufferVPtr}) tcbPPtr 

>          activateInitialThread tcbPPtr entry biFrameVPtr 

Insert thread into rootCNodeCap 

>          cap <- return $ ThreadCap tcbPPtr 
>          slot <- locateSlot (capCNodePtr rootCNCap) biCapITTCB
>          insertInitCap slot cap
>       return ()

create idle thread and set the Current Thread to Idle Thread
FIXME: Seems we need to setCurThread and setSchedulerAction here, otherwise errors raised.

> createIdleThread :: KernelInit ()
> createIdleThread = do
>       paddr <- allocRegion $ objBits (makeObject :: TCB)
>       let tcbPPtr = ptrFromPAddr paddr
>       doKernelOp $ do 
>             placeNewObject tcbPPtr (makeObject::TCB) 0
>             modify (\s -> s {ksIdleThread = tcbPPtr})
>             setCurThread tcbPPtr
>             setSchedulerAction ResumeCurrentThread
>       configureIdleThread tcbPPtr

> createUntypedObject :: Capability -> Region -> KernelInit ()
> createUntypedObject rootCNodeCap bootMemReuseReg = do
>     let regStart = (fst . fromRegion)
>     let regStartPAddr = (addrFromPPtr . regStart)
>     let regEnd = (snd . fromRegion)
>     let regEndPAddr = (addrFromPPtr . regEnd)
>     slotBefore <- noInitFailure $ gets initSlotPosCur
>     mapM_ (\i -> provideUntypedCap rootCNodeCap i (fromIntegral pageBits) slotBefore) 
>              [regStartPAddr bootMemReuseReg, (regStartPAddr bootMemReuseReg + bit pageBits) .. (regEndPAddr bootMemReuseReg - 1)]
>     currSlot <- noInitFailure $ gets initSlotPosCur
>     mapM_ (\_ -> do
>              paddr <- allocRegion pageBits
>              provideUntypedCap rootCNodeCap paddr (fromIntegral pageBits) slotBefore)
>           [(currSlot - slotBefore) .. (fromIntegral minNum4kUntypedObj - 1)]
>     freemem <- noInitFailure $ gets initFreeMemory
>     (flip mapM) (take maxNumFreememRegions freemem)
>         (\reg -> do
>             (\f -> mapM (f reg) [4 .. wordBits - 2])
>                 (\reg bits -> do
>                     reg' <- (if not (isAligned (regStartPAddr reg) (bits + 1)) 
>                                 && (regEndPAddr reg) - (regStartPAddr reg) >= bit bits 
>                         then do
>                             provideUntypedCap rootCNodeCap (regStartPAddr reg) (fromIntegral bits) slotBefore
>                             return $ Region (regStart reg + bit bits, regEnd reg)
>                         else return reg)
>                     if not (isAligned (regEndPAddr reg') (bits + 1)) && (regEndPAddr reg') - (regStartPAddr reg') >= bit bits
>                         then do
>                             provideUntypedCap rootCNodeCap (regEndPAddr reg' - bit bits) (fromIntegral bits) slotBefore
>                             return $ Region (regStart reg', regEnd reg' - bit bits)
>                         else return reg' )
>         )
>     let emptyReg = Region (PPtr 0, PPtr 0)
>     let freemem' = replicate maxNumFreememRegions emptyReg
>     slotAfter <- noInitFailure $ gets initSlotPosCur
>     noInitFailure $ modify (\s -> s { initFreeMemory = freemem', 
>                       initBootInfo = (initBootInfo s) { 
>                            bifUntypedObjCaps = [slotBefore .. slotAfter - 1] }})
>--   syncBIFrame


> mapTaskRegions :: [(PAddr,VPtr)] -> KernelInit ((VPtr,PPtr CTE),(VPtr,PPtr Word))
> mapTaskRegions taskMappings = do
>         fail $ "mapTaskRegions is not Implemented" ++ show taskMappings

Make the root cnode, alloc (cptr,slot) from allocRootSlot and insert root cnode cap into its self

Specific allocRegion for convenience, since most allocations are frame-sized.

> allocFrame :: KernelInit PAddr
> allocFrame = allocRegion pageBits

> makeRootCNode :: KernelInit Capability
> makeRootCNode = do
>       let slotBits = objBits (undefined::CTE)
>       let levelBits = rootCNodeSize
>       frame <- liftM ptrFromPAddr $ allocRegion (levelBits + slotBits)

>       rootCNCap <- doKernelOp $ createObject (fromAPIType CapTableObject) frame levelBits
>       rootCNCap <- return $ rootCNCap {capCNodeGuardSize = 32 - levelBits}
>       slot <- doKernelOp $ locateSlot (capCNodePtr rootCNCap) biCapITCNode
>       doKernelOp $ insertInitCap slot rootCNCap
>       return rootCNCap 


> provideCap :: Capability -> Capability -> KernelInit ()
> provideCap rootCNodeCap cap = do
>     currSlot <- noInitFailure $ gets initSlotPosCur
>     maxSlot <- noInitFailure $ gets initSlotPosMax
>     when (currSlot >= maxSlot) $ throwError InitFailure
>     slot <- doKernelOp $ locateSlot (capCNodePtr rootCNodeCap) currSlot
>     doKernelOp $ insertInitCap slot cap
>     noInitFailure $ modify (\st -> st { initSlotPosCur = currSlot + 1 })  

> provideUntypedCap :: Capability -> PAddr -> Word8 -> Word -> KernelInit ()
> provideUntypedCap rootCNodeCap pptr sizeBits slotPosBefore = do
>     currSlot <- noInitFailure $ gets initSlotPosCur
>     let i = currSlot - slotPosBefore
>     untypedObjs <- noInitFailure $ gets (bifUntypedObjPAddrs . initBootInfo)
>     assert (length untypedObjs == fromIntegral i) "Untyped Object List is inconsistent"
>     untypedObjs' <- noInitFailure $ gets (bifUntypedObjSizeBits . initBootInfo)
>     assert (length untypedObjs' == fromIntegral i) "Untyped Object List is inconsistent"
>     bootInfo <- noInitFailure $ gets initBootInfo
>     let bootInfo' = bootInfo { bifUntypedObjPAddrs = untypedObjs ++ [pptr],
>                                bifUntypedObjSizeBits = untypedObjs' ++ [sizeBits] }
>     noInitFailure $ modify (\st -> st { initBootInfo = bootInfo' })
>     provideCap rootCNodeCap $ UntypedCap {
>                                   capPtr = ptrFromPAddr pptr,
>                                   capBlockSize = fromIntegral sizeBits,
>                                   capFreeIndex = 0 }
 
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

> coverOf :: [Region] -> Region
> coverOf [] = Region (0,0) 
> coverOf [x] = x
> coverOf (x:xs) = Region (ln, hn)
>     where 
>         (l,h) = fromRegion x;
>         (ll,hh) = fromRegion $ coverOf xs;
>         ln = if l <= ll then l else ll;
>         hn = if h <= hh then hh else h

> distinct :: Eq a => [a] -> Bool
> distinct [] = True
> distinct (x:xs) = (notElem x xs && distinct xs)


