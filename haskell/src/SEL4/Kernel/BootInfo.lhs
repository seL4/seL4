%
% Copyright 2014, General Dynamics C4 Systems
%
% This software may be distributed and modified according to the terms of
% the GNU General Public License version 2. Note that NO WARRANTY is provided.
% See "LICENSE_GPLv2.txt" for details.
%
% @TAG(GD_GPL)
%



This module contains functions for maintaining the bootinfo structure that is passed to the initial user thread.

> module SEL4.Kernel.BootInfo where

\begin{impdetails}

> import Data.Bits
> import Control.Monad.State

> import SEL4.API.Types
> import SEL4.Config
> import SEL4.Object.Structures
> import SEL4.Machine

> import SEL4.Model.StateData

> import {-# SOURCE #-} SEL4.Kernel.Init


\subsection{Constant}

> biCapNull :: Word
> biCapNull = 0

> itASID :: ASID
> itASID = 1

> biCapITTCB :: Word
> biCapITTCB = 1

> biCapITCNode :: Word
> biCapITCNode = 2

> biCapITPD :: Word
> biCapITPD = 3

> biCapIRQControl :: Word
> biCapIRQControl = 4

> biCapASIDControl :: Word
> biCapASIDControl = 5

> biCapITASIDPool :: Word
> biCapITASIDPool = 6

> biCapIOPort :: Word
> biCapIOPort = 7

> biCapIOSpace :: Word
> biCapIOSpace = 8

> biCapBIFrame :: Word
> biCapBIFrame = 9

> biCapITIPCBuf :: Word
> biCapITIPCBuf = 10

> biCapDynStart :: Word
> biCapDynStart = 11

> biFrameSizeBits :: Int
> biFrameSizeBits = pageBits

Warning: rootCNodeSizeBits should be rootCNodeSize + objBits (undefined::CTE)


\subsection{Default Bootinfo}

> nopBIFrameData :: BIFrameData
> nopBIFrameData = BIFrameData {
>         bifNodeID = 0,        -- Initialized in createIPCBufferFrame
>         bifNumNodes = 0,      -- Initialized in createIPCBufferFrame 
>         bifNumIOPTLevels = 0, -- Initialized with 0 if fine
>         bifIPCBufVPtr =0,     -- Initialized in createIPCBufferFrame
>         bifNullCaps = [],
>         bifSharedFrameCaps = [], -- ARM no multikernel
>         bifUIFrameCaps = [], -- Initialized in createFramesOfRegion
>         bifUIPTCaps = [], -- Initialized in createITPDPTs
>         bifUntypedObjCaps = [],
>         bifUntypedObjPAddrs = [],
>         bifUntypedObjSizeBits = [],
>         bifITCNodeSizeBits = fromIntegral rootCNodeSize, -- Initialized here is fine
>         bifNumDeviceRegions = 0,
>         bifDeviceRegions = [] 
>         }


Functions for serialize BIFrameData into Memory

> data SerialData = SerialData {ptrCursor::PPtr Word,value::Word}
> type Serializer = StateT SerialData MachineMonad

> serializeStore :: Word -> Int -> Serializer ()
> serializeStore value intsize = do
>    forM_ [0 .. intsize-1] $ \size -> do

Warning: For little endian system we should use this size instead of intsize - size - 1

>        byte <- return $ (value `shiftR` (8 * size )) .&. ((bit 8) - 1)
>        serializeByte byte

> serializeByte :: Word -> Serializer ()
> serializeByte input = do
>    ptr <- gets ptrCursor
>    byte <- gets value
>    mod4 <- return $ fromIntegral $ fromPPtr $ (ptr .&. 3)
>    value <- return $ (input `shiftL` (mod4 * 8)) .|. (fromIntegral byte)
>    if (ptr .&. 3) == 3
>      then do
>        lift $ storeWordVM ((ptr `shiftR` 2) `shiftL` 2) value
>        modify (\st -> st {ptrCursor = ptr + 1, value = 0})
>      else
>        modify (\st -> st {ptrCursor = ptr + 1, value = value})

> paddingTo :: PPtr Word -> Serializer()
> paddingTo pptr = do
>    ptr <- gets ptrCursor
>    (flip mapM_) [ptr .. pptr-1] $ \_ -> (serializeByte 0)

> maxBIUntypedCaps :: Word
> maxBIUntypedCaps = 167

> maxBIDeviceRegions :: Word
> maxBIDeviceRegions = 200

> serialBIDeviceRegion :: BIDeviceRegion -> Serializer ()
> serialBIDeviceRegion biDeviceRegion = do
>    serializeStore (fromPAddr $ bidrBasePAddr biDeviceRegion) 4
>    serializeStore (fromIntegral $ bidrFrameSizeBits biDeviceRegion) 4
>    slotRegion <- return $ bidrFrameCaps biDeviceRegion
>    let (a,b) = case slotRegion of SlotRegion (a,b) -> (a,b)
>    serializeStore a 4
>    serializeStore b 4
>    return ()

The function syncBIFrame is used as the last step in KernelInit. 
It will write boot info back into the memory.

> syncBIFrame :: KernelInit ()
> syncBIFrame = do
>    frameData <- gets $ initBootInfo
>    frame <- gets $ initBootInfoFrame
>    doKernelOp $ doMachineOp $ do
>        (flip runStateT) (SerialData {ptrCursor = ptrFromPAddr $ frame,value = 0}) $ do
>            serializeStore (fromIntegral (bifNodeID frameData)) 4
>            serializeStore (fromIntegral (bifNumNodes frameData)) 4
>            serializeStore (fromIntegral (bifNumIOPTLevels frameData)) 4
>            serializeStore (fromIntegral $ fromVPtr $ (bifIPCBufVPtr frameData)) 4
>            let serializeRange = \ls -> if ls == []
>                       then do
>                         serializeStore 0 4
>                         serializeStore 0 4
>                       else do
>                         serializeStore (head ls) 4
>                         serializeStore (1 + last ls) 4
>            serializeRange $ bifNullCaps frameData
>            serializeRange $ bifSharedFrameCaps frameData
>            serializeRange $ bifUIFrameCaps frameData
>            serializeRange $ bifUIPTCaps frameData
>            serializeRange $ bifUntypedObjCaps frameData

>            ptr <- gets ptrCursor
>            ptr <- return $ ptr + (PPtr $ (maxBIUntypedCaps `shiftL` 2))
>            untypedAddrs <- return $ bifUntypedObjPAddrs frameData
>            (flip mapM_) untypedAddrs $ \addr -> do
>                serializeStore (fromPAddr addr) 4
>            paddingTo ptr
>            ptr <- return $ ptr + (PPtr maxBIUntypedCaps)
>            untypedSizeBits <- return $ bifUntypedObjSizeBits frameData
>            (flip mapM_) untypedSizeBits $ \bits -> do
>                serializeStore (fromIntegral bits) 1
>            paddingTo ptr
>            serializeStore (fromIntegral $ bifITCNodeSizeBits frameData) 1
>            serializeStore (fromIntegral $ bifNumDeviceRegions frameData) 4
>            (flip mapM_) (bifDeviceRegions frameData) $ \region -> do
>                serialBIDeviceRegion region
>        return ()

The KernelInit monad can fail - however, we do not care what type of failure occurred, only that a failure has happened.

> isAligned x n = x .&. mask n == 0

