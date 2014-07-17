--
-- Copyright 2014, General Dynamics C4 Systems
--
-- This software may be distributed and modified according to the terms of
-- the GNU General Public License version 2. Note that NO WARRANTY is provided.
-- See "LICENSE_GPLv2.txt" for details.
--
-- @TAG(GD_GPL)
--

{-# LANGUAGE EmptyDataDecls, ForeignFunctionInterface,
             GeneralizedNewtypeDeriving #-}

module SEL4.Machine.Hardware.ARM.Lyrebird where

import SEL4.Machine.RegisterSet
import Foreign.Ptr
import Data.Ix
import Data.Word(Word8)
import Data.Bits

data CallbackData

data IRQ = TimerInterrupt
    deriving (Enum, Bounded, Ord, Ix, Eq, Show)

newtype PAddr = PAddr { fromPAddr :: Word }
    deriving (Show, Eq, Ord, Bounded, Real, Enum, Integral, Num, Bits)

ptrFromPAddr :: PAddr -> PPtr a
ptrFromPAddr (PAddr addr) = PPtr addr

addrFromPPtr :: PPtr a -> PAddr
addrFromPPtr (PPtr ptr) = PAddr ptr

pageColourBits :: Int
pageColourBits = 0

foreign import ccall "arm_get_mem_top"
    getMemorySize :: Ptr CallbackData -> IO Int

getMemoryRegions :: Ptr CallbackData -> IO [(PAddr, PAddr)]
getMemoryRegions env = do
        size <- getMemorySize env
        return [(PAddr 0, PAddr (bit size))]

getDeviceRegions :: Ptr CallbackData -> IO [(PAddr, PAddr)]
getDeviceRegions _ = return []

getKernelDevices :: Ptr CallbackData -> IO [(PAddr, PPtr Word)]
getKernelDevices _ = return []

maskInterrupt :: Ptr CallbackData -> Bool -> IRQ -> IO ()
maskInterrupt env bool _ = maskIRQCallback env bool

ackInterrupt :: Ptr CallbackData -> IRQ -> IO ()
ackInterrupt env _ = ackIRQCallback env

foreign import ccall "arm_check_interrupt"
    getInterruptState :: Ptr CallbackData -> IO Bool

foreign import ccall "arm_mask_interrupt"
    maskIRQCallback :: Ptr CallbackData -> Bool -> IO ()

foreign import ccall "arm_ack_interrupt"
    ackIRQCallback :: Ptr CallbackData -> IO ()

getActiveIRQ :: Ptr CallbackData -> IO (Maybe IRQ)
getActiveIRQ env = do
    timer_irq <- getInterruptState env
    return $ if timer_irq then Just TimerInterrupt else Nothing

configureTimer :: Ptr CallbackData -> IO IRQ
configureTimer _ = return TimerInterrupt

resetTimer :: Ptr CallbackData -> IO ()
resetTimer _ = return ()

foreign import ccall unsafe "arm_load_word"
    loadWordCallback :: Ptr CallbackData -> PAddr -> IO Word

foreign import ccall unsafe "arm_store_word"
    storeWordCallback :: Ptr CallbackData -> PAddr -> Word -> IO ()

foreign import ccall unsafe "arm_tlb_flush"
    invalidateTLBCallback :: Ptr CallbackData -> IO ()

foreign import ccall unsafe "arm_tlb_flush_asid"
    invalidateHWASIDCallback :: Ptr CallbackData -> Word8 -> IO ()

foreign import ccall unsafe "arm_tlb_flush_vptr"
    invalidateMVACallback :: Ptr CallbackData -> Word -> IO ()

cacheCleanMVACallback :: Ptr CallbackData -> PPtr a -> IO ()
cacheCleanMVACallback _cptr _mva = return ()

cacheCleanRangeCallback :: Ptr CallbackData -> PPtr a -> PPtr a -> IO ()
cacheCleanRangeCallback _cptr _vbase _vtop = return ()

cacheCleanCallback :: Ptr CallbackData -> IO ()
cacheCleanCallback _cptr = return ()

cacheInvalidateRangeCallback :: Ptr CallbackData -> PPtr a -> PPtr a -> IO ()
cacheInvalidateRangeCallback _cptr _vbase _vtop = return ()

foreign import ccall unsafe "arm_set_asid"
    setHardwareASID :: Ptr CallbackData -> Word8 -> IO ()

foreign import ccall unsafe "arm_set_table_root"
    setCurrentPD :: Ptr CallbackData -> PAddr -> IO ()

foreign import ccall unsafe "arm_get_ifsr"
    getIFSR :: Ptr CallbackData -> IO Word

foreign import ccall unsafe "arm_get_dfsr"
    getDFSR :: Ptr CallbackData -> IO Word

foreign import ccall unsafe "arm_get_far"
    getFAR :: Ptr CallbackData -> IO VPtr
