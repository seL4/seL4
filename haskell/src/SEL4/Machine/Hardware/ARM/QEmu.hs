--
-- Copyright 2014, General Dynamics C4 Systems
--
-- This software may be distributed and modified according to the terms of
-- the GNU General Public License version 2. Note that NO WARRANTY is provided.
-- See "LICENSE_GPLv2.txt" for details.
--
-- @TAG(GD_GPL)
--

{-# LANGUAGE EmptyDataDecls, ForeignFunctionInterface, GeneralizedNewtypeDeriving #-}

module SEL4.Machine.Hardware.ARM.QEmu where

import SEL4.Machine.RegisterSet
import Foreign.Ptr
import Data.Bits
import Data.Word(Word8)
import Data.Ix
import Data.Maybe
import Control.Monad

data CallbackData

newtype IRQ = IRQ Word8
    deriving (Enum, Ord, Ix, Eq, Show)

instance Bounded IRQ where
    minBound = IRQ 0
    maxBound = IRQ 31

newtype PAddr = PAddr { fromPAddr :: Word }
    deriving (Integral, Real, Show, Eq, Num, Bits, Ord, Enum, Bounded)

physMappingOffset = 0xf0000000

ptrFromPAddr :: PAddr -> PPtr a
ptrFromPAddr (PAddr addr) = PPtr $ addr + physMappingOffset

addrFromPPtr :: PPtr a -> PAddr
addrFromPPtr (PPtr ptr) = PAddr $ ptr - physMappingOffset

pageColourBits :: Int
pageColourBits = 0 -- qemu has no cache

getMemoryRegions :: Ptr CallbackData -> IO [(PAddr, PAddr)]
getMemoryRegions _ = return [(0, 1 `shiftL` 24)]

getDeviceRegions :: Ptr CallbackData -> IO [(PAddr, PAddr)]
getDeviceRegions _ = return devices
    where devices = [
            (0x101e3000, 0x101e4000), -- second SP804; kernel uses first
            (0x10010000, 0x10011000) -- SMC91C111 ethernet
            ]

timerPPtr = PPtr 0xff001000
timerAddr = PAddr 0x101e2000
timerIRQ = IRQ 4

pl190PPtr = PPtr 0xff002000
pl190Addr = PAddr 0x10140000

getKernelDevices :: Ptr CallbackData -> IO [(PAddr, PPtr Word)]
getKernelDevices _ = return devices
    where devices = [
            (timerAddr, timerPPtr), -- kernel timer
            (pl190Addr, pl190PPtr) -- interrupt controller
            ]

maskInterrupt :: Ptr CallbackData -> Bool -> IRQ -> IO ()
maskInterrupt env mask (IRQ irq) = do
    let value = bit $ fromIntegral irq
    let pl190Reg = if mask then 0x14 else 0x10
    storeWordCallback env (pl190Addr + pl190Reg) value
    when (irq >= 21 && irq <= 30) $ do
        -- these IRQs go via a a separate secondary interrupt controller,
        -- which can either multiplex them to IRQ 31 or pass them through
        -- to the PL190. We choose the latter.
        let vpbSICBase = PAddr 0x10003000
        let vpbSICReg = PAddr $ if mask then 0x24 else 0x20
        storeWordCallback env (vpbSICBase + vpbSICReg) value

-- We don't need to acknowledge interrupts explicitly because we don't use
-- the vectored interrupt controller.
ackInterrupt :: Ptr CallbackData -> IRQ -> IO ()
ackInterrupt _ _ = return ()

foreign import ccall unsafe "qemu_run_devices"
    runDevicesCallback :: IO ()

interruptCallback :: Ptr CallbackData -> IO (Maybe IRQ)
interruptCallback env = do
    -- No need to call back to the simulator here; we just check the PIC's
    -- active interrupt register. This will probably work for real ARMs too,
    -- as long as we're not using vectored interrupts
    active <- loadWordCallback env pl190Addr
    -- the following line is equivalent to the ARMv5 CLZ instruction. This
    -- means the kernel will handle higher IRQ numbers earlier, but this has
    -- little significance --- *any* IRQ will cause an immediate kernel entry.
    -- It does have a small effect on accounting of CPU time usage by the
    -- kernel during the IRQ handler, depending on the timer's IRQ number.
    return $ listToMaybe $
        [ IRQ $ fromIntegral x | x <- reverse [0..31], testBit active x ]

getActiveIRQ :: Ptr CallbackData -> IO (Maybe IRQ)
getActiveIRQ env = do
    runDevicesCallback
    interruptCallback env

-- 1kHz tick; qemu's SP804s always run at 1MHz 
timerFreq :: Word
timerFreq = 100

timerLimit :: Word
timerLimit = 1000000 `div` timerFreq

configureTimer :: Ptr CallbackData -> IO IRQ
configureTimer env = do
    -- enabled, periodic, interrupts enabled
    let timerCtrl = bit 7 .|. bit 6 .|. bit 5
    storeWordCallback env (timerAddr+0x8) timerCtrl
    storeWordCallback env timerAddr timerLimit
    return timerIRQ

resetTimer :: Ptr CallbackData -> IO ()
resetTimer env = storeWordCallback env (timerAddr+0xc) 0

foreign import ccall unsafe "qemu_load_word_phys"
    loadWordCallback :: Ptr CallbackData -> PAddr -> IO Word

foreign import ccall unsafe "qemu_store_word_phys"
    storeWordCallback :: Ptr CallbackData -> PAddr -> Word -> IO ()

foreign import ccall unsafe "qemu_tlb_flush"
    invalidateTLBCallback :: Ptr CallbackData -> IO ()

foreign import ccall unsafe "qemu_tlb_flush_asid"
    invalidateTLB_ASIDCallback :: Ptr CallbackData -> Word8 -> IO ()

foreign import ccall unsafe "qemu_tlb_flush_vptr"
    invalidateTLB_VAASIDCallback :: Ptr CallbackData -> Word -> IO ()

isbCallback :: Ptr CallbackData -> IO ()
isbCallback _ = return ()

dsbCallback :: Ptr CallbackData -> IO ()
dsbCallback _ = return ()

dmbCallback :: Ptr CallbackData -> IO ()
dmbCallback _ = return ()

cacheCleanByVACallback :: Ptr CallbackData -> VPtr -> PAddr -> IO ()
cacheCleanByVACallback _cptr _mva _pa = return ()

cacheCleanByVA_PoUCallback :: Ptr CallbackData -> VPtr -> PAddr -> IO ()
cacheCleanByVA_PoUCallback _cptr _mva _pa = return ()

cacheInvalidateByVACallback :: Ptr CallbackData -> VPtr -> PAddr -> IO ()
cacheInvalidateByVACallback _cptr _mva _pa = return ()

cacheInvalidateByVA_ICallback :: Ptr CallbackData -> VPtr -> PAddr -> IO ()
cacheInvalidateByVA_ICallback _cptr _mva _pa = return ()

cacheInvalidate_I_PoUCallback :: Ptr CallbackData -> IO ()
cacheInvalidate_I_PoUCallback _ = return ()

cacheCleanInvalidateByVACallback ::
    Ptr CallbackData -> VPtr -> PAddr -> IO ()
cacheCleanInvalidateByVACallback _cptr _mva _pa = return ()

branchFlushCallback :: Ptr CallbackData -> VPtr -> PAddr -> IO ()
branchFlushCallback _cptr _mva _pa = return ()

cacheClean_D_PoUCallback :: Ptr CallbackData -> IO ()
cacheClean_D_PoUCallback _ = return ()

cacheCleanInvalidate_D_PoCCallback :: Ptr CallbackData -> IO ()
cacheCleanInvalidate_D_PoCCallback _ = return ()

cacheCleanInvalidate_D_PoUCallback :: Ptr CallbackData -> IO ()
cacheCleanInvalidate_D_PoUCallback _ = return ()

cacheCleanInvalidateL2RangeCallback ::
    Ptr CallbackData -> PAddr -> PAddr -> IO ()
cacheCleanInvalidateL2RangeCallback _ _ _ = return ()

cacheInvalidateL2RangeCallback :: Ptr CallbackData -> PAddr -> PAddr -> IO ()
cacheInvalidateL2RangeCallback _ _ _ = return ()

cacheCleanL2RangeCallback :: Ptr CallbackData -> PAddr -> PAddr -> IO ()
cacheCleanL2RangeCallback _ _ _ = return ()

-- For the ARM1136
cacheLine :: Int
cacheLine = 32

cacheLineBits :: Int
cacheLineBits = 5

foreign import ccall unsafe "qemu_set_asid"
    setHardwareASID :: Ptr CallbackData -> Word8 -> IO ()

foreign import ccall unsafe "qemu_set_root"
    setCurrentPD :: Ptr CallbackData -> PAddr -> IO ()

foreign import ccall unsafe "qemu_arm_get_ifsr"
    getIFSR :: Ptr CallbackData -> IO Word

foreign import ccall unsafe "qemu_arm_get_dfsr"
    getDFSR :: Ptr CallbackData -> IO Word

foreign import ccall unsafe "qemu_arm_get_far"
    getFAR :: Ptr CallbackData -> IO VPtr

