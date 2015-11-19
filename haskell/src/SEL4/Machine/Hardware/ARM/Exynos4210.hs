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

module SEL4.Machine.Hardware.ARM.Exynos4210 where

import SEL4.Machine.RegisterSet
import SEL4.Machine.Hardware.ARM.Callbacks
import SEL4.Machine.Hardware.GICInterface hiding (IRQ, maskInterrupt)
import qualified SEL4.Machine.Hardware.GICInterface as GIC
import Foreign.Ptr
import Data.Bits

-- Following harded coded address pair are used in getKernelDevices 
-- and will get mapped into kernel address space via mapKernelFrame
uart = (PAddr 0x13810000, PPtr 0xfff01000) 
mct = (PAddr 0x10050000, PPtr 0xfff02000)
l2cc = (PAddr 0x10502000, PPtr 0xfff03000)
gicController = (PAddr 0x10480000, PPtr 0xfff04000)
gicDistributor = (PAddr 0x10490000, PPtr 0xfff05000)

gicInterfaceBase = PAddr 0x10480000
gicDistributorBase = PAddr 0x10490000

physBase = 0x40000000
physMappingOffset = 0xe0000000 - physBase

ptrFromPAddr :: PAddr -> PPtr a
ptrFromPAddr (PAddr addr) = PPtr $ addr + physMappingOffset

addrFromPPtr :: PPtr a -> PAddr
addrFromPPtr (PPtr ptr) = PAddr $ ptr - physMappingOffset

pageColourBits :: Int
pageColourBits = 0 -- qemu has no cache

getMemoryRegions :: Ptr CallbackData -> IO [(PAddr, PAddr)]
getMemoryRegions _ = return [(0x40000000, 0x40000000 + (0x8 `shiftL` 24))]

getDeviceRegions :: Ptr CallbackData -> IO [(PAddr, PAddr)]
getDeviceRegions _ = return devices
    where devices = []

type IRQ = GIC.IRQ

mctPPtr = PPtr 0xfff00000
timerAddr = PAddr 0x53f94000
timerIRQ = GIC.IRQ 28


getKernelDevices :: Ptr CallbackData -> IO [(PAddr, PPtr Word)]
getKernelDevices _ = return devices
    where devices = [
            mct,-- kernel timer
            gicController, -- interrupt controller
            gicDistributor, -- interrupt controller
            uart
            ]

maskInterrupt :: Ptr CallbackData -> Bool -> IRQ -> IO ()
maskInterrupt env mask irq = do
     callGICApi (GicState { env = env, gicDistBase = gicDistributorBase, gicIFBase = gicInterfaceBase })
       (GIC.maskInterrupt mask irq)

-- We don't need to acknowledge interrupts explicitly because we don't use
-- the vectored interrupt controller.
ackInterrupt :: Ptr CallbackData -> IRQ -> IO ()
ackInterrupt env irq = callGICApi gic (GIC.ackInterrupt irq)
      where gic = GicState { env = env, 
        gicDistBase = gicDistributorBase,
        gicIFBase = gicInterfaceBase }

foreign import ccall unsafe "qemu_run_devices"
    runDevicesCallback :: IO ()

getActiveIRQ :: Ptr CallbackData -> IO (Maybe IRQ)
getActiveIRQ env = do
    runDevicesCallback
    callGICApi gicdata $ GIC.getActiveIRQ
      where gicdata = GicState { env = env, 
        gicDistBase = gicDistributorBase,
        gicIFBase = gicInterfaceBase }


-- 1kHz tick; qemu's SP804s always run at 1MHz 
timerFreq :: Word
timerFreq = 100

timerLimit :: Word
timerLimit = 1000000 `div` timerFreq

configureTimer :: Ptr CallbackData -> IO IRQ
configureTimer _ = do
    -- enabled, periodic, interrupts enabled
    return timerIRQ

initIRQController :: Ptr CallbackData -> IO ()
initIRQController env = callGICApi gicdata $ GIC.initIRQController
  where gicdata = GicState { env = env, 
    gicDistBase = gicDistributorBase,
    gicIFBase = gicInterfaceBase }

resetTimer :: Ptr CallbackData -> IO ()
resetTimer _ = return ()

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
