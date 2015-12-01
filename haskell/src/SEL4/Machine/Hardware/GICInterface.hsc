{-# LANGUAGE CPP, ForeignFunctionInterface #-}
{-# LANGUAGE CPP, GeneralizedNewtypeDeriving #-}
module SEL4.Machine.Hardware.GICInterface where

import Foreign.Ptr (Ptr)
import Data.Array
import Data.Bits
import Data.Word(Word32)

#include "gic.h"

-- helpers
import Control.Monad.Reader
import Control.Monad.State
import SEL4.Machine.Hardware.ARM.Callbacks
import SEL4.Machine.RegisterSet

irqSetAll = 0xffffffff
specialIRQStart = 1020 :: IRQ
irqNone = 1023 :: IRQ

data GicData = GicState { env :: Ptr CallbackData , 
    gicDistBase :: PAddr, 
    gicIFBase :: PAddr}

type GicMonad = StateT GicData IO

gicpoke :: PAddr -> Word -> GicMonad ()
gicpoke paddr value = do
    env <- gets env
    lift $ storeWordCallback env paddr value

gicpeek :: PAddr -> GicMonad Word
gicpeek paddr = do
    env <- gets env
    lift $ loadWordCallback env paddr

gicpeekOffset :: PAddr -> Int -> GicMonad Word
gicpeekOffset paddr idx = 
    gicpeek (paddr + (fromIntegral idx * 4))

gicpokeOffset :: PAddr -> Int -> Word -> GicMonad ()
gicpokeOffset paddr idx value = 
    gicpoke (paddr + (fromIntegral idx * 4)) value

gicpokeArray :: PAddr -> [Int] -> Word -> GicMonad ()
gicpokeArray paddr offsets value = (flip mapM_) offsets $
	(\offset -> gicpokeOffset paddr offset value)

newtype IRQ = IRQ Word32
    deriving (Num, Integral, Real, FiniteBits, Bits, Enum, Ord, Eq, Ix, Show)

instance Bounded IRQ where
    minBound = IRQ 0
    maxBound = IRQ 255

--paddrToPtr :: PAddr -> Ptr Word
--wordPtrToPtr . fromIntegral

--ptrToPAddr :: Ptr Word -> PAddr
--ptrToPAddr ptr = fromIntegral (ptrToWordPtr ptr)

replicateOffset n = [0 .. n-1] 	

distInit :: GicMonad ()
distInit = do
    gic_dist_base <- gets (gicDistBase)
    maxvec <- gicpeek (#{ptr gic_dist_map, ic_type} gic_dist_base)
    nirqs <- return $ fromIntegral $ (maxvec + 1) `shiftL` 5 
    lift $ putStrLn $ show nirqs
    gicpoke (#{ptr gic_dist_map, enable} gic_dist_base) 0
    gicpokeArray (#{ptr gic_dist_map, enable_clr} gic_dist_base)
       (replicateOffset $ nirqs `div` 32) irqSetAll
    gicpokeArray (#{ptr gic_dist_map, pending_clr} gic_dist_base)
       (replicateOffset $ nirqs `div` 32) irqSetAll
    gicpokeArray (#{ptr gic_dist_map, priority} gic_dist_base) 
       (replicateOffset $ nirqs `div` 32) 0
    gicpokeArray (#{ptr gic_dist_map, targets} gic_dist_base) 
       (replicateOffset $ nirqs `div` 4) 0x01010101
    gicpokeArray (#{ptr gic_dist_map, config} gic_dist_base)
       (replicateOffset $ nirqs `div` 32) 0x55555555
    gicpokeArray (#{ptr gic_dist_map, security} gic_dist_base)
       (replicateOffset $ nirqs `div` 32) 0
    gicpoke (#{ptr gic_dist_map, enable} gic_dist_base) 1
    gicpokeOffset (#{ptr gic_dist_map, enable_clr} gic_dist_base) 0 irqSetAll
    gicpokeOffset (#{ptr gic_dist_map, pending_clr} gic_dist_base) 0 irqSetAll
    gicpokeOffset (#{ptr gic_dist_map, priority} gic_dist_base) 0 0

    gicpokeArray (#{ptr gic_dist_map, sgi_pending_clr} gic_dist_base)
       (replicateOffset 4) irqSetAll

interfaceInit :: GicMonad ()
interfaceInit = do
    gic_interface_base <- gets (gicIFBase)
    gicpoke (#{ptr gic_cpu_iface_map, icontrol} gic_interface_base) 0x0 
    gicpoke (#{ptr gic_cpu_iface_map, pri_msk_c} gic_interface_base) 0xf0 
    gicpoke (#{ptr gic_cpu_iface_map, pb_c} gic_interface_base) 0x3
    gicpoke (#{ptr gic_cpu_iface_map, icontrol} gic_interface_base) 0x1


isIrqPending :: IRQ -> GicMonad Bool
isIrqPending irq = do
    gic_dist_base <- gets (gicDistBase)
    indicate <- gicpeekOffset (#{ptr gic_dist_map, pending_set} gic_dist_base) (fromIntegral idx)
    if ( indicate .&. bit == 0) then return True else return False
    where idx = (irq `shiftR` 5) 
          bit = 0x1 `shiftL` (fromIntegral $ irq .&. 0x1f);

isIrqActive :: IRQ -> GicMonad Bool
isIrqActive irq = do
    gic_dist_base <- gets (gicDistBase)
    indicate <- gicpeekOffset (#{ptr gic_dist_map, active} gic_dist_base) (fromIntegral idx)
    if ( indicate .&. bit == 0) then return True else return False
    where idx = (irq `shiftR` 5) 
          bit = 0x1 `shiftL` (fromIntegral $ irq .&. 0x1f);

isIrqEnabled :: IRQ -> GicMonad Bool
isIrqEnabled irq = do
    gic_dist_base <- gets (gicDistBase)
    indicate <- gicpeekOffset (#{ptr gic_dist_map, enable_set} gic_dist_base) (fromIntegral idx)
    if ( indicate .&. bit == 0) then return True else return False
    where idx = (irq `shiftR` 5) 
          bit = 0x1 `shiftL` (fromIntegral $ irq .&. 0x1f);

isIrqEdgeTriggered :: IRQ -> GicMonad Bool
isIrqEdgeTriggered irq = do
    gic_dist_base <- gets (gicDistBase)
    indicate <- gicpeekOffset (#{ptr gic_dist_map, config} gic_dist_base) (fromIntegral idx)
    if ( indicate .&. bit == 0) then return True else return False
    where idx = (irq `shiftR` 4) 
          bit = 0x1 `shiftL` (1 + (fromIntegral $ irq .&. 0xf));

distPendingClr :: IRQ -> GicMonad ()
distPendingClr irq = do
    gic_dist_base <- gets (gicDistBase)
    gicpokeOffset (#{ptr gic_dist_map, pending_clr} gic_dist_base) (fromIntegral idx) bit
    where idx = (irq `shiftR` 5) 
          bit = 0x1 `shiftL` (fromIntegral $ irq .&. 0x1f);

distPendingSet :: IRQ -> GicMonad ()
distPendingSet irq = do
    gic_dist_base <- gets (gicDistBase)
    gicpokeOffset (#{ptr gic_dist_map, pending_set} gic_dist_base) (fromIntegral idx) bit
    where idx = (irq `shiftR` 5) 
          bit = 0x1 `shiftL` (fromIntegral $ irq .&. 0x1f);

distEnableClr :: IRQ -> GicMonad ()
distEnableClr irq = do
    gic_dist_base <- gets (gicDistBase)
    gicpokeOffset (#{ptr gic_dist_map, enable_clr} gic_dist_base) (fromIntegral idx) bit
    where idx = (irq `shiftR` 5) 
          bit = 0x1 `shiftL` (fromIntegral $ irq .&. 0x1f);

distEnableSet :: IRQ -> GicMonad ()
distEnableSet irq = do
    gic_dist_base <- gets (gicDistBase)
    gicpokeOffset (#{ptr gic_dist_map, enable_set} gic_dist_base) (fromIntegral idx) bit
    where idx = (irq `shiftR` 5) 
          bit = 0x1 `shiftL` (fromIntegral $ irq .&. 0x1f);

initIRQController = do
    distInit
    interfaceInit

getActiveIRQ :: GicMonad (Maybe IRQ)
getActiveIRQ = do
    gic_cpu_base <- gets (gicIFBase)
    irq <- gicpeek $ (#{ptr gic_cpu_iface_map, int_ack} gic_cpu_base)
    return $ Just $ fromIntegral irq

maskInterrupt :: Bool -> IRQ -> GicMonad ()
maskInterrupt disable irq = if disable then (distEnableClr irq) else (distEnableSet irq)

ackInterrupt :: IRQ -> GicMonad ()
ackInterrupt irq = do
    gic_cpu_base <- gets (gicIFBase)
    gicpoke (#{ptr gic_cpu_iface_map, eoi} gic_cpu_base) (fromIntegral irq)

callGICApi :: GicData -> GicMonad a -> IO a
callGICApi gic oper = do
    r <- runStateT oper gic
    return $ fst r
