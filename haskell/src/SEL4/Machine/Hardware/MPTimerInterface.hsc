{-# LANGUAGE CPP, ForeignFunctionInterface #-}
{-# LANGUAGE CPP, GeneralizedNewtypeDeriving #-}
module SEL4.Machine.Hardware.MPTimerInterface where

import Foreign.Ptr (Ptr)
import Data.Bits

#include "mptimer.h"

-- helpers
import Control.Monad.Reader
import Control.Monad.State
import SEL4.Machine.Hardware.ARM.Callbacks
import SEL4.Machine.RegisterSet

data MPTimerData = MPTimerState { env :: Ptr CallbackData , 
    mptBase :: PAddr }

type MPTimerMonad = StateT MPTimerData IO

mptpoke :: PAddr -> Word -> MPTimerMonad ()
mptpoke paddr value = do
    env <- gets env
    lift $ storeWordCallback env paddr value

mptpeek :: PAddr -> MPTimerMonad Word
mptpeek paddr = do
    env <- gets env
    lift $ loadWordCallback env paddr


-- configure timer
timerFreq = 400 * 1000000 * 2

timerCtrlEnable = 0x1
timerCtrlAutoReload = 0x2
timerCtrlIRQEnable = 0x4
timerCtrlPrescale = 8
timerIntsEvent = 0x1

timerCountBits = 32


mpTimerInit :: MPTimerMonad ()
mpTimerInit = do
    mpt_base <- gets (mptBase)
    mptpoke (#{ptr priv_timer, ctrl} mpt_base) 0
    mptpoke (#{ptr priv_timer, ints} mpt_base) 0

-- setup 
    mptpoke (#{ptr priv_timer, load} mpt_base) (timerFreq `div` (prescale + 1))
    mptpoke (#{ptr priv_timer, ctrl} mpt_base) ctrl1

-- enable
    mptpoke (#{ptr priv_timer, ctrl} mpt_base) (ctrl1 .|. timerCtrlEnable)
    where ctrl1 = ((prescale `shiftL` timerCtrlPrescale) .|. timerCtrlAutoReload .|. timerCtrlIRQEnable)
          prescale = timerFreq `shiftR` timerCountBits


resetTimer :: MPTimerMonad ()
resetTimer = do
    mpt_base <- gets (mptBase)
    mptpoke (#{ptr priv_timer, ints} mpt_base) timerIntsEvent

callMPTimerApi :: MPTimerData -> MPTimerMonad a -> IO a
callMPTimerApi mct oper = do
    r <- runStateT oper mct
    return $ fst r
