{-# LANGUAGE CPP, ForeignFunctionInterface #-}
{-# LANGUAGE CPP, GeneralizedNewtypeDeriving #-}
module SEL4.Machine.Hardware.MCTInterface where

import Foreign.Ptr (Ptr)
import Data.Bits

#include "mct.h"

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Maybe
import SEL4.Machine.Hardware.ARM.Callbacks
import SEL4.Machine.RegisterSet

data MCTData = MCTState {
    env :: Ptr CallbackData, 
    mctBase :: PAddr }

type MCTMonad = StateT MCTData IO

mctpoke :: PAddr -> Word -> MCTMonad ()
mctpoke paddr value = do
    env <- gets env
    lift $ storeWordCallback env paddr value

mctpeek :: PAddr -> MCTMonad Word
mctpeek paddr = do
    env <- gets env
    lift $ loadWordCallback env paddr


enGTC = 1 `shiftL` 8
enGTCComp0 = 1
inGTCComp0 = 1
autoincGTCComp0 = 2
tconGWSTAT = 1 `shiftL` 16
comp0IRQ = 1

-- configure timer
timerFreq = 24 * 2 * 1000


mctInit :: MCTMonad ()
mctInit = do
    mct_base <- gets (mctBase)
    wstat <- mctpeek (#{ptr mct_global_map, wstat} mct_base)
    mctpoke (#{ptr mct_global_map, wstat} mct_base) wstat

--    cnt_wstat <- mctpeek (#{ptr mct_global_map, cnt_wstat} mct_base)
--    mctpoke (#{ptr mct_global_map, wstat} mct_base) cnt_wstat

    mctpoke (#{ptr mct_global_map, comp0_add_inc} mct_base) timerFreq

    cnth <- mctpeek (#{ptr mct_global_map, cnth} mct_base)
    mctpoke (#{ptr mct_global_map, comp0h} mct_base) cnth
    mctpoke (#{ptr mct_global_map, comp0l} mct_base) (cnth + timerFreq)


    mctpoke (#{ptr mct_global_map, int_en} mct_base) comp0IRQ 

    mctpoke (#{ptr mct_global_map, tcon} mct_base) (enGTC .|. enGTCComp0 .|. autoincGTCComp0)

    wstat_error <- runMaybeT (
      do
        wstat <- lift $ mctpeek (#{ptr mct_global_map, wstat} mct_base)
        if (wstat == tconGWSTAT) then (return Nothing) else (return $ Just wstat)
      )
    case wstat_error of 
      Nothing -> resetTimer
      Just _ -> return ()

resetTimer :: MCTMonad ()
resetTimer = do
    mct_base <- gets mctBase
    mctpoke (#{ptr mct_global_map, int_stat} mct_base) inGTCComp0 
      

callMCTApi :: MCTData -> MCTMonad a -> IO a
callMCTApi mct oper = do
    r <- runStateT oper mct
    return $ fst r
