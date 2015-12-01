{-# LINE 1 "MCTInterface.hsc" #-}
{-# LANGUAGE CPP, ForeignFunctionInterface #-}
{-# LINE 2 "MCTInterface.hsc" #-}
{-# LANGUAGE CPP, GeneralizedNewtypeDeriving #-}
module SEL4.Machine.Hardware.MCTInterface where

import Foreign.Ptr (Ptr)
import Data.Bits


{-# LINE 9 "MCTInterface.hsc" #-}

-- helpers
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Maybe
import SEL4.Machine.Hardware.ARM.Callbacks
import SEL4.Machine.RegisterSet

data MCTData = MCTState { env :: Ptr CallbackData , 
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


enGTCON = 1 `shiftL` 8
tconGWSTAT = 1 `shiftL` 16

mctInit :: MCTMonad ()
mctInit = do
    mct_base <- gets (mctBase)
    wstat <- mctpeek ((\hsc_ptr -> hsc_ptr `plusPtr` 588) mct_base)
{-# LINE 40 "MCTInterface.hsc" #-}
    mctpoke ((\hsc_ptr -> hsc_ptr `plusPtr` 588) mct_base) wstat
{-# LINE 41 "MCTInterface.hsc" #-}

    cnt_wstat <- mctpeek ((\hsc_ptr -> hsc_ptr `plusPtr` 268) mct_base)
{-# LINE 43 "MCTInterface.hsc" #-}
    mctpoke ((\hsc_ptr -> hsc_ptr `plusPtr` 588) mct_base) cnt_wstat
{-# LINE 44 "MCTInterface.hsc" #-}

    mctpoke ((\hsc_ptr -> hsc_ptr `plusPtr` 576) mct_base) enGTCON
{-# LINE 46 "MCTInterface.hsc" #-}


    wstat_error <- runMaybeT (
      do
        wstat <- lift $ mctpeek ((\hsc_ptr -> hsc_ptr `plusPtr` 588) mct_base)
{-# LINE 51 "MCTInterface.hsc" #-}
        if (wstat == tconGWSTAT) then (return Nothing) else (return $ Just wstat)
      )
    case wstat_error of 
      Nothing -> resetTimer
      Just _ -> error "failed to init mct ..."

resetTimer :: MCTMonad ()
resetTimer = return ()
      

callMCTApi :: MCTData -> MCTMonad a -> IO a
callMCTApi mct oper = do
    r <- runStateT oper mct
    return $ fst r
