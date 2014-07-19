%
% Copyright 2014, General Dynamics C4 Systems
%
% This software may be distributed and modified according to the terms of
% the GNU General Public License version 2. Note that NO WARRANTY is provided.
% See "LICENSE_GPLv2.txt" for details.
%
% @TAG(GD_GPL)
%

> module SEL4.Machine.Hardware.HaskellCPU where

> import SEL4.Machine.RegisterSet
> import qualified SEL4.Machine.RegisterSet.HaskellCPU as HaskellCPU

> import Data.Bits
> import qualified Data.Map as Map
> import Control.Monad.State
> import Control.Monad.Error

> type MachineMonad = State MachineState

> data MachineState = MS {
>     msMemory :: Map.Map PPtr Word,
>     msTLB :: Map.Map VPN (PFN, Bool),
>     msConsoleOutput :: [String] }

> pageBits :: Int
> pageBits = 12

> intSize :: Word
> intSize = fromIntegral $ bitSize (undefined::Word) `div` 8

> getMemoryTop :: MachineMonad PPtr
> getMemoryTop = return maxBound

> getDeviceRegions :: MachineMonad [(PPtr, PPtr)]
> getDeviceRegions = return []

> loadWord :: PPtr -> MachineMonad Word
> loadWord address = do
>     when (address .&. PPtr (intSize-1) /= 0) $
>         fail "unaligned access"
>     gets $ Map.findWithDefault 0 address . msMemory

> storeWord :: PPtr -> Word -> MachineMonad ()
> storeWord address value = do
>     when (address .&. PPtr (intSize-1) /= 0) $
>         fail "unaligned access"
>     modify (\ms -> ms {
>         msMemory = Map.insert address value $ msMemory ms })

> storeWordVM :: PPtr -> Word -> MachineMonad ()
> storeWordVM address value = return

> debugPrint :: String -> MachineMonad ()
> debugPrint str = modify (\ms -> ms {
>     msConsoleOutput = msConsoleOutput ms ++ [str] })

> tlbInsertEntry :: PFN -> VPN -> Bool -> MachineMonad ()
> tlbInsertEntry pptr vptr writable = modify (\ms -> ms {
>     msTLB = Map.insert vptr (pptr, writable) $ msTLB ms })

> tlbFlushAll :: MachineMonad ()
> tlbFlushAll = modify (\ms -> ms { msTLB = Map.empty })

> getFaultingPC = getRegister (Register HaskellCPU.PC)
> setNextPC = setRegister (Register HaskellCPU.PC)


