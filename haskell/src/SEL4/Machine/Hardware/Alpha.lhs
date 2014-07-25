%
% Copyright 2014, General Dynamics C4 Systems
%
% This software may be distributed and modified according to the terms of
% the GNU General Public License version 2. Note that NO WARRANTY is provided.
% See "LICENSE_GPLv2.txt" for details.
%
% @TAG(GD_GPL)
%

> module SEL4.Machine.Arch.Alpha (
>         Register(..), Word, MachineMonad, MachineData, CallbackData,
>         initMachine
>     ) where

> import SEL4.Machine
> import SEL4.API
> import SEL4.Model
> import SEL4.Object
> import SEL4.Kernel

> import qualified Data.Word
> import Data.Bits
> import Data.Ix
> import Data.Typeable
> import qualified Foreign.Storable
> import Foreign.Ptr
> import Control.Monad.Reader

> data Register = PC | NPC |
>     V0 | T0 | T1 | T2 | T3 | T4 | T5 | T6 | T7 | S0 | S1 | S2 | S3 | S4 |
>     S5 | FP | A0 | A1 | A2 | A3 | A4 | A5 | T8 | T9 | T10 | T11 | RA | T12 |
>     AT | GP | SP
>     deriving (Eq, Enum, Bounded, Ord, Ix, Typeable)

> instance RegisterName Register

> newtype Word = Word Data.Word.Word64
>     deriving (Show,Eq,Ord,Num,Enum,Real,Integral,Bits,Typeable,MachineWord,
>         Foreign.Storable.Storable)

> type MachineMonad = ReaderT MachineData IO

> initMachine :: PPtr Word -> [(PPtr Word, Int)] -> Ptr CallbackData ->
>         MachineData
> initMachine = MachineData

> data CallbackData

> data MachineData = MachineData {
>     sdMemoryTop :: PPtr Word,
>     sdDeviceRegions :: [(PPtr Word, PPtr Word)],
>     sdCallbackPtr :: Ptr CallbackData }

> instance Hardware Register Word MachineMonad where
>     getMemoryTop = asks sdMemoryTop
>     getDeviceRegions = asks sdDeviceRegions

>     loadWord ptr = do
>         cbptr <- asks sdCallbackPtr
>         lift $ loadWordCallback cbptr ptr
>     storeWord ptr val = do
>         cbptr <- asks sdCallbackPtr
>         lift $ storeWordCallback cbptr ptr val
>     storeWordVM ptr val = storeWord ptr val
>     tlbInsertEntry pptr vptr isWrite = do
>         cbptr <- asks sdCallbackPtr
>         lift $ tlbInsertCallback cbptr pptr vptr isWrite
>     tlbFlushAll = do
>         cbptr <- asks sdCallbackPtr
>         lift $ tlbFlushCallback cbptr

>     debugPrint str = lift $ putStrLn str

> foreign import ccall "alpha_load_word" loadWordCallback ::
>         Ptr CallbackData -> PPtr Word -> IO Word

> foreign import ccall "alpha_store_word" storeWordCallback ::
>         Ptr CallbackData -> PPtr Word -> Word -> IO ()

> foreign import ccall "alpha_tlb_insert" tlbInsertCallback ::
>         Ptr CallbackData -> PFN Word -> VPN Word -> Bool -> IO ()

> foreign import ccall "alpha_tlb_flush" tlbFlushCallback ::
>         Ptr CallbackData -> IO ()

> instance Machine Register Word APIObjectType MachineMonad where
>     retypeRegion = retypeRegionAPI
>     detypeObject = detypeObjectAPI
>     invokeMachineObject = error "No machine-specific object types defined"
>     lookupVPage = lookupVPageWithCNodes
>     isValidVTableRoot (CNodeCap {}) = True
>     isValidVTableRoot _ = False

> instance Storable (UserData Word) Register Word APIObjectType
>   where
>     objBits _ = 13
>     makeObject = UserData

> instance RegisterSet Register Word where
>     msgInfoRegister = A1
>     msgRegisters = [A2 .. T8]
>     capRegister = A0
>     badgeRegister = A0
>     frameRegisters = PC : NPC : SP : V0 : [A0 .. T8]
>     gpRegisters = [T0 .. FP] ++ [T9 .. GP]
>     getFaultingPC = getRegister PC
>     setNextPC addr = do
>         setRegister PC addr
>         setRegister NPC $ addr + 4


