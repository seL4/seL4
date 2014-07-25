%
% Copyright 2014, General Dynamics C4 Systems
%
% This software may be distributed and modified according to the terms of
% the GNU General Public License version 2. Note that NO WARRANTY is provided.
% See "LICENSE_GPLv2.txt" for details.
%
% @TAG(GD_GPL)
%

> module HaskellCPU where

> import SEL4.Kernel
> import SEL4.Object
> import SEL4.Machine
> import SEL4.Model
> import SEL4.API
> import qualified SEL4.Machine.Hardware.HaskellCPU as H
> import qualified SEL4.Machine.RegisterSet.HaskellCPU as R

> import Control.Monad.State
> import Data.Map as Map
> import Data.Bits

> data Instruction
>     = Arithmetic Register (Word -> Word -> Word) Register Register
>     | ArithmeticI Register (Word -> Word) Register
>     | Compare Register (Word -> Word -> Bool) Register Register
>     | CompareI Register (Word -> Bool) Register
>     | LoadImmediate Word Register
>     | Load Word Register Register
>     | Store Register Word Register
>     | Push Register
>     | Pop Register
>     | Move Register Register
>     | Branch Word
>     | BranchIf Register Word
>     | Jump Register
>     | JumpLinked Register Register
>     | Syscall Int
>     | DebugPrintf String [Register]
>     | IllegalInstruction
>     | Halt

> type UserText = Map.Map PPtr Instruction

> intSize :: Word
> intSize = fromIntegral $ bitSize (undefined::Word) `div` 8

> onMachine :: State H.MachineState () -> IO ()
> onMachine program = do
>     let finalState = program `execState` newMachineState
>     mapM_ putStrLn $ H.msConsoleOutput finalState
>     where newMachineState = H.MS Map.empty Map.empty []

> mainLoop :: UserText -> KernelMonad ()
> mainLoop text = do
>         ev <- executeInstructions text 10000
>         case ev of
>             Just event -> do
>                 callKernel event
>                 mainLoop text
>             Nothing -> return ()

> executeInstructions :: UserText -> Int ->
>         KernelMonad (Maybe Event)

> executeInstructions _ 0 = return $ Just TimerInterrupt

> executeInstructions text time = do
>         thread <- getCurThread
>         pc <- asUser thread $ getRegister $ Register R.PC
>         ppc <- tlbLookup (VPtr pc) False
>         case ppc of
>             Just addr -> do
>                 asUser thread $ setRegister (Register R.PC) (pc+4)
>                 ev <- executeInstruction thread $
>                     Map.findWithDefault IllegalInstruction addr text
>                 case ev of
>                     Just ev -> return ev
>                     Nothing -> executeInstructions text (time-1)
>             _ -> return $ Just $ VMFault (VPtr pc) False

> executeInstruction :: ThreadPtr -> Instruction ->
>         KernelMonad (Maybe (Maybe Event))

> executeInstruction tp (Arithmetic ra f rb rd) = asUser tp $ do
>         a <- getRegister ra
>         b <- getRegister rb
>         setRegister rd $ f a b
>         return Nothing

> executeInstruction tp (ArithmeticI ra f rd) = asUser tp $ do
>         a <- getRegister ra
>         setRegister rd $ f a
>         return Nothing

> executeInstruction tp (Compare ra f rb rd) = 
>         executeInstruction tp $
>             Arithmetic ra (\a b -> fromIntegral $ fromEnum $ f a b) rb rd

> executeInstruction tp (CompareI ra f rd) =
>         executeInstruction tp $
>             ArithmeticI ra (fromIntegral . fromEnum . f) rd

> executeInstruction tp (LoadImmediate i rd) =
>         asUser tp $ setRegister rd i >> return Nothing

> executeInstruction tp (Load i ra rb) = do
>         a <- asUser tp $ getRegister ra
>         doLoad tp (VPtr $ i+a) rb

> executeInstruction tp (Store ra i rb) = do
>         b <- asUser tp $ getRegister rb
>         doStore tp ra (VPtr $ i+b)

> executeInstruction tp (Push ra) = do
>         sp <- asUser tp $ getRegister $ Register R.SP
>         asUser tp $ setRegister (Register R.SP) $ sp-intSize
>         doStore tp ra (VPtr $ sp-intSize)

> executeInstruction tp (Pop ra) = do
>         sp <- asUser tp $ getRegister $ Register R.SP
>         asUser tp $ setRegister (Register R.SP) $ sp+intSize
>         doLoad tp (VPtr sp) ra

> executeInstruction tp (Move ra rb) = asUser tp $ do
>         a <- getRegister ra
>         setRegister rb a
>         return Nothing

> executeInstruction tp (Branch a) = asUser tp $ do
>         pc <- getRegister $ Register R.PC
>         setRegister (Register R.PC) $ pc+(a*intSize)-intSize
>         return Nothing

> executeInstruction tp (BranchIf ra b) = do
>         a <- asUser tp $ getRegister ra
>         if a/=0
>             then executeInstruction tp $ Branch b
>             else return Nothing

> executeInstruction tp (Jump ra) = asUser tp $ do
>         newpc <- getRegister ra
>         setRegister (Register R.PC) $ newpc-intSize
>         return Nothing

> executeInstruction tp (JumpLinked ra rl) = do
>         asUser tp $ do
>             pc <- getRegister (Register R.PC)
>             setRegister rl (pc+intSize)
>         executeInstruction tp $ Jump ra

> executeInstruction _ (Syscall n)
>     | n>=0 && n <= (fromIntegral $ fromEnum (maxBound :: Syscall)) =
>         return $ Just $ Just $ SyscallEvent $ toEnum $ fromIntegral n
>     | otherwise = return $ Just $ Just $ UnknownSyscall $ fromIntegral n

> executeInstruction tp (DebugPrintf msg regs) = do
>         values <- asUser tp $ mapM getRegister regs
>         doMachineOp $ debugPrint $ formatError msg values
>         return Nothing

> executeInstruction _ IllegalInstruction =
>         return $ Just $ Just $ UserLevelFault 0

> executeInstruction _ Halt =
>         return $ Just $ Nothing

> doLoad :: ThreadPtr -> VPtr -> Register ->
>         KernelMonad (Maybe (Maybe Event))
> doLoad tp ptr reg = do
>         pptr <- tlbLookup ptr False
>         case pptr of
>             Just addr -> do
>                 val <- doMachineOp $ loadWord addr
>                 asUser tp $ setRegister reg val
>                 return Nothing
>             _ -> return $ Just $ Just $ VMFault ptr False

> doStore :: ThreadPtr -> Register -> VPtr ->
>         KernelMonad (Maybe (Maybe Event))
> doStore tp reg ptr = do
>         pptr <- tlbLookup ptr True
>         case pptr of
>             Just addr -> do
>                 val <- asUser tp $ getRegister reg
>                 doMachineOp $ storeWord addr val
>                 return Nothing
>             _ -> return $ Just $ Just $ VMFault ptr True

> tlbLookup :: VPtr -> Bool -> KernelMonad (Maybe PPtr)
> tlbLookup vaddr write = do
>         tlb <- doMachineOp $ gets H.msTLB
>         let vpn = vPtrToVPN vaddr
>         let entry = Map.lookup vpn tlb
>         return $! case entry of
>             Just (pfn, writable) -> if writable || not write
>                 then Just $ findPPtr vaddr pfn
>                 else Nothing
>             Nothing -> Nothing

> formatError :: String -> [Word] -> String
> formatError "" _ = ""
> formatError ('%':chars) (i:values) = (show i) ++ (formatError chars values)
> formatError (x:chars) values = x : (formatError chars values)


