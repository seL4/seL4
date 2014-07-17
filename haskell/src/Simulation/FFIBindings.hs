--
-- Copyright 2014, General Dynamics C4 Systems
--
-- This software may be distributed and modified according to the terms of
-- the GNU General Public License version 2. Note that NO WARRANTY is provided.
-- See "LICENSE_GPLv2.txt" for details.
--
-- @TAG(GD_GPL)
--

{--
    Module FFIBindings provides C language bindings to the Haskell
    executable model of seL4.  Declarations and prototypes are generated in
    FFIBindings_stub.h.

    All routines take the following:
        int *breakFlag -
            If SIGINT (Ctrl-C) occurs during seL4 execution then on return,
            *breakFlag is true.
        bool *errorFlag, char **errorString - (except seL4NewState)
            If seL4 terminates with a call to 'error' then on return,
            *errorFlag is true, and *errorString points to the result of (Show
            e) i.e. the error string.

    Notioal prototypes:
        HsStablePtr seL4NewState(int *breakFlag, void *cbData,
                                 uint32_t dataStart, int *nBootFramesP,
                                 uint32_t **bootFramesPP);

        HsStablePtr seL4Bootstrap(int *breakFlag, int *errorFlag,
                                  char **errorString,
                                  uint32_t start, uint32_t vpOffset,
                                  int *nUserFrames, uint32_t *userFramesArray,
                                  int *nKernelFrames,
                                  uint32_t *kernelFramesArray,
                                  int *nBootFrames, uint32_t *bootFramesArray,
                                  HsStablePtr st);

        void        seL4Shutdown(int *breakFlag, HsStablePtr st);

        HsStablePtr seL4Syscall(int *breakFlag, int *errorFlag,
                                char **errorString, int number,
                                HsStablePtr st);

        HsStablePtr seL4Interrupt(int *breakFlag, int *errorFlag,
                                  char **errorString, HsStablePtr st);

        HsStablePtr seL4Fault(int *breakFlag, int *errorFlag, char **errorString,
                              int isPrefetch, HsStablePtr st);

        HsStablePtr seL4UserFault(int *breakFlag, int *errorFlag,
                                  char **errorString, uint32_t word1,
                                  uint32_t word2, HsStablePtr st);

        HsStablePtr seL4SaveContext(int *breakFlag, int *errorFlag,
                                    char **errorString, uint32_t *regptr,
                                    uint32_t *cpsrptr, uint32_t lrptr,
                                    uint32_t *faultptr, HsStablePtr st);

        HsStablePtr seL4RestoreContext(int *breakFlag, int *errorFlag,
                                       char **errorString, uint32_t *regptr,
                                       uint32_t *cpsrptr, uint32_t *lrptr,
                                       HsStablePtr st);
--}

{-# LANGUAGE ForeignFunctionInterface #-}
module Simulation.FFIBindings where

import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.Ptr
import Foreign.StablePtr
import Foreign.C.String
import Foreign.C.Types
import Control.Monad.State
import Control.Monad.Reader
import Control.Exception
import System.Posix.Signals

import SEL4

import qualified SEL4.Machine.Target as R
import qualified SEL4.Machine.Target as H

type StatePtr = StablePtr (KernelState, H.MachineData)

---- seL4NewState ----
-- Generates a new, uninitialized seL4 state.  This corresponds to system
-- state after the seL4 bootloader executes.  Allocates and returns (through
-- bootFramesPP) an array of frames used by the bootloader.  This array can
-- be passed to seL4Bootstrap, but must be freed by the caller.
-- Allocates and returns a pointer to a 'stable pointer'.  This stable pointer
-- will be updated by further calls, and deallocated by seL4Shutdown.
-- Returns NULL if allocation fails.
foreign export ccall seL4NewState :: Ptr CInt ->
                                     H.MachineData -> Word ->
                                     Ptr CInt -> Ptr (Ptr Word) ->
                                     IO (Ptr StatePtr)
seL4NewState breakFlag
             cbData dataStart
             nBootFramesP  bootFramesPP = do
    setBreak breakFlag

    let (st, bootFrames) = newKernelState (fromIntegral dataStart)

    bootFramesP <- newArray (map fromIntegral bootFrames)
    poke bootFramesPP bootFramesP
    poke nBootFramesP $ fromIntegral (length bootFrames)

    -- Returns (Ptr StatePtr)
    sptr <- newStablePtr (st, cbData)
    ptr <- malloc
    when (ptr /= nullPtr) $ poke ptr sptr
    return ptr

---- seL4Bootstrap ----
-- Bootstrap, or initialize the kernel.  This corresponds to calling
-- initKernel.
foreign export ccall seL4Bootstrap :: Ptr CInt -> Ptr CInt -> Ptr CString ->
                                      Word -> Word ->
                                      CInt -> Ptr Word ->
                                      CInt -> Ptr Word ->
                                      CInt -> Ptr Word ->
                                      Ptr StatePtr -> IO ()
seL4Bootstrap breakFlag errorFlag errorString
              start vpOffset
              nUserFrames   userFramesArray
              nKernelFrames kernelFramesArray
              nBootFrames   bootFramesArray
              st = do
    setBreak breakFlag

    userFrames   <- liftM (map fromIntegral) $
        peekArray (fromIntegral nUserFrames) userFramesArray
    kernelFrames <- liftM (map fromIntegral) $
        peekArray (fromIntegral nKernelFrames) kernelFramesArray
    bootFrames   <- liftM (map fromIntegral) $
        peekArray (fromIntegral nBootFrames) bootFramesArray

    runCatch (f userFrames kernelFrames bootFrames) errorFlag errorString
             undefined

    where
        f uf kf bf = runVoid (initKernel (fromIntegral start)
                                         uf (fromIntegral vpOffset) kf bf) st 
---- seL4Shutdown ----
-- Destroy an seL4 instance.
foreign export ccall seL4Shutdown :: Ptr CInt -> Ptr StatePtr -> IO ()
seL4Shutdown breakFlag st = do
    setBreak breakFlag
    sptr <- peek st
    freeStablePtr sptr
    free st

------ Kernel entry points ------
-- Any of these may call back to the simulator to modify memory state.

---- seL4Syscall ----
-- Process an seL4 system call.
foreign export ccall seL4Syscall :: Ptr CInt -> Ptr CInt -> Ptr CString ->
                                    Int ->
                                    Ptr StatePtr -> IO ()
seL4Syscall breakFlag errorFlag errorString
            number
            st = do
    setBreak breakFlag

    runCatch f errorFlag errorString undefined
    where
        f = runVoid (callKernel ev) st
        ev = if number > fromEnum (maxBound :: Syscall)
               then UnknownSyscall number
               else SyscallEvent (toEnum number)

---- seL4Interrupt ----
-- Handle an interrupt.
foreign export ccall seL4Interrupt :: Ptr CInt -> Ptr CInt -> Ptr CString ->
                                      Ptr StatePtr -> IO ()
seL4Interrupt breakFlag errorFlag errorString st = do
    setBreak breakFlag
    runCatch f errorFlag errorString undefined
    where
        f = runVoid (callKernel Interrupt) st

---- seL4Fault ----
-- Handle a VM fault.
foreign export ccall seL4Fault :: Ptr CInt -> Ptr CInt -> Ptr CString ->
                                  Int ->
                                  Ptr StatePtr -> IO ()
seL4Fault breakFlag errorFlag errorString
          isPrefetch
          st = do
    setBreak breakFlag
    runCatch f errorFlag errorString undefined
    where
        f = runVoid action st
        action = callKernel $ VMFaultEvent $
            if isPrefetch == 1 then H.ARMPrefetchAbort
                               else H.ARMDataAbort

---- seL4UserFault ----
-- Handle a user fault e.g. undefined instruction
foreign export ccall seL4UserFault :: Ptr CInt -> Ptr CInt -> Ptr CString ->
                                      Word -> Word ->
                                      Ptr StatePtr -> IO ()
seL4UserFault breakFlag errorFlag errorString
              word1 word2
              st = do
    setBreak breakFlag
    runCatch f errorFlag errorString undefined
    where
        f = runVoid
                (callKernel $ UserLevelFault word1 word2)
                st

---- seL4SaveContext ----
-- Save the CPU execution context to the current thread's TCB.  This mimics
-- the assembly code in the trap handler.
foreign export ccall seL4SaveContext ::
    Ptr CInt -> Ptr CInt -> Ptr CString ->
    Ptr Word -> Ptr Word -> Ptr Word -> Ptr Word ->
    Ptr StatePtr -> IO ()
seL4SaveContext breakFlag errorFlag errorString
                regptr cpsrptr lrptr faultptr
                st = do
    setBreak breakFlag
    runCatch f errorFlag errorString undefined
    where
        f = do
            regs  <- mapM (peekElemOff regptr) [0 .. length [R.R0 .. R.LR] - 1]
            cpsr  <- peek cpsrptr
            lr    <- peek lrptr
            fault <- peek faultptr
            (`runVoid` st) $ do
                thread <- getCurThread
                asUser thread $ do
                    zipWithM_ setRegister [Register R.R0 .. Register R.LR] regs
                    setRegister (Register R.CPSR) cpsr
                    setRegister (Register R.LR_svc) lr
                    setRegister (Register R.FaultInstruction) fault

---- seL4RestoreContext ----
-- Restore the CPU exception context from the current thread's TCB.  This
-- mimics the kernel's (assembly) return-to-user code.
foreign export ccall seL4RestoreContext ::
    Ptr CInt -> Ptr CInt -> Ptr CString ->
    Ptr Word -> Ptr Word -> Ptr Word ->
    StatePtr -> IO ()
seL4RestoreContext breakFlag errorFlag errorString
                   regptr cpsrptr lrptr
                   st = do
    setBreak breakFlag
    runCatch f errorFlag errorString undefined
    where
        f = do
            (regs, cpsr, lr) <- (`runPure` st) $ do
                thread <- getCurThread
                regs  <- asUser thread $
                    mapM getRegister [Register R.R0 .. Register R.LR]
                cpsr  <- asUser thread $ getRegister $ Register R.CPSR
                lr    <- asUser thread $ getRegister $ Register R.LR_svc
                return (regs, cpsr, lr)
            zipWithM_ (pokeElemOff regptr) [0,1..] regs
            poke cpsrptr cpsr
            poke lrptr lr

---- seL4Inspect ----
-- XXX - Deprecated
--foreign export ccall seL4Inspect :: Ptr CInt -> CString -> Ptr CString ->
                                    --Ptr CInt -> Ptr CString ->
                                    --StatePtr -> IO ()
--seL4Inspect breakFlag cmdline strptr errorFlag errorString st = do 
    --setBreak breakFlag
    --_cmdline <- peekCString cmdline
    --response <- runCatch (f _cmdline) errorFlag errorString ""
    --responseStr <- newCString response
    --poke strptr responseStr
    --where
        --f _cmdline = runStateFromC (inspectCommand _cmdline) st

-- Execute the given statement, flagging any calls to 'error' using errorFlag
-- and errorString
runCatch :: IO a -> Ptr CInt -> Ptr CString -> a -> IO a
runCatch f errorFlag errorString errorValue = do
    poke errorFlag 0
    a <- Control.Exception.catch f errorHandler
    return a
    where
        errorHandler e = do
            poke errorFlag 1
            errorString' <- newCString (show (e::ErrorCall))
            poke errorString errorString'
            return errorValue

-- Execute a statement 'f', from the Kernel monad, given a pointer to an
-- seL4 state.  Updates the given state as appropriate.
runVoid :: Kernel () -> Ptr StatePtr -> IO ()
runVoid f ptr = do
    sptr <- peek ptr
    (_, sptr') <- runStateT (runStateFromC f) sptr
    poke ptr sptr'

-- Executes a pure function of the kernel state.  Returns the result.
runPure :: Kernel a -> StatePtr -> IO a
runPure f sptr = do
    (a, _) <- runStateT (runStateFromC f) sptr
    return a

runStateFromC :: Kernel a -> StateT StatePtr IO a
runStateFromC f = do
    sptr <- get
    (a, sptr') <- lift $ do
        (st, md) <- deRefStablePtr sptr
        freeStablePtr sptr
        (a, st') <- f `runStateT` st `runReaderT` md
        sptr' <- newStablePtr (st', md)
        return (a, sptr')
    put sptr'
    return $! a

-- Flag any SIGINT that we receive.
handleBreak :: Ptr CInt -> IO ()
handleBreak breakFlag = do
    poke breakFlag $ fromIntegral (1::Integer)

-- Set the SIGINT handler to 'handleBreak'.
setBreak :: Ptr CInt -> IO System.Posix.Signals.Handler
setBreak breakFlag =
    installHandler sigINT (Catch $ handleBreak breakFlag) Nothing
