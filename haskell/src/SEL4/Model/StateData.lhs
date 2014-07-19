%
% Copyright 2014, General Dynamics C4 Systems
%
% This software may be distributed and modified according to the terms of
% the GNU General Public License version 2. Note that NO WARRANTY is provided.
% See "LICENSE_GPLv2.txt" for details.
%
% @TAG(GD_GPL)
%

This module defines the high-level structures used to represent the
state of the entire system, and the types and functions used to
perform basic operations on the state.

\begin{impdetails}

We use the C preprocessor to select a target architecture.

> {-# LANGUAGE CPP #-}

\end{impdetails}

> module SEL4.Model.StateData (
>         module SEL4.Model.StateData,
>         module Control.Monad, get, gets, put, modify,
>     ) where

The architecture-specific definitions are imported qualified with the "Arch" prefix.

> import qualified SEL4.Model.StateData.TARGET as Arch

\begin{impdetails}

> import SEL4.Config (numDomains, numPriorities)
> import SEL4.API.Types
> import {-# SOURCE #-} SEL4.Model.PSpace
> import SEL4.Object.Structures
> import SEL4.Machine
> import SEL4.Machine.Hardware.TARGET (VMPageSize)

> import Data.Array
> import Data.Helpers
> import Control.Monad
> import Control.Monad.State

\end{impdetails}

\subsection{Types}

\subsubsection{Kernel State}

The top-level kernel state structure is called "KernelState". It contains:

> data KernelState = KState {

\begin{itemize}
\item the physical address space, of type "PSpace" (defined in \autoref{sec:model.pspace});

>         ksPSpace :: PSpace,

\item ghost state, i.e. meta-information about the kernel objects living in "PSpace";

>         gsUserPages :: Word -> (Maybe VMPageSize),
>         gsCNodes :: Word -> (Maybe Int),

\item the cyclic domain schedule;

>         ksDomScheduleIdx :: Int,
>         ksDomSchedule :: [DomainSchedule],

\item the active security domain and the number to ticks remaining before it changes;

>         ksCurDomain :: Domain,
>         ksDomainTime :: Word,

\item an array of ready queues, indexed by thread priority and domain (see "getQueue");

>         ksReadyQueues :: Array (Domain, Priority) ReadyQueue,

\item a pointer to the current thread's control block;

>         ksCurThread :: PPtr TCB,

\item a pointer to the idle thread's control block;

>         ksIdleThread :: PPtr TCB,

\item the required action of the scheduler next time it runs;

>         ksSchedulerAction :: SchedulerAction,

\item the interrupt controller's state data;

>         ksInterruptState :: InterruptState,

\item the number of preemption point runs where IRQs have not been checked

>         ksWorkUnitsCompleted :: Word,

\item and some architecture-defined state data.

>         ksArchState :: Arch.KernelState }

\end{itemize}

Note that this definition of "KernelState" assumes a single processor. The behaviour of the kernel on multi-processor systems is not specified by this document.

\subsubsection{Monads}

Kernel functions are sequences of operations that transform a "KernelState" object. They are encapsulated in the monad "Kernel", which uses "StateT" to add a "KernelState" data structure to the monad that encapsulates the simulated machine, "MachineMonad". This allows functions to read and modify the kernel state.

> type Kernel = StateT KernelState MachineMonad

Note that there is no error-signalling mechanism available to functions in "Kernel". Therefore, all errors encountered in such functions are fatal, and will halt the kernel. See \autoref{sec:model.failures} for the definition of monads used for error handling.

\subsubsection{Scheduler Queues}

The ready queue is simply a list of threads that are ready to
run. Each thread in this list is at the same priority level.

> type ReadyQueue = [PPtr TCB]

This is a standard Haskell singly-linked list independent of the
thread control block structures. However, in a real implementation, it
would most likely be embedded in the thread control blocks themselves.

\subsection{Kernel State Functions}

The following two functions are used to get and set the value of the
current thread pointer which is stored in the kernel state.

\begin{impdetails} 

These functions have the same basic form as many
others in the kernel which fetch or set the value of some part of the
state data. They make use of "gets" and "modify", two functions which
each apply a given function to the current state --- either returning
some value extracted from the state, or calculating a new state which
replaces the previous one.  

\end{impdetails}

> getCurThread :: Kernel (PPtr TCB)
> getCurThread = gets ksCurThread

> setCurThread :: PPtr TCB -> Kernel ()
> setCurThread tptr = modify (\ks -> ks { ksCurThread = tptr })

Similarly, these functions access the idle thread pointer, the ready queue for a given priority level (adjusted to account for the active security domain), the requested action of the scheduler, and the interrupt handler state.

> getIdleThread :: Kernel (PPtr TCB)
> getIdleThread = gets ksIdleThread

> setIdleThread :: PPtr TCB -> Kernel ()
> setIdleThread tptr = modify (\ks -> ks { ksIdleThread = tptr })

> getQueue :: Domain -> Priority -> Kernel ReadyQueue
> getQueue qdom prio = gets (\ks -> ksReadyQueues ks ! (qdom, prio))

> setQueue :: Domain -> Priority -> ReadyQueue -> Kernel ()
> setQueue qdom prio q = modify (\ks -> ks { ksReadyQueues = (ksReadyQueues ks)//[((qdom, prio),q)] })

> getSchedulerAction :: Kernel SchedulerAction
> getSchedulerAction = gets ksSchedulerAction

> setSchedulerAction :: SchedulerAction -> Kernel ()
> setSchedulerAction a = modify (\ks -> ks { ksSchedulerAction = a })

> getInterruptState :: Kernel InterruptState
> getInterruptState = gets ksInterruptState

> setInterruptState :: InterruptState -> Kernel ()
> setInterruptState a = modify (\ks -> ks { ksInterruptState = a })

> getWorkUnits :: Kernel Word
> getWorkUnits = gets ksWorkUnitsCompleted

> setWorkUnits :: Word -> Kernel ()
> setWorkUnits a = modify (\ks -> ks { ksWorkUnitsCompleted = a })

> modifyWorkUnits :: (Word -> Word) -> Kernel ()
> modifyWorkUnits f = modify (\ks -> ks { ksWorkUnitsCompleted = 
>                                         f (ksWorkUnitsCompleted ks) })

These functions access and modify the current domain and the number of ticks remaining until the current domain changes.

> curDomain :: Kernel Domain
> curDomain = gets ksCurDomain

> nextDomain :: Kernel ()
> nextDomain = modify (\ks ->
>   let ksDomScheduleIdx' = (ksDomScheduleIdx ks + 1) `mod` length (ksDomSchedule ks) in
>   let next = ksDomSchedule ks !! ksDomScheduleIdx'
>   in ks { ksWorkUnitsCompleted = 0,
>           ksDomScheduleIdx = ksDomScheduleIdx',
>           ksCurDomain = dschDomain next,
>           ksDomainTime = dschLength next })

> getDomainTime :: Kernel Word
> getDomainTime = gets ksDomainTime

> decDomainTime :: Kernel ()
> decDomainTime = modify (\ks -> ks { ksDomainTime = ksDomainTime ks - 1 })

\subsection{Initial Kernel State}

A new kernel state structure contains an empty physical address space, a set of empty scheduler queues, and undefined values for the other global variables, which must be set during the bootstrap sequence.

> newKernelState :: PAddr -> (KernelState, [PAddr])
> newKernelState data_start = (state', frames) where
>     state' = KState {
>         ksPSpace = newPSpace,
>         gsUserPages = (\_ -> Nothing),
>         gsCNodes = (\_ -> Nothing),
>         ksDomScheduleIdx = 0,
>         ksDomSchedule = [(0, 15), (2, 42), (1, 73)],
>         ksCurDomain = 0,
>         ksDomainTime = 15,
>         ksReadyQueues =
>             funPartialArray (const [])
>                             ((0, 0), (fromIntegral numDomains, fromIntegral numPriorities)),
>         ksCurThread = error "No initial thread",
>         ksIdleThread = error "Idle thread has not been created",
>         ksSchedulerAction = error "scheduler action has not been set",
>         ksInterruptState = error "Interrupt controller is uninitialised",
>         ksWorkUnitsCompleted = 0,
>         ksArchState = archState }
>     (archState, frames) = Arch.newKernelState data_start

\subsection{Performing Machine Operations}

The following function allows the machine monad to be directly accessed from kernel code.

> doMachineOp :: MachineMonad a -> Kernel a
> doMachineOp = lift

\subsection{Miscellaneous Monad Functions}

\subsubsection{Assertions and Undefined Behaviour}

The function "assert" is used to state that a predicate must be true at a given point. If it is not, the behaviour of the kernel is undefined. The Haskell model will not terminate in this case.

> assert :: Monad m => Bool -> String -> m ()
> assert p e = if p then return () else fail $ "Assertion failed: " ++ e

The function "stateAssert" is similar to "assert", except that it reads the current state. This is typically used for more complex assertions that cannot be easily expressed in Haskell; in this case, the asserted function is "const True" in Haskell but is replaced with something stronger in the Isabelle translation.

> stateAssert :: (KernelState -> Bool) -> String -> Kernel ()
> stateAssert f e = get >>= \s -> assert (f s) e

The "capHasProperty" function is used with "stateAssert". As explained above, it is "const True" here, but is strengthened to actually check the capability in the translation to Isabelle.

> capHasProperty :: PPtr CTE -> (Capability -> Bool) -> KernelState -> Bool
> capHasProperty _ _ = const True

\subsubsection{Searching a List}

The function "findM" searches a list, returning the first item for which the given function returns "True". It is the monadic equivalent of "Data.List.find".

> findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
> findM _ [] = return Nothing
> findM f (x:xs) = do
>     r <- f x
>     if r then return $ Just x else findM f xs


