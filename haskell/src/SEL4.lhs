%
% Copyright 2014, General Dynamics C4 Systems
%
% This software may be distributed and modified according to the terms of
% the GNU General Public License version 2. Note that NO WARRANTY is provided.
% See "LICENSE_GPLv2.txt" for details.
%
% @TAG(GD_GPL)
%

This is the top-level module; it defines the interface between the kernel and the user-level simulator.

> module SEL4 (
>     module SEL4.Machine,
>     Event(..), Syscall(..), callKernel, asUser, putUC, getUC,
>     Kernel, KernelState, getCurThread,
>     module SEL4.Kernel.Init,
>     ) where

> import SEL4.API
> import SEL4.Machine
> import SEL4.Kernel.Init
> import SEL4.Kernel.Thread(schedule, activateThread)
> import SEL4.Object.TCB(asUser)
> import SEL4.Object.Interrupt(handleInterrupt)
> import SEL4.Model.StateData(KernelState, Kernel, getCurThread, doMachineOp)
> import SEL4.Model.Preemption(withoutPreemption)
> import Control.Monad.Error
> import Control.Monad.State
> import Data.Maybe

\subsection{Kernel Entry Point}

The following function is called by the simulator whenever an event
occurs which the kernel must handle. Such events include interrupts,
faults, and system calls; the set of possible events is defined in
\autoref{sec:api.types}.

> callKernel :: Event -> Kernel ()
> callKernel ev = do
>     runErrorT $ handleEvent ev
>         `catchError` (\_ -> withoutPreemption $ do 
>                       irq <- doMachineOp getActiveIRQ
>                       when (isJust irq) $ handleInterrupt (fromJust irq))
>     schedule
>     activateThread

\subsection{Saving and Restoring User State}

The following two functions save and restore the user-level context of the current thread.

> putUC :: UserContext -> Kernel ()
> putUC uc = do
>     thread <- getCurThread
>     asUser thread $ put uc

> getUC :: Kernel UserContext
> getUC = do
>     thread <- getCurThread
>     asUser thread get


