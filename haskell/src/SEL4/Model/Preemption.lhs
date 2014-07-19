%
% Copyright 2014, General Dynamics C4 Systems
%
% This software may be distributed and modified according to the terms of
% the GNU General Public License version 2. Note that NO WARRANTY is provided.
% See "LICENSE_GPLv2.txt" for details.
%
% @TAG(GD_GPL)
%

This module defines the types and functions used by the kernel model to implement preemption points and non-preemptible sections in the kernel code.

> module SEL4.Model.Preemption(
>     KernelP, withoutPreemption, preemptionPoint
>     ) where

\begin{impdetails}

> import SEL4.Machine
> import SEL4.Model.StateData

> import Control.Monad.Error

\end{impdetails}

\subsection{Types}

\subsubsection{Interrupts}

Objects of this type are thrown from an "ErrorT" monad transformer when the simulator signals a pending interrupt at a kernel preemption point. The nature of the interrupt is not relevant here, because the simulator will send it to the kernel model as an "Event" once the kernel has been preempted.

> instance Error IRQ

\subsubsection{Monads}

The "KernelP" monad is a transformation of the "Kernel" monad used for functions which may be preempted. Any function in this monad must not leave the kernel in an inconsistent state when calling other functions in the monad (though the model has no means of effectively enforcing this restriction).

> type KernelP a = ErrorT IRQ Kernel a

\subsection{Functions}

If an operation must be performed during which the kernel state is temporarily inconsistent, it should be performed in the argument of a "withoutPreemption" call, to ensure that no preemption points are encountered during the operation.

> withoutPreemption :: Kernel a -> KernelP a
> withoutPreemption = lift

In preemptible code, the kernel may explicitly mark a preemption point with the "preemptionPoint" function. The preemption will only be taken if an interrupt has occurred and the preemption point has been called "workUnitsLimit" times.

> workUnitsLimit = 0x64

> preemptionPoint :: KernelP ()
> preemptionPoint = do
>     lift $ modifyWorkUnits ((+) 1)
>     workUnits <- lift $ getWorkUnits
>     when (workUnitsLimit <= workUnits) $ do
>       lift $ setWorkUnits 0
>       preempt <- lift $ doMachineOp getActiveIRQ
>       case preempt of
>           Just irq -> throwError irq
>           Nothing -> return ()


