%
% Copyright 2014, General Dynamics C4 Systems
%
% This software may be distributed and modified according to the terms of
% the GNU General Public License version 2. Note that NO WARRANTY is provided.
% See "LICENSE_GPLv2.txt" for details.
%
% @TAG(GD_GPL)
%

This module provides the invocation handling for the kernel's two interrupt-handling capability types: the interrupt controller, and the IRQ handlers. It also provides a function that dispatches received interrupts to the appropriate handlers.

\begin{impdetails}

We use the C preprocessor to select a target architecture.

> {-# LANGUAGE CPP #-}

\end{impdetails}

> module SEL4.Object.Interrupt (
>     decodeIRQControlInvocation, decodeIRQHandlerInvocation,
>     invokeIRQControl, invokeIRQHandler,
>     deletingIRQHandler, deletedIRQHandler,
>     initInterruptController, handleInterrupt
>   ) where

The architecture-specific definitions are imported qualified with the "Arch" prefix.

> import qualified SEL4.Object.Interrupt.TARGET as Arch

\begin{impdetails}

> import SEL4.Machine
> import SEL4.Model
> import SEL4.API.Failures
> import SEL4.API.Invocation
> import SEL4.API.Types
> import SEL4.Object.Structures
> import SEL4.Object.AsyncEndpoint
> import {-# SOURCE #-} SEL4.Object.CNode
> import {-# SOURCE #-} SEL4.Kernel.CSpace
> import {-# SOURCE #-} SEL4.Kernel.Thread
> import {-# SOURCE #-} SEL4.Kernel.Init

> import Data.Bits
> import Data.Array
> import Data.Helpers

\end{impdetails}

\subsection{Interrupt Capability Invocations}

\subsubsection{Interrupt Controller Capabilities}

There is a single, global interrupt controller object; a capability to it is provided to the initial thread at boot time. Interrupt controller capabilities may be used to generate handler capabilities for specific interrupts (see \autoref{sec:object.interrupt.invoke.handler}), or to change architecture-specific interrupt controller parameters.

> decodeIRQControlInvocation :: Word -> [Word] -> PPtr CTE -> [Capability] ->
>         KernelF SyscallError IRQControlInvocation
> decodeIRQControlInvocation label args srcSlot extraCaps =
>     case (invocationType label,args,extraCaps) of
>         (IRQIssueIRQHandler,irqW:index:depth:_,cnode:_) -> do
>             rangeCheck irqW
>                 (fromEnum minIRQ) (fromEnum maxIRQ)
>             let irq = toEnum (fromIntegral irqW) :: IRQ
>             
>             irqActive <- withoutFailure $ isIRQActive irq
>             when irqActive $ throw RevokeFirst
>             
>             destSlot <- lookupTargetSlot cnode
>                 (CPtr index) (fromIntegral depth)
>             ensureEmptySlot destSlot
>             
>             return $ IssueIRQHandler irq destSlot srcSlot
>         (IRQIssueIRQHandler,_,_) -> throw TruncatedMessage
>         (IRQInterruptControl,_,_) -> liftM InterruptControl $
>             Arch.decodeInterruptControl args extraCaps
>         _ -> throw IllegalOperation

> invokeIRQControl :: IRQControlInvocation -> KernelP ()
> invokeIRQControl (IssueIRQHandler irq handlerSlot controlSlot) =
>   withoutPreemption $ do
>     setIRQState (IRQNotifyAEP) irq
>     cteInsert (IRQHandlerCap irq) controlSlot handlerSlot
> invokeIRQControl (InterruptControl invok) =
>     Arch.invokeInterruptControl invok

\subsubsection{IRQ Handler Capabilities}
\label{sec:object.interrupt.invoke.handler}

An IRQ handler capability allows a thread possessing it to set an endpoint which will be notified of incoming interrupts, and to acknowledge received interrupts.

> decodeIRQHandlerInvocation :: Word -> IRQ -> [(Capability, PPtr CTE)] ->
>         KernelF SyscallError IRQHandlerInvocation
> decodeIRQHandlerInvocation label irq extraCaps =
>     case (invocationType label,extraCaps) of
>         (IRQAckIRQ,_) -> return $ AckIRQ irq
>         (IRQSetIRQHandler,(cap,slot):_) -> case cap of
>                 AsyncEndpointCap { capAEPCanSend = True } ->
>                     return $ SetIRQHandler irq cap slot
>                 _ -> throw $ InvalidCapability 0
>         (IRQSetIRQHandler,_) -> throw TruncatedMessage
>         (IRQClearIRQHandler,_) -> return $ ClearIRQHandler irq
>         _ -> throw IllegalOperation

> invokeIRQHandler :: IRQHandlerInvocation -> Kernel ()
> invokeIRQHandler (AckIRQ irq) =
>     doMachineOp $ maskInterrupt False irq
> invokeIRQHandler (SetIRQHandler irq cap slot) = do
>     irqSlot <- getIRQSlot irq
>     cteDeleteOne irqSlot
>     cteInsert cap slot irqSlot
> invokeIRQHandler (ClearIRQHandler irq) = do
>     irqSlot <- getIRQSlot irq
>     cteDeleteOne irqSlot

\subsection{Kernel Functions}

\subsubsection{Deleting IRQ Handlers}

When the last IRQ handler capability for a given IRQ is deleted, the capability management code calls these functions, 'deletingIRQHandler' before deletion and 'deletedIRQHandler' after deletion. These mask the IRQ, delete the handler capability, and mark the IRQ as inactive (allowing a new IRQ handler cap to be generated).

> deletingIRQHandler :: IRQ -> Kernel ()
> deletingIRQHandler irq = do
>     slot <- getIRQSlot irq
>     cteDeleteOne slot

> deletedIRQHandler :: IRQ -> Kernel ()
> deletedIRQHandler irq =
>     setIRQState IRQInactive irq

\subsubsection{Initialisation}

This function is called during bootstrap to set up the initial state of the interrupt controller. It allocates a frame and converts its contents to capability slots, which are used as a table endpoints that are notified of incoming interrupts. It also sets the global interrupt controller state, which contains a pointer to each slot and a Boolean flag indicating whether a handler capability has been generated for each IRQ. An interrupt controller capability is provided to the initial thread.

> initInterruptController :: KernelInit Capability
> initInterruptController = do
>     frame <- allocFrame
>     doKernelOp $ do
>         assert (length [minBound..(maxBound::IRQ)]
>                `shiftL` (objBits (makeObject :: CTE)) <= bit pageBits)
>             "Interrupt vector slots must fit in one frame"
>         placeNewObject (ptrFromPAddr frame) (makeObject :: CTE)
>               (bit pageBits `shiftR` objBits (makeObject :: CTE))
>         doMachineOp $ mapM_ (maskInterrupt True) [minBound .. maxBound]
>         let irqTable = funArray $ const IRQInactive
>         setInterruptState $ InterruptState (ptrFromPAddr frame) irqTable
>         timerIRQ <- doMachineOp configureTimer
>         setIRQState IRQTimer timerIRQ
>     return IRQControlCap

\subsubsection{Handling Interrupts}
\label{sec:object.interrupt.kernel.handling}

This function is called when the kernel receives an interrupt event.

> handleInterrupt :: IRQ -> Kernel ()
> handleInterrupt irq = do
>     st <- getIRQState irq
>     case st of
>         IRQNotifyAEP -> do
>             slot <- getIRQSlot irq
>             cap <- getSlotCap slot
>             case cap of
>                 AsyncEndpointCap { capAEPCanSend = True } ->
>                     sendAsyncIPC (capAEPPtr cap) (capAEPBadge cap)
>                         (bit (fromEnum irq `mod` bitSize (undefined::Word)))
>                 _ -> doMachineOp $ debugPrint $
>                     "Undelivered interrupt: " ++ show irq
>             doMachineOp $ maskInterrupt True irq
>         IRQTimer -> do
>             timerTick
>             doMachineOp resetTimer
>         IRQInactive -> fail $ "Received disabled IRQ " ++ show irq
>     doMachineOp $ ackInterrupt irq

\subsection{Accessing the Global State}

The following functions are used within this module to access the global interrupt controller state.

> isIRQActive :: IRQ -> Kernel Bool
> isIRQActive irq = liftM (/=IRQInactive) $ getIRQState irq

> setIRQState :: IRQState -> IRQ -> Kernel ()
> setIRQState irqState irq = do
>     st <- getInterruptState
>     let table = intStateIRQTable st 
>     setInterruptState $ st { intStateIRQTable = table//[(irq, irqState)] }
>     doMachineOp $ maskInterrupt (irqState==IRQInactive) irq

> getIRQState :: IRQ -> Kernel IRQState
> getIRQState irq = liftM ((!irq) . intStateIRQTable) getInterruptState

> getIRQSlot :: IRQ -> Kernel (PPtr CTE)
> getIRQSlot irq = do
>     node <- liftM intStateIRQNode getInterruptState
>     locateSlot node (fromIntegral $ fromEnum irq)


