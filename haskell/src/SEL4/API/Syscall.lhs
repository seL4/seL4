%
% Copyright 2014, General Dynamics C4 Systems
%
% This software may be distributed and modified according to the terms of
% the GNU General Public License version 2. Note that NO WARRANTY is provided.
% See "LICENSE_GPLv2.txt" for details.
%
% @TAG(GD_GPL)
%

This module contains the top-level parts of the kernel model: the
system call interface, and the interrupt and fault handlers. It
exports the kernel entry points used by the simulator.

The system call interface is defined by functions in this module;
specifically by "handleEvent". This interface is distinct from the
interface to any specific type of kernel object; the operations that
may be performed on those objects are defined in their respective
modules.

> module SEL4.API.Syscall(Event(..), Syscall(..), handleEvent) where

\begin{impdetails}

> import SEL4.API.Types
> import SEL4.API.Failures
> import SEL4.Kernel.Thread
> import SEL4.Kernel.CSpace
> import SEL4.Kernel.VSpace
> import SEL4.Kernel.FaultHandler
> import SEL4.Object
> import SEL4.Model
> import SEL4.Machine
> import Data.Bits

\end{impdetails}

\subsection{Types}

\subsubsection{Events}

The kernel model works by processing events caused by sources outside the kernel --- either user-level code or hardware devices. The following type defines the events that the kernel can respond to. Other than "Interrupt", they all include additional information about the nature of the event.

> data Event
>         = SyscallEvent Syscall
>         | UnknownSyscall Int
>         | UserLevelFault Word Word
>         | Interrupt
>         | VMFaultEvent VMFaultType
>         deriving Show

\subsubsection{System Calls}

The "SyscallEvent" constructor defined above requires an additional
value which specifies the system call that was made. This value is of
the enumerated type "Syscall":

> data Syscall
>         = SysSend
>         | SysNBSend
>         | SysCall
>         | SysWait
>         | SysReply
>         | SysReplyWait
>         | SysYield
>         deriving (Show, Enum, Bounded, Eq)

\subsection{Handling Events}

The "handleEvent" function determines the type of event, checks that
any user-supplied inputs are correct, and then calls internal kernel
functions to perform the appropriate actions. The parameter is the event being handled.

> handleEvent :: Event -> KernelP ()

\subsubsection{System Call Events}

System call events are dispatched here to the appropriate system call handlers, defined in the next section.

> handleEvent (SyscallEvent call) = case call of
>         SysSend -> handleSend True
>         SysNBSend -> handleSend False
>         SysCall -> handleCall
>         SysWait -> withoutPreemption handleWait
>         SysReply -> withoutPreemption handleReply
>         SysReplyWait -> withoutPreemption $ do
>             handleReply
>             handleWait
>         SysYield -> withoutPreemption handleYield

\subsubsection{Interrupts}

Interrupt handling is performed by "handleInterrupt", defined in \autoref{sec:object.interrupt.kernel.handling}.

> handleEvent Interrupt = withoutPreemption $ do
>     active <- doMachineOp getActiveIRQ
>     case active of
>         Just irq -> handleInterrupt irq
>         Nothing -> doMachineOp $ debugPrint "spurious interrupt"

\subsubsection{Unknown System Calls}

An unknown system call raises an "UnknownSyscallException", which reports the system call number to the thread's fault handler. This may allow the fault handler to emulate system call interfaces other than seL4.

> handleEvent (UnknownSyscall n) = withoutPreemption $ do
>     thread <- getCurThread
>     handleFault thread $
>         UnknownSyscallException $ fromIntegral n
>     return ()

\subsubsection{Miscellaneous User-level Faults}

The "UserLevelFault" event represents a fault caused directly by user
level code. This might be, for example, an illegal instruction, or a
floating point exception. A real kernel implementation should provide
the handler with more information about the nature of the fault than
the following function does; the nature of that information is specific
to each architecture. In the second word, only the bottom 29 bits will
be communicated to the fault handler.

> handleEvent (UserLevelFault w1 w2) = withoutPreemption $ do
>     thread <- getCurThread
>     handleFault thread $ UserException w1 (w2 .&. mask 29)
>     return ()

\subsubsection{Virtual Memory Faults}

If the simulator reports a VM fault, the appropriate action depends on whether the architecture has a software-loaded TLB. If so, we look up the address, and then insert it into the TLB; otherwise we simply send a fault IPC.

> handleEvent (VMFaultEvent faultType) = withoutPreemption $ do
>     thread <- getCurThread
>     handleVMFault thread faultType `catchFailure` handleFault thread
>     return ()

\subsection{System Calls}

\subsubsection{Send System Call}

The "Send" system call sends a message to an object. The object is specified by a pointer to a capability in the caller's capability address space. The invocation is one-way; no reply is expected. This operation requires send rights on the invoked capability.

> handleSend :: Bool -> KernelP ()
> handleSend = handleInvocation False

\subsubsection{Call System Call}

The "Call" system call is similar to "Send", but it also requests a reply. For kernel capabilities, the kernel will provide information about the result of the operation directly. For synchronous endpoint capabilities, the receiver of the message will be provided with a single-use reply capability which it can use to send a reply and restart the caller. Asynchronous endpoint and reply capabilities will immediately reply with a 0-length message.

> handleCall :: KernelP ()
> handleCall = handleInvocation True True

\subsubsection{Reply System Call}

The "Reply" system call attempts to perform an immediate IPC transfer to the thread that sent the message received by the last successful "Wait" call. If this is not possible, the reply will be silently dropped. The transfer will succeed if:
\begin{itemize}
  \item there has been no other "Reply" call since the last successful "Wait" call;
  \item the sender used "Call" to generate a reply capability;
  \item the sender has not been halted or restarted while waiting for a reply; and
  \item the reply capability has not been moved aside.
\end{itemize}

> handleReply :: Kernel ()
> handleReply = do
>     thread <- getCurThread
>     callerSlot <- getThreadCallerSlot thread
>     callerCap <- getSlotCap callerSlot
>     case callerCap of
>         ReplyCap caller False -> do
>             assert (caller /= thread)
>                 "handleReply: caller must not be the current thread"
>             doReplyTransfer thread caller callerSlot
>         NullCap -> return ()
>         _ -> fail "handleReply: invalid caller cap"

\subsubsection{Wait System Call}

The "Wait" system call blocks waiting to receive a message through a specified endpoint. It will fail if the specified capability does not refer to an endpoint object.

> handleWait :: Kernel ()
> handleWait = do
>     thread <- getCurThread
>     deleteCallerCap thread
>     epCPtr <- asUser thread $ liftM CPtr $ getRegister capRegister
>     (capFaultOnFailure epCPtr True $ do
>         epCap <- lookupCap thread epCPtr
>         case epCap of
>             EndpointCap { capEPCanReceive = True } ->
>                 withoutFailure $ receiveIPC thread epCap
>             AsyncEndpointCap { capAEPCanReceive = True } ->
>                 withoutFailure $ receiveAsyncIPC thread epCap
>             _ -> throw $ MissingCapability { missingCapBitsLeft = 0 })
>       `catchFailure` handleFault thread
>     return ()

\subsubsection{Yield System Call}

The "Yield" system call is trivial; it simply moves the current thread to the end of its scheduler queue, then tells the scheduler to select a new thread.

> handleYield :: Kernel ()
> handleYield = do
>     thread <- getCurThread
>     tcbSchedDequeue thread
>     tcbSchedAppend thread
>     rescheduleRequired

\subsection{Capability Invocations}\label{sel4:api:syscall:invoke}

The following function implements the "Send" and "Call" system calls. It determines the type of invocation, based on the object type; then it calls the appropriate internal kernel function to perform the operation.

> handleInvocation :: Bool -> Bool -> KernelP ()
> handleInvocation isCall isBlocking = do
>     thread <- withoutPreemption getCurThread
>     info <- withoutPreemption $ getMessageInfo thread
>     ptr <- withoutPreemption $ asUser thread $ liftM CPtr $
>         getRegister capRegister
>     syscall

The destination capability's slot is located, and the capability read from it.

>         (do
>             (cap, slot) <- capFaultOnFailure ptr False $ lookupCapAndSlot thread ptr
>             buffer <- withoutFailure $ lookupIPCBuffer False thread
>             extracaps <- lookupExtraCaps thread buffer info
>             return (slot, cap, extracaps, buffer))

If a fault was encountered looking up the capability, and the invocation is a blocking one, a fault message is sent. If the invocation is non-blocking, the fault is ignored. In either case, no reply is sent.

>         (\fault ->
>             when isBlocking $ handleFault thread fault)

If there was no fault, then the capability, message registers and message label are used to determine the requested operation.

>         (\(slot, cap, extracaps, buffer) -> do
>             args <- withoutFailure $ getMRs thread buffer info
>             decodeInvocation (msgLabel info) args ptr slot cap extracaps)

If a system call error was encountered while decoding the operation, and the user is waiting for a reply, then generate an error message.

>         (\err -> when isCall $
>             replyFromKernel thread $ msgFromSyscallError err)

Otherwise, the operation is performed. If there is a result, it is converted to a success message (with label 0).

While the system call is running, the thread's state is set to "Restart", so any preemption will cause the system call to restart at user level when the thread resumes. If it is still set to "Restart" when the operation completes, it is reset to "Running" so the thread resumes at the next instruction. Also, if this is a call, then a reply message is generated when the thread is restarted.

>         (\oper -> do
>             withoutPreemption $ setThreadState Restart thread
>             reply <- performInvocation isBlocking isCall oper
>             withoutPreemption $ do
>                 state <- getThreadState thread
>                 case state of
>                     Restart -> do
>                         when isCall $ replyFromKernel thread (0, reply)
>                         setThreadState Running thread
>                     _ -> return ())


