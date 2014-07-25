%
% Copyright 2014, General Dynamics C4 Systems
%
% This software may be distributed and modified according to the terms of
% the GNU General Public License version 2. Note that NO WARRANTY is provided.
% See "LICENSE_GPLv2.txt" for details.
%
% @TAG(GD_GPL)
%

This module contains the scheduler, and miscellaneous functions that manipulate thread state.

\begin{impdetails}

We use the C preprocessor to select a target architecture.

> {-# LANGUAGE CPP #-}

\end{impdetails}

> module SEL4.Kernel.Thread where

\begin{impdetails}

% {-# BOOT-IMPORTS: SEL4.Model SEL4.Machine SEL4.Object.Structures SEL4.Object.Instances() SEL4.API.Types #-}
% {-# BOOT-EXPORTS: setDomain setPriority getThreadState setThreadState doIPCTransfer isRunnable restart suspend doAsyncTransfer doReplyTransfer attemptSwitchTo switchIfRequiredTo tcbSchedEnqueue tcbSchedDequeue rescheduleRequired timerTick #-}

> import SEL4.Config
> import SEL4.API.Types
> import SEL4.API.Faults
> import SEL4.Machine
> import SEL4.Model
> import SEL4.Object
> import SEL4.Object.Structures
> import SEL4.Kernel.VSpace
> import {-# SOURCE #-} SEL4.Kernel.Init

> import Data.Bits

\end{impdetails}

The architecture-specific definitions are imported qualified with the "Arch" prefix.

> import qualified SEL4.Kernel.Thread.TARGET as Arch

\subsection{Idle Thread Creation}

The idle thread must halt execution and wait for an interrupt to occur, at which point the kernel will be re-entered with an Interrupt event. The following function configures a given TCB to do this when activated.

> configureIdleThread :: PPtr TCB -> KernelInit ()
> configureIdleThread tcb = do
>     Arch.configureIdleThread tcb
>     doKernelOp $ setThreadState IdleThreadState tcb

\subsection{Initial Thread Creation}

This function activates the initial user-level thread. It sets the
first argument register and the program counter, sets the thread's
state so it can be scheduled, and then switches to the thread.
The initial user-level thread has the right to change the security domains of other threads.

> activateInitialThread :: PPtr TCB -> VPtr -> VPtr -> Kernel ()
> activateInitialThread threadPtr entry infoPtr = do
>         asUser threadPtr $ setRegister capRegister $ fromVPtr infoPtr
>         asUser threadPtr $ setNextPC $ fromVPtr entry
>         setupReplyMaster threadPtr
>         setThreadState Running threadPtr
>         setSchedulerAction ResumeCurrentThread
>         idle <- getIdleThread
>         setCurThread idle
>         switchToThread threadPtr

\subsection{Thread Activation}

The "activateThread" function is used to prepare a thread to run. If the thread is in the "Restart" state, then it is running for the first time, resuming after a fault, or restarting an aborted system call; in any of these cases, it will start running at the current instruction. Otherwise, it starts running at the next instruction.

> activateThread :: Kernel ()
> activateThread = do
>         thread <- getCurThread
>         state <- getThreadState thread
>         case state of
>             Running -> return ()
>             Restart -> do
>                 pc <- asUser thread $ getRestartPC
>                 asUser thread $ setNextPC pc
>                 setThreadState Running thread
>             IdleThreadState -> do
>                 Arch.activateIdleThread thread
>             _ -> fail $ "Current thread is blocked, state: " ++ show state

\subsection{Thread State}

The following functions are used by the scheduler to determine whether a particular thread is ready to be scheduled, and whether it is ready to run.

> isBlocked :: PPtr TCB -> Kernel Bool
> isBlocked thread = do
>         state <- getThreadState thread
>         return $ case state of
>             Inactive -> True
>             BlockedOnReceive {} -> True
>             BlockedOnSend {} -> True
>             BlockedOnAsyncEvent {} -> True
>             BlockedOnReply -> True
>             _ -> False

Note that the idle thread is not considered runnable; this is to prevent it being inserted in the scheduler queue.

> isRunnable :: PPtr TCB -> Kernel Bool
> isRunnable thread = do
>         state <- getThreadState thread
>         return $ case state of
>             Running -> True
>             Restart -> True
>             _ -> False

\subsubsection{Suspending a Thread}

When a thread is suspended, either explicitly by a TCB invocation or implicitly when it is being destroyed, any operation that it is currently performing must be cancelled.

> suspend :: PPtr TCB -> Kernel ()
> suspend target = do
>     ipcCancel target
>     setThreadState Inactive target
>     tcbSchedDequeue target

\subsubsection{Restarting a Blocked Thread}

The Restart operation forces a thread that has blocked to retry the operation that caused it to block. 

The invoked thread will return to the instruction that caused it to enter the kernel prior to blocking. If an IPC is in progress (including a fault IPC), it will be silently aborted. Beware of doing this to restart an atomic send and receive operation --- the thread will retry the send phase, even if it had previously succeeded in sending the message and was waiting for the receive phase to complete.

> restart :: PPtr TCB -> Kernel ()
> restart target = do
>     blocked <- isBlocked target
>     when blocked $ do
>         ipcCancel target
>         setupReplyMaster target
>         setThreadState Restart target
>         tcbSchedEnqueue target
>         switchIfRequiredTo target

\subsection{IPC Transfers}

The following function is called before resuming or suspending execution of a thread that has a pending IPC transfer. It looks up the sender and receiver's message buffers (in that order, and skipping the send buffer for a fault IPC), and then transfers the message.

If either of the buffers is missing, then the message will be truncated to include only the part not stored in the buffer.

> doIPCTransfer ::
>         PPtr TCB -> Maybe (PPtr Endpoint) -> Word -> Bool ->
>         PPtr TCB -> Bool -> Kernel ()
> doIPCTransfer sender endpoint badge grant receiver diminish = do
>         receiveBuffer <- lookupIPCBuffer True receiver
>         fault <- threadGet tcbFault sender

>         case fault of

For normal IPC messages, the message registers are transferred.

>             Nothing -> do
>                 sendBuffer <- lookupIPCBuffer False sender
>                 doNormalTransfer
>                     sender sendBuffer endpoint badge grant
>                     receiver receiveBuffer diminish

If the sent message is a fault IPC, the stored fault is transferred.

>             Just _ -> do
>                 doFaultTransfer badge sender receiver receiveBuffer

Replies sent by the "Reply" and "ReplyWait" system calls can either be normal IPC replies, or fault replies. In the former case, the transfer is the same as for an IPC send, but there is never a fault, capability grants are always allowed, the badge is always 0, and capabilities are never received with diminished rights.

> doReplyTransfer :: PPtr TCB -> PPtr TCB -> PPtr CTE -> Kernel ()
> doReplyTransfer sender receiver slot = do
>     state <- getThreadState receiver
>     assert (isReply state)
>         "Reply transfer to a thread that isn't listening"
>     mdbNode <- liftM cteMDBNode $ getCTE slot 
>     assert (mdbPrev mdbNode /= nullPointer
>                 && mdbNext mdbNode == nullPointer)
>         "doReplyTransfer: ReplyCap not at end of MDB chain"
>     parentCap <- getSlotCap (mdbPrev mdbNode)
>     assert (isReplyCap parentCap && capReplyMaster parentCap)
>         "doReplyTransfer: ReplyCap parent not reply master"
>     fault <- threadGet tcbFault receiver
>     case fault of
>         Nothing -> do
>             doIPCTransfer sender Nothing 0 True receiver False
>             cteDeleteOne slot
>             setThreadState Running receiver
>             attemptSwitchTo receiver
>         Just f -> do
>             cteDeleteOne slot
>             tag <- getMessageInfo sender
>             sendBuffer <- lookupIPCBuffer False sender
>             mrs <- getMRs sender sendBuffer tag
>             restart <- handleFaultReply f receiver (msgLabel tag) mrs
>             threadSet (\tcb -> tcb {tcbFault = Nothing}) receiver
>             if restart
>               then do
>                 setThreadState Restart receiver
>                 attemptSwitchTo receiver
>               else setThreadState Inactive receiver

\subsubsection{Ordinary IPC}

Ordinary IPC simply transfers all message registers. It requires pointers to the source and destination threads, and also to their respective IPC buffers.

> doNormalTransfer ::
>     PPtr TCB -> Maybe (PPtr Word) -> Maybe (PPtr Endpoint) -> Word -> Bool ->
>     PPtr TCB -> Maybe (PPtr Word) -> Bool -> Kernel ()
> doNormalTransfer sender sendBuffer endpoint badge canGrant
>         receiver receiveBuffer diminish = do
>         tag <- getMessageInfo sender
>         caps <- if canGrant
>             then lookupExtraCaps sender sendBuffer tag
>                 `catchFailure` const (return [])
>             else return []
>         msgTransferred <- copyMRs sender sendBuffer receiver receiveBuffer $
>                                   msgLength tag
>         tag' <- transferCaps tag caps endpoint receiver receiveBuffer diminish
>         let tag'' = tag' { msgLength = msgTransferred }
>         setMessageInfo receiver tag''
>         asUser receiver $ setRegister badgeRegister badge

\subsubsection{Fault IPC}

If the message is a fault --- either just generated, or loaded from the sender's TCB --- then it will be transferred instead of the sender's message registers. In this case, no pointer to the sender's buffer is required.

The recipient's argument registers are filled in with various information about the nature of the fault and the present state of the faulting thread.

> doFaultTransfer :: Word -> PPtr TCB -> PPtr TCB -> Maybe (PPtr Word) ->
>         Kernel ()
> doFaultTransfer badge sender receiver receiverIPCBuffer = do
>         fault <- threadGet tcbFault sender
>         f <- case fault of
>             Just f -> return f
>             Nothing -> fail "doFaultTransfer: no fault found"
>         (faultLabel, faultMsg) <- makeFaultMessage f sender
>         sent <- setMRs receiver receiverIPCBuffer faultMsg
>         let msgInfo = MI {
>              msgLength = sent,
>              msgExtraCaps = 0,
>              msgCapsUnwrapped = 0,
>              msgLabel = faultLabel }
>         setMessageInfo receiver msgInfo
>         asUser receiver $ setRegister badgeRegister badge

\subsubsection{IPC Capability Transfers}

This function is called when an IPC message includes a capability to transfer. It attempts to perform the transfer, and returns an adjusted messageInfo containing the number of caps transferred and the bitmask of which caps were unwrapped.

> transferCaps :: MessageInfo -> [(Capability, PPtr CTE)] -> 
>         Maybe (PPtr Endpoint) -> PPtr TCB -> Maybe (PPtr Word) -> Bool -> 
>         Kernel MessageInfo
> transferCaps info caps endpoint receiver receiveBuffer diminish = do
>     destSlots <- getReceiveSlots receiver receiveBuffer
>     let info' = info { msgExtraCaps = 0, msgCapsUnwrapped = 0 }
>     case receiveBuffer of 
>         Nothing -> return info'
>         Just rcvBuffer -> do
>             transferCapsToSlots endpoint diminish rcvBuffer 0
>                 caps destSlots info'

> transferCapsToSlots :: Maybe (PPtr Endpoint) -> Bool -> PPtr Word -> Int ->
>        [(Capability, PPtr CTE)] -> [PPtr CTE] -> MessageInfo ->
>        Kernel MessageInfo
> transferCapsToSlots _ _ _ n [] _ mi =
>     return $ mi { msgExtraCaps = fromIntegral n }
> transferCapsToSlots ep diminish rcvBuffer n (arg:caps) slots mi =
>     constOnFailure (mi { msgExtraCaps = fromIntegral n }) $ do
>         case (cap, ep, slots) of
>             (EndpointCap { capEPPtr = p1 }, Just p2, _) | p1 == p2 -> do
>                 withoutFailure $ 
>                     setExtraBadge rcvBuffer (capEPBadge cap) n
>                 withoutFailure $ transferAgain slots miCapUnfolded
>             (_, _, destSlot:slots') -> do
>                 cap' <- unifyFailure $ deriveCap srcSlot $ if diminish
>                         then allRights { capAllowWrite = False }
>                             `maskCapRights` cap
>                         else cap
>                 when (isNullCap cap') $ throw undefined
>                 withoutFailure $ cteInsert cap' srcSlot destSlot
>                 withoutFailure $ transferAgain slots' mi
>             _ -> return $ mi { msgExtraCaps = fromIntegral n }
>     where
>        transferAgain = transferCapsToSlots ep diminish rcvBuffer (n + 1) caps
>        bitN = 1 `shiftL` n
>        miCapUnfolded = mi { msgCapsUnwrapped = msgCapsUnwrapped mi .|. bitN }
>        (cap, srcSlot) = arg

\subsubsection{Asynchronous IPC}

In the case of asynchronous IPC, the message from the asynchronous endpoint's message buffer is loaded into the receiver's IPC buffer.

> doAsyncTransfer :: Word -> Word -> PPtr TCB -> Kernel ()
> doAsyncTransfer badge msgWord thread = do 
>         receiveBuffer <- lookupIPCBuffer True thread
>         msgTransferred <- setMRs thread receiveBuffer [msgWord]
>         asUser thread $ setRegister badgeRegister badge
>         setMessageInfo thread $ MI {
>             msgLength = msgTransferred,
>             msgExtraCaps = 0,
>             msgCapsUnwrapped = 0,
>             msgLabel = 0 }

\subsection{Scheduling}

\subsubsection{The Scheduler}

The scheduler will perform one of three actions, depending on the scheduler action field of the global kernel state.

> schedule :: Kernel ()
> schedule = do
>         curThread <- getCurThread
>         action <- getSchedulerAction
>         case action of

The default action is to do nothing; the current thread will resume execution.

>             ResumeCurrentThread -> return ()

An IPC operation may request that the scheduler switch to a specific thread.

>             SwitchToThread t -> do
>                 curRunnable <- isRunnable curThread
>                 when curRunnable $ tcbSchedEnqueue curThread
>                 switchToThread t
>                 setSchedulerAction ResumeCurrentThread

If the current thread is no longer runnable, has used its entire timeslice, an IPC cancellation has potentially woken multiple higher priority threads, or the domain timeslice is exhausted, then we scan the scheduler queues to choose a new thread. In the last case, we switch to the next domain beforehand.

>             ChooseNewThread -> do
>                 curRunnable <- isRunnable curThread
>                 when curRunnable $ tcbSchedEnqueue curThread
>                 domainTime <- getDomainTime
>                 when (domainTime == 0) $ nextDomain
>                 chooseThread
>                 setSchedulerAction ResumeCurrentThread

Threads are scheduled using a simple multiple-priority round robin algorithm.
It iterates through the ready queues, starting with the highest priority
queue; when it finds a non-empty ready queue, it selects the first
thread in the queue, and makes it the current thread.

Note that the ready queues are a separate structure in the kernel
model. In a real implementation, to avoid requiring
dynamically-allocated kernel memory, these queues would be linked
lists using the TCBs themselves as nodes.

> chooseThread :: Kernel ()
> chooseThread = do
>         curdom <- curDomain
>         r <- findM (chooseThread' curdom) (reverse [0 .. maxPriority])
>         when (r == Nothing) $ switchToIdleThread
>     where
>         chooseThread' :: Domain -> Priority -> Kernel Bool
>         chooseThread' qdom prio = do
>             q <- getQueue qdom prio
>             case q of
>                 thread : _ -> do
>                    switchToThread thread
>                    return True
>                 [] -> return False

\subsubsection{Switching Threads}

To switch to a new thread, we call the architecture-specific thread switch function, remove the new current thread from the ready queues, and then set the current thread pointer.

> switchToThread :: PPtr TCB -> Kernel ()
> switchToThread thread = do
>         Arch.switchToThread thread
>         tcbSchedDequeue thread
>         setCurThread thread

Switching to the idle thread is similar, except that we call a different architecture-specific idle thread switch function. Also, the conditional enqueue of the current thread is unnecessary, because we never switch to the idle thread when the current thread is runnable.

> switchToIdleThread :: Kernel ()
> switchToIdleThread = do
>         thread <- getIdleThread
>         Arch.switchToIdleThread
>         setCurThread thread

\subsubsection{Changing a Thread's Domain}

The following function is used to alter a thread's domain.

> setDomain :: PPtr TCB -> Domain -> Kernel ()
> setDomain tptr newdom = do
>         curThread <- getCurThread
>         tcbSchedDequeue tptr
>         threadSet (\t -> t { tcbDomain = newdom }) tptr
>         runnable <- isRunnable tptr
>         when runnable $ tcbSchedEnqueue tptr
>         when (tptr == curThread) $ rescheduleRequired

\subsubsection{Changing a Thread's Priority}

The following function is used to alter the priority of a thread.

> setPriority :: PPtr TCB -> Priority -> Kernel ()
> setPriority tptr prio = do

The thread must be removed from the old priority's queue, if it is queued.

>         tcbSchedDequeue tptr

Then, the new priority can be set.

>         threadSet (\t -> t { tcbPriority = prio }) tptr

If the thread is runnable, it is enqueued at the new priority.

>         runnable <- isRunnable tptr
>         when runnable $ tcbSchedEnqueue tptr

Finally, if the thread is the current one, we run the scheduler to choose a new thread.

>         curThread <- getCurThread
>         when (tptr == curThread) $ rescheduleRequired


\subsubsection{Switching to Woken Threads}

A successful IPC transfer will normally wake a thread other than the current thread. This function conditionally switches to the woken thread; it enqueues the thread if unable to switch to it.

> possibleSwitchTo :: PPtr TCB -> Bool -> Kernel ()
> possibleSwitchTo target onSamePriority = do
>     curThread <- getCurThread
>     curDom <- curDomain
>     curPrio <- threadGet tcbPriority curThread
>     targetDom <- threadGet tcbDomain target
>     targetPrio <- threadGet tcbPriority target
>     action <- getSchedulerAction
>     if (targetDom /= curDom)
>         then tcbSchedEnqueue target
>         else do
>             if ((targetPrio > curPrio
>                  || (targetPrio == curPrio && onSamePriority))
>                 && action == ResumeCurrentThread)
>                 then setSchedulerAction $ SwitchToThread target
>                 else tcbSchedEnqueue target
>             case action of
>                 SwitchToThread _ -> rescheduleRequired
>                 _ -> return ()

In most cases, the current thread has just sent a message to the woken thread, so we switch if the woken thread has the same or higher priority than the current thread; that is, whenever the priorities permit the switch.

> attemptSwitchTo :: PPtr TCB -> Kernel ()
> attemptSwitchTo target = possibleSwitchTo target True

The exception is when waking a thread that has just completed a "Send" system call. In this case, we switch only if the woken thread has a strictly higher priority; that is, when the priorities require the switch. This is done on the assumption that the recipient of a one-way message transfer is more likely to need to take action afterwards than the sender is. % FIXME: is this a sensible behaviour?

> switchIfRequiredTo :: PPtr TCB -> Kernel ()
> switchIfRequiredTo target = possibleSwitchTo target False

\subsubsection{Cancelling Stored Scheduler Action}

This function is called when the system state has changed sufficiently that the stored scheduler action may be invalid. It safely discards any stored state and organises for a full reschedule to be performed.

> rescheduleRequired :: Kernel ()
> rescheduleRequired = do
>     action <- getSchedulerAction
>     case action of
>         SwitchToThread target -> do
>             tcbSchedEnqueue target
>         _ -> return ()
>     setSchedulerAction ChooseNewThread

\subsubsection{Scheduling Parameters}

A trivial function is provided to fetch the current scheduler state of
a thread.

> getThreadState :: PPtr TCB -> Kernel ThreadState
> getThreadState = threadGet tcbState

When setting the scheduler state, we check for blocking of the current thread; in that case, we tell the scheduler to choose a new thread.

> setThreadState :: ThreadState -> PPtr TCB -> Kernel ()
> setThreadState st tptr = do
>         threadSet (\t -> t { tcbState = st }) tptr
>         runnable <- isRunnable tptr
>         curThread <- getCurThread
>         action <- getSchedulerAction
>         when (not runnable && curThread == tptr && action == ResumeCurrentThread) $
>             rescheduleRequired

\subsubsection{Scheduler Queue Manipulation}

The following two functions place a thread at the beginning or end of its priority's ready queue, unless it is already queued.

> tcbSchedEnqueue :: PPtr TCB -> Kernel ()
> tcbSchedEnqueue thread = do
>     queued <- threadGet tcbQueued thread
>     unless queued $ do
>         tdom <- threadGet tcbDomain thread
>         prio <- threadGet tcbPriority thread
>         queue <- getQueue tdom prio
>         setQueue tdom prio $ thread : queue
>         threadSet (\t -> t { tcbQueued = True }) thread

> tcbSchedAppend :: PPtr TCB -> Kernel ()
> tcbSchedAppend thread = do
>     queued <- threadGet tcbQueued thread
>     unless queued $ do
>         tdom <- threadGet tcbDomain thread
>         prio <- threadGet tcbPriority thread
>         queue <- getQueue tdom prio
>         setQueue tdom prio $ queue ++ [thread]
>         threadSet (\t -> t { tcbQueued = True }) thread

The following function dequeues a thread, if it is queued.

> tcbSchedDequeue :: PPtr TCB -> Kernel ()
> tcbSchedDequeue thread = do
>     queued <- threadGet tcbQueued thread
>     when queued $ do
>         tdom <- threadGet tcbDomain thread
>         prio <- threadGet tcbPriority thread
>         queue <- getQueue tdom prio
>         setQueue tdom prio $ filter (/=thread) queue
>         threadSet (\t -> t { tcbQueued = False }) thread

\subsubsection{Timer Ticks}

When the kernel's timer ticks, we decrement the timeslices of both the current thread and the current domain.

> timerTick :: Kernel ()
> timerTick = do
>   thread <- getCurThread
>   state <- getThreadState thread
>   case state of  
>     Running -> do
>       ts <- threadGet tcbTimeSlice thread
>       let ts' = ts - 1
>       if (ts' > 0)
>         then threadSet (\t -> t { tcbTimeSlice = ts' }) thread
>         else do

If the thread timeslice has expired, we reset it, move the thread to the end of its scheduler queue, and tell the scheduler to choose a new thread.

>           threadSet (\t -> t { tcbTimeSlice = timeSlice }) thread
>           tcbSchedAppend thread
>           rescheduleRequired
>     _ -> return ()

If there is more than one security domain and the domain timeslice has expired, we trigger the scheduler to change domain.

>   when (numDomains > 1) $ do
>       decDomainTime
>       domainTime <- getDomainTime
>       when (domainTime == 0) $ rescheduleRequired

%

