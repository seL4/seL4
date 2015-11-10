%
% Copyright 2014, General Dynamics C4 Systems
%
% This software may be distributed and modified according to the terms of
% the GNU General Public License version 2. Note that NO WARRANTY is provided.
% See "LICENSE_GPLv2.txt" for details.
%
% @TAG(GD_GPL)
%

This module specifies the behavior of notification objects.

> module SEL4.Object.Notification (
>         sendSignal, receiveSignal, 
>         cancelAllSignals, cancelSignal, completeSignal,
>         getNotification, setNotification, doUnbindNotification, unbindNotification,
>         unbindMaybeNotification, bindNotification, doNBRecvFailedTransfer
>     ) where

\begin{impdetails}

> import SEL4.Machine
> import SEL4.Model
> import SEL4.Object.Structures
> import {-# SOURCE #-} SEL4.Object.Endpoint(cancelIPC)
> import {-# SOURCE #-} SEL4.Object.TCB(asUser) 
> import SEL4.Object.Instances()

> import {-# SOURCE #-} SEL4.Kernel.Thread

> import Data.Bits
> import Data.List

\end{impdetails}

\subsection{Sending Signals}

> -- helper function
> receiveBlocked :: ThreadState -> Bool
> receiveBlocked st = case st of
>     BlockedOnReceive _ _ -> True
>     _ -> False

This function performs an signal operation, given a capability to a notification object, and a single machine word of message data (the badge). This operation will never block the signalling thread.

> sendSignal :: PPtr Notification -> Word -> Kernel ()

> sendSignal ntfnPtr badge = do 

Fetch the notification object object, and select the operation based on its state.

>         nTFN <- getNotification ntfnPtr
>         case (ntfnObj nTFN, ntfnBoundTCB nTFN) of

If the notification object is idle, store the badge and the value, and then 
mark the notification object as active. 

>             (IdleNtfn, Just tcb) -> do
>                     state <- getThreadState tcb
>                     if (receiveBlocked state) 
>                       then do
>                         cancelIPC tcb
>                         setThreadState Running tcb
>                         asUser tcb $ setRegister badgeRegister badge
>                         switchIfRequiredTo tcb
>                       else
>                         setNotification ntfnPtr $ nTFN { ntfnObj = ActiveNtfn badge }
>             (IdleNtfn, Nothing) -> setNotification ntfnPtr $ nTFN { ntfnObj = ActiveNtfn badge }

If the notification object is waiting, a thread is removed from its queue and the signal is transferred to it.

>             (WaitingNtfn (dest:queue), _) -> do 
>                 setNotification ntfnPtr $ nTFN {
>                   ntfnObj = case queue of 
>                     [] -> IdleNtfn
>                     _  -> WaitingNtfn queue
>                   }
>                 setThreadState Running dest
>                 asUser dest $ setRegister badgeRegister badge
>                 switchIfRequiredTo dest
>             (WaitingNtfn [], _) -> fail "WaitingNtfn Notification must have non-empty queue"

If the notification object is active, new values are calculated and stored in the notification object. The calculation is done by a bitwise OR operation of the currently stored, and the newly sent values.

>             (ActiveNtfn badge', _) -> do
>                 let newBadge = badge .|. badge'
>                 setNotification ntfnPtr $ nTFN { ntfnObj = ActiveNtfn newBadge }

\subsection{Receiving Signals}

This function performs an receive signal operation, given a thread pointer and a capability to a notification object. The receive can be either blocking (the thread will be blocked on the notification until a signal arrives) or non-blocking depending on the isBlocking flag.

> doNBRecvFailedTransfer :: PPtr TCB -> Kernel ()
> doNBRecvFailedTransfer thread = asUser thread $ setRegister badgeRegister 0 


> receiveSignal :: PPtr TCB -> Capability -> Bool -> Kernel ()

> receiveSignal thread cap isBlocking = do 

Fetch the notification object, and select the operation based on its state.

>         let ntfnPtr = capNtfnPtr cap
>         ntfn <- getNotification ntfnPtr
>         case ntfnObj ntfn of 

If the notification object is idle, then it becomes a waiting notification object, with the current thread in its queue. The thread is blocked.

>             IdleNtfn -> case isBlocking of 
>                 True -> do 
>                       setThreadState (BlockedOnNotification {
>                                          waitingOnNotification = ntfnPtr } ) thread
>                       setNotification ntfnPtr $ ntfn {ntfnObj = WaitingNtfn ([thread]) }
>                 False -> doNBRecvFailedTransfer thread

If the notification object is already waiting, the current thread is blocked and added to the queue. Note that this case cannot occur when the notification object is bound, as only the associated thread can wait on it.  

>             WaitingNtfn queue -> case isBlocking of 
>                 True -> do 
>                       setThreadState (BlockedOnNotification {
>                                          waitingOnNotification = ntfnPtr } ) thread
>                       setNotification ntfnPtr $ ntfn {ntfnObj = WaitingNtfn (queue ++ [thread]) }
>                 False -> doNBRecvFailedTransfer thread

If the notification object is active, the badge of the invoked notification object capability will be loaded to the badge of the receiving thread and the notification object will be marked as idle.

>             ActiveNtfn badge -> do 
>                 asUser thread $ setRegister badgeRegister badge
>                 setNotification ntfnPtr $ ntfn {ntfnObj = IdleNtfn }

\subsection{Delete Operation}

If a notification object is deleted, then pending receive operations must be cancelled.

> cancelAllSignals :: PPtr Notification -> Kernel ()
> cancelAllSignals ntfnPtr = do
>         ntfn <- getNotification ntfnPtr 
>         case ntfnObj ntfn of 
>             WaitingNtfn queue -> do
>                 setNotification ntfnPtr (ntfn { ntfnObj = IdleNtfn })
>                 forM_ queue (\t -> do
>                     setThreadState Restart t
>                     tcbSchedEnqueue t)
>                 rescheduleRequired
>             _ -> return ()

The following function will remove the given thread from the queue of the notification object, and replace the thread's IPC block with a fault block (which will retry the operation if the thread is resumed).

> cancelSignal :: PPtr TCB -> PPtr Notification -> Kernel ()
> cancelSignal threadPtr ntfnPtr = do 
>         ntfn <- getNotification ntfnPtr
>         assert (isWaiting (ntfnObj ntfn))
>             "cancelSignal: notification object must be waiting"
>         let queue' = delete threadPtr $ ntfnQueue $ ntfnObj ntfn
>         ntfn' <- case queue' of 
>             [] -> return $ IdleNtfn
>             _ -> return $ (ntfnObj ntfn) { ntfnQueue = queue' }
>         setNotification ntfnPtr (ntfn { ntfnObj = ntfn' })
>         setThreadState Inactive threadPtr
>     where 
>       isWaiting ntfn = case ntfn of 
>                       WaitingNtfn {} -> True 
>                       _ -> False

> completeSignal :: PPtr Notification -> PPtr TCB -> Kernel ()
> completeSignal ntfnPtr tcb = do
>         ntfn <- getNotification ntfnPtr
>         case ntfnObj ntfn of
>             ActiveNtfn badge -> do
>                 asUser tcb $ setRegister badgeRegister badge
>                 setNotification ntfnPtr $ ntfn {ntfnObj = IdleNtfn}
>             _ -> fail "tried to complete signal with inactive notification object"


\subsection{Accessing Notification Objects}

The following functions are specialisations of the "getObject" and "setObject" for the "Notification" object and pointer type. 

> getNotification :: PPtr Notification -> Kernel Notification
> getNotification = getObject

> setNotification :: PPtr Notification -> Notification -> Kernel ()
> setNotification = setObject


\subsection{Miscellaneous}

> bindNotification :: PPtr TCB -> PPtr Notification -> Kernel ()
> bindNotification tcb ntfnPtr = do
>     -- set the bound tcb inside the ntfn
>     ntfn <- getNotification ntfnPtr
>     setNotification ntfnPtr $ ntfn { ntfnBoundTCB = Just tcb }
>     -- set the bound ntfn inside the thread
>     setBoundNotification (Just ntfnPtr) tcb

> doUnbindNotification :: PPtr Notification -> Notification -> PPtr TCB -> Kernel ()
> doUnbindNotification ntfnPtr ntfn tcbptr = do
>     let ntfn' = ntfn { ntfnBoundTCB = Nothing }
>     setNotification ntfnPtr ntfn'
>     setBoundNotification Nothing tcbptr

> unbindNotification :: PPtr TCB -> Kernel ()
> unbindNotification tcb = do
>     ntfnPtr <- getBoundNotification tcb 
>     case ntfnPtr of
>         Just ntfnPtr' -> do
>              ntfn <- getNotification ntfnPtr'
>              doUnbindNotification ntfnPtr' ntfn tcb
>         Nothing -> return ()

> unbindMaybeNotification :: PPtr Notification -> Kernel ()
> unbindMaybeNotification ntfnPtr = do
>     ntfn <- getNotification ntfnPtr
>     case ntfnBoundTCB ntfn of
>         Just t -> doUnbindNotification ntfnPtr ntfn t
>         Nothing -> return ()


