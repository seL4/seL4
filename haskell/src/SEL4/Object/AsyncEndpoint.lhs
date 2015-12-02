%
% Copyright 2014, General Dynamics C4 Systems
%
% This software may be distributed and modified according to the terms of
% the GNU General Public License version 2. Note that NO WARRANTY is provided.
% See "LICENSE_GPLv2.txt" for details.
%
% @TAG(GD_GPL)
%

This module specify the behavior of a asynchronous IPC endpoints. 

> module SEL4.Object.AsyncEndpoint (
>         sendAsyncIPC, receiveAsyncIPC, 
>         aepCancelAll, asyncIPCCancel, completeAsyncIPC,
>         getAsyncEP, setAsyncEP, doUnbindAEP, unbindAsyncEndpoint,
>         unbindMaybeAEP, bindAsyncEndpoint, doNBWaitFailedTransfer
>     ) where

\begin{impdetails}

> import SEL4.Machine
> import SEL4.Model
> import SEL4.Object.Structures
> import {-# SOURCE #-} SEL4.Object.Endpoint(ipcCancel)
> import {-# SOURCE #-} SEL4.Object.TCB(asUser) 
> import SEL4.Object.Instances()

> import {-# SOURCE #-} SEL4.Kernel.Thread

> import Data.Bits
> import Data.List

\end{impdetails}

\subsection{Sending Messages}

> -- helper function
> receiveBlocked :: ThreadState -> Bool
> receiveBlocked st = case st of
>     BlockedOnReceive _ _ -> True
>     _ -> False

This function performs an asynchronous IPC send operation, given a capability to an asynchronous endpoint, and a single machine word of message data. This operation will never block the sending thread.

> sendAsyncIPC :: PPtr AsyncEndpoint -> Word -> Kernel ()

> sendAsyncIPC aepptr badge = do 

Fetch the asynchronous endpoint object, and select the operation based on its state.

>         aEP <- getAsyncEP aepptr
>         case (aepObj aEP, aepBoundTCB aEP) of

If the asynchronous endpoint is idle, store the badge and the value, and then 
mark the endpoint as active. 

>             (IdleAEP, Just tcb) -> do
>                     state <- getThreadState tcb
>                     if (receiveBlocked state) 
>                       then do
>                         ipcCancel tcb
>                         setThreadState Running tcb
>                         asUser tcb $ setRegister badgeRegister badge
>                         switchIfRequiredTo tcb
>                       else
>                         setAsyncEP aepptr $ aEP { aepObj = ActiveAEP badge }
>             (IdleAEP, Nothing) -> setAsyncEP aepptr $ aEP { aepObj = ActiveAEP badge }

If the asynchronous endpoint is waiting, a thread is removed from its queue and the message is transferred to it.

>             (WaitingAEP (dest:queue), _) -> do 
>                 setAsyncEP aepptr $ aEP {
>                   aepObj = case queue of 
>                     [] -> IdleAEP
>                     _  -> WaitingAEP queue
>                   }
>                 setThreadState Running dest
>                 asUser dest $ setRegister badgeRegister badge
>                 switchIfRequiredTo dest
>             (WaitingAEP [], _) -> fail "WaitingAEP AEP must have non-empty queue"

If the endpoint is active, new values are calculated and stored in the endpoint. The calculation is done by a bitwise OR operation of the currently stored, and the newly sent values.

>             (ActiveAEP badge', _) -> do
>                 let newBadge = badge .|. badge'
>                 setAsyncEP aepptr $ aEP { aepObj = ActiveAEP newBadge }

\subsection{Receiving Messages}

This function performs an asynchronous IPC receive operation, given a thread pointer and a capability to an asynchronous endpoint. 
The receive can be either blocking (the thread will be blocked on the endpoint till a message arrives) or non-blocking
depending on the isBlocking flag.

> doNBWaitFailedTransfer :: PPtr TCB -> Kernel ()
> doNBWaitFailedTransfer thread = asUser thread $ setRegister badgeRegister 0 


> receiveAsyncIPC :: PPtr TCB -> Capability -> Bool -> Kernel ()

> receiveAsyncIPC thread cap isBlocking = do 

Fetch the asynchronous endpoint, and select the operation based on its state.

>         let aepptr = capAEPPtr cap
>         aep <- getAsyncEP aepptr
>         case aepObj aep of 

If the asynchronous endpoint is idle, then it becomes a waiting asynchronous endpoint, with the current thread in its queue. The thread is blocked.

>             IdleAEP -> case isBlocking of 
>                 True -> do 
>                       setThreadState (BlockedOnAsyncEvent {
>                                          waitingOnAsyncEP = aepptr } ) thread
>                       setAsyncEP aepptr $ aep {aepObj = WaitingAEP ([thread]) }
>                 False -> doNBWaitFailedTransfer thread

If the asynchronous endpoint is already waiting, the current thread is blocked and added to the queue. Note that this case cannot occur when the asynchronous endpoint is bound, 
as only the associated thread can wait on it.  

>             WaitingAEP queue -> case isBlocking of 
>                 True -> do 
>                       setThreadState (BlockedOnAsyncEvent {
>                                          waitingOnAsyncEP = aepptr } ) thread
>                       setAsyncEP aepptr $ aep {aepObj = WaitingAEP (queue ++ [thread]) }
>                 False -> doNBWaitFailedTransfer thread

If the asynchronous endpoint is active, the message will be loaded to the MRs of the thread and the endpoint will be marked as idle.

>             ActiveAEP badge -> do 
>                 asUser thread $ setRegister badgeRegister badge
>                 setAsyncEP aepptr $ aep {aepObj = IdleAEP }

\subsection{Delete Operation}

If an asynchronous endpoint is deleted, then pending receive operations must be cancelled.

> aepCancelAll :: PPtr AsyncEndpoint -> Kernel ()
> aepCancelAll aepptr = do
>         aep <- getAsyncEP aepptr 
>         case aepObj aep of 
>             WaitingAEP queue -> do
>                 setAsyncEP aepptr (aep { aepObj = IdleAEP })
>                 forM_ queue (\t -> do
>                     setThreadState Restart t
>                     tcbSchedEnqueue t)
>                 rescheduleRequired
>             _ -> return ()

The following function will remove the given thread from the queue of the asynchronous endpoint, and replace the thread's IPC block with a fault block (which will retry the operation if the thread is resumed).

> asyncIPCCancel :: PPtr TCB -> PPtr AsyncEndpoint -> Kernel ()
> asyncIPCCancel threadPtr aepptr = do 
>         aep <- getAsyncEP aepptr
>         assert (isWaiting (aepObj aep))
>             "asyncIPCCancel: async endpoint must be waiting"
>         let queue' = delete threadPtr $ aepQueue $ aepObj aep
>         aep' <- case queue' of 
>             [] -> return $ IdleAEP
>             _ -> return $ (aepObj aep) { aepQueue = queue' }
>         setAsyncEP aepptr (aep { aepObj = aep' })
>         setThreadState Inactive threadPtr
>     where 
>       isWaiting aep = case aep of 
>                       WaitingAEP {} -> True 
>                       _ -> False

> completeAsyncIPC :: PPtr AsyncEndpoint -> PPtr TCB -> Kernel ()
> completeAsyncIPC aepptr tcb = do
>         aep <- getAsyncEP aepptr
>         case aepObj aep of
>             ActiveAEP badge -> do
>                 asUser tcb $ setRegister badgeRegister badge
>                 setAsyncEP aepptr $ aep {aepObj = IdleAEP}
>             _ -> fail "tried to complete async ipc with inactive aep"


\subsection{Accessing Asynchronous Endpoints}

The following functions are specialisations of the "getObject" and "setObject" for the "AsynchEndpoint" object and pointer type. 

> getAsyncEP :: PPtr AsyncEndpoint -> Kernel AsyncEndpoint
> getAsyncEP = getObject

> setAsyncEP :: PPtr AsyncEndpoint -> AsyncEndpoint -> Kernel ()
> setAsyncEP = setObject


\subsection{Miscellaneous}

> bindAsyncEndpoint :: PPtr TCB -> PPtr AsyncEndpoint -> Kernel ()
> bindAsyncEndpoint tcb aepptr = do
>     -- set the bound tcb inside the aep
>     aep <- getAsyncEP aepptr
>     setAsyncEP aepptr $ aep { aepBoundTCB = Just tcb }
>     -- set the bound aep inside the thread
>     setBoundAEP (Just aepptr) tcb

> doUnbindAEP :: PPtr AsyncEndpoint -> AsyncEndpoint -> PPtr TCB -> Kernel ()
> doUnbindAEP aepptr aep tcbptr = do
>     let aep' = aep { aepBoundTCB = Nothing }
>     setAsyncEP aepptr aep'
>     setBoundAEP Nothing tcbptr

> unbindAsyncEndpoint :: PPtr TCB -> Kernel ()
> unbindAsyncEndpoint tcb = do
>     aepptr <- getBoundAEP tcb 
>     case aepptr of
>         Just aepptr' -> do
>              aep <- getAsyncEP aepptr'
>              doUnbindAEP aepptr' aep tcb
>         Nothing -> return ()

> unbindMaybeAEP :: PPtr AsyncEndpoint -> Kernel ()
> unbindMaybeAEP aepptr = do
>     aep <- getAsyncEP aepptr
>     case aepBoundTCB aep of
>         Just t -> doUnbindAEP aepptr aep t
>         Nothing -> return ()


