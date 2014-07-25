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
>         aepCancelAll, asyncIPCCancel
>     ) where

\begin{impdetails}

> import SEL4.Machine
> import SEL4.Model
> import SEL4.Object.Structures
> import SEL4.Object.Instances()

> import {-# SOURCE #-} SEL4.Kernel.Thread

> import Data.Bits
> import Data.List

\end{impdetails}

\subsection{Sending Messages}

This function performs an asynchronous IPC send operation, given a capability to an asynchronous endpoint, and a single machine word of message data. This operation will never block the sending thread.

> sendAsyncIPC :: PPtr AsyncEndpoint -> Word -> Word -> Kernel ()

> sendAsyncIPC aepptr badge val = do 

Fetch the asynchronous endpoint object, and select the operation based on its state.

>         aEP <- getAsyncEP aepptr
>         case aEP of

If the asynchronous endpoint is idle, store the badge and the value, and then 
mark the endpoint as active. 

>             IdleAEP -> do
>                 setAsyncEP aepptr $ ActiveAEP badge val

If the asynchronous endpoint is waiting, a thread is removed from its queue and the message is transferred to it.

>             WaitingAEP (dest:queue) -> do 
>                 setAsyncEP aepptr $ case queue of 
>                     [] -> IdleAEP
>                     _  -> WaitingAEP queue
>                 setThreadState Running dest
>                 doAsyncTransfer badge val dest
>                 switchIfRequiredTo dest
>             WaitingAEP [] -> fail "WaitingAEP AEP must have non-empty queue"

If the endpoint is active, new values are calculated and stored in the endpoint. The calculation is done by a bitwise OR operation of the currently stored, and the newly sent values.

>             ActiveAEP badge' val' -> do
>                 let newVal   = val .|. val'
>                 let newBadge = badge .|. badge'
>                 setAsyncEP aepptr $ ActiveAEP newBadge newVal

\subsection{Receiving Messages}

This function performs an asynchronous IPC receive operation, given a thread pointer and a capability to an asynchronous endpoint. The receive is blocking -- the thread will be blocked on the endpoint till a message arrives.

> receiveAsyncIPC :: PPtr TCB -> Capability -> Kernel ()

> receiveAsyncIPC thread cap = do 

Fetch the asynchronous endpoint, and select the operation based on its state.

>         let aepptr = capAEPPtr cap
>         aep <- getAsyncEP aepptr 
>         case aep of 

If the asynchronous endpoint is idle, then it becomes a waiting asynchronous endpoint, with the current thread in its queue. The thread is blocked.

>             IdleAEP -> do 
>                 setThreadState (BlockedOnAsyncEvent {
>                      waitingOnAsyncEP = aepptr } ) thread
>                 setAsyncEP aepptr $ WaitingAEP [thread]

If the asynchronous endpoint is already waiting, the current thread is blocked and added to the queue.

>             WaitingAEP queue -> do 
>                 setThreadState (BlockedOnAsyncEvent {
>                     waitingOnAsyncEP = aepptr } ) thread
>                 setAsyncEP aepptr $ WaitingAEP $ queue ++ [thread]

If the asynchronous endpoint is active, the message will be loaded to the MRs of the thread and the endpoint will be marked as idle.

>             ActiveAEP badge currentValue -> do 
>                 doAsyncTransfer badge currentValue thread
>                 setAsyncEP aepptr $ IdleAEP 

\subsection{Delete Operation}

If an asynchronous endpoint is deleted, then pending receive operations must be cancelled.

> aepCancelAll :: PPtr AsyncEndpoint -> Kernel ()
> aepCancelAll aepptr = do
>         aep <- getAsyncEP aepptr 
>         case aep of 
>             WaitingAEP queue -> do
>                 setAsyncEP aepptr IdleAEP
>                 forM_ queue (\t -> do
>                     setThreadState Restart t
>                     tcbSchedEnqueue t)
>                 rescheduleRequired
>             _ -> return ()

The following function will remove the given thread from the queue of the asynchronous endpoint, and replace the thread's IPC block with a fault block (which will retry the operation if the thread is resumed).

> asyncIPCCancel :: PPtr TCB -> PPtr AsyncEndpoint -> Kernel ()
> asyncIPCCancel threadPtr aepptr = do 
>         aep <- getAsyncEP aepptr
>         assert (isWaiting aep)
>             "asyncIPCCancel: async endpoint must be waiting"
>         let queue' = delete threadPtr $ aepQueue aep
>         aep' <- case queue' of 
>             [] -> return IdleAEP
>             _ -> return $ aep { aepQueue = queue' }
>         setAsyncEP aepptr aep' 
>         setThreadState Inactive threadPtr
>     where 
>       isWaiting aep = case aep of 
>                       WaitingAEP {} -> True 
>                       _ -> False


\subsection{Accessing Asynchronous Endpoints}

The following functions are specialisations of the "getObject" and "setObject" for the "AsynchEndpoint" object and pointer type. 

> getAsyncEP :: PPtr AsyncEndpoint -> Kernel AsyncEndpoint
> getAsyncEP = getObject

> setAsyncEP :: PPtr AsyncEndpoint -> AsyncEndpoint -> Kernel ()
> setAsyncEP = setObject


