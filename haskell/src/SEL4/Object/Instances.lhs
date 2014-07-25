%
% Copyright 2014, General Dynamics C4 Systems
%
% This software may be distributed and modified according to the terms of
% the GNU General Public License version 2. Note that NO WARRANTY is provided.
% See "LICENSE_GPLv2.txt" for details.
%
% @TAG(GD_GPL)
%

This module defines the instances of "PSpaceStorable" objects. 

\begin{impdetails}

This module uses the C preprocessor to select a target architecture.

> {-# LANGUAGE CPP #-}

\end{impdetails}

> module SEL4.Object.Instances where

\begin{impdetails}

> import SEL4.API.Types
> import SEL4.Machine
> import SEL4.Object.Structures
> import SEL4.Model.PSpace
> import SEL4.Model.StateData
> import SEL4.Object.Instances.TARGET()
> import SEL4.Config

> import Data.Bits

\end{impdetails}

\subsection{Type Class Instances}

The following are the instances of "Storable" for the four main types of kernel object: synchronous IPC endpoints, asynchronous IPC endpoints, thread control blocks, and capability table entries. 

\subsubsection{Synchronous IPC Endpoint}

> instance PSpaceStorable Endpoint where
>     makeObject = IdleEP
>     injectKO   = KOEndpoint
>     projectKO o = case o of 
>         KOEndpoint e -> return e
>         _ -> typeError "Endpoint" o

\subsubsection{Asynchronous IPC Endpoint}

> instance PSpaceStorable AsyncEndpoint where 
>     makeObject = IdleAEP
>     injectKO   = KOAEndpoint
>     projectKO o = case o of
>         KOAEndpoint e -> return e
>         _ -> typeError "AsyncEndpoint" o

\subsubsection{Capability Table Entry}

> instance PSpaceStorable CTE where
>     makeObject = CTE {
>         cteCap = NullCap,
>         cteMDBNode = nullMDBNode }
>     injectKO   = KOCTE
>     projectKO o = case o of
>         KOCTE e -> return e
>         _ -> typeError "CTE" o

\begin{impdetails} 
As mentioned in the documentation for the type class "PSpaceStorable", there is one kernel object which needs its own definitions for "loadObject" and "storeObject"; it is the capability table entry. The reason for this is that thread control blocks contain capability table entries for the root of the capability and page tables; the capability copy and revocation functions need to access those "CTE"s while unaware that they are actually inside a "TCB". So the "CTE" versions of "loadObject" and "storeObject" must be able to handle accesses to "TCB"s as well.
\end{impdetails}

>     loadObject ptr ptr' next obj = case obj of
>         KOCTE cte -> do
>             unless (ptr == ptr') $ fail "no CTE found in pspace at address"
>             alignCheck ptr (objBits cte)
>             sizeCheck ptr next (objBits cte)
>             return cte
>         KOTCB tcb -> do
>             alignCheck ptr' (objBits tcb)
>             sizeCheck ptr' next (objBits tcb)
>             offsetReturn (ptr - ptr') tcb
>         _ -> typeError "CTE" obj
>         where
>             toOffset slot = slot `shiftL` objBits (undefined :: CTE)
>             offsetReturn x tcb
>                 | x == toOffset tcbVTableSlot = return $ tcbVTable tcb
>                 | x == toOffset tcbCTableSlot = return $ tcbCTable tcb
>                 | x == toOffset tcbReplySlot = return $ tcbReply tcb
>                 | x == toOffset tcbCallerSlot = return $ tcbCaller tcb
>                 | x == toOffset tcbIPCBufferSlot =
>                     return $ tcbIPCBufferFrame tcb
>                 | otherwise = fail "incorrect CTE offset into TCB"

>     updateObject cte oldObj ptr ptr' next = case oldObj of
>         KOCTE _ -> do
>             unless (ptr == ptr') $ fail "no CTE found in pspace at address"
>             alignCheck ptr (objBits cte)
>             sizeCheck ptr next (objBits cte)
>             return (KOCTE cte)
>         KOTCB tcb -> do
>             alignCheck ptr' (objBits tcb)
>             sizeCheck ptr' next (objBits tcb)
>             offsetAdjust (ptr - ptr') tcb
>         _ -> typeError "CTE" oldObj
>         where
>             toOffset slot = slot `shiftL` objBits (undefined :: CTE)
>             offsetAdjust x tcb
>                 | x == toOffset tcbVTableSlot
>                     = return $ KOTCB (tcb {tcbVTable = cte})
>                 | x == toOffset tcbCTableSlot
>                     = return $ KOTCB (tcb {tcbCTable = cte})
>                 | x == toOffset tcbReplySlot
>                     = return $ KOTCB (tcb { tcbReply = cte })
>                 | x == toOffset tcbCallerSlot
>                     = return $ KOTCB (tcb { tcbCaller = cte })
>                 | x == toOffset tcbIPCBufferSlot
>                     = return $ KOTCB (tcb { tcbIPCBufferFrame = cte })
>                 | otherwise = fail "incorrect CTE offset into TCB"

\subsubsection{Thread Control Block}

The value of "objBits" in this instance is an estimate; the value used in real implementations may vary and may be architecture-dependent.
By default, new threads are unable to change the security domains of other threads. They are later placed in the correct security domain by "createObject".

> instance PSpaceStorable TCB
>   where
>     makeObject = Thread {
>         tcbCTable = makeObject,
>         tcbVTable = makeObject,
>         tcbReply = makeObject,
>         tcbCaller = makeObject,
>         tcbIPCBufferFrame = makeObject,
>         tcbDomain = minBound,
>         tcbState = Inactive,
>         tcbPriority = minBound,
>         tcbQueued = False,
>         tcbFault = Nothing,
>         tcbTimeSlice = timeSlice,
>         tcbFaultHandler = CPtr 0,
>         tcbIPCBuffer = VPtr 0,
>         tcbContext = newContext }
>     injectKO   = KOTCB
>     projectKO o = case o of
>         KOTCB tcb -> return tcb
>         _ -> typeError "TCB" o

\subsubsection{User Data}

> instance PSpaceStorable UserData where
>     makeObject = UserData
>     injectKO  _ = KOUserData
>     projectKO o = case o of 
>         KOUserData -> return UserData
>         _ -> typeError "UserData" o


