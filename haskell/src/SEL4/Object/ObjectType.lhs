%
% Copyright 2014, General Dynamics C4 Systems
%
% This software may be distributed and modified according to the terms of
% the GNU General Public License version 2. Note that NO WARRANTY is provided.
% See "LICENSE_GPLv2.txt" for details.
%
% @TAG(GD_GPL)
%

This module defines several functions operating on kernel objects. These operations are applicable to all objects, but have implementations specific to each object type. These functions are partly implementation-defined, as they may operate on implementation-defined object types.

\begin{impdetails}

We use the C preprocessor to select a target architecture.

> {-# LANGUAGE CPP #-}

\end{impdetails}

> module SEL4.Object.ObjectType where

\begin{impdetails}

> {-# BOOT-IMPORTS: SEL4.Machine SEL4.API.Types SEL4.Model SEL4.Object.Structures #-}
> {-# BOOT-EXPORTS: createObject #-}

> import SEL4.API.Types
> import SEL4.API.Failures
> import SEL4.API.Invocation
> import SEL4.Machine
> import SEL4.Model
> import SEL4.Object.Structures
> import SEL4.Object.Instances()
> import SEL4.Object.Untyped
> import {-# SOURCE #-} SEL4.Object.CNode
> import SEL4.Object.Endpoint
> import SEL4.Object.AsyncEndpoint
> import SEL4.Object.Interrupt
> import {-# SOURCE #-} SEL4.Object.TCB
> import {-# SOURCE #-} SEL4.Kernel.Thread

> import Data.Bits

\end{impdetails}

The architecture-specific definitions are imported qualified with the "Arch" prefix.

> import qualified SEL4.Object.ObjectType.TARGET as Arch

\subsection{Creating Capabilities}

When copying a capability, it may be necessary to reset or modify data that is specific to each capability (rather than to each object). The following function is used when copying a capability, to allow such changes.

> deriveCap :: PPtr CTE -> Capability -> KernelF SyscallError Capability

Zombie capabilities refer to objects that are in the process of being destroyed because a thread has requested deletion of their last remaining capability (ie, this one). Copying them is not allowed.

> deriveCap _ (Zombie {}) = return NullCap

There may be at most one IRQ control capability in the system, it cannot be copied.

> deriveCap _ (IRQControlCap) = return NullCap

Untyped capabilities cannot be copied if they have children.

> deriveCap slot cap@(UntypedCap {}) = do
>     ensureNoChildren slot
>     return cap

Reply capabilities cannot be copied; to ensure that the "Reply" system call is fast, there can never be more than one reply capability for a given thread.

> deriveCap _ (ReplyCap {}) = return NullCap

Architecture-specific capability types are handled in the relevant module.

> deriveCap slot (ArchObjectCap cap) =
>     liftM ArchObjectCap $ Arch.deriveCap slot cap

Other capabilities do not require modification.

> deriveCap _ cap = return cap

\subsection{Finalising Capabilities}
\label{sec:object.objecttype.finalise}

Similarly, when deleting a capability, it may be necessary to change other parts of the kernel or machine state that refer to that specific capability. If the deleted capability is the last one referring to the object, it is also necessary to clean up any references to the object itself.

The "finaliseCap" operation takes any finalisation actions that need to be taken before this capability can be deleted. The first boolean flag indicates whether this is the final capability to an object. A special case is when the capability refers to other capability slots. If these need to be cleared, we have a recursive problem which may take unbounded time to resolve. The second flag asserts that the capability being passed is not of a type that can cause such recursion.

During the unbounded capability clearing operation, the capability to the slots is replaced by a "Zombie" capability which records which slots remain to be cleared but cannot be invoked, traversed or copied. This case is handled here by returning the "Zombie" which will be inserted. The "Zombie" finalisation process is handled elsewhere (see "finaliseSlot", \autoref{sec:object.cnode.ops.delete}).

> finaliseCap :: Capability -> Bool -> Bool -> Kernel (Capability, Maybe IRQ)

When the last capability to an endpoint is deleted, any IPC operations currently using it are aborted.

> finaliseCap (EndpointCap { capEPPtr = ptr }) final _ = do
>     when final $ epCancelAll ptr
>     return (NullCap, Nothing)

> finaliseCap (AsyncEndpointCap { capAEPPtr = ptr }) final _ = do
>     when final $ aepCancelAll ptr
>     return (NullCap, Nothing)

> finaliseCap (ReplyCap {}) _ _ = return (NullCap, Nothing)

No action need be taken for Null or Domain capabilities.

> finaliseCap NullCap _ _ = return (NullCap, Nothing)
> finaliseCap DomainCap _ _ = return (NullCap, Nothing)

Capabilities other than the above should never be passed with the second boolean flag set.

> finaliseCap _ _ True = fail "finaliseCap: failed to finalise immediately."

A "CNodeCap" is replaced with the appropriate "Zombie". No other action is needed.

> finaliseCap (CNodeCap { capCNodePtr = ptr, capCNodeBits = bits }) True _ =
>     return (Zombie ptr (ZombieCNode bits) (bit bits), Nothing)

Threads are treated as special capability nodes; they also become zombies when their final capabilities are deleted, but they must first be suspended to prevent them being scheduled during deletion.

> finaliseCap (ThreadCap { capTCBPtr = tcb}) True _ = do
>     cte_ptr <- getThreadCSpaceRoot tcb
>     suspend tcb
>     return (Zombie cte_ptr ZombieTCB 5, Nothing)

Zombies have already been finalised.

> finaliseCap z@(Zombie {}) True _ =
>     return (z, Nothing)

Deletion of architecture-specific capabilities are handled in the architecture module.

> finaliseCap (ArchObjectCap { capCap = cap }) final _ =
>     liftM (\cap -> (cap, Nothing)) $ Arch.finaliseCap cap final

When a final IRQ handler cap is finalised, the interrupt controller is notified. It will mask the IRQ, delete any internal references to the notification endpoint, and allow future "IRQControl" calls to create caps for this IRQ.

> finaliseCap (IRQHandlerCap { capIRQ = irq }) True _ = do
>     deletingIRQHandler irq
>     return (NullCap, Just irq)

Zombie capabilities are always final.

> finaliseCap (Zombie {}) False _ = fail "Finalising a non-final zombie cap"

For any other capability, no special action is required.

> finaliseCap _ _ _ = return (NullCap, Nothing)

\subsection{Recycling Capabilities}
\label{sec:object.objecttype.recycle}

When an object is recycled, its final capability will be finalised (see above) to clean up any in-kernel references to the object. Then, the object and capability must both be returned to their initial states. The "recycleCap" operation, given a capability to a finalised object, re-initialises the object and returns a capability to it.

> recycleCap :: Bool -> Capability -> Kernel Capability

Null and Domain capabilities can't be reconstructed.

> recycleCap _ NullCap = fail "recycleCap: can't reconstruct Null"
> recycleCap _ DomainCap = return DomainCap

Zombie capabilities must be transformed back into their original types. Also, TCBs must be returned to their initial states, to remove any register values from the old thread. The TCB cap slots have already been cleared, and will not be recleared at this stage.

> recycleCap _ (Zombie { capZombiePtr = ptr, capZombieType = tp }) = do
>     case tp of
>         ZombieTCB -> do
>                 let tcbPtr = (PPtr . fromPPtr) ptr
>                 tcb <- threadGet id tcbPtr
>                 flip assert "Zombie cap should point at inactive thread."
>                     $ case tcbState tcb of
>                         Inactive -> True
>                         _ -> False
>                 flip assert "Zombie cap should not point at queued thread."
>                     $ not (tcbQueued tcb)
>                 curdom <- curDomain
>                 threadSet (\tcb ->
>                     makeObject { tcbCTable = tcbCTable tcb,
>                                  tcbVTable = tcbVTable tcb,
>                                  tcbReply = tcbReply tcb,
>                                  tcbCaller = tcbCaller tcb,
>                                  tcbDomain = curdom,
>                                  tcbIPCBufferFrame = tcbIPCBufferFrame tcb })
>                     tcbPtr
>                 return $ ThreadCap tcbPtr
>         ZombieCNode sz -> return $ CNodeCap ptr sz 0 0

Recycling a badged synchronous endpoint capability will abort all messages sent to the endpoint with that badge. Note that receive attempts are not aborted (as they are not marked with badges). Also, the aborted messages include those that were sent by any other capability with the same badge --- including those which were separately minted, and therefore are not deleted when the recycled capability is revoked.

Note that if the badge is 0, then this was the original endpoint capability, and all of the messages on it have been cancelled already.

> recycleCap _ (cap@EndpointCap { capEPPtr = ep, capEPBadge = b }) = do
>     when (b /= 0) $ epCancelBadgedSends ep b
>     return cap

Architecture-specific capabilities are handled in the target module.

> recycleCap is_final (ArchObjectCap cap) =
>     liftM ArchObjectCap $ Arch.recycleCap is_final cap

Any "CNode" or "TCB" capability which is encountered here is one that is not converted into a "Zombie" by "finaliseCap". This is because it was not final to begin with, which in turn is because the original exists elsewhere. The right thing to do in this case is nothing.

All other object types are left in their initial states by "finaliseCap", and their capabilities can be returned unchanged.

> recycleCap _ cap = return cap


To prevent privilege escalation, the capability must have sufficient access
rights if it is to be recycled. Endpoint and Thread caps need to have full
rights.

> hasRecycleRights :: Capability -> Bool

> hasRecycleRights NullCap = False
> hasRecycleRights DomainCap = False
> hasRecycleRights (EndpointCap { capEPCanSend = True, 
>                                 capEPCanReceive = True, 
>                                 capEPCanGrant = True }) = True
> hasRecycleRights (EndpointCap {}) = False
> hasRecycleRights (AsyncEndpointCap { capAEPCanSend = True, 
>                                      capAEPCanReceive = True }) = True
> hasRecycleRights (AsyncEndpointCap {}) = False
> hasRecycleRights (ArchObjectCap cap) = Arch.hasRecycleRights cap
> hasRecycleRights _ = True

\subsection{Comparing Capabilities}


> sameRegionAs :: Capability -> Capability -> Bool

This function will return "True" if the left hand capability grants access to the physical resources that the right hand object grants access to. For example, if the left hand object is an untyped memory region, the result will be "True" for every capability with an object pointer inside that region. This is used by "sameObjectAs" and "isMDBParentOf".

This function assumes that its arguments are in MDB order.

> sameRegionAs a@(UntypedCap {}) b = 
>     isPhysicalCap b && (baseA <= baseB) && (topB <= topA) && (baseB <= topB)
>     where
>         baseA = capPtr a
>         topA = baseA + PPtr (bit $ capBlockSize a) - 1
>         baseB = capUntypedPtr b
>         topB = baseB + PPtr (capUntypedSize b) - 1

> sameRegionAs (a@EndpointCap {}) (b@EndpointCap {}) =
>     capEPPtr a == capEPPtr b

> sameRegionAs (a@AsyncEndpointCap {}) (b@AsyncEndpointCap {}) =
>     capAEPPtr a == capAEPPtr b

> sameRegionAs (a@CNodeCap {}) (b@CNodeCap {}) =
>     capCNodePtr a == capCNodePtr b && capCNodeBits a == capCNodeBits b

> sameRegionAs (a@ThreadCap {}) (b@ThreadCap {}) =
>     capTCBPtr a == capTCBPtr b

> sameRegionAs (a@ReplyCap {}) (b@ReplyCap {}) =
>     capTCBPtr a == capTCBPtr b

> sameRegionAs DomainCap     DomainCap     = True

> sameRegionAs IRQControlCap IRQControlCap = True
> sameRegionAs IRQControlCap (IRQHandlerCap {}) = True

> sameRegionAs (IRQHandlerCap a) (IRQHandlerCap b) = a == b

> sameRegionAs (ArchObjectCap a) (ArchObjectCap b) =
>     a `Arch.sameRegionAs` b

> sameRegionAs _ _ = False

> isPhysicalCap :: Capability -> Bool

This helper function to "sameRegionAs" checks that we have a physical capability, one which is generateable from an Untyped capability. Capabilities which refer to no particular kernel object, such as the IRQControl capability, and Reply capabilities generated by IPC, should never be compared to Untyped capabilities.

> isPhysicalCap NullCap = False
> isPhysicalCap IRQControlCap = False
> isPhysicalCap DomainCap = False
> isPhysicalCap (IRQHandlerCap {}) = False
> isPhysicalCap (ReplyCap {}) = False
> isPhysicalCap (ArchObjectCap a) = Arch.isPhysicalCap a
> isPhysicalCap _ = True

> sameObjectAs :: Capability -> Capability -> Bool

If this function returns true, neither of the two arguments is a final typed capability for the purposes of "finaliseCap". Like "sameRegionAs", it assumes that its arguments are in MDB order.

The rules for determining this are generally the same as for "sameRegionAs". However, an untyped capability for an enclosing region on the left hand side does not prevent the right hand side capability being final. Likewise, an IRQ control capability on the left hand side does not prevent a IRQ handler capability being final.

> sameObjectAs (UntypedCap {}) _ = False
> sameObjectAs IRQControlCap (IRQHandlerCap {}) = False
> sameObjectAs (ArchObjectCap a) (ArchObjectCap b) = a `Arch.sameObjectAs` b
> sameObjectAs a b = a `sameRegionAs` b

\subsection{Modifying Capabilities}

The "updateCapData" function is used to update a capability when moving or copying it, given a data word provided by the user. It may return "NullCap" (given a valid input cap) if the user provides an invalid data word. The meaning of the data word depends on the type of the capability; some types do not use it at all.

The boolean argument is true when the capability is being updated by a "Mutate" or "Rotate" operation. In this case, any changes to the capability should not affect its MDB parent / child relationships; for example, endpoint capabilities may not have their badges changed.

> updateCapData :: Bool -> Word -> Capability -> Capability

Endpoint badges can never be changed once a nonzero badge is set; if the existing badge is not zero (the default value), then the update will fail.

> updateCapData preserve new cap@(EndpointCap {})
>     | not preserve && capEPBadge cap == 0 = cap { capEPBadge = new .&. mask badgeBits }
>     | otherwise = NullCap

> updateCapData preserve new cap@(AsyncEndpointCap {})
>     | not preserve && capAEPBadge cap == 0 = cap { capAEPBadge = new .&. mask badgeBits }
>     | otherwise = NullCap

The total of the guard size and the radix of the node cannot exceed the number of bits to be resolved in the entire address space. This prevents an overflow in the encoding used for CNode capabilities in the ARM implementation. Note that a CNode capability violating this restriction could never be used to look up a capability, so nothing is lost by enforcing it on all platforms.

> updateCapData _ w cap@(CNodeCap {})
>     | guardSize + capCNodeBits cap > bitSize w = NullCap
>     | otherwise = cap {
>         capCNodeGuard = guard,
>         capCNodeGuardSize = guardSize }
>     where
>         guard = (w `shiftR` (rightsBits + guardSizeBits)) .&.
>             mask guardBits .&. mask guardSize
>         guardSize = fromIntegral $ (w `shiftR` rightsBits) .&.
>             mask guardSizeBits
>         rightsBits = 3
>         guardBits = case bitSize w of
>             32 -> 18
>             64 -> 48
>             _ -> error "Unknown word size"
>         guardSizeBits = case bitSize w of
>             32 -> 5
>             64 -> 6
>             _ -> error "Unknown word size"

> updateCapData p w (ArchObjectCap { capCap = aoCap }) =
>     Arch.updateCapData p w aoCap
> updateCapData _ _ cap = cap

The C implementation only has space for 28 bits in the badge field.

> badgeBits :: Int
> badgeBits = 28

The "maskCapRights" function restricts the operations that can be performed on a capability, given a set of rights.

> maskCapRights :: CapRights -> Capability -> Capability

> maskCapRights _ NullCap = NullCap
> maskCapRights _ DomainCap = DomainCap

> maskCapRights _ c@(UntypedCap {}) = c

> maskCapRights r c@(EndpointCap {}) = c {
>     capEPCanSend = capEPCanSend c && capAllowWrite r,
>     capEPCanReceive = capEPCanReceive c && capAllowRead r,
>     capEPCanGrant = capEPCanGrant c && capAllowGrant r }

> maskCapRights r c@(AsyncEndpointCap {}) = c {
>     capAEPCanSend = capAEPCanSend c && capAllowWrite r,
>     capAEPCanReceive = capAEPCanReceive c && capAllowRead r }

> maskCapRights _ c@(ReplyCap {}) = c

> maskCapRights _ c@(CNodeCap {}) = c 

> maskCapRights _ c@(ThreadCap {}) = c 

> maskCapRights _ c@IRQControlCap = c

> maskCapRights _ c@(IRQHandlerCap {}) = c

> maskCapRights r (ArchObjectCap {capCap = aoCap}) = Arch.maskCapRights r aoCap

> maskCapRights _ c@(Zombie {}) = c

\subsection{Creating and Deleting Objects}

The "createObject" function creates a new object in physical memory, and
returns the capabilities referring to it. Its parameters are an object
type; the base address of the destination memory region; and the object
size requested by the user. The latter is used only for variable-sized
objects such as frames or CNodes.

New threads are placed in the current security domain, which must be the domain of the creating thread.

> createObject :: ObjectType -> PPtr () -> Int -> Kernel Capability
> createObject t regionBase userSize =
>     let funupd = (\f x v y -> if y == x then v else f y) in
>     case toAPIType t of
>         Just TCBObject -> do
>             placeNewObject regionBase (makeObject :: TCB) 0
>             curdom <- curDomain
>             threadSet (\t -> t { tcbDomain = curdom })
>                 (PPtr $ fromPPtr regionBase)
>             return $! ThreadCap (PPtr $ fromPPtr regionBase)
>         Just EndpointObject -> do
>             placeNewObject regionBase (makeObject :: Endpoint) 0
>             return $! EndpointCap (PPtr $ fromPPtr regionBase) 0 True True True
>         Just AsyncEndpointObject -> do
>             placeNewObject (PPtr $ fromPPtr regionBase) (makeObject :: AsyncEndpoint) 0
>             return $! AsyncEndpointCap (PPtr $ fromPPtr regionBase) 0 True True
>         Just CapTableObject -> do
>             placeNewObject (PPtr $ fromPPtr regionBase) (makeObject :: CTE) userSize
>             modify (\ks -> ks { gsCNodes =
>               funupd (gsCNodes ks) (fromPPtr regionBase) (Just userSize)})
>             return $! CNodeCap (PPtr $ fromPPtr regionBase) userSize 0 0
>         Just Untyped ->
>             return $! UntypedCap (PPtr $ fromPPtr regionBase) userSize 0
>         Nothing -> do
>             archCap <- Arch.createObject t regionBase userSize
>             return $! ArchObjectCap archCap

\subsection{Invoking Objects}

The following functions are used to handle messages that are sent to kernel objects by user level code using a "Send" or "SendWait" system call.

The "decodeInvocation" function parses the message, determines the operation that is being performed, and checks for any error conditions. If it returns successfully, the invocation is guaranteed to complete without any errors.

> decodeInvocation :: Word -> [Word] -> CPtr -> PPtr CTE ->
>         Capability -> [(Capability, PPtr CTE)] ->
>         KernelF SyscallError Invocation
>
> decodeInvocation _ _ _ _ cap@(EndpointCap {capEPCanSend=True}) _ =
>     return $ InvokeEndpoint
>         (capEPPtr cap) (capEPBadge cap) (capEPCanGrant cap)
>
> decodeInvocation _ args _ _ cap@(AsyncEndpointCap {capAEPCanSend=True}) _ = do
>     let msg = case args of
>             (x:_) -> x
>             _ -> 0
>     return $ InvokeAsyncEndpoint (capAEPPtr cap) (capAEPBadge cap) msg
>
> decodeInvocation _ _ _ slot cap@(ReplyCap {capReplyMaster=False}) _ = do
>     return $ InvokeReply (capTCBPtr cap) slot
>
> decodeInvocation
>         label args _ slot cap@(ThreadCap {}) extraCaps =
>     liftM InvokeTCB $ decodeTCBInvocation label args cap slot extraCaps
>
> decodeInvocation label args _ _ DomainCap extraCaps =
>     liftM (uncurry InvokeDomain) $ decodeDomainInvocation label args extraCaps
>
> decodeInvocation
>         label args _ _ cap@(CNodeCap {}) extraCaps =
>     liftM InvokeCNode $
>         decodeCNodeInvocation label args cap $ map fst extraCaps
>
> decodeInvocation label args _ slot cap@(UntypedCap {}) extraCaps =
>     liftM InvokeUntyped $
>         decodeUntypedInvocation label args slot cap $ map fst extraCaps
>
> decodeInvocation label args _ slot IRQControlCap extraCaps =
>     liftM InvokeIRQControl $
>         decodeIRQControlInvocation label args slot $ map fst extraCaps
> 
> decodeInvocation label _ _ _ (IRQHandlerCap { capIRQ = irq }) extraCaps =
>     liftM InvokeIRQHandler $
>         decodeIRQHandlerInvocation label irq extraCaps
>
> decodeInvocation label args capIndex slot (ArchObjectCap cap) extraCaps =
>     liftM InvokeArchObject $
>         Arch.decodeInvocation label args capIndex slot cap extraCaps

If the capability cannot be invoked, because it is null or does not have a required right, then the operation returns "InvalidCapability".

> decodeInvocation _ _ _ _ _ _ = throw $ InvalidCapability 0

The "invoke" function performs the operation itself. It cannot throw faults, but it may be pre-empted. If it returns a list of words, they will be sent as a reply message with label 0; this is optional because the kernel does not generate replies from endpoint invocations.

This function just dispatches invocations to the type-specific invocation functions.

> performInvocation :: Bool -> Bool -> Invocation -> KernelP [Word]
> 
> performInvocation _ _ (InvokeUntyped invok) = do
>     withoutPreemption $ invokeUntyped invok
>     return $! []
> 
> performInvocation block call (InvokeEndpoint ep badge canGrant) =
>   withoutPreemption $ do
>     thread <- getCurThread
>     sendIPC block call badge canGrant thread ep
>     return $! []
> 
> performInvocation _ _ (InvokeAsyncEndpoint ep badge message) = do
>     withoutPreemption $ sendAsyncIPC ep badge message
>     return $! []
>
> performInvocation _ _ (InvokeReply thread slot) = withoutPreemption $ do
>     sender <- getCurThread
>     doReplyTransfer sender thread slot
>     return $! []
>
> performInvocation _ _ (InvokeTCB invok) = invokeTCB invok
>
> performInvocation _ _ (InvokeDomain thread domain) = withoutPreemption $ do
>     setDomain thread domain
>     return $! []
> 
> performInvocation _ _ (InvokeCNode invok) = do
>     invokeCNode invok
>     return $! []
> 
> performInvocation _ _ (InvokeIRQControl invok) = do
>     invokeIRQControl invok
>     return $! []
> 
> performInvocation _ _ (InvokeIRQHandler invok) = do
>     withoutPreemption $ invokeIRQHandler invok
>     return $! []
> 
> performInvocation _ _ (InvokeArchObject invok) = Arch.performInvocation invok

\subsection{Helper Functions}

The following two functions returns the base and size of the object a capability points to. They are used to determine whether the object is enclosed by an untyped capability, and are therefore undefined for capability types that cannot be MDB children of untyped capabilities.

> capUntypedPtr :: Capability -> PPtr ()
> capUntypedPtr NullCap = error "No valid pointer"
> capUntypedPtr (UntypedCap { capPtr = p }) = p
> capUntypedPtr (EndpointCap { capEPPtr = PPtr p }) = PPtr p
> capUntypedPtr (AsyncEndpointCap { capAEPPtr = PPtr p }) = PPtr p
> capUntypedPtr (ReplyCap { capTCBPtr = PPtr p }) = PPtr p
> capUntypedPtr (CNodeCap { capCNodePtr = PPtr p }) = PPtr p
> capUntypedPtr (ThreadCap { capTCBPtr = PPtr p }) = PPtr p
> capUntypedPtr DomainCap = error "Domain control has no pointer"
> capUntypedPtr (Zombie { capZombiePtr = PPtr p }) = PPtr p
> capUntypedPtr IRQControlCap = error "IRQ control has no pointer"
> capUntypedPtr (IRQHandlerCap {}) = error "IRQ handler has no pointer"
> capUntypedPtr (ArchObjectCap a) = Arch.capUntypedPtr a

> capUntypedSize :: Capability -> Word
> capUntypedSize NullCap = 0 -- was error in haskell
> capUntypedSize (UntypedCap { capBlockSize = b }) = 1 `shiftL` b
> capUntypedSize (CNodeCap { capCNodeBits = c })
>     = 1 `shiftL` (objBits (undefined::CTE) + c)
> capUntypedSize (EndpointCap {})
>     = 1 `shiftL` objBits (undefined::Endpoint)
> capUntypedSize (AsyncEndpointCap {})
>     = 1 `shiftL` objBits (undefined::AsyncEndpoint) 
> capUntypedSize (ThreadCap {})
>     = 1 `shiftL` objBits (undefined::TCB)
> capUntypedSize (DomainCap {})
>     = 1 -- error in haskell
> capUntypedSize (ArchObjectCap a)
>     = Arch.capUntypedSize a
> capUntypedSize (Zombie { capZombieType = ZombieTCB })
>     = 1 `shiftL` objBits (undefined::TCB)
> capUntypedSize (Zombie { capZombieType = ZombieCNode sz })
>     = 1 `shiftL` (objBits (undefined::CTE) + sz)
> capUntypedSize (ReplyCap {})
>     = 1 `shiftL` objBits (undefined::TCB) -- error in haskell
> capUntypedSize (IRQControlCap {})
>     = 1 -- error in haskell
> capUntypedSize (IRQHandlerCap {})
>     = 1 -- error in haskell


