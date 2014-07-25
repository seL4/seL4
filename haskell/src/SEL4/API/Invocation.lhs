%
% Copyright 2014, General Dynamics C4 Systems
%
% This software may be distributed and modified according to the terms of
% the GNU General Public License version 2. Note that NO WARRANTY is provided.
% See "LICENSE_GPLv2.txt" for details.
%
% @TAG(GD_GPL)
%

This module defines the interfaces presented to clients by the kernel's objects.

\begin{impdetails}

We use the C preprocessor to select a target architecture.

> {-# LANGUAGE CPP #-}

\end{impdetails}

> module SEL4.API.Invocation where

\begin{impdetails}

> import SEL4.Machine
> import SEL4.API.Types
> import SEL4.Object.Structures

\end{impdetails}

The architecture-specific definitions are imported qualified with the "Arch" prefix.

> import qualified SEL4.API.Invocation.TARGET as Arch

\subsection{Invocation Type}

The following type can specify any kernel object invocation. It contains physical pointers to any kernel objects required for the operation, and other arguments decoded from the message registers.

> data Invocation
>         = InvokeUntyped UntypedInvocation
>         | InvokeEndpoint (PPtr Endpoint) Word Bool
>         | InvokeAsyncEndpoint (PPtr AsyncEndpoint) Word Word
>         | InvokeReply (PPtr TCB) (PPtr CTE)
>         | InvokeDomain (PPtr TCB) Domain
>         | InvokeTCB TCBInvocation
>         | InvokeCNode CNodeInvocation
>         | InvokeIRQControl IRQControlInvocation
>         | InvokeIRQHandler IRQHandlerInvocation
>         | InvokeArchObject Arch.Invocation
>         deriving Show

\subsubsection{TCB Object Invocations}

The following data type defines the set of possible TCB invocation operations. The operations are discussed and defined in more detail in \autoref{sec:object.tcb}.

> data TCBInvocation
>         = ReadRegisters {
>             readRegsThread :: PPtr TCB,
>             readRegsSuspend :: Bool,
>             readRegsLength :: Word,
>             readRegsArch :: Arch.CopyRegisterSets }
>         | WriteRegisters {
>             writeRegsThread :: PPtr TCB,
>             writeRegsResume :: Bool,
>             writeRegsValues :: [Word],
>             writeRegsArch :: Arch.CopyRegisterSets }
>         | CopyRegisters {
>             copyRegsTarget :: PPtr TCB,
>             copyRegsSource :: PPtr TCB,
>             copyRegsSuspendSource, copyRegsResumeTarget :: Bool, 
>             copyRegsTransferFrame, copyRegsTransferInteger :: Bool,
>             copyRegsTransferArch :: Arch.CopyRegisterSets }
>         | ThreadControl {
>             tcThread :: PPtr TCB,
>             tcThreadCapSlot :: PPtr CTE,
>             tcNewFaultEP :: Maybe CPtr,
>             tcNewPriority :: Maybe Priority,
>             tcNewCRoot, tcNewVRoot :: Maybe (Capability, PPtr CTE),
>             tcNewIPCBuffer :: Maybe (VPtr, Maybe (Capability, PPtr CTE)) }
>         | Suspend { suspendThread :: PPtr TCB }
>         | Resume { resumeThread :: PPtr TCB }
>         deriving Show

\subsubsection{CNode Invocations}

The following data type defines the set of possible CNode invocation operations. The operations are discussed and defined in more detail in \autoref{sec:object.cnode}.

> data CNodeInvocation
>         = Revoke { targetSlot :: PPtr CTE }
>         | Delete { targetSlot :: PPtr CTE }
>         | Recycle { targetSlot :: PPtr CTE }
>         | Insert {
>             insertCap :: Capability,
>             sourceSlot, targetSlot :: PPtr CTE }
>         | Move {
>             moveCap :: Capability,
>             sourceSlot, targetSlot :: PPtr CTE }
>         | Rotate {
>             moveCap1, moveCap2 :: Capability,
>             sourceSlot, pivotSlot, targetSlot :: PPtr CTE }
>         | SaveCaller {
>             targetSlot :: PPtr CTE }
>         deriving Show

\subsubsection{Untyped Invocations}

The following data type defines the parameters expected for invocations of Untyped objects.

> data UntypedInvocation
>         = Retype {
>             retypeSource :: PPtr CTE,
>             retypeRegionBase :: PPtr (),
>             retypeFreeRegionBase :: PPtr (),
>             retypeNewType :: ObjectType,
>             retypeNewSizeBits :: Int,
>             retypeSlots :: [PPtr CTE] }
>         deriving Show

\subsubsection{Interrupt Controller Invocations}

The following data type defines the set of possible invocations for interrupt controller capabilities.

> data IRQControlInvocation
>         = IssueIRQHandler {
>             issueHandlerIRQ :: IRQ,
>             issueHandlerSlot, issueHandlerControllerSlot :: PPtr CTE }
>         | InterruptControl { interruptControlArch :: Arch.InterruptControl }
>         deriving Show

\subsubsection{IRQ Handler Invocations}

The following data type defines the set of possible invocations for IRQ capabilities.

> data IRQHandlerInvocation
>         = AckIRQ { irqHandlerIRQ :: IRQ }
>         | SetIRQHandler {
>             irqHandlerIRQ :: IRQ,
>             setIRQHandlerCap :: Capability,
>             setIRQHandlerSlot :: PPtr CTE }
>         | ClearIRQHandler { irqHandlerIRQ :: IRQ }
>         deriving Show

\subsection{Invocation Labels}

The following type enumerates all the kinds of invocations that clients can request of the kernel. The derived Enum instance defines the message label that clients should use when requesting that service. These labels are enumerated globally to ensure that no objects share an invocation label. This is to avoid confusion: service requests to the wrong object will fail immediately rather than perform unexpected actions.

This datatype is defined globally over architectures as well as object types.

> data InvocationLabel
>         = InvalidInvocation
>         | UntypedRetype
>         | TCBReadRegisters
>         | TCBWriteRegisters
>         | TCBCopyRegisters
>         | TCBConfigure
>         | TCBSetPriority
>         | TCBSetIPCBuffer
>         | TCBSetSpace
>         | TCBSuspend
>         | TCBResume
>         | CNodeRevoke
>         | CNodeDelete
>         | CNodeRecycle
>         | CNodeCopy
>         | CNodeMint
>         | CNodeMove
>         | CNodeMutate
>         | CNodeRotate
>         | CNodeSaveCaller
>         | IRQIssueIRQHandler
>         | IRQInterruptControl
>         | IRQAckIRQ
>         | IRQSetIRQHandler
>         | IRQClearIRQHandler
>         | DomainSetSet
>         | ARMPDClean_Data
>         | ARMPDInvalidate_Data
>         | ARMPDCleanInvalidate_Data
>         | ARMPDUnify_Instruction
>         | ARMPageTableMap
>         | ARMPageTableUnmap
>         | ARMPageMap
>         | ARMPageRemap
>         | ARMPageUnmap
>         | ARMPageClean_Data
>         | ARMPageInvalidate_Data
>         | ARMPageCleanInvalidate_Data
>         | ARMPageUnify_Instruction
>         | ARMPageGetAddress
>         | ARMASIDControlMakePool
>         | ARMASIDPoolAssign
>         deriving (Enum, Bounded, Eq)

Decode the invocation type requested by a particular message label.

> invocationType :: Word -> InvocationLabel
> invocationType x
>     | x' <= fromEnum (maxBound :: InvocationLabel) = toEnum x'
>     | otherwise = InvalidInvocation
>     where x' = fromIntegral x

> isPDFlush :: InvocationLabel -> Bool
> isPDFlush x = case x of
>       ARMPDClean_Data -> True
>       ARMPDInvalidate_Data -> True
>       ARMPDCleanInvalidate_Data -> True
>       ARMPDUnify_Instruction -> True
>       _ -> False

> isPageFlush :: InvocationLabel -> Bool
> isPageFlush x = case x of
>       ARMPageClean_Data -> True
>       ARMPageInvalidate_Data -> True
>       ARMPageCleanInvalidate_Data -> True
>       ARMPageUnify_Instruction -> True
>       _ -> False


