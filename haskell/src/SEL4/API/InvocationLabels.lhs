%
% Copyright 2014, General Dynamics C4 Systems
%
% This software may be distributed and modified according to the terms of
% the GNU General Public License version 2. Note that NO WARRANTY is provided.
% See "LICENSE_GPLv2.txt" for details.
%
% @TAG(GD_GPL)
%

This module defines the machine-independent invocation labels.

\begin{impdetails}

This module makes use of the GHC extension allowing data types with no constructors.

> {-# LANGUAGE EmptyDataDecls, CPP #-}

\end{impdetails}

> module SEL4.API.InvocationLabels where

\begin{impdetails}

> import SEL4.Machine
> import qualified SEL4.API.InvocationLabels.TARGET as ArchLabels

\end{impdetails}

\subsection{Invocation Labels}

The following type enumerates all the kinds of invocations that clients can request of the kernel. The derived Enum instance defines the message label that clients should use when requesting that service. These labels are enumerated globally to ensure that no objects share an invocation label. This is to avoid confusion: service requests to the wrong object will fail immediately rather than perform unexpected actions.

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
>         | TCBBindNotification
>         | TCBUnbindNotification
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
>         | IRQAckIRQ
>         | IRQSetIRQHandler
>         | IRQClearIRQHandler
>         | DomainSetSet
>         | ArchInvocationLabel ArchLabels.ArchInvocationLabel
>         deriving (Show, Eq)

> instance Bounded InvocationLabel where
>     minBound = InvalidInvocation
>     maxBound = ArchInvocationLabel $ (maxBound :: ArchLabels.ArchInvocationLabel)

> instance Enum InvocationLabel where
>     fromEnum e = case e of
>          InvalidInvocation -> 0
>          UntypedRetype -> 1
>          TCBReadRegisters -> 2
>          TCBWriteRegisters -> 3
>          TCBCopyRegisters -> 4
>          TCBConfigure -> 5
>          TCBSetPriority -> 6
>          TCBSetIPCBuffer -> 7
>          TCBSetSpace -> 8
>          TCBSuspend -> 9
>          TCBResume -> 10
>          TCBBindNotification -> 11
>          TCBUnbindNotification -> 12
>          CNodeRevoke -> 13
>          CNodeDelete -> 14
>          CNodeRecycle -> 15
>          CNodeCopy -> 16
>          CNodeMint -> 17
>          CNodeMove -> 18
>          CNodeMutate -> 19
>          CNodeRotate -> 20
>          CNodeSaveCaller -> 21
>          IRQIssueIRQHandler -> 22
>          IRQAckIRQ -> 23
>          IRQSetIRQHandler -> 24
>          IRQClearIRQHandler -> 25
>          DomainSetSet -> apiMax
>          ArchInvocationLabel a -> apiMax + 1 + fromEnum a
>          where apiMax = 26
>     toEnum n
>         | n == 0 = InvalidInvocation
>         | n == 1 = UntypedRetype
>         | n == 2 = TCBReadRegisters
>         | n == 3 = TCBWriteRegisters
>         | n == 4 = TCBCopyRegisters
>         | n == 5 = TCBConfigure
>         | n == 6 = TCBSetPriority
>         | n == 7 = TCBSetIPCBuffer
>         | n == 8 = TCBSetSpace
>         | n == 9 = TCBSuspend
>         | n == 10 = TCBResume
>         | n == 11 = TCBBindNotification
>         | n == 12 = TCBUnbindNotification
>         | n == 13 = CNodeRevoke
>         | n == 14 = CNodeDelete
>         | n == 15 = CNodeRecycle
>         | n == 16 = CNodeCopy
>         | n == 17 = CNodeMint
>         | n == 18 = CNodeMove
>         | n == 19 = CNodeMutate
>         | n == 20 = CNodeRotate
>         | n == 21 = CNodeSaveCaller
>         | n == 22 = IRQIssueIRQHandler
>         | n == 23 = IRQAckIRQ
>         | n == 24 = IRQSetIRQHandler
>         | n == 25 = IRQClearIRQHandler
>         | n == 26 = DomainSetSet
>         | n > apiMax = ArchInvocationLabel $ toEnum (n - 1 - apiMax)
>         | otherwise = error "toEnum out of range for InvocationLabel"
>         where apiMax = 26

Decode the invocation type requested by a particular message label.

> invocationType :: Word -> InvocationLabel
> invocationType x
>     | x' <= fromEnum (maxBound :: InvocationLabel) = toEnum x'
>     | otherwise = InvalidInvocation
>     where x' = fromIntegral x

