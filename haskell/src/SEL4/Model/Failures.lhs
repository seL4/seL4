%
% Copyright 2014, General Dynamics C4 Systems
%
% This software may be distributed and modified according to the terms of
% the GNU General Public License version 2. Note that NO WARRANTY is provided.
% See "LICENSE_GPLv2.txt" for details.
%
% @TAG(GD_GPL)
%

This module defines the kernel's mechanisms for handling failures in kernel code.

> module SEL4.Model.Failures where

\begin{impdetails}

> import SEL4.API.Failures
> import SEL4.API.Types
> import SEL4.Object.Structures
> import SEL4.Model.StateData

> import Control.Monad.Error

\end{impdetails}

\subsection{Data Types}

\subsubsection{Monads}

The "KernelF" monad is a transformation of the "Kernel" monad defined in \autoref{sec:model.statedata}. The "ErrorT" monad transformer is applied to "Kernel" to allow kernel functions to abort with a non-fatal error value. Depending on the type of error, which is indicated here by the type parameter "f", it may be either handled internally by the kernel or propagated to user level.

> type KernelF f = ErrorT f Kernel

Note that fatal errors, which are caused by kernel bugs or invalid states and should \emph{never} actually occur, are modelled by evaluating "undefined". This typically happens via a call to the Haskell function "error", or an implicit evaluation of "undefined" by the Haskell compiler on a pattern or guard match failure.

\subsection{Class Instances}

All four of the failure types must have an instance of the "Error" class to be usable with "ErrorT" (and therefore with "KernelF").

> instance Error Fault
> instance Error SyscallError
> instance Error LookupFailure

\subsection{Failure Handling}

\subsubsection{Allowing and Preventing Failure}

The use of the "ErrorT" monad transformer to encapsulate code that can fail requires that transitions in and out of such code be explicitly marked. The following functions may be used to do so. Note that these are simply specialisations of existing functions defined on the "ErrorT" transformer or the "MonadTrans" class.

> withoutFailure :: Error f => Kernel a -> KernelF f a
> withoutFailure = lift

> throw :: Error f => f -> KernelF f a
> throw = throwError

The "catchFailure" function is used to call code that may fail, given a function that can handle any failures.

> catchFailure :: Error f => KernelF f a -> (f -> Kernel a) -> Kernel a
> catchFailure f h = do
>     result <- runErrorT f
>     either h return result

The "rethrowFailure" function converts one type of failure into another. This is used to convert a "LookupFailure" into the appropriate "Fault" or "SyscallError".

> rethrowFailure :: (Error f1, Error f2) =>
>         (f1 -> f2) -> KernelF f1 a -> KernelF f2 a
> rethrowFailure t m = do
>     result <- lift $ runErrorT m
>     either (throw . t) return result

\subsubsection{Lookup Failures}

Lookup failures are handled by converting them to either faults or system call errors, depending on the type of lookup. The following functions perform this conversion.

> capFaultOnFailure :: CPtr -> Bool -> KernelF LookupFailure a ->
>         KernelF Fault a
> capFaultOnFailure cptr rp = rethrowFailure $ CapFault cptr rp

> lookupErrorOnFailure :: Bool -> KernelF LookupFailure a ->
>         KernelF SyscallError a
> lookupErrorOnFailure isSource = rethrowFailure $ FailedLookup isSource

\subsubsection{Silent Failures}

Some failures are silent; the kernel simply aborts the operation.

> ignoreFailure :: Error f => KernelF f () -> Kernel ()
> ignoreFailure = (`catchFailure` const (return ()))

If the expected result is a capability, silenced failures return null capabilities.

> nullCapOnFailure :: Error f => KernelF f Capability -> Kernel Capability
> nullCapOnFailure = flip catchFailure $ const $ return NullCap

If the expected result is a list, silenced failures return the empty list.

> emptyOnFailure :: Error f => KernelF f [a] -> Kernel [a]
> emptyOnFailure m = m `catchFailure` (const $ return [])

Returns the specified constant when execution fails. 

> constOnFailure :: Error f => a -> KernelF f a -> Kernel a
> constOnFailure x m = m `catchFailure` (const $ return x)

When silencing failures using either of the functions above, the type of failure is irrelevant; it only matters that a failure has or has not occurred. The "unifyFailure" function, along with an instance of "Error" for the unit type, allows failures of different types to be handled together.

> unifyFailure :: Error f => KernelF f a -> KernelF () a
> unifyFailure = rethrowFailure $ const ()

> instance Error ()

\subsection{Detecting Failures}

This trivial helper function is used to check that an argument is within an acceptable range.

> rangeCheck :: (Integral a, Integral b) =>
>     a -> b -> b -> KernelF SyscallError ()
> rangeCheck value minV maxV =
>     unless (value >= fromIntegral minV && value <= fromIntegral maxV) $
>         throw $ RangeError (fromIntegral minV) (fromIntegral maxV)


