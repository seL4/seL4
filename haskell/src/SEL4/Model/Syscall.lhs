%
% Copyright 2014, General Dynamics C4 Systems
%
% This software may be distributed and modified according to the terms of
% the GNU General Public License version 2. Note that NO WARRANTY is provided.
% See "LICENSE_GPLv2.txt" for details.
%
% @TAG(GD_GPL)
%

This module contains utility functions used in system call implementations.

> module SEL4.Model.Syscall where

\begin{impdetails}

> import SEL4.API.Failures
> import SEL4.Model.StateData
> import SEL4.Model.Failures
> import SEL4.Model.Preemption
> import Control.Monad.Error

\end{impdetails}

System calls in seL4 have three stages: one which may fault, one which may encounter system call errors, and one which cannot fail but may be interrupted. Generally the first is used to look up capabilities, the second to decode arguments and check for possible failures, and the third to perform the operation itself. If either of the first two stages fails, a failure handler runs instead of the following stages.

The "syscall" function lifts code into the appropriate monads, and handles faults and errors.

> syscall :: KernelF Fault a -> (Fault -> Kernel c) ->
>         (a -> KernelF SyscallError b) -> (SyscallError -> Kernel c) ->
>         (b -> KernelP c) -> KernelP c
> syscall mFault hFault mError hError mFinalise = do
>     rFault <- withoutPreemption $ runErrorT mFault
>     case rFault of
>         Left f -> withoutPreemption $ hFault f
>         Right a -> do
>             rError <- withoutPreemption $ runErrorT $ mError a
>             case rError of
>                 Left e -> withoutPreemption $ hError e
>                 Right b -> mFinalise b


