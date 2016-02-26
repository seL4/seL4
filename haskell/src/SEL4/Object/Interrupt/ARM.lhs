%
% Copyright 2014, General Dynamics C4 Systems
%
% This software may be distributed and modified according to the terms of
% the GNU General Public License version 2. Note that NO WARRANTY is provided.
% See "LICENSE_GPLv2.txt" for details.
%
% @TAG(GD_GPL)
%

This module defines the machine-specific interrupt handling routines for the ARM.
Apparently ARM does not have any.

> module SEL4.Object.Interrupt.ARM where

\begin{impdetails}

> import SEL4.Machine
> import SEL4.Model
> import SEL4.Object.Structures
> import SEL4.API.Failures
> import SEL4.API.Invocation.ARM as ArchInv

\end{impdetails}

> decodeIRQControlInvocation :: Word -> [Word] -> PPtr CTE -> [Capability] ->
>     KernelF SyscallError ArchInv.IRQControlInvocation
> decodeIRQControlInvocation _ _ _ _ = throw IllegalOperation

> performIRQControl :: ArchInv.IRQControlInvocation -> KernelP ()
> performIRQControl _ = fail "performIRQControl: not defined"

> checkIRQ :: Word -> KernelF SyscallError ()
> checkIRQ irq = rangeCheck irq (fromEnum minIRQ) (fromEnum maxIRQ)

