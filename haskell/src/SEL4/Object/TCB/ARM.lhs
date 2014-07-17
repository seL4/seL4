%
% Copyright 2014, General Dynamics C4 Systems
%
% This software may be distributed and modified according to the terms of
% the GNU General Public License version 2. Note that NO WARRANTY is provided.
% See "LICENSE_GPLv2.txt" for details.
%
% @TAG(GD_GPL)
%

This module contains ARM-specific TCB management functions. Specifically, these functions are used by the "CopyRegisters" operation to transfer ARM-specific subsets of the register set.

There are presently no ARM-specific register subsets defined, but in future this may be extended to transfer floating point registers and other coprocessor state.

> module SEL4.Object.TCB.ARM where

\begin{impdetails}

> import SEL4.Machine
> import SEL4.Model
> import SEL4.Object.Structures
> import SEL4.API.Failures
> import SEL4.API.Invocation.ARM

> import Data.Word(Word8)

\end{impdetails}

> decodeTransfer :: Word8 -> KernelF SyscallError CopyRegisterSets
> decodeTransfer _ = return ARMNoExtraRegisters

> performTransfer :: CopyRegisterSets -> PPtr TCB -> PPtr TCB -> Kernel ()
> performTransfer _ _ _ = return ()

