%
% Copyright 2014, General Dynamics C4 Systems
%
% This software may be distributed and modified according to the terms of
% the GNU General Public License version 2. Note that NO WARRANTY is provided.
% See "LICENSE_GPLv2.txt" for details.
%
% @TAG(GD_GPL)
%

> module SEL4.Machine.RegisterSet.HaskellCPU where

> import qualified Data.Word
> import Data.Array

> data Register = 
>     PC | SP |
>     -- syscall argument registers
>     AR0 | AR1 | AR2 | AR3 | AR4 | AR5 | AR6 | AR7 |
>     -- general purpose integer registers
>     R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 |
>     R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15 |
>     R16 | R17 | R18 | R19 | R20 | R21 | R22 | R23 |
>     R24 | R25 | R26 | R27 | R28 | R29 | R30 | R31
>     deriving (Eq, Enum, Bounded, Ord, Ix, Show)

> type Word = Data.Word.Word32

> msgInfoRegister = AR1
> msgRegisters = [AR2 .. AR7]
> capRegister = AR0
> badgeRegister = AR0
> frameRegisters = PC : SP : [AR0 .. AR7]
> gpRegisters = [R0 .. R31]


