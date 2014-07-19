%
% Copyright 2014, General Dynamics C4 Systems
%
% This software may be distributed and modified according to the terms of
% the GNU General Public License version 2. Note that NO WARRANTY is provided.
% See "LICENSE_GPLv2.txt" for details.
%
% @TAG(GD_GPL)
%

This module contains operations on machine-specific object types for the Haskell-based CPU simulator. Since there are no such object types, the functions here are all undefined, and should never be called.

> module SEL4.Object.ObjectType.HaskellCPU where

> import SEL4.Machine
> import SEL4.Model.StateData
> import SEL4.API.Types
> import SEL4.Object.Types

> retypeRegion :: PPtr -> Int -> ObjectType -> Kernel Int
> retypeRegion = undefined

> detypeObject :: PPtr -> Int -> ObjectType -> Kernel (Maybe ThreadSuicide)
> detypeObject = undefined

> invokeObject :: Word -> [Word] -> CPtr -> CTEPtr -> Capability ->
>         KernelF Fault (Maybe (Word, [Word]))
> invokeObject = undefined


