%
% Copyright 2014, General Dynamics C4 Systems
%
% This software may be distributed and modified according to the terms of
% the GNU General Public License version 2. Note that NO WARRANTY is provided.
% See "LICENSE_GPLv2.txt" for details.
%
% @TAG(GD_GPL)
%

This module defines instances of "PSpaceStorable" for ARM-specific kernel objects. This includes page table and page directory entries, and ASID pools.

> module SEL4.Object.Instances.ARM where

\begin{impdetails}

> import SEL4.Machine.Hardware.ARM(PTE(..), PDE(..))
> import SEL4.Object.Structures
> import SEL4.Model
> import Data.Helpers

\end{impdetails}

> instance PSpaceStorable PDE where
>     makeObject = InvalidPDE
>     injectKO = KOArch . KOPDE
>     projectKO o = case o of 
>                   KOArch (KOPDE p) -> return p
>                   _ -> typeError "PDE" o

> instance PSpaceStorable PTE where
>     makeObject = InvalidPTE
>     injectKO = KOArch . KOPTE
>     projectKO o = case o of 
>                 KOArch (KOPTE p) -> return p
>                 _ -> typeError "PTE" o

> instance PSpaceStorable ASIDPool where
>     makeObject = ASIDPool $
>         funArray (const Nothing) 
>     injectKO = KOArch . KOASIDPool
>     projectKO o = case o of
>         KOArch (KOASIDPool e) -> return e
>         _ -> typeError "ASID pool" o

