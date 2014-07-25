%
% Copyright 2014, General Dynamics C4 Systems
%
% This software may be distributed and modified according to the terms of
% the GNU General Public License version 2. Note that NO WARRANTY is provided.
% See "LICENSE_GPLv2.txt" for details.
%
% @TAG(GD_GPL)
%

This module defines the machine-specific invocations for the ARM.

\begin{impdetails}

This module makes use of the GHC extension allowing data types with no constructors.

> {-# LANGUAGE EmptyDataDecls #-}

\end{impdetails}

> module SEL4.API.Invocation.ARM where

\begin{impdetails}

> import SEL4.Machine
> import SEL4.Machine.Hardware.ARM hiding (PAddr)
> import SEL4.Object.Structures

\end{impdetails}

\subsection{ARM-Specific Objects}

There are five ARM-specific object types; however, only four of them may be invoked. These are the page table, page, ASID control, and ASID pool objects.

> data Invocation
>     = InvokePageTable PageTableInvocation
>     | InvokePageDirectory PageDirectoryInvocation
>     | InvokePage PageInvocation
>     | InvokeASIDControl ASIDControlInvocation
>     | InvokeASIDPool ASIDPoolInvocation
>     deriving Show

> data PageTableInvocation
>     = PageTableMap {
>         ptMapCap :: Capability,
>         ptMapCTSlot :: PPtr CTE,
>         ptMapPDE :: PDE,
>         ptMapPDSlot :: PPtr PDE }
>     | PageTableUnmap {
>         ptUnmapCap :: ArchCapability,
>         ptUnmapCapSlot :: PPtr CTE }
>     deriving Show

> data PageDirectoryInvocation
>     = PageDirectoryFlush {
>         pdFlushType :: FlushType,
>         pdFlushStart :: VPtr,
>         pdFlushEnd :: VPtr,
>         pdFlushPStart :: PAddr,
>         pdFlushPD :: PPtr PDE,
>         pdFlushASID :: ASID }
>     | PageDirectoryNothing
>     deriving Show

> -- FIXME: should we consolidate start, end into a tuple
> data PageInvocation
>     = PageMap {
>         pageMapASID :: ASID,
>         pageMapCap :: Capability,
>         pageMapCTSlot :: PPtr CTE,
>         pageMapEntries :: Either (PTE, [PPtr PTE]) (PDE, [PPtr PDE]) }
>     | PageRemap {
>         pageRemapASID :: ASID,
>         pageRemapEntries :: Either (PTE, [PPtr PTE]) (PDE, [PPtr PDE]) }
>     | PageUnmap {
>         pageUnmapCap :: ArchCapability,
>         pageUnmapCapSlot :: PPtr CTE }
>     | PageFlush {
>         pageFlushType :: FlushType,
>         pageFlushStart :: VPtr,
>         pageFlushEnd :: VPtr,
>         pageFlushPStart :: PAddr,
>         pageFlushPD :: PPtr PDE,
>         pageFlushASID :: ASID } 
>     | PageGetAddr {
>         pageGetBasePtr :: PPtr Word }
>     deriving Show

> data FlushType
>     = Clean | Invalidate | CleanInvalidate | Unify
>     deriving Show

> data ASIDControlInvocation
>     = MakePool {
>         makePoolFrame :: PPtr (),
>         makePoolSlot :: PPtr CTE,
>         makePoolParent :: PPtr CTE,
>         makePoolBase :: ASID }
>     deriving Show

> data ASIDPoolInvocation
>     = Assign {
>         assignASID :: ASID,
>         assignASIDPool :: PPtr ASIDPool,
>         assignASIDCTSlot :: PPtr CTE }
>     deriving Show

\subsection{Interrupt Control}

The ARM platform presently does not require any additional interrupt control calls.

> data InterruptControl
> instance Show InterruptControl

\subsection{Additional Register Subsets}

The ARM platform currently does not define any additional register sets for the "CopyRegisters" operation. This may be changed in future to support a floating point unit.

> data CopyRegisterSets = ARMNoExtraRegisters
>     deriving Show


