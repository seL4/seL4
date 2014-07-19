%
% Copyright 2014, General Dynamics C4 Systems
%
% This software may be distributed and modified according to the terms of
% the GNU General Public License version 2. Note that NO WARRANTY is provided.
% See "LICENSE_GPLv2.txt" for details.
%
% @TAG(GD_GPL)
%

This module contains the physical memory model's representations of the ARM-specific data structures, as well as a type representing a capability to an ARM-specific object.

\begin{impdetails}

This module makes use of the GHC extension allowing declaration of types with no constructors, so GHC language extensions are enabled.

> {-# LANGUAGE EmptyDataDecls, GeneralizedNewtypeDeriving #-}

\end{impdetails}

> module SEL4.Object.Structures.ARM where

\begin{impdetails}

> import SEL4.Machine.RegisterSet
> import SEL4.Machine.Hardware.ARM
> import Data.Array
> import Data.Word(Word32)
> import Data.Bits

\end{impdetails}

\subsection{Capabilities}

There are six ARM-specific capability types: the global ASID control capability, ASID pools, page tables, page directories, and pages.

> data ArchCapability
>     = PageCap {
>         capVPBasePtr :: PPtr Word,
>         capVPRights :: VMRights,
>         capVPSize :: VMPageSize,
>         capVPMappedAddress :: Maybe (ASID, VPtr) }
>     | PageTableCap {
>         capPTBasePtr :: PPtr PTE,
>         capPTMappedAddress :: Maybe (ASID, VPtr) }
>     | PageDirectoryCap {
>         capPDBasePtr :: PPtr PDE,
>         capPDMappedASID :: Maybe ASID }
>     | ASIDControlCap
>     | ASIDPoolCap {
>         capASIDPool :: PPtr ASIDPool,
>         capASIDBase :: ASID }
>     deriving (Eq, Show)

\subsection{Kernel Objects}

The ARM kernel stores one ARM-specific type of object in the PSpace: ASID pools, which are second level nodes in the global ASID table. 

> data ArchKernelObject
>     = KOASIDPool ASIDPool
>     | KOPTE PTE
>     | KOPDE PDE
>     deriving Show

> archObjSize ::  ArchKernelObject -> Int
> archObjSize a = case a of 
>                 KOASIDPool _ -> pageBits
>                 KOPTE _ -> 2 
>                 KOPDE _ -> 2

\subsection{ASID Pools}

An ASID pool is an array of pointers to page directories. This is used to implement virtual ASIDs on ARM; it is not accessed by the hardware.

> newtype ASIDPool = ASIDPool (Array ASID (Maybe (PPtr PDE)))
>     deriving Show

An ASID is an unsigned word. Note that it is a \emph{virtual} address space identifier, and may not correspond to any hardware-defined identifier --- especially on ARMv5 and earlier, where the only identifier implemented in hardware is the 4-bit domain number.

> newtype ASID = ASID Word32
>     deriving (Show, Eq, Ord, Enum, Real, Integral, Num, Bits, Ix, Bounded)

ASIDs are mapped to address space roots by a global two-level table. The actual ASID values are opaque to the user, as are the sizes of the levels of the tables; ASID allocation calls will simply return an error once the available ASIDs are exhausted.

> asidHighBits :: Int
> asidHighBits = 8

> asidLowBits :: Int
> asidLowBits = 10

> asidBits :: Int
> asidBits = asidHighBits + asidLowBits

> asidRange :: (ASID, ASID)
> asidRange = (0, (1 `shiftL` asidBits) - 1)

> asidHighBitsOf :: ASID -> ASID
> asidHighBitsOf asid = (asid `shiftR` asidLowBits) .&. mask asidHighBits


