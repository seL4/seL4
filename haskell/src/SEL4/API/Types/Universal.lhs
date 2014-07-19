%
% Copyright 2014, General Dynamics C4 Systems
%
% This software may be distributed and modified according to the terms of
% the GNU General Public License version 2. Note that NO WARRANTY is provided.
% See "LICENSE_GPLv2.txt" for details.
%
% @TAG(GD_GPL)
%

This module defines the set of kernel object types that are available on all implementations.

> module SEL4.API.Types.Universal where

\subsection{Types}

\subsubsection{Object Types}

The following is the definition of the five object types that are always available, as well as untyped memory. This enumeration may be extended on some platforms to add platform-specific object types.

> data APIObjectType
>         = Untyped
>         | TCBObject
>         | EndpointObject
>         | AsyncEndpointObject
>         | CapTableObject
>         deriving (Enum, Bounded, Eq, Show)

> tcbBlockSizeBits :: Int
> tcbBlockSizeBits = 9

> epSizeBits :: Int
> epSizeBits = 4

> aepSizeBits :: Int
> aepSizeBits = 4

> cteSizeBits :: Int
> cteSizeBits = 4

> apiGetObjectSize :: APIObjectType -> Int -> Int
> apiGetObjectSize Untyped size = size
> apiGetObjectSize TCBObject _ = tcbBlockSizeBits
> apiGetObjectSize EndpointObject _ = epSizeBits
> apiGetObjectSize AsyncEndpointObject _ = aepSizeBits
> apiGetObjectSize CapTableObject size = cteSizeBits + size



