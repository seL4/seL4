%
% Copyright 2014, General Dynamics C4 Systems
%
% This software may be distributed and modified according to the terms of
% the GNU General Public License version 2. Note that NO WARRANTY is provided.
% See "LICENSE_GPLv2.txt" for details.
%
% @TAG(GD_GPL)
%

> module SEL4.Config where

> import SEL4.Machine.RegisterSet

This module contains configuration parameters that may be set at compile time to suit different applications.

\subsection{Initialisation Parameters}

The initial task is guaranteed to receive at least this many empty capability slots, allocated by the kernel during initialisation. 

> minFreeSlots :: Int
> minFreeSlots = 128

The initial task is guaranteed to receive at least this many page-sized untyped capabilities.

> minSmallBlocks :: Int
> minSmallBlocks = 16

The size of the root node of the initial capability space determines the area of the initial address space that may be used by mappings for the root task.

> rootCNodeSize :: Int
> rootCNodeSize = 8

\subsection{Timing Parameters}

The default number of timer ticks between scheduling and preemption. Note that the real time that elapses between timer ticks is implementation-defined.

> timeSlice :: Int
> timeSlice = 5

The default number of security domains.

> numDomains :: Int
> numDomains = 16

The number of priority levels per domain. There is one ready queue per domain and per priority.

> numPriorities :: Int
> numPriorities = 256

To limit the impact of "Retype" calls on interrupt latency, there is a fixed maximum number of capabilities that may be created by a single "Retype" operation. This limit may be changed at kernel configuration time.

> retypeFanOutLimit :: Word
> retypeFanOutLimit = 256

