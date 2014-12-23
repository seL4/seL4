%
% Copyright 2014, General Dynamics C4 Systems
%
% This software may be distributed and modified according to the terms of
% the GNU General Public License version 2. Note that NO WARRANTY is provided.
% See "LICENSE_GPLv2.txt" for details.
%
% @TAG(GD_GPL)
%

Each thread has a virtual memory address space, possibly shared with other threads.

The representation of the virtual address space is implementation-defined. On many architectures --- those with software-loaded TLBs, for example --- it will simply be a second CSpace structure, possibly identical to or a subtree of the thread's main CSpace. On others, such as those with hardware-defined page table structures, it must be constructed using implementation-defined kernel objects.

\begin{impdetails}

We use the C preprocessor to select a target architecture.

> {-# LANGUAGE CPP #-}

\end{impdetails}

> module SEL4.Kernel.VSpace where

\begin{impdetails}

% {-# BOOT-IMPORTS: SEL4.Machine SEL4.Model SEL4.Object.Structures SEL4.API.Failures #-}
% {-# BOOT-EXPORTS: lookupIPCBuffer isValidVTableRoot checkValidIPCBuffer #-} 

> import SEL4.Machine
> import SEL4.Model
> import SEL4.Object
> import SEL4.API.Failures
> import SEL4.API.Types
> import {-# SOURCE #-} SEL4.Kernel.Init

\end{impdetails}

The architecture-specific definitions are imported qualified with the "Arch" prefix.

> import qualified SEL4.Kernel.VSpace.TARGET as Arch

\subsection{Implementation-defined Functions}

This module defines architecture-specific virtual memory management procedures. The operations required are:

\begin{itemize}

\item preparing the virtual memory environment, if any, that the kernel requires to run;

> initKernelVM :: Kernel ()
> initKernelVM = Arch.mapKernelWindow

> initPlatform :: Kernel ()
> initPlatform = do
>   doMachineOp $ configureTimer
>   initL2Cache

> initL2Cache = return ()

> initCPU :: Kernel ()
> initCPU = Arch.activateGlobalPD


> createIPCBufferFrame :: Capability -> VPtr -> KernelInit Capability
> createIPCBufferFrame  = Arch.createIPCBufferFrame

> createBIFrame  = Arch.createBIFrame

> createFramesOfRegion :: Capability -> Region -> Bool -> VPtr -> KernelInit () 
> createFramesOfRegion = Arch.createFramesOfRegion

> createITPDPTs :: Capability -> VPtr -> VPtr -> KernelInit Capability
> createITPDPTs = Arch.createITPDPTs

> writeITPDPTs :: Capability -> Capability -> KernelInit ()
> writeITPDPTs = Arch.writeITPDPTs 

> createITASIDPool :: Capability -> KernelInit Capability
> createITASIDPool = Arch.createITASIDPool

> writeITASIDPool :: Capability -> Capability -> Kernel ()
> writeITASIDPool = Arch.writeITASIDPool

> createDeviceFrames :: Capability -> KernelInit ()
> createDeviceFrames = Arch.createDeviceFrames


\item handling virtual memory faults, given the current thread, the faulting address, and a boolean value that is true for write accesses;

> handleVMFault :: PPtr TCB -> VMFaultType -> KernelF Fault ()
> handleVMFault = Arch.handleVMFault

\item determining whether a given capability is a valid VSpace root;

> isValidVTableRoot :: Capability -> Bool
> isValidVTableRoot = Arch.isValidVTableRoot

\item determining whether a given capability is a valid IPC buffer frame that can contain the given virtual address;

> checkValidIPCBuffer :: VPtr -> Capability -> KernelF SyscallError ()
> checkValidIPCBuffer = Arch.checkValidIPCBuffer

\item locating the IPC buffer for a given thread;

> lookupIPCBuffer :: Bool -> PPtr TCB -> Kernel (Maybe (PPtr Word))
> lookupIPCBuffer = Arch.lookupIPCBuffer

\end{itemize}

