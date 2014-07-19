%
% Copyright 2014, General Dynamics C4 Systems
%
% This software may be distributed and modified according to the terms of
% the GNU General Public License version 2. Note that NO WARRANTY is provided.
% See "LICENSE_GPLv2.txt" for details.
%
% @TAG(GD_GPL)
%

This module contains definitions of the types and functions that the kernel uses for interaction with the hardware, such as performing memory accesses and controlling virtual memory mappings.

The functions and types in this module are required for all architectures. Specific architectures may define additional functions and types in their respective modules.

\begin{impdetails}

We use the C preprocessor to select a target architecture. Also, this file makes use of the GHC extension allowing derivation of arbitrary type classes for types defined with "newtype".

> {-# LANGUAGE CPP, GeneralizedNewtypeDeriving #-}

\end{impdetails}

> module SEL4.Machine.Hardware where

\begin{impdetails}

> import SEL4.Machine.RegisterSet
> import Control.Monad(liftM)

> import Data.Ix

\end{impdetails}

The architecture-specific definitions are imported qualified with the "Arch" prefix.

> import qualified SEL4.Machine.Hardware.TARGET as Arch

\subsection{Types}

\subsubsection{Hardware Monad}

Each simulator must define a monad that encapsulates the state of the underlying hardware.

> type MachineMonad = Arch.MachineMonad

\subsubsection{Physical Addresses}

Depending on the architecture, real physical addresses may be the same as the addresses the kernel uses to access kernel objects, or they may be offset by a constant. The "PAddr" type is used to represent a real physical address; functions are provided that convert between this and the kernel pointer type, "PPtr".

> type PAddr = Arch.PAddr

> ptrFromPAddr :: PAddr -> PPtr a
> ptrFromPAddr = Arch.ptrFromPAddr

> addrFromPPtr :: PPtr a -> PAddr
> addrFromPPtr = Arch.addrFromPPtr

\subsubsection{Interrupts}

An interrupt request from an external device, or from the CPU's timer, is represented by the type "IRQ".

> newtype IRQ = IRQ Arch.IRQ
>     deriving (Enum, Bounded, Ord, Ix, Eq, Show)

The maximum and minimum IRQ are given explicit constant names here. In Haskell, these are extracted from instantiation of IRQ into the Bounded class. In the formalisation, these constants are specified directly.

> minIRQ :: IRQ
> minIRQ = minBound

> maxIRQ :: IRQ
> maxIRQ = maxBound

\subsubsection{Virtual Memory Faults}

Most architectures provide some information about virtual memory faults in hardware registers, and other information is implied by the hardware's choice between a set of different trap handlers. The latter is represented in this model by the following data type.

> type VMFaultType = Arch.VMFaultType

\subsection{Hardware Operations}

The simulator must define several operations on the underlying hardware. These operations are used by the kernel to determine the configuration of the hardware, and to perform privileged operations such as virtual memory and interrupt management.

\subsubsection{Hardware Configuration}

These operations are used to determine the configuration of the machine:

\begin{itemize}
\item the number of address bits covered by a virtual page;

> pageBits :: Int
> pageBits = Arch.pageBits

\item the start and end addresses (plus one) of the regions of the physical address space occupied by free physical memory (ie, physical memory other than the kernel code and stack);

> getMemoryRegions :: MachineMonad [(PAddr, PAddr)]
> getMemoryRegions = Arch.getMemoryRegions

\item the start and end addresses (plus one) of the regions of the physical address space occupied by memory-mapped I/O devices that are not accessed by the kernel;

> getDeviceRegions :: MachineMonad [(PAddr, PAddr)]
> getDeviceRegions = Arch.getDeviceRegions

\item and the base addresses, kernel-accessible addresses and sizes (in address bits) of the memory-mapped I/O devices that the kernel uses internally.

> getKernelDevices :: MachineMonad [(PAddr, PPtr Word)]
> getKernelDevices = Arch.getKernelDevices

\end{itemize}

\subsubsection{Memory Accesses}

These operations load or store a word in the physical address space.

> loadWord :: PPtr Word -> MachineMonad Word
> loadWord = Arch.loadWord

> storeWord :: PPtr Word -> Word -> MachineMonad ()
> storeWord = Arch.storeWord

This storeWord operation is for caching virtual memory/page table writes in simulator environments. (Instead on relying on PTE/PDE values in PSpace)

> storeWordVM :: PPtr Word -> Word -> MachineMonad ()
> storeWordVM = Arch.storeWordVM

\subsubsection{Virtual Memory Management}

Before a user task is given direct access to the contents of a memory region, the region must be cleared, to avoid leaking information belonging to the kernel or another user task. This is an architecture-specific operation because it may require a cache flush.

> clearMemory :: PPtr Word -> Int -> MachineMonad ()
> clearMemory = Arch.clearMemory

This function is called to free a region of user-memory after use.

> freeMemory :: PPtr Word -> Int -> MachineMonad ()
> freeMemory = Arch.freeMemory

The following constant is used to determine the correct page colouring when allocating the initial task's IPC buffer; it is the number of bits of the virtual page number that are significant in determining the page colour. It is zero if there are no page colouring restrictions.

> pageColourBits :: Int
> pageColourBits = Arch.pageColourBits

\subsubsection{Interrupts}

After receiving an Interrupt event, or at preemption points, the kernel calls this function to check for any pending interrupts which might preempt the kernel. If there are any, it returns the one with highest priority. In a real kernel, this would be implemented by briefly enabling interrupts.

> getActiveIRQ :: MachineMonad (Maybe IRQ)
> getActiveIRQ = liftM (liftM IRQ) Arch.getActiveIRQ

This function is used to enable or disable a specific interrupt, either when its handler is set or cleared, or when an IRQ has been forwarded to a user level handler and not yet acknowledged. If the argument is "True", delivery of the specified interrupt to the kernel will be disabled; otherwise it will be enabled.

> maskInterrupt :: Bool -> IRQ -> MachineMonad ()
> maskInterrupt mask' (IRQ irq) = Arch.maskInterrupt mask' irq

This function is used by the kernel to acknowledge an interrupt, after it has been handled by the kernel or forwarded to user level. If a user level handler is called, the interrupt will be disabled before calling this function.

> ackInterrupt :: IRQ -> MachineMonad ()
> ackInterrupt (IRQ irq) = Arch.ackInterrupt irq

\subsubsection{Timers}

The timer interval is set at boot time by calling this function. It returns the IRQ that is used for timer interrupts.

> configureTimer :: MachineMonad IRQ
> configureTimer = liftM IRQ Arch.configureTimer

The kernel calls this function after handling a timer interrupt, but before acknowledging it. It should take whatever action is necessary to clear the interrupt and reset the timer.

> resetTimer :: MachineMonad ()
> resetTimer = Arch.resetTimer

\subsubsection{Debugging}

This operation prints a debugging message to the console.

> debugPrint :: String -> MachineMonad ()
> debugPrint = Arch.debugPrint

\subsection{User Context Operations}

There are also two functions that perform specific operations on the register set. These are used to fetch and set the program counter. We must use functions here, rather than aliases for register names, because the handling of these registers may differ significantly depending on the architecture and the type of fault. These functions:

\begin{itemize}
\item fetch the address of the instruction that is currently executing, from the user level thread's point of view (such as a system call instruction, or an instruction which accesses virtual memory and has faulted);

> getRestartPC :: UserMonad Word
> getRestartPC = Arch.getRestartPC

\item and set the address of the next instruction to be executed, which by default is the instruction after the current one.

> setNextPC :: Word -> UserMonad ()
> setNextPC = Arch.setNextPC

\end{itemize}

The value read by "getRestartPC" should also appear in "frameRegisters", while the value set by "setNextPC" should not be directly accessible to user level code. This is necessary even if the hardware uses a single physical register for both values. It ensures that "CopyRegisters" reads the current instruction address and sets the next instruction address, no matter how the thread last entered the kernel.

\subsection{Constants}

The constant "nullPointer" is a physical pointer guaranteed to be invalid.

> nullPointer :: PPtr a
> nullPointer = PPtr 0


