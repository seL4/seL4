%
% Copyright 2014, General Dynamics C4 Systems
%
% This software may be distributed and modified according to the terms of
% the GNU General Public License version 2. Note that NO WARRANTY is provided.
% See "LICENSE_GPLv2.txt" for details.
%
% @TAG(GD_GPL)
%

This module defines the interface that the kernel model uses to determine the properties of the architecture of the machine being simulated, and particularly the types, names and functions of the general purpose registers.

\begin{impdetails}

We use the C preprocessor to select a target architecture. Also, this file makes use of the GHC extension allowing derivation of arbitrary type classes for types defined with "newtype", so GHC language extensions are enabled.

> {-# LANGUAGE CPP, GeneralizedNewtypeDeriving #-}

\end{impdetails}

> module SEL4.Machine.RegisterSet where

\begin{impdetails}

> import Data.Bits
> import Data.Array
> import Data.Helpers
> import Foreign.Storable

> import Control.Monad.State(State, gets, modify)

\end{impdetails}

The architecture-specific definitions are imported qualified with the "Arch" prefix.

> import qualified SEL4.Machine.RegisterSet.TARGET as Arch

\subsection{Types}

The architecture must define two types: one for the type of the machine's word, and one for the set of valid register names.

\subsubsection{Word Types}

The type "Word" represents the native word size of the modelled machine. It must be an instance of the type classes that allow bitwise arithmetic, use as an integer, use as a foreign type (for the external simulator interface), and conversion to a string.

> newtype Word = Word Arch.Word
>     deriving (Eq, Ord, Enum, Real, Num, Bits, Integral, Bounded, Storable, Show)

\subsubsection{Register Names}

The "Register" type defines a bounded, enumerated set of register names.

> newtype Register = Register Arch.Register
>     deriving (Eq, Ord, Enum, Bounded, Ix, Show)

\subsubsection{Pointers}

To enforce explicit casts between pointer types, "newtype" is used to
declare types for various types of user and kernel pointers. These
types derive a number of basic type classes allowing sorting,
comparison for equality, pointer arithmetic and printing.

\begin{impdetails} Note that they do not derive "Integral", despite
being integers; this is to avoid allowing them to be cast using
"fromIntegral", which does not indicate that the cast is to or from a
pointer type.

Also, these types derive "Num", which requires a GHC extension.
\end{impdetails}

The types defined here are used for kernel and user pointers. Depending on the architecture, kernel pointers may either be unmodified physical pointers, or virtual pointers into a region that is direct-mapped (with a fixed offset) to physical memory. Note that another pointer type, "PAddr", is defined by the "SEL4.Machine.Hardware" module in \autoref{sec:machine.hardware}; it always represents a physical pointer, and may or may not be equivalent to "PPtr a".

> newtype PPtr a = PPtr { fromPPtr :: Word }
>         deriving (Show, Eq, Num, Bits, Ord, Enum, Bounded)

> newtype VPtr = VPtr { fromVPtr :: Word }
>         deriving (Show, Eq, Num, Bits, Ord, Enum, Bounded)

\subsubsection{User-level Context}

The representation of the user-level context of a thread is an array of machine words, indexed by register name.

> newtype UserContext = UC { fromUC :: Array Register Word }
>         deriving Show

\subsection{Monads}

"UserMonad" is a specialisation of "Control.Monad.State", used to execute functions that have access only to the user-level context of a single thread.

> type UserMonad = State UserContext

\subsection{Functions and Constants}

\subsubsection{Register Set}

The following functions and constants define registers or groups of user-level registers that are used for specific purposes by the kernel.

\begin{description}
\item[The message information register] contains metadata about the contents of an IPC message, such as the length of the message and whether a capability is attached.

> msgInfoRegister :: Register
> msgInfoRegister = Register Arch.msgInfoRegister

\item[Message registers] are used to hold the message being sent by an object invocation.

This list may be empty, though it should contain as many registers as possible. Message words that do not fit in these registers will be transferred in a buffer in user-accessible memory.

> msgRegisters :: [Register]
> msgRegisters = map Register Arch.msgRegisters

\item[The capability register] is used when performing a system call, to specify the location of the invoked capability.

> capRegister :: Register
> capRegister = Register Arch.capRegister

\item[The badge register] is used to return the badge of the capability from which a message was received. This is typically the same as "capRegister".

> badgeRegister :: Register
> badgeRegister = Register Arch.badgeRegister

\item[The frame registers] are the registers that are used by the architecture's function calling convention. They generally include the current instruction and stack pointers, and the argument registers. They appear at the beginning of a "ReadRegisters" or "WriteRegisters" message, and are one of the two subsets of the integer registers that can be copied by "CopyRegisters".

> frameRegisters :: [Register]
> frameRegisters = map Register Arch.frameRegisters

\item[The general-purpose registers] include all registers that are not in "frameRegisters", except any kernel-reserved or constant registers (such as the MIPS "zero", "k0" and "k1" registers). They appear after the frame registers in a "ReadRegisters" or "WriteRegisters" message, and are the second of two subsets of the integer registers that can be copied by "CopyRegisters".

> gpRegisters :: [Register]
> gpRegisters = map Register Arch.gpRegisters

\item[An exception message] is sent by the kernel when a hardware exception is raised by a user-level thread. The message contains registers from the current user-level state, as specified by this list. Two architecture-defined words, specifying the type and cause of the exception, are appended to the message. The reply may contain updated values for these registers.

> exceptionMessage :: [Register]
> exceptionMessage = map Register Arch.exceptionMessage

\item[A syscall message] is sent by the kernel when a user thread performs a system call that is not recognised by seL4. The message contains registers from the current user-level state, as specified by this list. A word containing the system call number is appended to the message. The reply may contain updated values for these registers.

> syscallMessage :: [Register]
> syscallMessage = map Register Arch.syscallMessage

\end{description}

\subsubsection{User-level Context}

A new user-level context is a list of values for the machine's registers. Registers are generally initialised to 0, but there may be machine-specific initial values for certain registers.

> newContext :: UserContext
> newContext = UC $ (funArray $ const 0)//initContext
>     where initContext = map (\(r,v) -> (Register r, Word v)) Arch.initContext

Functions are provided to get and set a single register.

> getRegister :: Register -> UserMonad Word
> getRegister r = gets $ (!r) . fromUC

> setRegister :: Register -> Word -> UserMonad ()
> setRegister r v = modify $ UC . (//[(r, v)]) . fromUC

On some architectures, the thread context may include registers that may be modified by user level code, but cannot safely be given arbitrary values. For example, some of the bits in the ARM architecture's CPSR are used for conditional execution, and others enable kernel mode. This function is used to filter out any bits that should not be modified by user level programs.

> sanitiseRegister :: Register -> Word -> Word
> sanitiseRegister (Register r) (Word w) = Word $ Arch.sanitiseRegister r w

\subsubsection{Miscellaneous}

The "mask" function is a trivial function which, given a number of bits, returns a word with that number of low-order bits set.

> mask :: (Bits w, Num w) => Int -> w
> mask bits = bit bits - 1


