%
% Copyright 2014, General Dynamics C4 Systems
%
% This software may be distributed and modified according to the terms of
% the GNU General Public License version 2. Note that NO WARRANTY is provided.
% See "LICENSE_GPLv2.txt" for details.
%
% @TAG(GD_GPL)
%

This module specifies the user-level interface to the various object types defined by the kernel. Parts of this interface are architecture-specific; they are defined in the other modules in the "SEL4.API.Types" branch of the module hierarchy.

\begin{impdetails}

We use the C preprocessor to select a target architecture. Also, this file makes use of the GHC extension allowing derivation of arbitrary type classes for types defined with "newtype".

> {-# LANGUAGE CPP, GeneralizedNewtypeDeriving #-}

\end{impdetails}

> module SEL4.API.Types (
>         module SEL4.API.Types,
>         module SEL4.API.Types.Universal,
>     ) where

\begin{impdetails}

> import SEL4.Machine

> import Data.Bits
> import Data.Word(Word8)

\end{impdetails}

The architecture-specific definitions are imported qualified with the "Arch" prefix.

> import qualified SEL4.API.Types.TARGET as Arch

\subsection{Object Types}

User-allocated memory can contain objects of several kernel-defined types, or be untyped. The set of defined types is partly platform-specific --- it includes some universal types, and possibly some additional platform-defined types. The object types that are available on all architectures are defined by "APIObjectType"; for implementation reasons, that type is defined in the "Universal" module.

> import SEL4.API.Types.Universal(APIObjectType(..))

The following is a type alias for the platform-specific enumeration of all valid object types.

> type ObjectType = Arch.ObjectType

> getObjectSize :: ObjectType -> Int -> Int
> getObjectSize = Arch.getObjectSize

The following functions are used to convert between the above two types.

> fromAPIType :: APIObjectType -> ObjectType
> fromAPIType = Arch.fromAPIType

> toAPIType :: ObjectType -> Maybe APIObjectType
> toAPIType = Arch.toAPIType

The following constant defines the default page type for the architecture, used by the kernel's initialisation code to map the root task's pages. It must have the size defined by "pageBits".

> pageType :: ObjectType
> pageType = Arch.pageType

\subsection{Capability Rights}

This is a set of boolean values that specifies the operations that may
be performed using a capability.

> data CapRights = CapRights {

The rights are:

\begin{itemize} 

\item the right to write or send data to an object, and to retain this right on received capabilities;

>     capAllowWrite,

\item the right to read or receive data from an object;

>     capAllowRead,

\item and the right to send capabilities via IPC. 

>     capAllowGrant :: Bool }
>     deriving (Show, Eq)

\end{itemize}

These are the default values for rights and right masks, with all or none of the bits set.

> allRights :: CapRights
> allRights = CapRights True True True

> noRights :: CapRights
> noRights = CapRights False False False

The following function finds the intersection of two sets of capability rights.

> andCapRights :: CapRights -> CapRights -> CapRights
> andCapRights (CapRights a1 a2 a3) (CapRights b1 b2 b3) =
>         CapRights (a1&&b1) (a2&&b2) (a3&&b3)

A set of capability rights may be converted to or from a machine word.

> rightsFromWord :: Word -> CapRights
> rightsFromWord p = 
>          CapRights (p `testBit` 0) (p `testBit` 1) (p `testBit` 2)

> wordFromRights :: CapRights -> Word
> wordFromRights (CapRights r1 r2 r3) =
>         (bitIf r1 0) .|. (bitIf r2 1) .|. (bitIf r3 2)
>         where bitIf b n = if b then bit n else 0

\subsection{Security Domains}

The current security domain is represented by an 8-bit unsigned integer.

> type Domain = Word8

\subsection{Thread Priority}

The priority of a thread is represented by an 8-bit unsigned integer.

> type Priority = Word8

\subsection{Capability References}

The type "CPtr" is a reference to a capability in a user-level thread's capability space; that is, a \emph{capability pointer}.

> newtype CPtr = CPtr { fromCPtr :: Word }
>         deriving (Show, Eq, Num, Bits, Ord, Bounded)

\subsection{Message Parameters}

The first word of a message contains information about the contents and type of the message. The kernel may modify this information if it is unable to transfer the message as is; for example, the message length will be reduced and any extra capabilities omitted if either the sender or the receiver has no valid IPC buffer.

> data MessageInfo = MI {
>         msgLength :: Word,
>         msgExtraCaps :: Word,
>         msgCapsUnwrapped :: Word,
>         msgLabel :: Word }
>     deriving Show

> messageInfoFromWord :: Word -> MessageInfo
> messageInfoFromWord w = MI {
>         msgLength = if msgLen > msgMaxLength then msgMaxLength else msgLen,
>         msgExtraCaps = (w `shiftR` msgLengthBits) .&.
>             (bit msgExtraCapBits - 1),
>         msgCapsUnwrapped = w `shiftR` (msgLengthBits + msgExtraCapBits) .&.
>             (bit msgMaxExtraCaps - 1),
>         msgLabel = w `shiftR` otherBits }
>         where otherBits = msgLengthBits + msgExtraCapBits + msgMaxExtraCaps
>               msgLen    = w .&. (bit msgLengthBits - 1)

> wordFromMessageInfo :: MessageInfo -> Word
> wordFromMessageInfo mi = label .|. extra .|. un .|. len
>     where
>         len = msgLength mi
>         extra = msgExtraCaps mi `shiftL` msgLengthBits
>         un = msgCapsUnwrapped mi `shiftL` (msgLengthBits + msgExtraCapBits)
>         label = msgLabel mi `shiftL` otherBits
>         otherBits = msgLengthBits + msgExtraCapBits + msgMaxExtraCaps

The maximum number of message registers transferred between threads by an IPC operation.

> msgLengthBits :: Int
> msgLengthBits = 7

> msgMaxLength :: (Num a, Bits a) => a
> msgMaxLength = 120

The maximum number of capabilities passed as arguments to a method invocation. This does not include the capability that specifies the recipient.

> msgExtraCapBits :: Int
> msgExtraCapBits = 2

> msgMaxExtraCaps :: (Num a, Bits a) => a
> msgMaxExtraCaps = bit msgExtraCapBits - 1

> msgAlignBits :: Int
> msgAlignBits = 9

\subsection{Capability Transfers}

Each thread has an IPC buffer, which contains message data that does not fit in the available registers of the host architecture. It also contains information about the source or destination of an IPC capability transfer, defined by the following structure.

> data CapTransfer = CT {
>         ctReceiveRoot :: CPtr,
>         ctReceiveIndex :: CPtr,
>         ctReceiveDepth :: Int }

> capTransferDataSize :: Word
> capTransferDataSize = 3

\subsection{Initial Address Space}

The following structures are used by the kernel to communicate the initial
state of the system to the root task.

The top-level structure is "BootInfo", which contains an IPC buffer pointer,
information about the initial untyped capabilities, and an array of virtual
address space regions.

> data BootInfo = BootInfo {
>         biIPCBuffer :: VPtr,
>         -- insert (length biRegions) here
>         biRegions :: [BootRegion] }
>     deriving Show

> wordsFromBootInfo :: BootInfo -> [Word]
> wordsFromBootInfo bi = [
>         fromVPtr $ biIPCBuffer bi,
>         fromIntegral $ length $ biRegions bi ]
>         ++ (concat $ map wordsFromBootRegion $ biRegions bi)

Each region descriptor has start and end pointers (with the latter pointing to
the last address in the region, rather than the first address after it), a
type, and some data whose use depends on the type.

> data BootRegion = BootRegion {
>         brBase :: CPtr,
>         brEnd :: CPtr,
>         brType :: BootRegionType,
>         brData :: Word }
>     deriving Show

> wordsFromBootRegion :: BootRegion -> [Word]
> wordsFromBootRegion br = [
>         fromCPtr $ brBase br,
>         fromCPtr $ brEnd br,
>         fromIntegral $ fromEnum $ brType br,
>         brData br ]

The boot regions are of various types, and are used for three separate purposes: describing what \emph{can} appear in a specific region of the address space, describing what \emph{is} in the initial task's address space, and describing the structure of the capability table. Regions with different purposes may overlap as noted below.

> data BootRegionType

\begin{description}

\item[Empty] regions are not used by capabilities or virtual memory mappings in the initial task's address space.

>         = BREmpty

\item[RootTask] regions contain the root task's mapped text or data, backed by one or more page-sized frames.

>         | BRRootTask

\item[CapsOnly] regions cannot be used for virtual memory mappings, but are still usable for capabilities. This may be, for example, because the kernel maps its own data at these addresses, or because the hardware MMU does not implement addressing of the region. This may overlap any region other than "BRRootTask".

>         | BRCapsOnly

\item[NodeL1] appears once, and represents the area covered by the initial thread's root page table node. The data word contains the radix of the node.

>         | BRNodeL1

\item[NodeL2] regions represent second-level nodes in the initial capability table. Again, the data word contains the radix of the node. They all occur within the "BRNodeL1" region.

>         | BRNodeL2

Regions of any of the types below only appear in "BRNodeL2" regions.

\item[FreeSlots] regions contain empty capability slots accessible in the root task's CSpace, which may be used to bootstrap the system's capability management. The data word gives the CSpace depth required by the Retype call to locate the CNode containing the slots.

>         | BRFreeSlots

\item[InitCaps] is the region containing the root task's six guaranteed initial capabilities; it always appears exactly once with a base address of zero. Address 0 contains a null capability (which should not be replaced with a valid one). It is followed by the initial thread's TCB, CSpace root, VSpace root, and reply endpoint, and then .

>         | BRInitCaps

\item[SmallBlocks] regions are filled with untyped memory objects of a single size, generally the platform's standard page size. The minimum number of page-sized untyped blocks that will be provided to the root task by the kernel is fixed at compile time. The data word contains the block size, in address bits.

>         | BRSmallBlocks

\item[LargeBlocks] regions are filled with untyped memory objects of varying sizes. The kernel provides large blocks for any physical memory not already covered by other memory regions. The blocks are in order from smallest to largest, with at most one of each size; the sizes present have the corresponding bits in the region's data word set. In spite of the name, some of these blocks may have sizes less than one page. There may be multiple large block regions.

>         | BRLargeBlocks

\item[DeviceCaps] regions each contain the capabilities required to access one hardware device. The meaning of the data word is implementation-defined.

>         | BRDeviceCaps

\end{description}

>     deriving (Show, Enum)


