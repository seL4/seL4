%
% Copyright 2014, General Dynamics C4 Systems
%
% This software may be distributed and modified according to the terms of
% the GNU General Public License version 2. Note that NO WARRANTY is provided.
% See "LICENSE_GPLv2.txt" for details.
%
% @TAG(GD_GPL)
%

\begin{impdetails}

> {-# LANGUAGE CPP, GeneralizedNewtypeDeriving #-}

\end{impdetails}

This module defines the low-level ARM hardware interface.

> module SEL4.Machine.Hardware.ARM where

\begin{impdetails}

> import SEL4.Machine.RegisterSet

> import Foreign.Ptr
> import Control.Monad.Reader
> import Data.Bits
> import Data.Word(Word8)
> import Data.Ix

\end{impdetails}

The ARM-specific register set definitions are qualified with the "ARM" prefix, and the platform-specific hardware access functions are qualified with the "Platform" prefix. The latter module is outside the scope of the reference manual; for the executable model, it is specific to the external simulator used for user-level code.

> import qualified SEL4.Machine.RegisterSet.ARM as ARM
> import qualified SEL4.Machine.Hardware.ARM.PLATFORM as Platform

\subsection{Data Types}

The machine monad contains a platform-specific opaque pointer, used by the external simulator interface.

> type MachineMonad = ReaderT MachineData IO

> type MachineData = Ptr Platform.CallbackData

> type IRQ = Platform.IRQ

> newtype HardwareASID = HardwareASID { fromHWASID :: Word8 }
>     deriving (Num, Enum, Bounded, Ord, Ix, Eq, Show)

> toPAddr = Platform.PAddr

\subsubsection{Virtual Memory}

ARM hardware-defined pages come in four sizes: 4k, 64k, 1M and 16M. The 16M page size only has hardware support on some ARMv6 CPUs, such as the ARM1136; the kernel will simulate them using 16 1M mappings on other CPUs.

> data VMPageSize
>     = ARMSmallPage
>     | ARMLargePage
>     | ARMSection
>     | ARMSuperSection
>     deriving (Show, Eq, Ord, Enum, Bounded)

ARM virtual memory faults are handled by one of two trap handlers: one for data aborts, and one for instruction aborts.

> data VMFaultType
>     = ARMDataAbort
>     | ARMPrefetchAbort
>     deriving Show

\subsubsection{Physical Memory}

The ARM MMU does not allow access to physical addresses while translation is enabled; the kernel must access its objects via virtual addresses. Depending on the platform, these virtual addresses may either be the same as the physical addresses, or offset by a constant.

> type PAddr = Platform.PAddr

> ptrFromPAddr :: PAddr -> PPtr a
> ptrFromPAddr = Platform.ptrFromPAddr

> addrFromPPtr :: PPtr a -> PAddr
> addrFromPPtr = Platform.addrFromPPtr

> fromPAddr :: PAddr -> Word
> fromPAddr = Platform.fromPAddr

\subsection{Hardware Access}

The following functions define the ARM-specific interface between the kernel and the hardware. Most of them depend on the simulator in use, and are therefore defined in the platform module.

> pageBits :: Int
> pageBits = 12

> pageBitsForSize :: VMPageSize -> Int
> pageBitsForSize ARMSmallPage = 12
> pageBitsForSize ARMLargePage = 16
> pageBitsForSize ARMSection = 20
> pageBitsForSize ARMSuperSection = 24

> getMemoryRegions :: MachineMonad [(PAddr, PAddr)]
> getMemoryRegions = do
>     cpbtr <- ask
>     liftIO $ Platform.getMemoryRegions cpbtr

> getDeviceRegions :: MachineMonad [(PAddr, PAddr)]
> getDeviceRegions = do
>     cbptr <- ask
>     liftIO $ Platform.getDeviceRegions cbptr

> getKernelDevices :: MachineMonad [(PAddr, PPtr Word)]
> getKernelDevices = do
>     cbptr <- ask
>     liftIO $ Platform.getKernelDevices cbptr

> loadWord :: PPtr Word -> MachineMonad Word
> loadWord ptr = do
>     cbptr <- ask
>     liftIO $ Platform.loadWordCallback cbptr $ addrFromPPtr ptr

> storeWord :: PPtr Word -> Word -> MachineMonad ()
> storeWord ptr val = do
>     cbptr <- ask
>     liftIO $ Platform.storeWordCallback cbptr (addrFromPPtr ptr) val

> storeWordVM :: PPtr Word -> Word -> MachineMonad ()
> storeWordVM ptr val = storeWord ptr val

> pageColourBits :: Int
> pageColourBits = Platform.pageColourBits

> getActiveIRQ :: MachineMonad (Maybe IRQ)
> getActiveIRQ = do
>     cbptr <- ask
>     liftIO $ Platform.getActiveIRQ cbptr

> ackInterrupt :: IRQ -> MachineMonad ()
> ackInterrupt irq = do
>     cbptr <- ask
>     liftIO $ Platform.ackInterrupt cbptr irq

> maskInterrupt :: Bool -> IRQ -> MachineMonad ()
> maskInterrupt maskI irq = do
>     cbptr <- ask
>     liftIO $ Platform.maskInterrupt cbptr maskI irq

> configureTimer :: MachineMonad IRQ
> configureTimer = do
>     cbptr <- ask
>     liftIO $ Platform.configureTimer cbptr

> resetTimer :: MachineMonad ()
> resetTimer = do
>     cbptr <- ask
>     liftIO $ Platform.resetTimer cbptr

> debugPrint :: String -> MachineMonad ()
> debugPrint str = liftIO $ putStrLn str

> getRestartPC = getRegister (Register ARM.FaultInstruction)
> setNextPC = setRegister (Register ARM.LR_svc)

\subsection{ARM Memory Management}

There are several operations used by the ARM memory management code to access relevant hardware registers.

\subsubsection{Cleaning Memory}

This function is called before a region of user-memory is recycled.
It zeros every word to ensure that user tasks cannot access any private data
that might previously have been stored in the region and
then flushes the kernel's mapping from the virtually-indexed caches.

> clearMemory :: PPtr Word -> Int -> MachineMonad ()
> clearMemory ptr byteLength = do
>     let wordSize = fromIntegral $ bitSize (undefined::Word) `div` 8
>     let ptr' = PPtr $ fromPPtr ptr
>     let ptrs = [ptr', ptr' + wordSize .. ptr' + fromIntegral byteLength - 1]
>     mapM_ (\p -> storeWord p 0) ptrs
>     cleanCacheRange_PoU (VPtr $ fromPPtr ptr)
>                         (VPtr (fromPPtr (ptr + fromIntegral byteLength - 1)))
>                         (toPAddr $ fromPPtr ptr)

This function is called before a region of memory is made user-accessible.
Though in Haskell, it is implemented as "clearMemory",
we draw the logical distinction to gain more freedom for its interpretation
in the Isabelle formalization.

> initMemory :: PPtr Word -> Int -> MachineMonad ()
> initMemory = clearMemory

This function is called to free a region of user-memory after use.
In Haskell, this operation does not do anything.
We just use it as a stub for the Isabelle formalization.

> freeMemory :: PPtr Word -> Int -> MachineMonad ()
> freeMemory _ _ = return ()

Same as "clearMemory", but uses storeWordVM to write to memory.
To be used when creating mapping objects (page tables and -dirs)
Flushing the kernel's mapping from the virtually-indexed
caches must be done separately.

> clearMemoryVM :: PPtr Word -> Int -> MachineMonad ()
> clearMemoryVM ptr bits = do
>     let wordSize = fromIntegral $ bitSize (undefined::Word) `div` 8
>     let ptr' = PPtr $ fromPPtr ptr
>     let ptrs = [ptr', ptr' + wordSize .. ptr' + 1 `shiftL` bits - 1]
>     mapM_ (\p -> storeWordVM p 0) ptrs

\subsubsection{Address Space Setup}

> setCurrentPD :: PAddr -> MachineMonad ()
> setCurrentPD pd = do
>     cbptr <- ask
>     liftIO $ Platform.setCurrentPD cbptr pd

> setHardwareASID :: HardwareASID -> MachineMonad ()
> setHardwareASID (HardwareASID hw_asid) = do
>     cbptr <- ask
>     liftIO $ Platform.setHardwareASID cbptr hw_asid

\subsubsection{Memory Barriers}

> isb :: MachineMonad ()
> isb = do
>     cbptr <- ask
>     liftIO $ Platform.isbCallback cbptr

> dsb :: MachineMonad ()
> dsb = do
>     cbptr <- ask
>     liftIO $ Platform.dsbCallback cbptr

> dmb :: MachineMonad ()
> dmb = do
>     cbptr <- ask
>     liftIO $ Platform.dmbCallback cbptr

\subsubsection{Cache Cleaning and TLB Flushes}

> invalidateTLB :: MachineMonad ()
> invalidateTLB = do
>     cbptr <- ask
>     liftIO $ Platform.invalidateTLBCallback cbptr

> invalidateTLB_ASID :: HardwareASID -> MachineMonad ()
> invalidateTLB_ASID (HardwareASID hw_asid) = do
>     cbptr <- ask
>     liftIO $ Platform.invalidateTLB_ASIDCallback cbptr hw_asid

> invalidateTLB_VAASID :: Word -> MachineMonad ()
> invalidateTLB_VAASID mva_plus_asid = do
>     cbptr <- ask
>     liftIO $ Platform.invalidateTLB_VAASIDCallback cbptr mva_plus_asid

> cleanByVA :: VPtr -> PAddr -> MachineMonad ()
> cleanByVA mva pa = do
>     cbptr <- ask
>     liftIO $ Platform.cacheCleanByVACallback cbptr mva pa

> cleanByVA_PoU :: VPtr -> PAddr -> MachineMonad ()
> cleanByVA_PoU mva pa = do
>     cbptr <- ask
>     liftIO $ Platform.cacheCleanByVA_PoUCallback cbptr mva pa

> invalidateByVA :: VPtr -> PAddr -> MachineMonad ()
> invalidateByVA mva pa = do
>     cbptr <- ask
>     liftIO $ Platform.cacheInvalidateByVACallback cbptr mva pa

> invalidateByVA_I :: VPtr -> PAddr -> MachineMonad ()
> invalidateByVA_I mva pa = do
>     cbptr <- ask
>     liftIO $ Platform.cacheInvalidateByVA_ICallback cbptr mva pa

> invalidate_I_PoU :: MachineMonad ()
> invalidate_I_PoU = do
>     cbptr <- ask
>     liftIO $ Platform.cacheInvalidate_I_PoUCallback cbptr

> cleanInvalByVA :: VPtr -> PAddr -> MachineMonad ()
> cleanInvalByVA mva pa = do
>     cbptr <- ask
>     liftIO $ Platform.cacheCleanInvalidateByVACallback cbptr mva pa

> branchFlush :: VPtr -> PAddr -> MachineMonad ()
> branchFlush mva pa = do
>     cbptr <- ask
>     liftIO $ Platform.branchFlushCallback cbptr mva pa

> clean_D_PoU :: MachineMonad ()
> clean_D_PoU = do
>     cbptr <- ask
>     liftIO $ Platform.cacheClean_D_PoUCallback cbptr

> cleanInvalidate_D_PoC :: MachineMonad ()
> cleanInvalidate_D_PoC = do
>     cbptr <- ask
>     liftIO $ Platform.cacheCleanInvalidate_D_PoCCallback cbptr

> cleanInvalidate_D_PoU :: MachineMonad ()
> cleanInvalidate_D_PoU = do
>     cbptr <- ask
>     liftIO $ Platform.cacheCleanInvalidate_D_PoUCallback cbptr

> cleanInvalidateL2Range :: PAddr -> PAddr -> MachineMonad ()
> cleanInvalidateL2Range start end = do
>     cbptr <- ask
>     liftIO $ Platform.cacheCleanInvalidateL2RangeCallback cbptr start end

> invalidateL2Range :: PAddr -> PAddr -> MachineMonad ()
> invalidateL2Range start end = do
>     cbptr <- ask
>     liftIO $ Platform.cacheInvalidateL2RangeCallback cbptr start end

> cleanL2Range :: PAddr -> PAddr -> MachineMonad ()
> cleanL2Range start end = do
>     cbptr <- ask
>     liftIO $ Platform.cacheCleanL2RangeCallback cbptr start end

> lineStart addr = (addr `shiftR` cacheLineBits) `shiftL` cacheLineBits

Performs the given operation on every cache line that intersects the
supplied range.

> cacheRangeOp :: (VPtr -> PAddr -> MachineMonad ()) ->
>                 VPtr -> VPtr -> PAddr -> MachineMonad ()
> cacheRangeOp operation vstart vend pstart = do
>     let pend = pstart + (toPAddr $ fromVPtr (vend - vstart))
>     let vptrs = [lineStart vstart, lineStart vstart + fromIntegral cacheLine .. lineStart vend]
>     let pptrs = [lineStart pstart, lineStart pstart + fromIntegral cacheLine .. lineStart pend]
>     mapM_ (\(v,p) -> operation v p) (zip vptrs pptrs)

> cleanCacheRange_PoC :: VPtr -> VPtr -> PAddr -> MachineMonad ()
> cleanCacheRange_PoC vstart vend pstart =
>     cacheRangeOp cleanByVA vstart vend pstart

> cleanInvalidateCacheRange_RAM :: VPtr -> VPtr -> PAddr -> MachineMonad ()
> cleanInvalidateCacheRange_RAM vstart vend pstart = do
>     cleanCacheRange_PoC vstart vend pstart
>     dsb
>     cleanInvalidateL2Range pstart (pstart + (toPAddr $ fromVPtr $ vend - vstart))
>     cacheRangeOp cleanInvalByVA vstart vend pstart
>     dsb

> cleanCacheRange_RAM :: VPtr -> VPtr -> PAddr -> MachineMonad ()
> cleanCacheRange_RAM vstart vend pstart = do
>     cleanCacheRange_PoC vstart vend pstart
>     dsb
>     cleanL2Range pstart (pstart + (toPAddr $ fromVPtr $ vend - vstart))

> cleanCacheRange_PoU :: VPtr -> VPtr -> PAddr -> MachineMonad ()
> cleanCacheRange_PoU vstart vend pstart =
>     cacheRangeOp cleanByVA_PoU vstart vend pstart

> invalidateCacheRange_RAM :: VPtr -> VPtr -> PAddr -> MachineMonad ()
> invalidateCacheRange_RAM vstart vend pstart = do
>     when (vstart /= lineStart vstart) $
>         cleanCacheRange_RAM vstart vstart pstart
>     when (vend + 1 /= lineStart (vend + 1)) $
>         cleanCacheRange_RAM (lineStart vend) (lineStart vend)
>             (pstart + (toPAddr $ fromVPtr $ lineStart vend - vstart))
>     invalidateL2Range pstart
>         (pstart + (toPAddr $ fromVPtr $ vend - vstart))
>     cacheRangeOp invalidateByVA vstart vend pstart
>     dsb

> invalidateCacheRange_I :: VPtr -> VPtr -> PAddr -> MachineMonad ()
> invalidateCacheRange_I vstart vend pstart =
>     cacheRangeOp invalidateByVA_I vstart vend pstart

> branchFlushRange :: VPtr -> VPtr -> PAddr -> MachineMonad ()
> branchFlushRange vstart vend pstart =
>     cacheRangeOp branchFlush vstart vend pstart

> cleanCaches_PoU :: MachineMonad ()
> cleanCaches_PoU = do
>     dsb
>     clean_D_PoU
>     dsb
>     invalidate_I_PoU
>     dsb

> cleanInvalidateL1Caches :: MachineMonad ()
> cleanInvalidateL1Caches = do
>     dsb
>     cleanInvalidate_D_PoC
>     dsb
>     invalidate_I_PoU
>     dsb

This function is used to clear the load exclusive monitor. This dummy
implementation assumes the monitor is not modelled in our simulator.

> clearExMonitor :: MachineMonad ()
> clearExMonitor = return ()

\subsubsection{Fault Status Registers}

> getIFSR :: MachineMonad Word
> getIFSR = do
>     cbptr <- ask
>     liftIO $ Platform.getIFSR cbptr

> getDFSR :: MachineMonad Word
> getDFSR = do
>     cbptr <- ask
>     liftIO $ Platform.getDFSR cbptr

> getFAR :: MachineMonad VPtr
> getFAR = do
>     cbptr <- ask
>     liftIO $ Platform.getFAR cbptr

\subsubsection{Page Table Structure}

The ARM architecture defines a two-level hardware-walked page table. The kernel must write entries to this table in the defined format to construct address spaces for user-level tasks.

The following types are Haskell representations of an entry in an ARMv6 page table. The "PDE" (page directory entry) type is an entry in the first level, and the "PTE" (page table entry) type is an entry in the second level. Note that "SuperSectionPDE" is an extension provided by some ARMv6 cores.

> data PDE
>     = InvalidPDE
>     | PageTablePDE {
>         pdeTable :: PAddr,
>         pdeParity :: Bool,
>         pdeDomain :: Word }
>     | SectionPDE {
>         pdeFrame :: PAddr,
>         pdeParity :: Bool,
>         pdeDomain :: Word,
>         pdeCacheable :: Bool,
>         pdeGlobal :: Bool,
>         pdeRights :: VMRights }
>     | SuperSectionPDE {
>         pdeFrame :: PAddr,
>         pdeParity :: Bool,
>         pdeCacheable :: Bool,
>         pdeGlobal :: Bool,
>         pdeRights :: VMRights }
>     deriving (Show, Eq)

> wordFromPDE :: PDE -> Word
> wordFromPDE InvalidPDE = 0
> wordFromPDE (PageTablePDE table parity domain) = 1 .|.
>     (fromIntegral table .&. 0xfffffc00) .|.
>     (if parity then bit 9 else 0) .|.
>     ((domain .&. 0xf) `shiftL` 5)
> wordFromPDE (SectionPDE frame parity domain cacheable global rights) = 2 .|.
>     (fromIntegral frame .&. 0xfff00000) .|.
>     (if parity then bit 9 else 0) .|.
>     (if cacheable then bit 2 .|. bit 3 else 0) .|.
>     ((domain .&. 0xf) `shiftL` 5) .|.
>     (if global then 0 else bit 17) .|.
>     (fromIntegral $ fromEnum rights `shiftL` 10)
> wordFromPDE (SuperSectionPDE frame parity cacheable global rights) = 2 .|.
>     bit 18 .|.
>     (fromIntegral frame .&. 0xff000000) .|.
>     (if parity then bit 9 else 0) .|.
>     (if cacheable then bit 2 .|. bit 3 else 0) .|.
>     (if global then 0 else bit 17) .|.
>     (fromIntegral $ fromEnum rights `shiftL` 10)

> data PTE
>     = InvalidPTE
>     | LargePagePTE {
>         pteFrame :: PAddr,
>         pteCacheable :: Bool,
>         pteGlobal :: Bool,
>         pteRights :: VMRights }
>     | SmallPagePTE {
>         pteFrame :: PAddr,
>         pteCacheable :: Bool,
>         pteGlobal :: Bool,
>         pteRights :: VMRights }
>     deriving (Show, Eq)

> wordFromPTE :: PTE -> Word
> wordFromPTE InvalidPTE = 0

\iffalse
#ifdef PLATFORM_QEmu

> wordFromPTE (LargePagePTE frame cacheable _ rights) = 1 .|.
>     (fromIntegral frame .&. 0xffff0000) .|.
>     (if cacheable then bit 2 .|. bit 3 else 0) .|.
>     (fromIntegral $ fromEnum rights * 0x55 `shiftL` 4)
> wordFromPTE (SmallPagePTE frame cacheable _ rights) = 2 .|.
>     (fromIntegral frame .&. 0xfffff000) .|.
>     (if cacheable then bit 2 .|. bit 3 else 0) .|.
>     (fromIntegral $ fromEnum rights * 0x55 `shiftL` 4)

#else
\fi

> wordFromPTE (LargePagePTE frame cacheable global rights) = 1 .|.
>     (fromIntegral frame .&. 0xffff0000) .|.
>     (if cacheable then bit 2 .|. bit 3 else 0) .|.
>     (if global then 0 else bit 11) .|.
>     (fromIntegral $ fromEnum rights `shiftL` 4)
> wordFromPTE (SmallPagePTE frame cacheable global rights) = 2 .|.
>     (fromIntegral frame .&. 0xfffff000) .|.
>     (if cacheable then bit 2 .|. bit 3 else 0) .|.
>     (if global then 0 else bit 11) .|.
>     (fromIntegral $ fromEnum rights `shiftL` 4)

\iffalse
#endif
\fi

> data VMRights
>     = VMNoAccess
>     | VMKernelOnly
>     | VMReadOnly
>     | VMReadWrite
>     deriving (Show, Eq, Enum)

> data VMAttributes = VMAttributes {
>     armPageCacheable, armParityEnabled :: Bool }

ARM page directories and page tables occupy four frames and one quarter of a frame, respectively.

> pdBits :: Int
> pdBits = pageBits + 2

> ptBits :: Int
> ptBits = pageBits - 2

> cacheLineBits = Platform.cacheLineBits
> cacheLine = Platform.cacheLine


