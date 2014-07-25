%
% Copyright 2014, General Dynamics C4 Systems
%
% This software may be distributed and modified according to the terms of
% the GNU General Public License version 2. Note that NO WARRANTY is provided.
% See "LICENSE_GPLv2.txt" for details.
%
% @TAG(GD_GPL)
%

This module defines the handling of the ARM hardware-defined page tables.

> module SEL4.Kernel.VSpace.ARM where

\begin{impdetails}

> import SEL4.API.Types
> import SEL4.API.Failures
> import SEL4.Machine.RegisterSet
> import SEL4.Machine.Hardware.ARM
> import SEL4.Model
> import SEL4.Object.Structures
> import SEL4.Model.StateData.ARM
> import SEL4.Object.Instances()
> import SEL4.API.Invocation
> import {-# SOURCE #-} SEL4.Object.CNode
> import {-# SOURCE #-} SEL4.Object.TCB
> import {-# SOURCE #-} SEL4.Kernel.Init
> import {-# SOURCE #-} SEL4.Kernel.CSpace

> import Data.Bits
> import Data.Maybe
> import Data.List
> import Data.Array

\end{impdetails}

The ARM-specific invocations are imported with the "ArchInv" prefix. This is necessary to avoid namespace conflicts with the generic invocations.

> import SEL4.API.Invocation.ARM as ArchInv

\subsection{Constants}

All virtual addresses above "kernelBase" cannot be mapped by user-level tasks. With the exception of one page, at "globalsBase", they cannot be read; the globals page is mapped read-only.

> kernelBase :: VPtr
> kernelBase = VPtr 0xf0000000

> globalsBase :: VPtr
> globalsBase = VPtr 0xff000000

The idle thread's code is at an arbitrary location in kernel memory. For convenience in the Haskell model, we place it in the globals frame, but there is no need for it to be in user-accessible memory.

> idleThreadStart :: VPtr
> idleThreadStart = globalsBase + VPtr 0x100

The idle thread executes a short loop that drains the CPU's write buffer and then waits for an interrupt. Note that the wait for interrupt instruction always completes before the interrupt is delivered, so the interrupt handler will return to the following branch instruction.

> idleThreadCode :: [Word]
> idleThreadCode =
>     [ 0xe3a00000 -- mov r0, \#0
>     , 0xee070f9a -- 1: mcr p15, 0, r0, c7, c10, 4 -- drain write buffer
>     , 0xee070f90 -- mcr p15, 0, r0, c7, c0, 4 -- wait for interrupt
>     , 0xeafffffc -- b 1b
>     ]

The "initKernelVM" function is called at the beginning of the kernel's bootstrap sequence. It initialises the kernel's global page directory and page table, and sets it as the active page table. This assumes that the MMU is either disabled, or enabled with the kernel mapped at the same addresses it will use while running.

> initKernelVM :: Kernel ()
> initKernelVM = do
>     createGlobalPD
>     allMemory <- doMachineOp getMemoryRegions
>     mapM_ mapKernelRegion allMemory
>     kernelDevices <- doMachineOp getKernelDevices
>     mapM_ mapKernelDevice kernelDevices
>     createGlobalsFrame
>     mapGlobalsFrame
>     activateGlobalPD

The "initVSpace" function creates and populates the initial task's VSpace structures. Page-sized initial mappings are copied from the initial task's CSpace.

> initVSpace :: PPtr CTE -> PPtr CTE -> KernelInit ()
> initVSpace cRootSlot vRootSlot = do

Because the kernel reserves all virtual addresses above "kernelBase" for its own use, it specifies this region as a "BRCapsOnly" region; that is, user tasks may use it for capabilities, but not for virtual memory mappings.

>     provideRegion $ BootRegion
>         (CPtr $ fromVPtr kernelBase) maxBound BRCapsOnly 0

Create and populate the initial task's virtual address space.

>     vRoot <- createInitialRoot vRootSlot
>     cRoot <- doKernelOp $ getSlotCap cRootSlot
>     populateInitialRoot vRoot cRoot
>
>     doKernelOp $ doMachineOp cleanCaches_PoU

The global PD contains all of the kernel mappings, including direct-mapped physical memory, IO mappings for the timer and interrupt controller, the trap vectors, and the user-accessible globals frame. The global PTs are also initialised at this point.

> createGlobalPD :: Kernel ()
> createGlobalPD = do
>     globalPD <- gets $ armKSGlobalPD . ksArchState
>     deleteObjects (PPtr $ fromPPtr globalPD) pdBits
>     placeNewObject (PPtr $ fromPPtr globalPD) (makeObject :: PDE)
>           (pdBits `shiftR` (objBits (makeObject :: PDE)))
>     globalPTs <- gets $ armKSGlobalPTs . ksArchState
>     deleteObjects (PPtr $ fromPPtr $ head globalPTs) pageBits
>     placeNewObject (PPtr $ fromPPtr $ head globalPTs) (makeObject :: PTE)
>           (pageBits `shiftR` (objBits (makeObject :: PTE)))
>     return ()

> activateGlobalPD :: Kernel ()
> activateGlobalPD = do
>     doMachineOp cleanCaches_PoU
>     globalPD <- gets $ armKSGlobalPD . ksArchState
>     doMachineOp $ do
>         setCurrentPD $ addrFromPPtr globalPD
>         invalidateTLB

The kernel creates mappings for all physical memory regions in the global page directory before switching to it. This implementation requires each region to be at least one 1MB section in size.

> mapKernelRegion :: (PAddr, PAddr) -> Kernel ()
> mapKernelRegion (phys, physEnd) = do
>     let size = physEnd - phys
>     let pdeBits = objBits (undefined :: PDE)
>     assert (size >= bit (pageBitsForSize ARMSection))
>         "Kernel regions must be at least 1MB"
>     globalPD <- gets $ armKSGlobalPD . ksArchState
>     if (size < bit (pageBitsForSize ARMSuperSection))
>       then forM_ [phys, phys + bit (pageBitsForSize ARMSection) ..
>               phys + size - 1] $ \phys -> do
>           let virt = VPtr $ fromPPtr $ ptrFromPAddr phys
>           let pde = SectionPDE {
>                   pdeFrame = phys,
>                   pdeParity = True,
>                   pdeDomain = 0,
>                   pdeCacheable = True,
>                   pdeGlobal = True,
>                   pdeRights = VMKernelOnly }
>           let offset = fromVPtr virt `shiftR` pageBitsForSize ARMSection
>           let slot = globalPD + PPtr (offset `shiftL` pdeBits)
>           storePDE slot pde
>       else forM_ [phys, phys + bit (pageBitsForSize ARMSuperSection) ..
>                 phys + size - 1] $ \phys -> do
>           let virt = VPtr $ fromPPtr $ ptrFromPAddr phys
>           let pde = SuperSectionPDE {
>                   pdeFrame = phys,
>                   pdeParity = True,
>                   pdeCacheable = True,
>                   pdeGlobal = True,
>                   pdeRights = VMKernelOnly }
>           let offset = fromVPtr virt `shiftR` pageBitsForSize ARMSection
>           let slots = map (\n -> globalPD + PPtr (n `shiftL` pdeBits))
>                   [offset .. offset + 15]
>           mapM_ (flip storePDE pde) slots

The "mapKernelFrame" helper function is used when mapping the globals frame, kernel IO devices, and the trap frame.

> mapKernelFrame :: PAddr -> VPtr -> VMRights -> VMAttributes -> Kernel ()
> mapKernelFrame paddr vptr rights attributes = do
>     assert (vptr >= kernelBase) "mapKernelFrame: must be in kernel area"
>     pd <- gets $ armKSGlobalPD . ksArchState
>     let pdSlot = lookupPDSlot pd vptr
>     pde <- getObject pdSlot
>     let pteBits = objBits (undefined :: PTE)
>     let ptIndex = fromVPtr $ vptr `shiftR` 12 .&. 0xff
>     ptSlot <- case pde of
>         PageTablePDE {} -> do
>             let pt = ptrFromPAddr $ pdeTable pde
>             return $ pt + (PPtr $ ptIndex `shiftL` pteBits)
>         InvalidPDE {} -> do
>             ptFrame <- allocKernelPT
>             let pde = PageTablePDE {
>                     pdeTable = addrFromPPtr ptFrame,
>                     pdeParity = True,
>                     pdeDomain = 0 }
>             storePDE pdSlot pde
>             return $ PPtr $ fromPPtr ptFrame + (ptIndex `shiftL` pteBits)
>         _ -> fail "mapKernelFrame: section already mapped"
>     pte <- getObject ptSlot
>     case pte of
>         InvalidPTE {} -> do
>             let pte' = SmallPagePTE {
>                 pteFrame = paddr,
>                 pteCacheable = armPageCacheable attributes,
>                 pteGlobal = True,
>                 pteRights = rights }
>             storePTE ptSlot pte'
>         _ -> fail "mapKernelFrame: frame already mapped"

The "allocKernelPT" helper function is used when "mapKernelFrame" needs to allocate a new page table. It returns a table taken from from a small statically-allocated pool.

> allocKernelPT :: Kernel (PPtr PTE)
> allocKernelPT = do
>     pts <- gets $ armKSGlobalPTs . ksArchState
>     case pts of
>         [] -> fail "mapKernelFrame: no more global PTs"
>         (pt:pts') -> do
>             modify (\ks -> ks {
>                 ksArchState = (ksArchState ks) { armKSGlobalPTs = pts' } })
>             return pt

Any IO devices used directly by the kernel --- generally including the interrupt controller, one of the timer devices, and optionally a serial port for debugging --- must be mapped in the global address space. This implementation limits device mappings to one page; it may need to be extended to support multiple-page mappings.

> mapKernelDevice :: (PAddr, PPtr Word) -> Kernel ()
> mapKernelDevice (addr, ptr) = do
>     let vptr = VPtr $ fromPPtr ptr
>     mapKernelFrame addr vptr VMKernelOnly $ VMAttributes False False

The globals frame is a user-readable frame mapped at a well-known location, "globalsBase", in the kernel's virtual memory region.

> createGlobalsFrame :: Kernel ()
> createGlobalsFrame = do
>     globals <- gets $ armKSGlobalsFrame . ksArchState
>     deleteObjects (PPtr $ fromPPtr globals) pageBits
>     reserveFrame (PPtr $ fromPPtr globals) True
>     let offset = fromVPtr $ idleThreadStart - globalsBase
>     doMachineOp $ zipWithM_ storeWord
>         [globals + PPtr offset, globals + PPtr offset + 4 ..]
>         idleThreadCode

Insert an entry into the global PD for the globals frame.

> mapGlobalsFrame :: Kernel ()
> mapGlobalsFrame = do
>     globalsFrame <- gets $ armKSGlobalsFrame . ksArchState
>     mapKernelFrame (addrFromPPtr globalsFrame) globalsBase VMReadOnly $
>         VMAttributes True True

Create the initial thread's page directory and allocate an ASID for it. Returns the page directory capability, which it also stores in the given slot.

> createInitialRoot :: PPtr CTE -> KernelInit Capability
> createInitialRoot slot = do
>     let asid = 1
>     initPDFrame <- allocRegion pdBits
>     let initPD = ptrFromPAddr initPDFrame
>     let rootCap = ArchObjectCap $ PageDirectoryCap {
>             capPDBasePtr = initPD,
>             capPDMappedASID = Just asid }
>     doKernelOp $ do
>         placeNewObject (ptrFromPAddr initPDFrame) (makeObject :: PDE)
>               (pdBits `shiftR` (objBits (makeObject :: PDE)))
>         copyGlobalMappings initPD
>         insertInitCap slot rootCap
>
>     initASIDPoolFrame <- allocRegion $ objBits (undefined :: ASIDPool)
>     let initASIDPoolPtr = ptrFromPAddr initASIDPoolFrame
>     let ASIDPool emptyASIDPool = (makeObject :: ASIDPool)
>     let initASIDPool = ASIDPool $ emptyASIDPool//[(asid .&. mask asidLowBits, Just initPD)]
>     doKernelOp $ do
>         placeNewObject (ptrFromPAddr initASIDPoolFrame) initASIDPool 0
>         asidTable <- gets (armKSASIDTable . ksArchState)
>         let asidTable' =
>                 asidTable//[(asidHighBitsOf asid, Just initASIDPoolPtr)]
>         modify (\s -> s {
>             ksArchState = (ksArchState s) { armKSASIDTable = asidTable' }})
>     provideCap $ ArchObjectCap $ ASIDControlCap
>     provideCap $ ArchObjectCap $ ASIDPoolCap {
>         capASIDPool = initASIDPoolPtr,
>         capASIDBase = 0 }
>
>     return rootCap

Map all of the frame capabilities in the initial capability space into the virtual address space.

> populateInitialRoot :: Capability -> Capability -> KernelInit ()
> populateInitialRoot vRoot cRoot = do
>     let asid = fromJust $ capPDMappedASID $ capCap vRoot
>     let ArchObjectCap pdCap = vRoot
>     let pd = capPDBasePtr pdCap

The CSpace root should resolve the right number of bits for its slots to cover the same region as the corresponding slots in page tables.

>     case cRoot of
>         CNodeCap {} -> return ()
>         _ -> fail "Initial CSpace cap is not valid"
>     assert (capCNodeGuard cRoot == 0) $
>         "populateInitialRoot: CSpace root guard must be 0"
>     assert (capCNodeBits cRoot + capCNodeGuardSize cRoot + pageBits ==
>             bitSize (undefined::Word)) $
>         "populateInitialRoot: CSpace region must match page table region"

Any frame capabilities present in the node are mapped into the VSpace.

>     forM_ [0 .. 1 `shiftL` capCNodeBits cRoot - 1] $ \index -> do
>         cSlot <- doKernelOp $ locateSlot (capCNodePtr cRoot) index
>         cte <- doKernelOp $ getObject cSlot
>         case cteCap cte of
>             ArchObjectCap (pageCap@PageCap {}) -> do
>                 assert (capVPSize pageCap == ARMSmallPage) $
>                     "Initial frames must all be small-page sized"
>                 let vaddr = VPtr $ index `shiftL` pageBits
>                 let pageCap' = pageCap {
>                         capVPMappedAddress = Just (asid, vaddr) }
>                 let cte' = cte { cteCap = ArchObjectCap pageCap' }
>                 doKernelOp $ setObject cSlot cte'
>                 mapUserFrame pd asid
>                     (addrFromPPtr $ capVPBasePtr pageCap) vaddr
>             _ -> return ()

> allocUserPT :: ASID -> VPtr -> KernelInit (PPtr PTE)
> allocUserPT asid vaddr = do
>     initPTFrame <- allocRegion ptBits
>     doKernelOp $ placeNewObject (ptrFromPAddr initPTFrame)
>         (makeObject :: PTE) (ptBits `shiftR` (objBits (makeObject :: PTE)))
>     let initPT = ptrFromPAddr initPTFrame
>     provideCap $ ArchObjectCap $ PageTableCap {
>         capPTBasePtr = initPT,
>         capPTMappedAddress = Just (asid, vaddr) }
>     return initPT

> mapUserFrame :: PPtr PDE -> ASID -> PAddr -> VPtr -> KernelInit ()
> mapUserFrame pd asid paddr vptr = do
>     assert (vptr < kernelBase) "mapUserFrame: must not be in kernel area"
>     let pdSlot = lookupPDSlot pd vptr
>     pde <- doKernelOp $ getObject pdSlot
>     let pteBits = objBits (undefined :: PTE)
>     let ptIndex = fromVPtr $ vptr `shiftR` 12 .&. 0xff
>     ptSlot <- case pde of
>         PageTablePDE {} -> do
>             let pt = ptrFromPAddr $ pdeTable pde
>             return $ pt + (PPtr $ ptIndex `shiftL` pteBits)
>         InvalidPDE {} -> do
>             ptFrame <- allocUserPT asid vptr
>             let pde = PageTablePDE {
>                     pdeTable = addrFromPPtr ptFrame,
>                     pdeParity = True,
>                     pdeDomain = 0 }
>             doKernelOp $ storePDE pdSlot pde
>             return $ PPtr $ fromPPtr ptFrame + (ptIndex `shiftL` pteBits)
>         _ -> fail "mapUserFrame: section already mapped"
>     doKernelOp $ do
>         pte <- getObject ptSlot
>         case pte of
>             InvalidPTE {} -> do
>                 let pte' = SmallPagePTE {
>                     pteFrame = paddr,
>                     pteCacheable = True,
>                     pteGlobal = False,
>                     pteRights = VMReadWrite }
>                 storePTE ptSlot pte'
>             _ -> fail "mapUserFrame: frame already mapped"

\subsubsection{Creating a New Address Space}

When a new page directory is created, the kernel copies all of the global mappings from the kernel page directory into the new page directory.

> copyGlobalMappings :: PPtr PDE -> Kernel ()
> copyGlobalMappings newPD = do
>     globalPD <- gets (armKSGlobalPD . ksArchState)
>     let pdeBits = objBits (undefined :: PDE)
>     let pdSize = 1 `shiftL` (pdBits - pdeBits)
>     forM_ [fromVPtr kernelBase `shiftR` 20 .. pdSize - 1] $ \index -> do
>         let offset = PPtr index `shiftL` pdeBits
>         pde <- getObject (globalPD + offset)
>         storePDE (newPD + offset) pde

\subsection{Creating and Updating Mappings}

When a frame is being mapped, or an existing mapping updated, the following function is used to locate the page table or page directory slots that will be updated and to construct the entry that will be written into them.

> createMappingEntries :: PAddr -> VPtr ->
>     VMPageSize -> VMRights -> VMAttributes -> PPtr PDE ->
>     KernelF SyscallError (Either (PTE, [PPtr PTE]) (PDE, [PPtr PDE]))
> createMappingEntries base vptr ARMSmallPage vmRights attrib pd = do
>     p <- lookupErrorOnFailure False $ lookupPTSlot pd vptr
>     return $ Left (SmallPagePTE {
>         pteFrame = base,
>         pteCacheable = armPageCacheable attrib,
>         pteGlobal = False,
>         pteRights = vmRights }, [p])
>
> createMappingEntries base vptr ARMLargePage vmRights attrib pd = do
>     p <- lookupErrorOnFailure False $ lookupPTSlot pd vptr
>     return $ Left (LargePagePTE {
>         pteFrame = base,
>         pteCacheable = armPageCacheable attrib,
>         pteGlobal = False,
>         pteRights = vmRights }, [p, p + 4 .. p + 60])
>
> createMappingEntries base vptr ARMSection vmRights attrib pd = do
>     let p = lookupPDSlot pd vptr
>     return $ Right (SectionPDE {
>         pdeFrame = base,
>         pdeParity = armParityEnabled attrib,
>         pdeDomain = 0,
>         pdeCacheable = armPageCacheable attrib,
>         pdeGlobal = False,
>         pdeRights = vmRights }, [p])
>
> createMappingEntries base vptr ARMSuperSection vmRights attrib pd = do
>     let p = lookupPDSlot pd vptr
>     return $ Right (SuperSectionPDE {
>         pdeFrame = base,
>         pdeParity = armParityEnabled attrib,
>         pdeCacheable = armPageCacheable attrib,
>         pdeGlobal = False,
>         pdeRights = vmRights }, [p, p + 4 .. p + 60])

The following function is called before creating or modifying mappings in a page table or page directory, and is responsible for ensuring that the mapping is safe --- that is, that inserting it will behave predictably and will not damage the hardware. The ARMv6 specifications require that there are never two mappings of different sizes at any virtual address in the active address space, so this function will throw a fault if the requested operation would change the size of the mapping of any existing valid entry.

> ensureSafeMapping :: Either (PTE, [PPtr PTE]) (PDE, [PPtr PDE]) ->
>     KernelF SyscallError ()

> ensureSafeMapping (Left (InvalidPTE, _)) = return ()
>
> ensureSafeMapping (Left (SmallPagePTE {}, ptSlots)) =
>     forM_ ptSlots $ \slot -> do
>         pte <- withoutFailure $ getObject slot
>         case pte of
>             InvalidPTE -> return ()
>             SmallPagePTE {} -> return ()
>             _ -> throw DeleteFirst
>
> ensureSafeMapping (Left (LargePagePTE {}, ptSlots)) =
>     forM_ ptSlots $ \slot -> do
>         pte <- withoutFailure $ getObject slot
>         case pte of
>             InvalidPTE -> return ()
>             LargePagePTE {} -> return ()
>             _ -> throw DeleteFirst

> ensureSafeMapping (Right (InvalidPDE, _)) = return ()
>
> ensureSafeMapping (Right (PageTablePDE {}, _)) =
>     fail "This function is not called when mapping page tables"
>
> ensureSafeMapping (Right (SectionPDE {}, pdSlots)) =
>     forM_ pdSlots $ \slot -> do
>         pde <- withoutFailure $ getObject slot
>         case pde of
>             InvalidPDE -> return ()
>             SectionPDE {} -> return ()
>             _ -> throw DeleteFirst
>
> ensureSafeMapping (Right (SuperSectionPDE {}, pdSlots)) =
>     forM_ pdSlots $ \slot -> do
>         pde <- withoutFailure $ getObject slot
>         case pde of
>             InvalidPDE -> return ()
>             SuperSectionPDE {} -> return ()
>             _ -> throw DeleteFirst

\subsection{Lookups and Faults}

\subsubsection{IPC Buffer Accesses}

When the kernel tries to access a thread's IPC buffer, this function is called to determine whether the buffer exists and to find its physical address.

> lookupIPCBuffer :: Bool -> PPtr TCB -> Kernel (Maybe (PPtr Word))
> lookupIPCBuffer isReceiver thread = do
>     bufferPtr <- threadGet tcbIPCBuffer thread
>     bufferFrameSlot <- getThreadBufferSlot thread
>     bufferCap <- getSlotCap bufferFrameSlot
>     case bufferCap of
>         ArchObjectCap (frame@PageCap {}) -> do
>             let rights = capVPRights frame
>             let pBits = pageBitsForSize $ capVPSize frame
>             if (rights == VMReadWrite || not isReceiver && rights == VMReadOnly)
>               then do
>                  let ptr = capVPBasePtr frame +
>                            PPtr (fromVPtr bufferPtr .&. mask pBits)
>                  assert (ptr /= 0)
>                             "IPC buffer pointer must be non-null"
>                  return $ Just ptr
>               else return Nothing
>         _ -> return Nothing

\subsubsection{ASID Lookups}

Locating the page directory for a given ASID is necessary when updating or deleting a mapping given its ASID and virtual address.

> findPDForASID :: ASID -> KernelF LookupFailure (PPtr PDE)
> findPDForASID asid = do
>     assert (asid > 0) "ASID 0 is used for objects that are not mapped"
>     assert (asid <= snd asidRange) "ASID out of range"
>     asidTable <- withoutFailure $ gets (armKSASIDTable . ksArchState)
>     let poolPtr = asidTable!(asidHighBitsOf asid)
>     ASIDPool pool <- case poolPtr of
>         Just ptr -> withoutFailure $ getObject ptr
>         Nothing -> throw InvalidRoot
>     let pd = pool!(asid .&. mask asidLowBits)
>     case pd of
>         Just ptr -> do
>             assert (ptr /= 0) "findPDForASID: found null PD"
>             withoutFailure $ checkPDAt ptr
>             return ptr
>         Nothing -> throw InvalidRoot

This version of findPDForASID will fail rather than raise an exception if the ASID does not look up a page directory.

> findPDForASIDAssert :: ASID -> Kernel (PPtr PDE)
> findPDForASIDAssert asid = do
>     pd <- findPDForASID asid `catchFailure`
>         const (fail "findPDForASIDAssert: pd not found")
>     assert (pd .&. mask pdBits == 0)
>         "findPDForASIDAssert: page directory pointer alignment check"
>     checkPDAt pd
>     checkPDUniqueToASID pd asid
>     asidMap <- gets (armKSASIDMap . ksArchState)
>     flip assert "findPDForASIDAssert: page directory map mismatch"
>         $ case asidMap ! asid of
>             Nothing -> True
>             Just (_, pd') -> pd == pd'
>     return pd

These checks are too expensive to run in haskell. The first funcion checks that the pointer is to a page directory, which would require testing that each entry of the table is present. The second checks that the page directory appears in armKSASIDMap only on the ASIDs specified, which would require walking all possible ASIDs to test. In the formalisation of this specification, these functions are given alternative definitions that make the appropriate checks.

> checkPDAt :: PPtr PDE -> Kernel ()
> checkPDAt _ = return ()


> checkPTAt :: PPtr PDE -> Kernel ()
> checkPTAt _ = return ()

> checkPDASIDMapMembership :: PPtr PDE -> [ASID] -> Kernel ()
> checkPDASIDMapMembership _ _ = return ()

> checkPDUniqueToASID :: PPtr PDE -> ASID -> Kernel ()
> checkPDUniqueToASID pd asid = checkPDASIDMapMembership pd [asid]

> checkPDNotInASIDMap :: PPtr PDE -> Kernel ()
> checkPDNotInASIDMap pd = checkPDASIDMapMembership pd []

\subsubsection{Locating Page Table and Page Directory Slots}

The "lookupPTSlot" function locates the page table slot that maps a given virtual address, and returns a pointer to the slot. It will throw a lookup failure if the required page directory slot does not point to a page table.

> lookupPTSlot :: PPtr PDE -> VPtr -> KernelF LookupFailure (PPtr PTE)
> lookupPTSlot pd vptr = do
>     let pdSlot = lookupPDSlot pd vptr
>     pde <- withoutFailure $ getObject pdSlot
>     case pde of
>         PageTablePDE {} -> do
>             let pt = ptrFromPAddr $ pdeTable pde
>             let ptIndex = fromVPtr $ vptr `shiftR` 12 .&. 0xff
>             let ptSlot = pt + (PPtr $ ptIndex `shiftL` 2)
>             withoutFailure $ checkPTAt pt
>             return ptSlot
>         _ -> throw $ MissingCapability 20

Similarly, "lookupPDSlot" locates a slot in the top-level page directory. However, it does not access the kernel state and never throws a fault, so it is not in the kernel monad.

> lookupPDSlot :: PPtr PDE -> VPtr -> PPtr PDE
> lookupPDSlot pd vptr =
>     let pdIndex = fromVPtr $ vptr `shiftR` 20
>     in pd + (PPtr $ pdIndex `shiftL` 2)

\subsubsection{Handling Faults}

If the kernel receives a VM fault from the CPU, it must determine the address and cause of the fault and then throw it to the user-level fault handler. The C datastructure to sture the cause of the fault has only 12 bits space, hence the mask. Only the lower bits are significant anyway.

> handleVMFault :: PPtr TCB -> VMFaultType -> KernelF Fault ()
> handleVMFault _ ARMDataAbort = do
>     addr <- withoutFailure $ doMachineOp getFAR
>     fault <- withoutFailure $ doMachineOp getDFSR
>     throw $ VMFault addr [0, fault .&. mask 12]
>
> handleVMFault thread ARMPrefetchAbort = do
>     pc <- withoutFailure $ asUser thread $ getRestartPC
>     fault <- withoutFailure $ doMachineOp getIFSR
>     throw $ VMFault (VPtr pc) [1, fault .&. mask 12]

\subsection{Unmapping and Deletion}

When a capability backing a virtual memory mapping is deleted, or when an explicit request is made to remove a mapping, the kernel must locate the corresponding entries in the page table or ASID table and remove them. It is also necessary to flush the removed mappings from the hardware caches.

\subsubsection{Deleting an ASID Pool}

> deleteASIDPool :: ASID -> PPtr ASIDPool -> Kernel ()
> deleteASIDPool base ptr = do
>     assert (base .&. mask asidLowBits == 0)
>         "ASID pool's base must be aligned"
>     asidTable <- gets (armKSASIDTable . ksArchState)
>     when (asidTable!(asidHighBitsOf base) == Just ptr) $ do
>         ASIDPool pool <- getObject ptr
>         forM [0 .. (bit asidLowBits) - 1] $ \offset -> do
>             when (isJust $ pool ! offset) $ do
>                 flushSpace $ base + offset
>                 invalidateASIDEntry $ base + offset
>         let asidTable' = asidTable//[(asidHighBitsOf base, Nothing)]
>         modify (\s -> s {
>             ksArchState = (ksArchState s) { armKSASIDTable = asidTable' }})
>         tcb <- getCurThread
>         setVMRoot tcb

\subsubsection{Deleting an Address Space}

> deleteASID :: ASID -> PPtr PDE -> Kernel ()
> deleteASID asid pd = do
>     asidTable <- gets (armKSASIDTable . ksArchState)
>     case asidTable!(asidHighBitsOf asid) of
>         Nothing -> return ()
>         Just poolPtr -> do
>             ASIDPool pool <- getObject poolPtr
>             when (pool!(asid .&. mask asidLowBits) == Just pd) $ do
>                 flushSpace asid
>                 invalidateASIDEntry asid
>                 let pool' = pool//[(asid .&. mask asidLowBits, Nothing)]
>                 setObject poolPtr $ ASIDPool pool'
>                 tcb <- getCurThread
>                 setVMRoot tcb

\subsubsection{Deleting a Page Table}

> pageTableMapped :: ASID -> VPtr -> PPtr PTE -> Kernel (Maybe (PPtr PDE))
> pageTableMapped asid vaddr pt = catchFailure
>     (do
>         pd <- findPDForASID asid
>         let pdSlot = lookupPDSlot pd vaddr
>         pde <- withoutFailure $ getObject pdSlot
>         case pde of
>             PageTablePDE { pdeTable = pt' } -> return $
>                 if pt' == addrFromPPtr pt then Just pd else Nothing
>             _ -> return Nothing)
>     (\_ -> return Nothing)

> unmapPageTable :: ASID -> VPtr -> PPtr PTE -> Kernel ()
> unmapPageTable asid vaddr pt = do
>     maybePD <- pageTableMapped asid vaddr pt
>     case maybePD of
>         Just pd -> do
>             let pdSlot = lookupPDSlot pd vaddr
>             storePDE pdSlot InvalidPDE
>             doMachineOp $ cleanByVA_PoU (VPtr $ fromPPtr pdSlot) (addrFromPPtr pdSlot)
>             flushTable pd asid vaddr
>         Nothing -> return ()

\subsubsection{Unmapping a Frame}

> unmapPage :: VMPageSize -> ASID -> VPtr -> PPtr Word -> Kernel ()
> unmapPage size asid vptr ptr = ignoreFailure $ do
>     pd <- findPDForASID asid
>     case size of
>         ARMSmallPage -> do
>             p <- lookupPTSlot pd vptr
>             checkMappingPPtr ptr size (Left p)
>             withoutFailure $ do
>                 storePTE p InvalidPTE
>                 doMachineOp $ cleanByVA_PoU (VPtr $ fromPPtr p) (addrFromPPtr p)
>         ARMLargePage -> do
>             p <- lookupPTSlot pd vptr
>             checkMappingPPtr ptr size (Left p)
>             withoutFailure $ do
>                 let slots = map (+p) [0, 4 .. 60]
>                 mapM (flip storePTE InvalidPTE) slots
>                 doMachineOp $
>                     cleanCacheRange_PoU (VPtr $ fromPPtr $ (head slots))
>                                         (VPtr $ (fromPPtr (last slots)) + (bit (objBits (undefined :: PTE)) - 1 ))
>                                         (addrFromPPtr (head slots))
>         ARMSection -> do
>             let p = lookupPDSlot pd vptr
>             checkMappingPPtr ptr size (Right p)
>             withoutFailure $ do
>                 storePDE p InvalidPDE
>                 doMachineOp $ cleanByVA_PoU (VPtr $ fromPPtr p) (addrFromPPtr p)
>         ARMSuperSection -> do
>             let p = lookupPDSlot pd vptr
>             checkMappingPPtr ptr size (Right p)
>             withoutFailure $ do
>                 let slots = map (+p) [0, 4 .. 60]
>                 mapM (flip storePDE InvalidPDE) slots
>                 doMachineOp $
>                     cleanCacheRange_PoU (VPtr $ fromPPtr $ (head slots))
>                                         (VPtr $ (fromPPtr  (last slots)) + (bit (objBits (undefined :: PDE)) - 1))
>                                         (addrFromPPtr (head slots))
>     withoutFailure $ flushPage size pd asid vptr

This helper function checks that the mapping installed at a given PT or PD slot points at the given physical address. If that is not the case, the mapping being unmapped has already been displaced, and the unmap need not be performed.

> checkMappingPPtr :: PPtr Word -> VMPageSize ->
>                 Either (PPtr PTE) (PPtr PDE) -> KernelF LookupFailure ()
> checkMappingPPtr pptr size (Left pt) = do
>     pte <- withoutFailure $ getObject pt
>     case (pte, size) of
>         (SmallPagePTE { pteFrame = base }, ARMSmallPage) ->
>             unless (base == addrFromPPtr pptr) $ throw InvalidRoot
>         (LargePagePTE { pteFrame = base }, ARMLargePage) ->
>             unless (base == addrFromPPtr pptr) $ throw InvalidRoot
>         _ -> throw InvalidRoot
> checkMappingPPtr pptr size (Right pd) = do
>     pde <- withoutFailure $ getObject pd
>     case (pde, size) of
>         (SectionPDE { pdeFrame = base }, ARMSection) ->
>             unless (base == addrFromPPtr pptr) $ throw InvalidRoot
>         (SuperSectionPDE { pdeFrame = base }, ARMSuperSection) ->
>             unless (base == addrFromPPtr pptr) $ throw InvalidRoot
>         _ -> throw InvalidRoot


> armv_contextSwitch :: PPtr PDE -> ASID -> Kernel ()
> armv_contextSwitch pd asid = do
>    doMachineOp $ setCurrentPD $ addrFromPPtr pd
>    setCurrentASID asid


\subsection{Address Space Switching}

When switching threads, or after deleting an ASID or page directory, the kernel must locate the current thread's page directory, check the validity of the thread's ASID, and set the hardware's ASID and page directory registers.

If the current thread has no page directory, or if it has an invalid ASID, the hardware page directory register is set to the global page directory, which contains only kernel mappings. In this case it is not necessary to set the current ASID, since the valid mappings are all global.

> setVMRoot :: PPtr TCB -> Kernel ()
> setVMRoot tcb = do
>     threadRootSlot <- getThreadVSpaceRoot tcb
>     threadRoot <- getSlotCap threadRootSlot
>     catchFailure
>         (case threadRoot of
>             ArchObjectCap (PageDirectoryCap {
>                     capPDMappedASID = Just asid,
>                     capPDBasePtr = pd }) -> do
>                 pd' <- findPDForASID asid
>                 when (pd /= pd') $ do
>                     throw InvalidRoot
>                 withoutFailure $ armv_contextSwitch pd asid
>             _ -> throw InvalidRoot)
>         (\_ -> do
>             case threadRoot of
>                 ArchObjectCap (PageDirectoryCap {
>                     capPDMappedASID = Just _,
>                     capPDBasePtr = pd }) -> checkPDNotInASIDMap pd
>                 _ -> return ()
>             globalPD <- gets (armKSGlobalPD . ksArchState)
>             doMachineOp $ setCurrentPD $ addrFromPPtr globalPD)

When cleaning the cache by user virtual address on ARM11, the active address space must be the one that contains the mappings being cleaned. The following function is used to temporarily switch to a given page directory and ASID, in order to clean the cache. It returns "True" if the address space was not the same as the current one, in which case the caller must switch back to the current address space once the cache is clean.

> setVMRootForFlush :: PPtr PDE -> ASID -> Kernel Bool
> setVMRootForFlush pd asid = do
>     tcb <- getCurThread
>     threadRootSlot <- getThreadVSpaceRoot tcb
>     threadRoot <- getSlotCap threadRootSlot
>     case threadRoot of
>         ArchObjectCap (PageDirectoryCap {
>                 capPDMappedASID = Just _,
>                 capPDBasePtr = cur_pd }) | cur_pd == pd -> return False
>         _ -> do
>             doMachineOp $ setCurrentPD $ addrFromPPtr pd
>             setCurrentASID asid
>             return True

\subsection{Helper Functions}

The VSpace root must be an ARM page directory with an ASID allocated.

Note that this does not check that the ASID is valid, so invalid-root faults are still possible after setting this capability as the root. This is because the ASID may become invalid at any time. % XXX fix this

> isValidVTableRoot :: Capability -> Bool
> isValidVTableRoot
>     (ArchObjectCap (PageDirectoryCap { capPDMappedASID = Just _ })) = True
> isValidVTableRoot _ = False

The location of an IPC buffer is computed using the relevant bits of a VPtr as an offset within a frame.
The IPC buffer frame must be an ARM frame capability, and the buffer must be aligned.

Note that implementations with separate high and low memory regions may also wish to limit valid IPC buffer frames to low memory, so the kernel can access them without extra mappings. This function may also be used to enforce cache colouring restrictions.

> checkValidIPCBuffer :: VPtr -> Capability -> KernelF SyscallError ()
> checkValidIPCBuffer vptr (ArchObjectCap (PageCap {})) = do
>     when (vptr .&. mask msgAlignBits /= 0) $ throw AlignmentError
>     return ()
> checkValidIPCBuffer _ _ = throw IllegalOperation

The following two functions are used by the bootstrap code to create the initial thread's virtual memory mappings and the system's device capabilities.

> createInitPage :: PAddr -> Kernel Capability
> createInitPage addr = do
>     let ptr = ptrFromPAddr addr
>     reserveFrame ptr False
>     return $ ArchObjectCap $ PageCap ptr VMReadWrite ARMSmallPage Nothing
>
> createDeviceCap :: (PAddr, PAddr) -> Kernel Capability
> createDeviceCap (addr, end) = do
>     let wptr = ptrFromPAddr addr
>     let rawsize = end - addr
>     let sz = find (\sz -> rawsize == bit (pageBitsForSize sz))
>                   [minBound .. maxBound]
>     size <- case sz of
>         Just size -> return size
>         Nothing -> fail "Couldn't find appropriate size for device"
>     return $ ArchObjectCap $ PageCap wptr VMReadWrite size Nothing

ARM memory mappings may be read-only or read-write; on newer revisions of the ARM they may also be marked non-executable. Write-only mappings are not possible.

> maskVMRights :: VMRights -> CapRights -> VMRights
> maskVMRights r m = case (r, capAllowRead m, capAllowWrite m) of
>     (VMNoAccess, _, _) -> VMNoAccess
>     (VMReadOnly, True, _) -> VMReadOnly
>     (VMReadWrite, True, False) -> VMReadOnly
>     (VMReadWrite, True, True) -> VMReadWrite
>     _ -> VMKernelOnly

ARM memory mappings may be marked cacheable or non-cacheable. Also, parity checking can be enabled or disabled at a page table level.

> attribsFromWord :: Word -> VMAttributes
> attribsFromWord w = VMAttributes {
>     armPageCacheable = w `testBit` 0,
>     armParityEnabled = w `testBit` 1 }

\subsection{ARM Hardware ASID allocation}

Manage the stored HW ASID.

> storeHWASID :: ASID -> HardwareASID -> Kernel ()
> storeHWASID asid hw_asid = do
>     pd <- findPDForASIDAssert asid
>     asidMap <- gets (armKSASIDMap . ksArchState)
>     let asidMap' = asidMap//[(asid, Just (hw_asid, pd))]
>     modify (\s -> s {
>         ksArchState = (ksArchState s)
>         { armKSASIDMap = asidMap' }})
>     hwASIDMap <- gets (armKSHWASIDTable . ksArchState)
>     let hwASIDMap' = hwASIDMap//[(hw_asid, Just asid)]
>     modify (\s -> s {
>         ksArchState = (ksArchState s)
>         { armKSHWASIDTable = hwASIDMap' }})

> loadHWASID :: ASID -> Kernel (Maybe HardwareASID)
> loadHWASID asid = do
>     asidMap <- gets (armKSASIDMap . ksArchState)
>     findPDForASIDAssert asid
>     return $ case asidMap ! asid of
>         Nothing -> Nothing
>         Just (hw_asid, _) -> Just hw_asid

> invalidateASID :: ASID -> Kernel ()
> invalidateASID asid = do
>     findPDForASIDAssert asid
>     asidMap <- gets (armKSASIDMap . ksArchState)
>     let asidMap' = asidMap//[(asid, Nothing)]
>     modify (\s -> s {
>         ksArchState = (ksArchState s)
>         { armKSASIDMap = asidMap' }})

> invalidateHWASIDEntry :: HardwareASID -> Kernel ()
> invalidateHWASIDEntry hwASID = do
>     asidMap <- gets (armKSHWASIDTable . ksArchState)
>     let asidMap' = asidMap//[(hwASID, Nothing)]
>     modify (\s -> s {
>         ksArchState = (ksArchState s)
>         { armKSHWASIDTable = asidMap' }})

> invalidateASIDEntry :: ASID -> Kernel ()
> invalidateASIDEntry asid = do
>     maybeHWASID <- loadHWASID asid
>     when (isJust maybeHWASID) $ invalidateHWASIDEntry (fromJust maybeHWASID)
>     invalidateASID asid


> findFreeHWASID :: Kernel HardwareASID
> findFreeHWASID = do

Look for a free Hardware ASID.

>     hwASIDTable <- gets (armKSHWASIDTable . ksArchState)
>     nextASID <- gets (armKSNextASID . ksArchState)
>     let maybe_asid = find (\a -> isNothing (hwASIDTable ! a))
>                       ([nextASID .. maxBound] ++ init [minBound .. nextASID])

If there is one, return it, otherwise revoke the next one in a strict
round-robin.

>     case maybe_asid of
>         Just hw_asid -> return hw_asid
>         Nothing -> do
>             invalidateASID $ fromJust $ hwASIDTable ! nextASID
>             doMachineOp $ invalidateTLB_ASID nextASID
>             invalidateHWASIDEntry nextASID
>             let new_nextASID =
>                     if nextASID == maxBound
>                     then minBound
>                     else nextASID + 1
>             modify (\s -> s {
>                 ksArchState = (ksArchState s)
>                 { armKSNextASID = new_nextASID }})
>             return nextASID

> getHWASID :: ASID -> Kernel HardwareASID
> getHWASID asid = do
>     maybe_hw_asid <- loadHWASID asid
>     case maybe_hw_asid of
>         Just hw_asid ->
>             return hw_asid
>         Nothing -> do
>             new_hw_asid <- findFreeHWASID
>             storeHWASID asid new_hw_asid
>             return new_hw_asid

> setCurrentASID :: ASID -> Kernel ()
> setCurrentASID asid = do
>     hw_asid <- getHWASID asid
>     doMachineOp $ setHardwareASID hw_asid

\subsection {ARM Cache and TLB consistency}

> doFlush :: FlushType -> VPtr -> VPtr -> PAddr -> MachineMonad ()
> doFlush Clean vstart vend pstart =
>     cleanCacheRange_RAM vstart vend pstart
> doFlush Invalidate vstart vend pstart =
>     invalidateCacheRange_RAM vstart vend pstart
> doFlush CleanInvalidate vstart vend pstart =
>     cleanInvalidateCacheRange_RAM vstart vend pstart
> doFlush Unify vstart vend pstart = do
>     cleanCacheRange_PoU vstart vend pstart
>     dsb
>     invalidateCacheRange_I vstart vend pstart
>     branchFlushRange vstart vend pstart
>     isb

> flushPage :: VMPageSize -> PPtr PDE -> ASID -> VPtr -> Kernel ()
> flushPage _ pd asid vptr = do
>     assert (vptr .&. mask pageBits == 0)
>         "vptr must be 4k aligned"
>     root_switched <- setVMRootForFlush pd asid
>     maybe_hw_asid <- loadHWASID asid
>     when (isJust maybe_hw_asid) $ do
>       let Just hw_asid = maybe_hw_asid
>       doMachineOp $ invalidateTLB_VAASID (fromVPtr vptr .|. (fromIntegral $ fromHWASID hw_asid))
>       when root_switched $ do
>           tcb <- getCurThread
>           setVMRoot tcb

> flushTable :: PPtr PDE -> ASID -> VPtr -> Kernel ()
> flushTable pd asid vptr = do
>     assert (vptr .&. mask (pageBitsForSize ARMSection) == 0)
>         "vptr must be 1MB aligned"
>     root_switched <- setVMRootForFlush pd asid
>     maybe_hw_asid <- loadHWASID asid
>     when (isJust maybe_hw_asid) $ do
>       doMachineOp $ invalidateTLB_ASID (fromJust maybe_hw_asid)
>       when root_switched $ do
>           tcb <- getCurThread
>           setVMRoot tcb

> flushSpace :: ASID -> Kernel ()
> flushSpace asid = do
>     maybe_hw_asid <- loadHWASID asid
>     doMachineOp cleanCaches_PoU
>     case maybe_hw_asid of
>         Nothing -> return ()
>         Just hw_asid -> do
>             doMachineOp $ invalidateTLB_ASID hw_asid

> invalidateTLBByASID :: ASID -> Kernel ()
> invalidateTLBByASID asid = do
>     maybe_hw_asid <- loadHWASID asid
>     case maybe_hw_asid of
>         Nothing -> return ()
>         Just hw_asid -> do
>             doMachineOp $ invalidateTLB_ASID hw_asid

\subsection{Decoding ARM Invocations}

> labelToFlushType :: Word -> FlushType
> labelToFlushType label = case invocationType label of
>       ARMPDClean_Data -> Clean
>       ARMPageClean_Data -> Clean
>       ARMPDInvalidate_Data -> Invalidate
>       ARMPageInvalidate_Data -> Invalidate
>       ARMPDCleanInvalidate_Data -> CleanInvalidate
>       ARMPageCleanInvalidate_Data -> CleanInvalidate
>       ARMPDUnify_Instruction -> Unify
>       ARMPageUnify_Instruction -> Unify
>       _ -> error "Should never be called without a flush invocation"

> pageBase :: VPtr -> VMPageSize -> VPtr
> pageBase vaddr size = vaddr .&. (complement $ mask (pageBitsForSize size))

> lookupPTSlot_nofail :: PPtr PTE -> VPtr -> PPtr PTE
> lookupPTSlot_nofail pt vptr = 
>     let ptIndex = fromVPtr $ (vptr `shiftR` 12) .&. mask 8
>     in pt + (PPtr $ ptIndex `shiftL` 2) 

> resolveVAddr :: PPtr PDE -> VPtr -> Kernel (Maybe (VMPageSize, PAddr))
> resolveVAddr pd vaddr = do
>     let pdSlot = lookupPDSlot pd vaddr
>     pde <- getObject pdSlot
>     case pde of
>         SectionPDE frame _ _ _ _ _ -> return $ Just (ARMSection, frame)
>         SuperSectionPDE frame _ _ _ _ -> return $ Just (ARMSuperSection, frame)
>         PageTablePDE table _ _ -> do
>             let pt = ptrFromPAddr table
>             let pteSlot = lookupPTSlot_nofail pt vaddr
>             pte <- getObject pteSlot
>             case pte of
>                 LargePagePTE frame _ _ _ -> return $ Just (ARMLargePage, frame)
>                 SmallPagePTE frame _ _ _ -> return $ Just (ARMSmallPage, frame)
>                 _ -> return Nothing 
>         _ -> return Nothing
>     


> decodeARMMMUInvocation :: Word -> [Word] -> CPtr -> PPtr CTE ->
>         ArchCapability -> [(Capability, PPtr CTE)] ->
>         KernelF SyscallError ArchInv.Invocation

There are five ARM-specific capability types. They correspond to the two levels of the hardware-defined page table, the two levels of the global ASID table, and the frames used to back virtual memory pages.

Capabilities for page directories --- the top level of the hardware-defined page table --- have only a single invocation, which allows the user to clean and/or invalidate caches.

> decodeARMMMUInvocation label args _ _ cap@(PageDirectoryCap {}) _ =
>     case (isPDFlush (invocationType label), args) of
>         (True, start:end:_) -> do
>             when (end <= start) $ 
>                 throw $ InvalidArgument 1
>             when (VPtr start >= kernelBase || VPtr end > kernelBase) $
>                 throw IllegalOperation 
>             (pd,asid) <- case cap of
>                 PageDirectoryCap {
>                          capPDMappedASID = Just asid,
>                          capPDBasePtr = pd}
>                     -> return (pd,asid)
>                 _ -> throw $ InvalidCapability 0 
>             pdCheck <- lookupErrorOnFailure False $ findPDForASID asid
>             when (pdCheck /= pd) $ throw $ InvalidCapability 0
>             frameInfo <-
>                  withoutFailure $ resolveVAddr (capPDBasePtr cap) (VPtr start)
>             case frameInfo of
>                 -- Fail if there is nothing mapped here
>                 Nothing -> return $ InvokePageDirectory PageDirectoryNothing
>                 Just frameInfo -> do
>                     let baseStart = pageBase (VPtr start) (fst frameInfo)
>                     let baseEnd = pageBase (VPtr end - 1) (fst frameInfo)
>                     when (baseStart /= baseEnd) $
>                         throw $ RangeError start $ fromVPtr $ baseStart + 
>                                   mask (pageBitsForSize (fst frameInfo))
>                     let offset = start .&. mask (pageBitsForSize (fst frameInfo))
>                     let pStart = snd frameInfo + toPAddr offset
>                     return $ InvokePageDirectory $ PageDirectoryFlush {
>                          pdFlushType = labelToFlushType label,
>                          pdFlushStart = VPtr start,
>                          pdFlushEnd = VPtr end - 1,
>                          pdFlushPStart = pStart,
>                          pdFlushPD = pd,
>                          pdFlushASID = asid }
>         (True, _) -> throw TruncatedMessage
>         _ -> throw IllegalOperation

Capabilities for page tables --- that is, the second level of the hardware-defined page table structure --- have one method. It is used to attach the table to a top-level page directory, at a specific virtual address. It is a single-use method; if it succeeds, the table cannot be mapped again at a different address or in a different page directory, even if the original page directory is deleted. The mapping may only be removed by deleting the page table capability.

Note that these capabilities cannot be copied until they have been mapped, so any given page table object can only appear in one page directory. This is to ensure that the page unmapping operation always succeeds.

> decodeARMMMUInvocation label args _ cte cap@(PageTableCap {}) extraCaps =
>     case (invocationType label, args, extraCaps) of
>         (ARMPageTableMap, vaddr:attr:_, (pdCap,_):_) -> do
>             when (isJust $ capPTMappedAddress cap) $
>                 throw $ InvalidCapability 0
>             (pd,asid) <- case pdCap of
>                 ArchObjectCap (PageDirectoryCap {
>                          capPDMappedASID = Just asid,
>                          capPDBasePtr = pd })
>                     -> return (pd,asid)
>                 _ -> throw $ InvalidCapability 1
>             when (VPtr vaddr >= kernelBase) $
>                 throw $ InvalidArgument 0
>             pdCheck <- lookupErrorOnFailure False $ findPDForASID asid
>             when (pdCheck /= pd) $ throw $ InvalidCapability 1
>             let pdIndex = vaddr `shiftR` 20
>             let vaddr' = pdIndex `shiftL` 20
>             let pdSlot = pd + (PPtr $ pdIndex `shiftL` 2)
>             oldpde <- withoutFailure $ getObject pdSlot
>             unless (oldpde == InvalidPDE) $ throw DeleteFirst
>             let pde = PageTablePDE {
>                     pdeTable = addrFromPPtr $ capPTBasePtr cap,
>                     pdeParity = armParityEnabled $ attribsFromWord attr,
>                     pdeDomain = 0 }
>             return $ InvokePageTable $ PageTableMap {
>                 ptMapCap = ArchObjectCap $
>                     cap { capPTMappedAddress = Just (asid, VPtr vaddr') },
>                 ptMapCTSlot = cte,
>                 ptMapPDE = pde,
>                 ptMapPDSlot = pdSlot }
>         (ARMPageTableMap, _, _) -> throw TruncatedMessage
>         (ARMPageTableUnmap, _, _) -> do
>             cteVal <- withoutFailure $ getCTE cte
>             final <- withoutFailure $ isFinalCapability cteVal
>             unless final $ throw RevokeFirst
>             return $ InvokePageTable $ PageTableUnmap {
>                 ptUnmapCap = cap,
>                 ptUnmapCapSlot = cte }
>         _ -> throw IllegalOperation

Virtual page capabilities may each represent a single mapping into a page table. Unlike page table capabilities, they may be unmapped without deletion, and may be freely copied to allow multiple mappings of the same page. Along with the \emph{Map} and \emph{Unmap} operations, there is a \emph{Remap} operation, which is used to change the access permissions on an existing mapping.

> decodeARMMMUInvocation label args _ cte cap@(PageCap {}) extraCaps =
>     case (invocationType label, args, extraCaps) of
>         (ARMPageMap, vaddr:rightsMask:attr:_, (pdCap,_):_) -> do
>             when (isJust $ capVPMappedAddress cap) $
>                 throw $ InvalidCapability 0
>             (pd,asid) <- case pdCap of
>                 ArchObjectCap (PageDirectoryCap {
>                         capPDMappedASID = Just asid,
>                         capPDBasePtr = pd })
>                     -> return (pd,asid)
>                 _ -> throw $ InvalidCapability 1
>             pdCheck <- lookupErrorOnFailure False $ findPDForASID asid
>             when (pdCheck /= pd) $ throw $ InvalidCapability 1
>             let vtop = vaddr + bit (pageBitsForSize $ capVPSize cap) - 1
>             when (VPtr vtop >= kernelBase) $
>                 throw $ InvalidArgument 0
>             let vmRights = maskVMRights (capVPRights cap) $
>                     rightsFromWord rightsMask
>             checkVPAlignment (capVPSize cap) (VPtr vaddr)
>             entries <- createMappingEntries (addrFromPPtr $ capVPBasePtr cap)
>                 (VPtr vaddr) (capVPSize cap) vmRights
>                 (attribsFromWord attr) pd
>             ensureSafeMapping entries
>             return $ InvokePage $ PageMap {
>                 pageMapASID = asid,
>                 pageMapCap = ArchObjectCap $
>                     cap { capVPMappedAddress = Just (asid, VPtr vaddr) },
>                 pageMapCTSlot = cte,
>                 pageMapEntries = entries }
>         (ARMPageMap, _, _) -> throw TruncatedMessage
>         (ARMPageRemap, rightsMask:attr:_, (pdCap, _):_) -> do
>             (pd,asid) <- case pdCap of
>                 ArchObjectCap (PageDirectoryCap {
>                         capPDMappedASID = Just asid,
>                         capPDBasePtr = pd })
>                     -> return (pd,asid)
>                 _ -> throw $ InvalidCapability 1
>             (asidCheck, vaddr) <- case capVPMappedAddress cap of
>                 Just a -> return a
>                 _ -> throw $ InvalidCapability 0
>             pdCheck <- lookupErrorOnFailure False $ findPDForASID asidCheck
>             when (pdCheck /= pd || asidCheck /= asid) $ throw $ InvalidCapability 1
>             let vmRights = maskVMRights (capVPRights cap) $
>                     rightsFromWord rightsMask
>             checkVPAlignment (capVPSize cap) vaddr
>             entries <- createMappingEntries (addrFromPPtr $ capVPBasePtr cap)
>                 vaddr (capVPSize cap) vmRights (attribsFromWord attr) pd
>             ensureSafeMapping entries
>             return $ InvokePage $ PageRemap {
>                 pageRemapASID = asidCheck,
>                 pageRemapEntries = entries }
>         (ARMPageRemap, _, _) -> throw TruncatedMessage
>         (ARMPageUnmap, _, _) -> return $ InvokePage $ PageUnmap {
>                 pageUnmapCap = cap,
>                 pageUnmapCapSlot = cte }
>         (ARMPageClean_Data, _, _) -> decodeARMPageFlush label args cap
>         (ARMPageInvalidate_Data, _, _) -> decodeARMPageFlush label args cap
>         (ARMPageCleanInvalidate_Data, _, _) -> decodeARMPageFlush label args cap
>         (ARMPageUnify_Instruction, _, _) -> decodeARMPageFlush label args cap
>         (ARMPageGetAddress, _, _) -> return $ InvokePage $ PageGetAddr (capVPBasePtr cap)
>         _ -> throw IllegalOperation


The ASID control capability refers to the top level of a global two-level table used for allocating address space identifiers. It has only one method, "MakePool", which creates an ASID allocation pool given a single frame of untyped memory. Since this method allocates part of a global range of ASIDs, it may return a "DeleteFirst" error if the entire range has been allocated to existing ASID pools.

> decodeARMMMUInvocation label args _ _ ASIDControlCap extraCaps =
>     case (invocationType label, args, extraCaps) of
>         (ARMASIDControlMakePool, index:depth:_,
>                 (untyped,parentSlot):(root,_):_) -> do
>             asidTable <- withoutFailure $ gets (armKSASIDTable . ksArchState)
>             let free = filter (\(x,y) -> x <= (1 `shiftL` asidHighBits) - 1 && isNothing y) $ assocs asidTable
>             when (null free) $ throw DeleteFirst
>             let base = (fst $ head free) `shiftL` asidLowBits
>             let pool = makeObject :: ASIDPool
>             frame <- case untyped of
>                 UntypedCap {} | capBlockSize untyped == objBits pool -> do
>                     ensureNoChildren parentSlot
>                     return $ capPtr untyped
>                 _ -> throw $ InvalidCapability 1
>             destSlot <- lookupTargetSlot
>                 root (CPtr index) (fromIntegral depth)
>             ensureEmptySlot destSlot
>             return $ InvokeASIDControl $ MakePool {
>                 makePoolFrame = frame,
>                 makePoolSlot = destSlot,
>                 makePoolParent = parentSlot,
>                 makePoolBase = base }
>         (ARMASIDControlMakePool, _, _) -> throw TruncatedMessage
>         _ -> throw IllegalOperation

ASID pool capabilities are used to allocate unique address space identifiers for virtual address spaces. They support the "Assign" method, which allocates an ASID for a given page directory capability. The directory must not already have an ASID. Page directories cannot be used until they have been allocated an ASID using this method.

> decodeARMMMUInvocation label _ _ _ cap@(ASIDPoolCap {}) extraCaps =
>     case (invocationType label, extraCaps) of
>         (ARMASIDPoolAssign, (pdCap,pdCapSlot):_) ->
>             case pdCap of
>                 ArchObjectCap (PageDirectoryCap { capPDMappedASID = Nothing })
>                   -> do
>                     asidTable <- withoutFailure $ gets (armKSASIDTable . ksArchState)
>                     let base = capASIDBase cap
>                     let poolPtr = asidTable!(asidHighBitsOf base)
>                     when (isNothing poolPtr) $ throw $ FailedLookup False InvalidRoot
>                     let Just p = poolPtr
>                     when (p /= capASIDPool cap) $ throw $ InvalidCapability 0
>                     ASIDPool pool <- withoutFailure $ getObject $ p
>                     let free = filter (\(x,y) -> x <=  (1 `shiftL` asidLowBits) - 1
>                                                  && x + base /= 0 && isNothing y) $ assocs pool
>                     when (null free) $ throw DeleteFirst
>                     let asid = fst $ head free
>                     return $ InvokeASIDPool $ Assign {
>                         assignASID = asid + base,
>                         assignASIDPool = capASIDPool cap,
>                         assignASIDCTSlot = pdCapSlot }
>                 _ -> throw $ InvalidCapability 1
>         (ARMASIDPoolAssign, _) -> throw TruncatedMessage
>         _ -> throw IllegalOperation

> decodeARMPageFlush :: Word -> [Word] -> ArchCapability ->
>                       KernelF SyscallError ArchInv.Invocation
> decodeARMPageFlush label args cap = case (args, capVPMappedAddress cap) of
>     (start:end:_, Just (asid, vaddr)) -> do
>         pd <- lookupErrorOnFailure False $ findPDForASID asid
>         when (end <= start) $ 
>             throw $ InvalidArgument 1
>         let pageSize = 1 `shiftL` pageBitsForSize (capVPSize cap)
>         let pageBase = addrFromPPtr $ capVPBasePtr cap
>         when (start >= pageSize || end > pageSize) $
>             throw $ InvalidArgument 0
>         let pstart = pageBase + toPAddr start
>         let start' = start + fromVPtr vaddr
>         let end' = end + fromVPtr vaddr
>         return $ InvokePage $ PageFlush {
>               pageFlushType = labelToFlushType label,
>               pageFlushStart = VPtr $ start',
>               pageFlushEnd = VPtr $ end' - 1,
>               pageFlushPStart = pstart,
>               pageFlushPD = pd,
>               pageFlushASID = asid }
>     (_:_:_, Nothing) -> throw IllegalOperation     
>     _ -> throw TruncatedMessage


Checking virtual address for page size dependent alignment:

> checkVPAlignment :: VMPageSize -> VPtr -> KernelF SyscallError ()
>
> checkVPAlignment sz w =
>     unless (w .&. mask (pageBitsForSize sz) == 0) $
>            throw AlignmentError

\subsection{Invocation Implementations}

> performARMMMUInvocation :: ArchInv.Invocation -> KernelP [Word]
> performARMMMUInvocation i = withoutPreemption $ do
>     case i of
>         InvokePageDirectory oper -> performPageDirectoryInvocation oper
>         InvokePageTable oper -> performPageTableInvocation oper
>         InvokePage oper -> performPageInvocation oper
>         InvokeASIDControl oper -> performASIDControlInvocation oper
>         InvokeASIDPool oper -> performASIDPoolInvocation oper
>     return $ []

> performPageDirectoryInvocation :: PageDirectoryInvocation -> Kernel ()
> performPageDirectoryInvocation (PageDirectoryFlush typ start end pstart pd asid) =

Don't flush an empty range.

>     when (start < end) $ do
>         root_switched <- setVMRootForFlush pd asid
>         doMachineOp $ doFlush typ start end pstart
>         when root_switched $ do
>             tcb <- getCurThread
>             setVMRoot tcb

> performPageDirectoryInvocation PageDirectoryNothing = return ()

> performPageTableInvocation :: PageTableInvocation -> Kernel ()

> performPageTableInvocation (PageTableMap cap ctSlot pde pdSlot) = do
>     updateCap ctSlot cap
>     storePDE pdSlot pde
>     doMachineOp $ cleanByVA_PoU (VPtr $ fromPPtr $ pdSlot) (addrFromPPtr pdSlot)

> performPageTableInvocation (PageTableUnmap cap ctSlot) = do
>     case capPTMappedAddress cap of
>         Just (asid, vaddr) -> do
>             unmapPageTable asid vaddr (capPTBasePtr cap)
>             let ptr = capPTBasePtr cap
>             let pteBits = objBits InvalidPTE
>             let slots = [ptr, ptr + bit pteBits .. ptr + bit ptBits - 1]
>             mapM_ (flip storePTE InvalidPTE) slots
>             doMachineOp $
>                 cleanCacheRange_PoU (VPtr $ fromPPtr $ ptr)
>                                     (VPtr $ fromPPtr $ (ptr + (1 `shiftL` ptBits) - 1))
>                                     (addrFromPPtr ptr)
>         Nothing -> return ()
>     ArchObjectCap cap <- getSlotCap ctSlot
>     updateCap ctSlot (ArchObjectCap $
>                            cap { capPTMappedAddress = Nothing })

When checking if there was already something mapped before a PageMap or PageRemap,
we need only check the first slot because ensureSafeMapping tells us that
the PT/PD is consistent.

> pteCheckIfMapped :: PPtr PTE -> Kernel Bool
> pteCheckIfMapped slot = do
>     pt <- getObject slot
>     return $ pt /= InvalidPTE

> pdeCheckIfMapped :: PPtr PDE -> Kernel Bool
> pdeCheckIfMapped slot = do
>     pd <- getObject slot
>     return $ pd /= InvalidPDE

> performPageInvocation :: PageInvocation -> Kernel ()
>
> performPageInvocation (PageMap asid cap ctSlot entries) = do
>     updateCap ctSlot cap
>     case entries of
>         Left (pte, slots) -> do
>             tlbFlush <- pteCheckIfMapped (head slots)
>             mapM (flip storePTE pte) slots
>             doMachineOp $
>                 cleanCacheRange_PoU (VPtr $ fromPPtr $ head slots)
>                                     (VPtr $ (fromPPtr (last slots)) + (bit (objBits (undefined::PTE)) - 1))
>                                     (addrFromPPtr (head slots))
>             when tlbFlush $ invalidateTLBByASID asid
>         Right (pde, slots) -> do
>             tlbFlush <- pdeCheckIfMapped (head slots)
>             mapM (flip storePDE pde) slots
>             doMachineOp $
>                 cleanCacheRange_PoU (VPtr $ fromPPtr $ head slots)
>                                     (VPtr $ (fromPPtr (last slots)) + (bit (objBits (undefined::PDE)) - 1))
>                                     (addrFromPPtr (head slots))
>             when tlbFlush $ invalidateTLBByASID asid
>
> performPageInvocation (PageRemap asid (Left (pte, slots))) = do
>     tlbFlush <- pteCheckIfMapped (head slots)
>     mapM (flip storePTE pte) slots
>     doMachineOp $
>         cleanCacheRange_PoU (VPtr $ fromPPtr $ head slots)
>                             (VPtr $ (fromPPtr (last slots)) + (bit (objBits (undefined::PTE)) - 1))
>                             (addrFromPPtr (head slots))
>     when tlbFlush $ invalidateTLBByASID asid
>
> performPageInvocation (PageRemap asid (Right (pde, slots))) = do
>     tlbFlush <- pdeCheckIfMapped (head slots)
>     mapM (flip storePDE pde) slots
>     doMachineOp $
>         cleanCacheRange_PoU (VPtr $ fromPPtr $ head slots)
>                             (VPtr $ (fromPPtr (last slots)) + (bit (objBits (undefined::PDE)) - 1))
>                             (addrFromPPtr (head slots))
>     when tlbFlush $ invalidateTLBByASID asid
>
> performPageInvocation (PageUnmap cap ctSlot) = do
>     case capVPMappedAddress cap of
>         Just (asid, vaddr) -> unmapPage (capVPSize cap) asid vaddr
>                                     (capVPBasePtr cap)
>         Nothing -> return ()
>     ArchObjectCap cap <- getSlotCap ctSlot
>     updateCap ctSlot (ArchObjectCap $
>                            cap { capVPMappedAddress = Nothing })
>
> performPageInvocation (PageFlush typ start end pstart pd asid) = 
>     when (start < end) $ do
>         root_switched <- setVMRootForFlush pd asid
>         doMachineOp $ doFlush typ start end pstart
>         when root_switched $ do
>             tcb <- getCurThread
>             setVMRoot tcb
>
> performPageInvocation (PageGetAddr ptr) = do
>     let paddr = fromPAddr $ addrFromPPtr ptr
>     ct <- getCurThread
>     msgTransferred <- setMRs ct Nothing [paddr]
>     msgInfo <- return $ MI {
>             msgLength = msgTransferred,
>             msgExtraCaps = 0,
>             msgCapsUnwrapped = 0,
>             msgLabel = 0 }
>     setMessageInfo ct msgInfo

> performASIDControlInvocation :: ASIDControlInvocation -> Kernel ()
> performASIDControlInvocation (MakePool frame slot parent base) = do
>     deleteObjects frame pageBits
>     pcap <- getSlotCap parent
>     updateCap parent (pcap {capFreeIndex = maxFreeIndex (capBlockSize pcap) })
>     placeNewObject frame (makeObject :: ASIDPool) 0
>     let poolPtr = PPtr $ fromPPtr frame
>     cteInsert (ArchObjectCap $ ASIDPoolCap poolPtr base) parent slot
>     assert (base .&. mask asidLowBits == 0)
>         "ASID pool's base must be aligned"
>     asidTable <- gets (armKSASIDTable . ksArchState)
>     let asidTable' = asidTable//[(asidHighBitsOf base, Just poolPtr)]
>     modify (\s -> s {
>         ksArchState = (ksArchState s) { armKSASIDTable = asidTable' }})

> performASIDPoolInvocation :: ASIDPoolInvocation -> Kernel ()
> performASIDPoolInvocation (Assign asid poolPtr ctSlot) = do
>     oldcap <- getSlotCap ctSlot
>     ASIDPool pool <- getObject poolPtr
>     let ArchObjectCap cap = oldcap
>     updateCap ctSlot (ArchObjectCap $ cap { capPDMappedASID = Just asid })
>     let pool' = pool//[(asid .&. mask asidLowBits, Just $ capPDBasePtr cap)]
>     setObject poolPtr $ ASIDPool pool'

\subsection{Simulator Support}

The kernel model's ARM targets use an external simulation of the physical address space for user-level virtual memory, I/O devices and MMU data structures, separate from the "PSpace" which is used for kernel objects. However, "PDE" objects are accessed by the kernel, so they must be stored in both the external physical memory model and the internal "PSpace". To make verification simpler we do the same for "PTE" objects.

> storePDE :: PPtr PDE -> PDE -> Kernel ()
> storePDE slot pde = do
>     setObject slot pde
>     doMachineOp $ storeWordVM (PPtr $ fromPPtr slot) $ wordFromPDE pde

> storePTE :: PPtr PTE -> PTE -> Kernel ()
> storePTE slot pte = do
>     setObject slot pte
>     doMachineOp $ storeWordVM (PPtr $ fromPPtr slot) $ wordFromPTE pte


