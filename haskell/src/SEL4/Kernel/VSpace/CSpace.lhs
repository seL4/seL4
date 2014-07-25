%
% Copyright 2014, General Dynamics C4 Systems
%
% This software may be distributed and modified according to the terms of
% the GNU General Public License version 2. Note that NO WARRANTY is provided.
% See "LICENSE_GPLv2.txt" for details.
%
% @TAG(GD_GPL)
%

This module contains definitions of the VSpace functions for platforms that do not define specific data structures for virtual memory page tables. On these platforms, the kernel uses CSpace structures to represent virtual memory address spaces.

> module SEL4.Kernel.VSpace.CSpace where

\begin{impdetails}

> import SEL4.Machine
> import SEL4.Model
> import SEL4.Object
> import SEL4.Kernel.CSpace
> import SEL4.API.Types
> import SEL4.API.Failures
> import {-# SOURCE #-} SEL4.Kernel.Init

> import Data.Bits

\end{impdetails}

> initVSpace :: PPtr CTE -> PPtr CTE -> KernelInit ()
> initVSpace cRootSlot vRootSlot = doKernelOp $ do
>     cRootCap <- getSlotCap cRootSlot
>     cteInsert cRootCap cRootSlot vRootSlot

> lookupVPtr :: PPtr TCB -> VPtr -> Bool -> KernelF LookupFailure (PPtr Word)
> lookupVPtr thread vptr isWrite = do
>         threadRootSlot <- withoutFailure $ getThreadVSpaceRoot thread
>         threadRoot <- withoutFailure $ getSlotCap threadRootSlot

>         let cptr = CPtr $ fromVPtr vptr
>         let bits = bitSize cptr
>         (slot,endBits) <-
>             resolveAddressBits threadRoot cptr bits
>         cap <- withoutFailure $ getSlotCap slot

>         case (isWrite, cap) of
>             (_, FrameCap { capVPSizeBits = sizeBits })
>                 | sizeBits /= endBits -> throw $ DepthMismatch {
>                     depthMismatchBitsLeft = endBits,
>                     depthMismatchBitsFound = sizeBits }
>             (True, FrameCap { capVPCanWrite = True }) -> return ()
>             (False, FrameCap { capVPCanRead = True }) -> return ()
>             _ -> throw $ MissingCapability { missingCapBitsLeft = 0 }
>         
>         let offset = fromVPtr vptr .&. mask (capVPSizeBits cap)
>         return $! capVPBasePtr cap + PPtr offset

> handleVMFault :: PPtr TCB -> VMFaultType -> KernelF Fault ()
> handleVMFault thread (vptr, isWrite) = do
>         pptr <- lookupVPtr thread vptr isWrite `catchFailure`
>             \lf -> throwFault $ VMFault vptr isWrite $ msgFromLookupFailure lf
>         withoutFailure $ doMachineOp $
>             insertMapping pptr vptr pageBits isWrite

> isValidVTableRoot :: Capability -> Bool
> isValidVTableRoot (CNodeCap {}) = True
> isValidVTableRoot _ = False

> createInitPage :: PAddr -> Kernel Capability
> createInitPage addr = do
>     let ptr = ptrFromPAddr addr
>     reserveFrame ptr False
>     return $ FrameCap ptr pageBits True True
> 
> createDeviceCap :: (PAddr, PAddr) -> Kernel Capability
> createDeviceCap (addr, end) = do
>     let wptr = ptrFromPAddr addr
>     -- need to figure out size in bits based on start and end addresses
>     let rawsize = end - addr
>     let size = find (\sz -> rawsize == bit (pageBitsForSize sz))
>                     [minBound .. maxBound]
>     size <- case sz of
>         Just size -> return size
>         Nothing -> fail "Couldn't find appropriate size for device"
>     return $ FrameCap wptr (pageBitsForSize size) True True


