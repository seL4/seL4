%
% Copyright 2014, General Dynamics C4 Systems
%
% This software may be distributed and modified according to the terms of
% the GNU General Public License version 2. Note that NO WARRANTY is provided.
% See "LICENSE_GPLv2.txt" for details.
%
% @TAG(GD_GPL)
%

This module contains the data structure and operations for the physical memory model.

> module SEL4.Model.PSpace (
>         PSpace, newPSpace, initPSpace,
>         PSpaceStorable,
>         objBits, injectKO, projectKO, makeObject, loadObject, updateObject,
>         getObject, setObject, deleteObjects, reserveFrame,
>         typeError, alignError, alignCheck, sizeCheck,
>         loadWordUser, storeWordUser, placeNewObject
>     ) where

\begin{impdetails}

% {-# BOOT-IMPORTS: Data.Map SEL4.Object.Structures SEL4.Machine.RegisterSet #-}
% {-# BOOT-EXPORTS: PSpace #PRegion newPSpace #-}

> import SEL4.Model.StateData
> import SEL4.Object.Structures

> import qualified Data.Map
> import Data.Bits
> import SEL4.Machine.RegisterSet
> import SEL4.Machine.Hardware

\end{impdetails}

\subsection{Physical Address Space}

The physical address space is represented by a map from physical addresses to objects. The objects themselves are wrapped in the "KernelObject" type.

> newtype PSpace = PSpace { psMap :: Data.Map.Map Word KernelObject }

\subsection{Storable Objects}

The type class "PSpaceStorable" defines a set of operations that may be performed on any object that is storable in physical memory.

> class PSpaceStorable a where

\begin{impdetails}
For a \emph{pure} kernel object --- one which is only accessed by the kernel itself, and may therefore be stored in the Haskell "PSpace" structure --- it is usually sufficient to define the "objBits", "injectKO" and "projectKO" functions for an instance of this class.

Some kernel objects, such as capability table entries ("CTE"s), may be stored either alone in the "PSpace" or encapsulated inside another kernel object. Instances for such objects must override the default "loadObject" and "updateObject" definitions, as "injectKO" and "projectKO" are not sufficient.

Objects such as virtual memory pages or hardware-defined page tables must be accessed in the hardware monad. Instances for these objects must override "getObject" and "setObject".

All instances must either define "injectKO" and "makeObject" or override "placeNewObject" to allow new objects to be created.
\end{impdetails}

\subsubsection{Object Properties}

The size and alignment of the physical region occupied by objects of type "a". This is the logarithm base 2 of the object's size (i.e., the number of low-order bits of the physical address that are not necessary to locate the object).

The default contents of a kernel object of this type.

>     makeObject :: a

\subsubsection{Storing Objects in PSpace}

The "loadObject" and "updateObject" functions are used to insert or extract an object from a "KernelObject" wrapper, given any remaining unresolved physical address bits. Normally these bits must all be zero, and the number of bits must equal the result of "objBits"; this ensures that the alignment is correct.

\begin{impdetails}
The default definitions are sufficient for most kernel objects. There is one exception in the platform-independent code, for "CTE" objects; it can be found in \autoref{sec:object.instances}.
\end{impdetails}

>     loadObject :: (Monad m) => Word -> Word -> Maybe Word ->
>                                 KernelObject -> m a
>     loadObject ptr ptr' next obj = do
>         unless (ptr == ptr') $ fail "no object at address given in pspace"
>         val <- projectKO obj 
>         alignCheck ptr (objBits val)
>         sizeCheck ptr next (objBits val)
>         return val

>     updateObject :: (Monad m) => a -> KernelObject -> Word ->
>                         Word -> Maybe Word -> m KernelObject
>     updateObject val oldObj ptr ptr' next = do 
>         unless (ptr == ptr') $ fail "no object at address given in pspace"
>         liftM (asTypeOf val) $ projectKO oldObj -- for the type error
>         alignCheck ptr (objBits val)
>         sizeCheck ptr next (objBits val)
>         return (injectKO val)

The "injectKO" and "projectKO" functions convert to and from a "KernelObject", which is the type used to encapsulate all objects stored in the "PSpace" structure.

>     injectKO :: a -> KernelObject
>     projectKO :: (Monad m) => KernelObject -> m a

> objBits :: PSpaceStorable a => a -> Int
> objBits a = objBitsKO (injectKO a)

\subsection{Functions}

\subsubsection{Initialisation}

A new physical address space has an empty object map.

> newPSpace :: PSpace
> newPSpace = PSpace { psMap = Data.Map.empty }

The "initPSpace" function currently does nothing. In earlier versions of the Haskell model, it was used to configure the "PSpace" model to signal a bus error if an invalid physical address was accessed. This is useful only for debugging of the Haskell model, and is not strictly necessary; it has no equivalent in a real implementation.
% FIXME maybe check that the arguments are OK

> initPSpace :: [(PPtr (), PPtr ())] -> Kernel ()
> initPSpace _ = return ()

\subsubsection{Accessing Objects}

Given a pointer into physical memory, an attempt may be made to fetch
or update an object of any storable type from the address space. The caller is
assumed to have checked that the address is correctly aligned for the
requested object type and that it actually contains an object of the
requested type.

> getObject :: PSpaceStorable a => PPtr a -> Kernel a
> getObject ptr = do
>         map <- gets $ psMap . ksPSpace
>         let (before, after) = lookupAround2 (fromPPtr ptr) map
>         (ptr', val) <- maybeToMonad before
>         loadObject (fromPPtr ptr) ptr' after val

> setObject :: PSpaceStorable a => PPtr a -> a -> Kernel ()
> setObject ptr val = do
>         ps <- gets ksPSpace
>         let map = psMap ps
>         let (before, after) = lookupAround2 (fromPPtr ptr) map
>         (ptr', obj) <- maybeToMonad before
>         obj' <- updateObject val obj (fromPPtr ptr) ptr' after
>         let map' = Data.Map.insert ptr' obj' map
>         let ps' = ps { psMap = map' }
>         modify (\ks -> ks { ksPSpace = ps'})

> lookupAround :: Ord k => k -> Data.Map.Map k a ->
>                         (Maybe (k, a), Maybe a, Maybe (k, a))
> lookupAround ptr map = (nullProtect Data.Map.findMax before,
>                         at, nullProtect Data.Map.findMin after)
>     where
>         (before, at, after) = Data.Map.splitLookup ptr map
>         nullProtect f m
>             | Data.Map.null m = Nothing
>             | otherwise = Just (f m)

> lookupAround2 :: Ord k => k -> Data.Map.Map k a -> (Maybe (k, a), Maybe k)
> lookupAround2 ptr mp = case at of
>                             Just v -> (Just (ptr, v), after')
>                             Nothing -> (before, after')
>     where
>         (before, at, after) = lookupAround ptr mp
>         after' = maybe Nothing (Just . fst) after

> maybeToMonad :: Monad m => Maybe a -> m a
> maybeToMonad (Just x) = return x
> maybeToMonad Nothing  = fail "maybeToMonad: got Nothing"

\subsubsection{Creating Objects}

Create a new object, and place it in memory. Some objects (such as page
directories) are actually arrays of smaller objects. To handle these cases, the
function `placeNewObject' accepts an argument allowing multiple objects of the
same type to be created as a group. For standard object types, this argument
will always be set to create only a single object.

The arguments to "placeNewObject" are a pointer to the start of the region
(which must be aligned to the object's size); the value used to initialise the
created objects; and the number of copies that will be created (the input
represent as a power-of-two).

> placeNewObject :: PSpaceStorable a => PPtr () -> a -> Int -> Kernel ()
> placeNewObject ptr val groupSizeBits =
>         placeNewObject' ptr (injectKO val) groupSizeBits
>
> placeNewObject' :: PPtr () -> KernelObject -> Int -> Kernel ()
> placeNewObject' ptr val groupSizeBits = do

Calculate the size (as a power of two) of the region the new object will be placed in.

>         let objSizeBits = objBitsKO val
>         let totalBits = objSizeBits + groupSizeBits

Check the alignment of the specified region.

>         unless (fromPPtr ptr .&. mask totalBits == 0) $
>             alignError totalBits

Fetch the "PSpace" structure from the current state, and search the region for existing objects; fail if any are found.

>         ps <- gets ksPSpace
>         let end = fromPPtr ptr + ((1 `shiftL` totalBits) - 1)
>         let (before, _) = lookupAround2 end (psMap ps)
>         case before of
>             Nothing -> return ()
>             Just (x, _) -> assert (x < fromPPtr ptr)
>                 "Object creation would destroy an existing object"

Make a list of addresses at which to create objects.

>         let addresses = map
>                 (\n -> fromPPtr ptr + n `shiftL` objSizeBits)
>                 [0 .. (1 `shiftL` groupSizeBits) - 1]

Insert the objects into the "PSpace" map.

>         let map' = foldr
>                (\addr map -> Data.Map.insert addr val map)
>                (psMap ps) addresses

Update the state with the new "PSpace" map.

>         let ps' = ps { psMap = map' }
>         modify (\ks -> ks { ksPSpace = ps'})

\subsubsection{Deleting Objects}

No type checks are performed when deleting objects; "deleteObjects" simply deletes every object in the given region. If an object partially overlaps with the given region but is not completely inside it, this function's behaviour is undefined.

> deleteObjects :: PPtr a -> Int -> Kernel ()
> deleteObjects ptr bits = do
>         unless (fromPPtr ptr .&. mask bits == 0) $
>             alignError bits
>         stateAssert (deletionIsSafe ptr bits)
>             "Object deletion would leave dangling pointers"
>         doMachineOp $ freeMemory (PPtr (fromPPtr ptr)) bits
>         ps <- gets ksPSpace
>         let inRange = (\x -> x .&. (- mask bits) - 1 == fromPPtr ptr)
>         let map' = Data.Map.filterWithKey
>                         (\x _ -> not (inRange x))
>                         (psMap ps)
>         let ps' = ps { psMap = map' }
>         modify (\ks -> ks { ksPSpace = ps'})

delete the ghost state just in case the deleted objects have been user pages or cnodes.


>         modify (\ks -> ks { gsUserPages = (\x -> if inRange x
>                                    then Nothing else gsUserPages ks x) })
>         modify (\ks -> ks { gsCNodes = (\x -> if inRange x
>                                    then Nothing else gsCNodes ks x) })
>         stateAssert ksASIDMapSafe "Object deletion would leave dangling PD pointers"

In "deleteObjects" above, we assert "deletionIsSafe"; that is, that there are no pointers to these objects remaining elsewhere in the kernel state. Since we cannot easily check this in the Haskell model, we assume that it is always true; the assertion is strengthened during translation into Isabelle.

> deletionIsSafe :: PPtr a -> Int -> KernelState -> Bool
> deletionIsSafe _ _ _ = True

After deletion, we assert ksASIDMapSafe which states that there are page directories at the addresses in the asid map. Again, the real assertion is only inserted in the translation to the theorem prover. 

> ksASIDMapSafe :: KernelState -> Bool
> ksASIDMapSafe _ = True


\subsubsection{Reserving Memory Regions}

The "reserveFrame" function marks a page-sized physical region as being in use for kernel or user data. This prevents any new kernel objects being created in these regions, but does not store any real data in the "PSpace".

\begin{impdetails}
This is intended for use by alternate implementations of "placeNewObject", for objects that are stored outside the "PSpace" structure.
\end{impdetails}

> reserveFrame :: PPtr a -> Bool -> Kernel ()
> reserveFrame ptr isKernel = do
>         let val = if isKernel then KOKernelData else KOUserData
>         placeNewObject' (PPtr (fromPPtr ptr)) val 0
>         return ()

\subsubsection{Access Failures}

These two functions halt the kernel with an error message when a memory access is performed with incorrect type or alignment.

> typeError :: Monad m => String -> KernelObject -> m a
> typeError t1 t2 = fail ("Wrong object type - expected " ++ t1 ++ 
>                         ", found " ++ (kernelObjectTypeName t2))

> alignError :: Monad m => Int -> m a
> alignError n = fail ("Unaligned access - lowest " ++
>                       (show n) ++ " bits must be 0")

> alignCheck :: Monad m => Word -> Int -> m ()
> alignCheck x n = unless (x .&. mask n == 0) $ alignError n

> sizeCheck :: Monad m => Word -> Maybe Word -> Int -> m ()
> sizeCheck _ Nothing _ = return ()
> sizeCheck start (Just end) n =
>     when (end - start < 1 `shiftL` n)
>         (fail ("Object must be at least 2^" ++ (show n) ++ " bytes long."))

\subsubsection{Accessing user data}

The following functions are used to access words in user-accessible data pages. They are equivalent to "loadWord" and "storeWord", except that they also assert that the pointer is in a user data page.

> loadWordUser :: PPtr Word -> Kernel Word
> loadWordUser p = do
>     stateAssert (pointerInUserData p)
>         "loadWordUser needs a user data page"
>     doMachineOp $ loadWord p

> storeWordUser :: PPtr Word -> Word -> Kernel ()
> storeWordUser p w = do
>     stateAssert (pointerInUserData p)
>         "storeWordUser needs a user data page"
>     doMachineOp $ storeWord p w

The following predicate is used above to assert that the pointer is a valid pointer to user data. It is always "True" here, but is replaced with a stronger assertion in the Isabelle translation. % FIXME: this can probably actually be stronger here too

> pointerInUserData :: PPtr Word -> KernelState -> Bool
> pointerInUserData _ _ = True


