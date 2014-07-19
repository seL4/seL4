--
-- Copyright 2014, General Dynamics C4 Systems
--
-- This software may be distributed and modified according to the terms of
-- the GNU General Public License version 2. Note that NO WARRANTY is provided.
-- See "LICENSE_GPLv2.txt" for details.
--
-- @TAG(GD_GPL)
--

module Data.BinaryTree (
        BinaryTree, empty, isEmpty, findWithDefault, insert, adjust, delete,
                  Data.BinaryTree.lookup, adjustM
    ) where

-- Types

data BinaryTree a
    = Node { btTrue, btFalse :: BinaryTree a }
    | Leaf a
    | Empty

-- Creation

empty :: BinaryTree a
empty = Empty

-- Lookups

isEmpty :: [Bool] -> BinaryTree a -> Bool
isEmpty _ Empty = True
isEmpty (True:a) (Node { btTrue = t }) = isEmpty a t
isEmpty (False:a) (Node { btFalse = t}) = isEmpty a t
isEmpty _ _ = False

findWithDefault :: a -> [Bool] -> BinaryTree a -> ([Bool],a)
findWithDefault _ a (Leaf v) = (a,v)
findWithDefault d (True:a) (Node { btTrue = t }) = findWithDefault d a t
findWithDefault d (False:a) (Node { btFalse = t }) = findWithDefault d a t
findWithDefault d a _ = (a,d)

lookup :: Monad m => [Bool] -> BinaryTree a -> m ([Bool],a)
lookup a (Leaf v) = return (a,v)
lookup (True:a) (Node { btTrue = t }) = Data.BinaryTree.lookup a t
lookup (False:a) (Node { btFalse = t }) = Data.BinaryTree.lookup a t
lookup _ _ = fail "Data.BinaryTree.lookup: Key not found"

-- Insertion

insert :: [Bool] -> a -> BinaryTree a -> BinaryTree a
insert [] v Empty = Leaf v
insert (True:a) v n@(Node {}) = n { btTrue = insert a v $ btTrue n }
insert (False:a) v n@(Node {}) = n { btFalse = insert a v $ btFalse n }
insert (True:a) v Empty = Node { btTrue = insert a v Empty, btFalse = Empty }
insert (False:a) v Empty = Node { btFalse = insert a v Empty, btTrue = Empty }
insert _ _ _ = error "BinaryTree.insert: location is not empty"

-- Updates and Deletion

adjust :: ([Bool] -> a -> a) -> [Bool] -> BinaryTree a -> BinaryTree a
adjust f a (Leaf v) = Leaf $ f a v
adjust f (True:a) n@(Node {}) = n { btTrue = adjust f a $ btTrue n }
adjust f (False:a) n@(Node {}) = n { btFalse = adjust f a $ btFalse n }
adjust _ _ _ = error "BinaryTree.adjust: object not found"

adjustM :: Monad m => ([Bool] -> a -> m a) -> [Bool] -> BinaryTree a -> m (BinaryTree a)
adjustM f a (Leaf v) = f a v >>= return . Leaf 
adjustM f (True:a) n@(Node {}) = do { v <- adjustM f a $ btTrue n
                                    ; return $ n { btTrue = v }}
adjustM f (False:a) n@(Node {}) = do { v <- adjustM f a $ btFalse n
                                     ; return $ n { btFalse = v }}
adjustM _ _ _ = fail "BinaryTree.adjustM: object not found"

delete :: [Bool] -> BinaryTree a -> BinaryTree a
delete [] _ = Empty
delete _ Empty = Empty
delete (True:a) n@(Node {}) = flatten $ n { btTrue = delete a $ btTrue n }
delete (False:a) n@(Node {}) = flatten $ n { btFalse = delete a $ btFalse n }
delete _ _ = error "BinaryTree.delete: cannot partially delete object"

flatten :: BinaryTree a -> BinaryTree a
flatten (Node Empty Empty) = Empty
flatten t = t

