--
-- Copyright 2014, General Dynamics C4 Systems
--
-- This software may be distributed and modified according to the terms of
-- the GNU General Public License version 2. Note that NO WARRANTY is provided.
-- See "LICENSE_GPLv2.txt" for details.
--
-- @TAG(GD_GPL)
--

module Data.Helpers where

import Data.Array

funPartialArray :: (Ix i) => (i -> a) -> (i, i) -> Array i a
funPartialArray f b = listArray b $ (map f) $ range b

funArray :: (Ix i, Bounded i) => (i -> a) -> Array i a
funArray f = funPartialArray f (minBound, maxBound)

