{-# LANGUAGE TypeFamilies, NoImplicitPrelude #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Keyed
-- Copyright   :  (c) William Yager 2015
-- License     :  BSD
-- Maintainer  :  will (dot) yager (at) gmail (dot) com
-- Stability   :  provisional
-- Portability :  portable
--
-- This module provides functions for generic indexing of data structures.
-----------------------------------------------------------------------------

module Data.Keyed (
    Keyed, Key, Value,
    index, lookup, (!), (!?)
) where

import Data.Maybe (Maybe(Just, Nothing))
import Data.Int (Int)
import Prelude ((-))
import Data.Ord (Ord, (>=))

import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import qualified Data.Map.Strict as MS
import qualified Data.Map.Lazy as ML
import qualified Data.List as List
import qualified Data.Sequence as Seq

-- | Data structures that allow lookup of values.
class Keyed d where
    -- | The key to look up by.
    type Key d
    -- | The value that is returned.
    type Value d
    -- | Raises an exception on invalid key.
    index :: d -> Key d -> Value d
    -- | Returns `Nothing` on invalid key.
    lookup :: d -> Key d -> Maybe (Value d)

-- | An inline version of `index`.
(!) :: Keyed d => d -> Key d -> Value d
(!) = index

-- | An inline version of `lookup`.
(!?) :: Keyed d => d -> Key d -> Maybe (Value d)
(!?) = lookup

instance Keyed (Seq.Seq a) where
    type Key (Seq.Seq a) = Int
    type Value (Seq.Seq a) = a
    index = Seq.index
    lookup seq i = if i >= Seq.length seq
        then Nothing
        else Just (index seq i)

instance Keyed [a] where
    type Key [a] = Int
    type Value [a] = a
    index = (List.!!)
    lookup [] _ = Nothing
    lookup (x : xs) 0 = Just x
    lookup (x : xs) n = lookup xs (n-1)

instance Ord k => Keyed (MS.Map k v) where
    type Key (MS.Map k v) = k
    type Value (MS.Map k v) = v
    index = (MS.!)
    lookup m k = MS.lookup k m

instance Keyed (V.Vector a) where
    type Key (V.Vector a) = Int
    type Value (V.Vector a) = a
    index = (V.!)
    lookup = (V.!?)

instance VP.Prim a => Keyed (VP.Vector a) where
    type Key (VP.Vector a) = Int
    type Value (VP.Vector a) = a
    index = (VP.!)
    lookup = (VP.!?)

instance VS.Storable a => Keyed (VS.Vector a) where
    type Key (VS.Vector a) = Int
    type Value (VS.Vector a) = a
    index = (VS.!)
    lookup = (VS.!?)

instance VU.Unbox a => Keyed (VU.Vector a) where
    type Key (VU.Vector a) = Int
    type Value (VU.Vector a) = a
    index = (VU.!)
    lookup = (VU.!?)