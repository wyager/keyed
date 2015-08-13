{-# LANGUAGE TypeFamilies, NoImplicitPrelude,
    FlexibleInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Keyed
-- Copyright   :  (c) William Yager 2015
-- License     :  BSD
-- Maintainer  :  will (dot) yager (at) gmail (dot) com
-- Stability   :  provisional
-- Portability :  portable
--
-- This module provides functions for generic indexing of Monadic data structures.
-----------------------------------------------------------------------------

module Data.MKeyed (
    MKeyed, MKey, MValue, MContainer,
    indexM, lookupM, (!!), (!!?)
) where

import Data.Maybe (Maybe(Just, Nothing))
import Data.Ord (Ord, (>=))
import Data.Int (Int)
import Control.Monad (fmap, return)
import Control.Monad.ST (ST)
import System.IO (IO)

import Data.Vector.Mutable as VM

-- | Data structures that allow monadic lookup of values.
class MKeyed d where
    -- | The key to look up by.
    type MKey d
    -- | The value that is returned.
    type MValue d
    -- | The container it is returned in.
    type MContainer d :: * -> *
    indexM :: d -> MKey d -> MContainer d (MValue d)
    lookupM :: d -> MKey d -> MContainer d (Maybe (MValue d))

-- | An inline version of `indexM`.
(!!) :: MKeyed d => d -> MKey d -> MContainer d (MValue d)
(!!) = indexM

-- | An inline version of `lookupM`.
(!!?) :: MKeyed d => d -> MKey d -> MContainer d (Maybe (MValue d))
(!!?) = lookupM

-- How to deal with this vs. STVector?
--instance MKeyed (VM.IOVector a) where 
--    type MKey (VM.IOVector a) = Int
--    type MValue (VM.IOVector a) = a
--    type MContainer (VM.IOVector a) = IO
--    indexM = VM.read
--    lookupM v i = if i >= VM.length v
--        then return Nothing
--        else fmap Just (VM.read v i)

instance MKeyed (VM.STVector s a) where
    type MKey (VM.STVector s a) = Int
    type MValue (VM.STVector s a) = a
    type MContainer (VM.STVector s a) = ST s
    indexM = VM.read
    lookupM v i = if i >= VM.length v
        then return Nothing
        else fmap Just (VM.read v i)