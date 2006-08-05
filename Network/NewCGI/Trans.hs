{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances -fallow-overlapping-instances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Network.NewCGI.Trans
-- Copyright   :  (c) Bjorn Bringert 2004-2006
--                (c) Ian Lynagh 2005
-- License     :  BSD-style
--
-- Maintainer  :  bjorn@bringert.net
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Automatically lift MonadCGI through all monad transformers. 
--
-----------------------------------------------------------------------------

module Network.NewCGI.Trans (module Network.NewCGI) where

import Control.Monad.Trans

import Network.NewCGI
import Network.NewCGI.Internals

-- requires -fallow-undecidable-instances and -fallow-overlapping-instances
-- FIXME: can we achieve this without hairy extensions?
instance (MonadTrans t, MonadCGI m, Monad (t m)) => MonadCGI (t m) where
    cgiAddHeader n v = lift $ cgiAddHeader n v
    cgiGet = lift . cgiGet
