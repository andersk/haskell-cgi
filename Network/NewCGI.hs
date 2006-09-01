-----------------------------------------------------------------------------
-- |
-- Module      :  Network.NewCGI
-- Copyright   :  (c) Bjorn Bringert 2006
-- License     :  BSD-style
--
-- Maintainer  :  bjorn@bringert.net
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module allows backwards compatibility with
-- programs which use the old Network.NewCGI module name.
--
-- This module is deprecated and will be removed in the future.
-- Use "Network.CGI" instead.
--
-----------------------------------------------------------------------------

module Network.NewCGI 
    {-# DEPRECATED "Use Network.CGI instead" #-} 
    (module Network.CGI) where

import Network.CGI
