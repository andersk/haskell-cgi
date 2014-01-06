{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Network.CGI.Monad
-- Copyright   :  (c) Bjorn Bringert 2006
-- License     :  BSD-style
--
-- Maintainer  :  Anders Kaseorg <andersk@mit.edu>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Internal stuff that most people shouldn't have to use.
-- This module mostly deals with the 
-- internals of the CGIT monad transformer.
--
-----------------------------------------------------------------------------

module Network.CGI.Monad (
  -- * CGI monad class
  MonadCGI(..), 
  -- * CGI monad transformer
  CGIT(..), CGI,
  runCGIT,
  -- * Request info
  CGIRequest(..),
  -- * Error handling
  throwCGI, catchCGI, tryCGI, handleExceptionCGI,
 ) where

import Prelude hiding (catch)
#if MIN_VERSION_base(4,0,0)
import Control.Exception
#else
import Control.Exception.Extensible
#endif
  as Exception (SomeException, throwIO)
import Control.Monad (liftM)
import Control.Monad.CatchIO (MonadCatchIO, block, catch, try, unblock)
import Control.Monad.Error (MonadError(..))
import Control.Monad.Reader (ReaderT(..), asks)
import Control.Monad.Writer (WriterT(..), tell)
import Control.Monad.Trans (MonadTrans, MonadIO, liftIO, lift)
import Data.Typeable (Typeable(..), Typeable1(..), 
                      mkTyConApp, mkTyCon)

import Network.CGI.Protocol


--
-- * CGIT monad transformer
--

-- | A simple CGI monad with just IO.
type CGI a = CGIT IO a

-- | The CGIT monad transformer.
newtype CGIT m a = CGIT { unCGIT :: ReaderT CGIRequest (WriterT Headers m) a }

instance (Typeable1 m, Typeable a) => Typeable (CGIT m a) where
    typeOf _ = mkTyConApp (mkTyCon "Network.CGI.Monad.CGIT") 
                [typeOf1 (undefined :: m a), typeOf (undefined :: a)]

instance (Functor m, Monad m) => Functor (CGIT m) where
    fmap f c = CGIT (fmap f (unCGIT c))

instance Monad m => Monad (CGIT m) where
    c >>= f = CGIT (unCGIT c >>= unCGIT . f)
    return = CGIT . return
    -- FIXME: should we have an error monad instead?
    fail = CGIT . fail

instance MonadIO m => MonadIO (CGIT m) where
    liftIO = lift . liftIO

instance MonadCatchIO m => MonadCatchIO (CGIT m) where
    CGIT m `catch` h = CGIT $ m `catch` (unCGIT . h)
    block (CGIT m) = CGIT (block m)
    unblock (CGIT m) = CGIT (unblock m)

-- | The class of CGI monads. Most CGI actions can be run in
--   any monad which is an instance of this class, which means that
--   you can use your own monad transformers to add extra functionality.
class Monad m => MonadCGI m where
    -- | Add a response header.
    cgiAddHeader :: HeaderName -> String -> m ()
    -- | Get something from the CGI request.
    cgiGet :: (CGIRequest -> a) -> m a

instance Monad m => MonadCGI (CGIT m) where
    cgiAddHeader n v = CGIT $ lift $ tell [(n,v)]
    cgiGet = CGIT . asks

instance MonadTrans CGIT where
    lift = CGIT . lift . lift

-- | Run a CGI action.
runCGIT :: Monad m => CGIT m a -> CGIRequest -> m (Headers, a)
runCGIT (CGIT c) = liftM (uncurry (flip (,))) . runWriterT . runReaderT c



--
-- * Error handling
--

instance MonadCatchIO m => MonadError SomeException (CGIT m) where
    throwError = throwCGI
    catchError = catchCGI

-- | Throw an exception in a CGI monad. The monad is required to be
--   a 'MonadIO', so that we can use 'throwIO' to guarantee ordering.
throwCGI :: (MonadCGI m, MonadIO m) => SomeException -> m a
throwCGI = liftIO . throwIO

-- | Catches any expection thrown by a CGI action, and uses the given 
--   exception handler if an exception is thrown.
catchCGI :: (MonadCGI m, MonadCatchIO m) => m a -> (SomeException -> m a) -> m a
catchCGI = catch

-- | Catches any exception thrown by an CGI action, and returns either
--   the exception, or if no exception was raised, the result of the action.
tryCGI :: (Functor m, MonadCGI m, MonadCatchIO m) => m a -> m (Either SomeException a)
tryCGI = try

{-# DEPRECATED handleExceptionCGI "Use catchCGI instead." #-}
-- | Deprecated version of 'catchCGI'. Use 'catchCGI' instead.
handleExceptionCGI :: (MonadCGI m, MonadCatchIO m) => m a -> (SomeException -> m a) -> m a
handleExceptionCGI = catchCGI
