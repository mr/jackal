{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.Jackal.Server.App
    ( ServerApp(..)
    , ServerEnv(..)
    , DownloadApp(..)
    , DownloadEnv(..)
    , MonadDownloadQueue
    , modifyDownloadQueue
    , readDownloadQueue
    , setDownloadQueue
    , MonadCommandChannel
    , writeCommandChannel
    , tryReadCommandChannel
    , MonadDownloadThread
    , pollDownloadThread
    , HasManager
    , getManager
    , HasConfig
    , getConfig
    ) where

import Control.Concurrent.Async
    ( Async
    , poll
    )
import Control.Concurrent.STM
    ( atomically
    )
import Control.Concurrent.STM.TChan
    ( TChan
    , tryReadTChan
    , writeTChan
    )
import qualified Control.Exception as E
import Control.Monad.IO.Class
    ( MonadIO
    , liftIO
    )
import Control.Monad.Reader
    ( ReaderT
    , ask
    , MonadReader
    )
import Data.IORef
    ( IORef
    , readIORef
    , modifyIORef'
    )
import Network.HTTP.Conduit
    ( Manager
    )

import Network.Jackal.Server.Types

newtype ServerApp a = ServerApp { unServerApp :: ReaderT ServerEnv IO a } deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader ServerEnv
    , MonadIO
    , MonadDownloadQueue
    , MonadCommandChannel
    , MonadDownloadThread
    )

newtype DownloadApp a = DownloadApp { unDownloadApp :: ReaderT DownloadEnv IO a } deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader DownloadEnv
    , MonadIO
    , MonadDownloadQueue
    , MonadCommandChannel
    )

data ServerEnv = ServerEnv
    { senvDownloadQueue :: !(IORef DownloadQueue)     -- ^ Current download progress
    , senvCommandChannel :: !(TChan DownloadCommand)  -- ^ Writer channel for sending downloading commands
    , senvDownloadThread :: !(Async ())               -- ^ Running download thread
    , senvConfig :: !Settings                         -- ^ Config
    , senvManager :: !Manager                         -- ^ Manager for HTTP requests
    }

data DownloadEnv = DownloadEnv
    { denvDownloadQueue :: !(IORef DownloadQueue)     -- ^ Current download progress
    , denvCommandChannel :: !(TChan DownloadCommand)  -- ^ Reader channel for getting download commands
    , denvConfig :: !Settings                         -- ^ Config
    , denvManager :: !Manager                         -- ^ Manager for HTTP requests
    }

class HasDownloadQueue a where
    getDownloadQueue :: a -> IORef DownloadQueue
instance HasDownloadQueue (IORef DownloadQueue) where
    getDownloadQueue = id
instance HasDownloadQueue DownloadEnv where
    getDownloadQueue = denvDownloadQueue
instance HasDownloadQueue ServerEnv where
    getDownloadQueue = senvDownloadQueue

class Monad m => MonadDownloadQueue m where
    modifyDownloadQueue :: (DownloadQueue -> DownloadQueue) -> m ()
    readDownloadQueue :: m DownloadQueue
    setDownloadQueue :: DownloadQueue -> m ()
    setDownloadQueue = modifyDownloadQueue . const
instance (HasDownloadQueue env, MonadIO m) => MonadDownloadQueue (ReaderT env m) where
    modifyDownloadQueue f = do
        env <- ask
        liftIO $ modifyIORef' (getDownloadQueue env) f
    readDownloadQueue = ask >>= liftIO . readIORef . getDownloadQueue

class HasCommandChannel a where
    getComandChannel :: a -> TChan DownloadCommand
instance HasCommandChannel (TChan DownloadCommand) where
    getComandChannel = id
instance HasCommandChannel DownloadEnv where
    getComandChannel = denvCommandChannel
instance HasCommandChannel ServerEnv where
    getComandChannel = senvCommandChannel

class Monad m => MonadCommandChannel m where
    writeCommandChannel :: DownloadCommand -> m ()
    tryReadCommandChannel :: m (Maybe DownloadCommand)
instance (HasCommandChannel env, MonadIO m) => MonadCommandChannel (ReaderT env m) where
    writeCommandChannel dc = do
        env <- ask
        liftIO $ atomically $ writeTChan (getComandChannel env) dc
    tryReadCommandChannel = ask >>= liftIO . atomically . tryReadTChan . getComandChannel

class HasDownloadThread a where
    getDownloadThread :: a -> Async ()
instance HasDownloadThread (Async ()) where
    getDownloadThread = id
instance HasDownloadThread ServerEnv where
    getDownloadThread = senvDownloadThread

class Monad m => MonadDownloadThread m where
    pollDownloadThread :: m (Maybe (Either E.SomeException ()))
instance (HasDownloadThread env, MonadIO m) => MonadDownloadThread (ReaderT env m) where
    pollDownloadThread = ask >>= liftIO . poll . getDownloadThread

class HasConfig a where
    getConfig :: a -> Settings
instance HasConfig (Settings) where
    getConfig = id
instance HasConfig DownloadEnv where
    getConfig = denvConfig
instance HasConfig ServerEnv where
    getConfig = senvConfig

class HasManager a where
    getManager :: a -> Manager
instance HasManager (Manager) where
    getManager = id
instance HasManager DownloadEnv where
    getManager = denvManager
instance HasManager ServerEnv where
    getManager = senvManager
