{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.Jackal.Server.App
    ( App
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

newtype App a = App { unApp :: ReaderT Env IO a } deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader Env
    , MonadIO
    , MonadDownloadQueue
    , MonadCommandChannel
    , MonadCommandChannelBroadcast
    , MonadDownloadThread
    )

data Env = Env
    { envDownloadQueue :: !(IORef DownloadQueue)     -- ^ Current download progress
    , envCommandChannel :: !(TChan DownloadCommand)  -- ^ Reader channel for getting download commands
    , envCommandChannelB :: !(TChan DownloadCommand) -- ^ Writer channel for sending downloading commands
    , envDownloadThread :: !(Async ())               -- ^ Running download thread
    , envConfig :: !Settings                         -- ^ Config
    , envManager :: !Manager                         -- ^ Manager for HTTP requests
    }

class HasDownloadQueue a where
    getDownloadQueue :: a -> IORef DownloadQueue
instance HasDownloadQueue (IORef DownloadQueue) where
    getDownloadQueue = id
instance HasDownloadQueue Env where
    getDownloadQueue = envDownloadQueue

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
instance HasCommandChannel Env where
    getComandChannel = envCommandChannel

class Monad m => MonadCommandChannel m where
    writeCommandChannel :: DownloadCommand -> m ()
    tryReadCommandChannel :: m (Maybe DownloadCommand)
instance (HasCommandChannel env, MonadIO m) => MonadCommandChannel (ReaderT env m) where
    writeCommandChannel dc = do
        env <- ask
        liftIO $ atomically $ writeTChan (getComandChannel env) dc
    tryReadCommandChannel = ask >>= liftIO . atomically . tryReadTChan . getComandChannel

class HasCommandChannelBroadcast a where
    getComandChannelBroadcast :: a -> TChan DownloadCommand
instance HasCommandChannelBroadcast (TChan DownloadCommand) where
    getComandChannelBroadcast = id
instance HasCommandChannelBroadcast Env where
    getComandChannelBroadcast = envCommandChannelB

class Monad m => MonadCommandChannelBroadcast m where
    broadcastCommandChannel :: DownloadCommand -> m ()
instance (HasCommandChannelBroadcast env, MonadIO m) => MonadCommandChannelBroadcast (ReaderT env m) where
    broadcastCommandChannel dc = do
        env <- ask
        liftIO $ atomically $ writeTChan (getComandChannelBroadcast env) dc

class HasDownloadThread a where
    getDownloadThread :: a -> Async ()
instance HasDownloadThread (Async ()) where
    getDownloadThread = id
instance HasDownloadThread Env where
    getDownloadThread = envDownloadThread

class Monad m => MonadDownloadThread m where
    pollDownloadThread :: m (Maybe (Either E.SomeException ()))
instance (HasDownloadThread env, MonadIO m) => MonadDownloadThread (ReaderT env m) where
    pollDownloadThread = ask >>= liftIO . poll . getDownloadThread

class HasConfig a where
    getConfig :: a -> Settings
instance HasConfig (Settings) where
    getConfig = id
instance HasConfig Env where
    getConfig = envConfig

class HasManager a where
    getManager :: a -> Manager
instance HasManager (Manager) where
    getManager = id
instance HasManager Env where
    getManager = envManager
