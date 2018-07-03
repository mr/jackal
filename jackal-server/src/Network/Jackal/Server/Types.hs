module Network.Jackal.Server.Types
    ( CalculatingState(..)
    , DownloadCommand(..)
    , DownloadQueue(..)
    , DownloadStage(..)
    , DownloadState(..)
    , FTPSettings(..)
    , FTPSState(..)
    , FTPSJob(..)
    , FTPSPending(..)
    , FTPSCalc(..)
    , PendingState(..)
    , PollResult
    , RTorrentSettings(..)
    , Settings(..)
    , TorrentingState(..)
    ) where

import Control.Concurrent.Async
    ( Async
    )

import Control.Exception as E

import Data.IORef
    ( IORef
    )

import Data.Vector
    ( Vector
    )

import Network.Jackal.Server.Types.Config
    ( DownloadCommand(..)
    , FTPSettings(..)
    , RTorrentSettings(..)
    , Settings(..)
    )

import Network.RTorrent
    ( TorrentInfo(..)
    )

-- Is the torrent downloading or are we getting from FTPS
data DownloadStage = Torrent | FTPS
    deriving (Show, Eq)

data TorrentingState = TorrentingState {
    tsInfo :: TorrentInfo
} deriving (Show, Eq)

type PollResult a = Maybe (Either E.SomeException a)

data FTPSState = FTPSState {
    fsProgress :: Int,
    fsResult :: PollResult Int
} deriving (Show)

data PendingState = PendingState {
    psInfo :: TorrentInfo,
    psPath :: String
} deriving (Show)

data CalculatingState = CalculatingState {
    csInfo :: TorrentInfo,
    csResult :: PollResult [String]
} deriving (Show)

-- Communication of jobs to main thread
data DownloadState
    = DSTorrenting TorrentingState
    | DSPending PendingState
    | DSCalculating CalculatingState
    | DSFTPS FTPSState
    deriving (Show)

-- Torrent being downloaded, progress reference, async download job
data FTPSJob = FTPSJob {
    fjInfo :: TorrentInfo,
    fjProgress :: IORef Int,
    fjResult :: Async Int
}

data FTPSPending = FTPSPending {
    fpInfo :: TorrentInfo,
    fpPath :: String
} deriving (Show)

data FTPSCalc = FTPSCalc {
    fcInfo :: TorrentInfo,
    fcPath :: Async [String]
}

-- Current downloads
-- current: currently downloading ftps
-- pending: waiting for space in current list to start ftps
-- calculating: finding full list of files to download
-- torrents: downloading on rtorrent upstream
data DownloadQueue = DownloadQueue {
    dqCurrent :: Vector FTPSJob,
    dqPending :: Vector FTPSPending,
    dqCalculating :: Vector FTPSCalc,
    dqTorrents :: [TorrentInfo]
}

instance Show DownloadQueue where
    show dq =
        "DownloadQueue {\n"
        <> "\tcurrent = " <> (show $ length $ dqCurrent dq) <> "\n"
        <> "\tpending = " <> (show $ dqPending dq) <> "\n"
        <> "\tcalculating = " <> (show $ dqPending dq) <> "\n"
        <> "\tdqTorrents = " <> (show $ dqTorrents dq) <> "\n"
        <> "}"
