module Network.Jackal.Server.Types
    ( DownloadCommand(..)
    , DownloadQueue(..)
    , FTPSettings(..)
    , FTPSJob(..)
    , FTPSPending(..)
    , FTPSCalc(..)
    , RTorrentSettings(..)
    , Settings(..)
    ) where

import Control.Concurrent.Async (Async)
import Control.Exception as E
import Data.ByteString (ByteString)
import Data.IORef (IORef)
import Data.Vector (Vector)
import Network.Jackal.Server.Types.Config
    ( FTPSettings(..)
    , RTorrentSettings(..)
    , Settings(..)
    )
import Network.RTorrent (TorrentInfo(..))

-- TODO
-- Placeholder for sending commands to the download queue
-- Right now Start takes a filename and the others crash the app
data DownloadCommand = Pause | Stop | Start ByteString
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
