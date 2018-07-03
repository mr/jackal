{-# LANGUAGE RecordWildCards #-}

module Network.Jackal.Server
    ( app
    ) where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.ByteString (ByteString)
import Data.IORef (readIORef)
import qualified Data.Vector as V
import Network.Jackal.Api
import Network.Jackal.Server.App
import Network.Jackal.Server.Types
import Network.RTorrent
    ( TorrentInfo(..)
    )
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

torrentInfoToProgress :: TorrentInfo -> TorrentProgress
torrentInfoToProgress TorrentInfo{..} = TorrentProgress {
    tpName = torrentName,
    tpCurrent = torrentSize - torrentBytesLeft,
    tpTotal = torrentSize
}

ftpsPendingToPending :: FTPSPending -> Pending
ftpsPendingToPending FTPSPending{..} = Pending {
    pName = torrentName fpInfo
}

ftpsCalcToCalculating :: FTPSCalc -> Calculating
ftpsCalcToCalculating FTPSCalc{..} = Calculating {
    cName = torrentName fcInfo
}

ftpsJobToProgress :: MonadIO m => FTPSJob -> m FtpProgress
ftpsJobToProgress FTPSJob{..} = do
    progress <- liftIO $ readIORef fjProgress
    return $ FtpProgress {
        fpCurrent = progress,
        fpTotal = torrentSize fjInfo
    }

downloadQueueToJackalProgress :: MonadIO m => DownloadQueue -> m JackalProgress
downloadQueueToJackalProgress DownloadQueue{..} = do
    let torrents = fmap torrentInfoToProgress dqTorrents
        pending = V.toList $ fmap ftpsPendingToPending dqPending
        calculating = V.toList $ fmap ftpsCalcToCalculating dqCalculating
    ftp <- V.toList <$> mapM ftpsJobToProgress dqCurrent
    return $ JackalProgress torrents pending calculating ftp

appToHandler :: ServerEnv -> ServerApp a -> Handler a
appToHandler env app = liftIO $ runReaderT (unServerApp app) env

jackalApi :: Proxy JackalApi
jackalApi = Proxy

start :: ByteString -> ServerApp NoContent
start torrent = do
    liftIO $ print "starting"
    writeCommandChannel (Start torrent)
    return NoContent

progress :: ServerApp JackalProgress
progress = readDownloadQueue >>= downloadQueueToJackalProgress

serverAppT :: ServerT JackalApi ServerApp
serverAppT = start :<|> progress

serverApp :: ServerEnv -> Server JackalApi
serverApp env = hoistServer jackalApi (appToHandler env) serverAppT

app :: ServerEnv -> Application
app env = serve jackalApi $ serverApp env
