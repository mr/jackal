{-# LANGUAGE RecordWildCards #-}

module Network.Jackal.Server.Download (
    DownloadCommand(..),
    ftpsDownloadPath,
    download,
    downloadInit,
) where

import Control.Concurrent.Async
import Data.IORef
import Control.Concurrent.STM.TChan
import Control.Monad.STM
import Network.HTTP.Conduit
import Control.Monad.Except
import qualified Control.Exception as E
import Control.Exception.Safe (Exception, MonadMask, catches)
import qualified Network.XmlRpc.Internals as Xml
import Network.XmlRpc.Internals
    ( MethodResponse(..)
    , Value(..)
    , MethodCall(..)
    , renderCall
    , parseResponse
    , getType
    )
import Data.BEncode
import Crypto.Hash
import Data.Monoid ((<>))
import Data.Aeson

import Data.List

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as LBC
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map

import Data.Vector (Vector)
import qualified Data.Vector as V

import Debug.Trace
import qualified Network.FTP.Client as FTP
import qualified Network.FTP.Client.Conduit as FC
import Conduit

import Data.Typeable

import System.FilePath
import System.Directory

import Data.Maybe
    ( fromMaybe
    , isJust
    )
import Control.Arrow ((***))
import Network.RTorrent
import Control.Monad.Reader

import Network.Jackal.Server.RTorrent
import Network.Jackal.Server.Types
import Network.Jackal.Server.App

printM :: (MonadIO m, Show s) => s -> m ()
printM = liftIO . print

(??) :: Maybe a -> a -> a
(??) = flip fromMaybe

-- bunch of empty lists to start with
initDownloadQueue :: DownloadQueue
initDownloadQueue = DownloadQueue {
    dqCurrent = V.fromList [],
    dqPending = V.fromList [],
    dqCalculating = V.fromList [],
    dqTorrents = []
}

download :: IO ServerEnv
download = do
    putStrLn "Starting server"
    bcc <- newBroadcastTChanIO
    cc <- atomically $ dupTChan bcc
    dq <- newIORef initDownloadQueue
    m <- newManager tlsManagerSettings
    configBS <- LBS.readFile "config.json"
    let (Right settings) = eitherDecode configBS
        denv = DownloadEnv dq cc settings m
    dt <- async $ downloadInit denv
    return $ ServerEnv dq bcc dt settings m

downloadInit :: DownloadEnv -> IO ()
downloadInit = runReaderT (unDownloadApp downloadLoop)

downloadLoop :: DownloadApp ()
downloadLoop = do
    -- get all commands that were sent
    -- and modify the download queue based on them
    commandM <- tryReadCommandChannel
    forM_ commandM performCommand

    m <- asks getManager
    settings <- asks getConfig

    -- get all torrents from server to be filtered by TorrentID
    einfos <- callRTorrentWithBasicAuth m (sRTorrent settings) getTorrents
    case einfos of
        Left _ -> return ()
        Right infos -> updateQueue infos
    -- restart loop
    downloadLoop

updateQueue :: [TorrentInfo] -> DownloadApp ()
updateQueue infos = do
    dq <- readDownloadQueue
    settings <- asks $ sFTP . getConfig
    -- Find IDs from RTorrent that we were already tracking and
    -- split into completed and uncompleted
    let oldQueueIDs = torrentId <$> dqTorrents dq
        validInfos = filter (flip any oldQueueIDs . (==) . torrentId) infos
        (torrenting, completed) =
            partition ((/= 0) . torrentBytesLeft) validInfos

    -- Create async jobs for finding all file paths in a torrent
    newCalculating <- liftIO $ forM completed $ \i -> do
        calc <- async $ ftpsGetNewJobs settings i
        return $ FTPSCalc i calc

    -- Append new path calculation jobs to the existing list
    let allCalculating = dqCalculating dq <> (V.fromList newCalculating)

    -- Split the calculation jobs into finished
    -- and still running
    (willPend, stillCalculating) <- partitionM
        (fmap isJust . liftIO . poll . fcMlsx)
        allCalculating

    -- Get the new paths pending download by
    -- waiting on the finished calculation
    newPendings <- V.forM willPend $ \(FTPSCalc i calc) -> do
        result <- liftIO $ waitCatch calc
        return $ case result of
            Left _ -> V.fromList []
            Right x -> FTPSPending i <$> V.fromList x

    -- Remove completed downloads
    -- TODO add these to a completed queue
    (_, stillDownloading) <- partitionM
        (fmap isJust . liftIO . poll . fjResult)
        $ dqCurrent dq

    -- Take all the files we found and create a single list out of them
    let newPending = join newPendings
    setDownloadQueue $ dq {
        dqCurrent = stillDownloading,
        dqPending = dqPending dq <> newPending,
        dqCalculating = stillCalculating,
        dqTorrents = torrenting
    }

    -- start FTPS jobs if there is room
    let len = length $ dqCurrent dq
        maxConnections = ftpMaxConnections settings
    when (len < maxConnections) enqueueFTPS

ftpsGetNewJobs :: FTPSettings -> TorrentInfo -> IO [FTP.MlsxResponse]
ftpsGetNewJobs FTPSettings{..} tInfo = do
    -- path is the root directory (or single file)
    -- we take the root off due to differences in RTorrent and the FTP server
    let path = torrentPath tInfo
        strippedPath = stripPrefix ftpHome path ?? path
    FTP.withFTPS ftpHost ftpPort $ \h _ -> do
        FTP.login h ftpUser ftpPass
        ftpsAllFiles h strippedPath

-- This function walks the FTP server directory tree with MLS(T/D)
-- It collects all files under the given path
ftpsAllFiles :: FTP.Handle -> String -> IO [FTP.MlsxResponse]
ftpsAllFiles h dir = do
    fileInfo <- FTP.mlst h dir
    printM fileInfo
    let isType t = (== Just t) . Map.lookup "type" . FTP.mrFacts
        isFile = isType "file"
        isDir = isType "dir"
        isntSpecialDir x = (x /= ".") && (x /= "..")
        prependRoot = (FTP.mrFilename fileInfo </>) . FTP.mrFilename
    if isDir fileInfo
        then do
            -- Remove special directories that we don't want to count
            allFileInfo <- filter (isntSpecialDir . FTP.mrFilename)
                <$> FTP.mlsd h dir
            printM allFileInfo

            -- split out files and directories
            -- and construct their absolute paths
            let (filesL, dirsL) = partition isFile allFileInfo
                files = prependRoot <$> filesL
                dirs  = prependRoot <$> dirsL
                filesMlsx = (\(name, resp) ->
                    resp {
                        FTP.mrFilename = name
                    }) <$> zip files allFileInfo
            -- recursively find all files in directories we just found
            fileChildren <- mapM (ftpsAllFiles h) dirs
            return $ filesMlsx <> join fileChildren
        else
            -- single files should return just themselves
            return $ [fileInfo | isFile fileInfo]

enqueueFTPS :: DownloadApp ()
enqueueFTPS = do
    dq <- readDownloadQueue
    Settings{..} <- asks getConfig
    let len = length $ dqCurrent dq
        n = ftpMaxConnections sFTP - len
        (enqueue, rest) = V.splitAt n (dqPending dq)
        ftransferring = dqCurrent dq
    started <- V.forM enqueue $ \(FTPSPending info mlsx) -> do
        ftpsIORef <- liftIO $ newIORef 0
        a <- liftIO
            $ async
            $ ftpsDownloadPath sFTP (FTP.mrFilename mlsx) ftpsIORef
        return $ FTPSJob info mlsx ftpsIORef a
    setDownloadQueue $ dq {
        dqCurrent = ftransferring <> started,
        dqPending = rest
    }

ftpsDownloadPath
    :: FTPSettings
    -> String
    -> IORef Int
    -> IO Int
ftpsDownloadPath FTPSettings{..} path comp = do
    let relPath = if isRelative path
            then path
            else makeRelative "/" path
        fullDestDir = ftpDestination </> takeDirectory relPath
        fullDest = ftpDestination </> relPath
    FTP.withFTPS ftpHost ftpPort $ \h _ -> do
        FTP.login h ftpUser ftpPass
        createDirectoryIfMissing True fullDestDir
        runConduitRes
            $ FC.retr h path
            .| iterMC (liftIO . modifyIORef' comp . (+) . BS.length)
            .| sinkFile fullDest
        return 0

infoHash :: BS.ByteString -> Maybe TorrentId
infoHash tfile = toTorrentId <$> getInfo (LBS.fromStrict tfile)
    where
        toTorrentId = TorrentId . show . sha1 . bPack
        getInfo file = bRead file >>= bDict >>= Map.lookup "info"

sha1 :: LBS.ByteString -> Digest SHA1
sha1 = hashlazy

bDict :: BEncode -> Maybe (Map.Map String BEncode)
bDict (BDict d) = Just d
bDict _ = Nothing

startTorrent
    :: MonadIO m
    => BS.ByteString
    -> RTorrentSettings
    -> Manager
    -> m (Either String TorrentInfo)
startTorrent tfile rSettings m =
    case infoHash tfile of
        Nothing -> do
            liftIO $ putStrLn "Bad torrent file"
            return $ Left "Invalid torrent file"
        Just hash -> do
            liftIO $ putStrLn "Loading raw torrent"
            callRTorrentWithBasicAuth m rSettings (loadStartTorrentRaw tfile)
            liftIO $ putStrLn "Getting torrent info"
            callRTorrentWithBasicAuth m rSettings (getTorrent hash)

-- Placeholder!! TODO
-- This function will take a command and use it to modify the DownloadQueue
performCommand :: DownloadCommand -> DownloadApp ()
performCommand (Start tfile)= do
    liftIO $ putStrLn "Starting torrent"
    rtorrentConfig <- asks $ sRTorrent . getConfig
    liftIO $ print rtorrentConfig
    manager <- asks getManager
    liftIO $ putStrLn "Got manager"
    eStartedInfo <- startTorrent tfile rtorrentConfig manager
    liftIO $ putStrLn "Attempted to start torrent"
    case eStartedInfo of
        Right startedInfo -> do
            liftIO $ putStrLn "Torrent started"
            modifyDownloadQueue $ \dq -> dq {
                dqTorrents = dqTorrents dq <> [startedInfo]
            }
        Left err ->
            liftIO $ putStrLn $ "Error starting torrent: " <> show err
performCommand c = undefined

-- TODO
-- This could be more efficient, consider a contribution to vector
partitionM :: Monad m => (a -> m Bool) -> Vector a -> m (Vector a, Vector a)
partitionM m = V.foldM foldFunc (V.fromList [], V.fromList [])
    where
        foldFunc (vas, vbs) x = do
            res <- m x
            return $ if res
                then (vas <> V.fromList [x], vbs)
                else (vas, vbs <> V.fromList [x])
