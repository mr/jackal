{-# LANGUAGE RecordWildCards #-}

module Network.Jackal.Server.Download (
    DownloadState(..),
    DownloadCommand(..),
    ftpsDownloadPath,
    download,
    downloadInit,
    callRTorrentWithManager,
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

import Network.Jackal.Server.Types
import network.Jackal.Server.App

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

download
    :: MonadIO m
    => Manager
    -> m
        ( Event (Vector DownloadState)
        , TChan DownloadCommand
        , Async ()
        )
download m = do
    (stateE, fire) <- newEvent
    bcc <- liftIO newBroadcastTChanIO
    cc <- liftIO $ atomically $ dupTChan bcc
    dr <- liftIO $ async (downloadInit m fire cc)
    return (stateE, bcc, dr)

downloadInit
    :: Manager
    -> Handler (Vector DownloadState)
    -> TChan DownloadCommand
    -> IO ()
downloadInit m fire cc = do
    configBS <- LBS.readFile "config.json"
    case eitherDecode configBS of
        Left err -> print err
        Right settings -> downloadLoop m settings initDownloadQueue fire cc

downloadLoop
    ::
        ( MonadIO m
        , MonadMask m
        , MonadDownloadQueue m
        , MonadCommandChannel m
        , MonadReader env m
        , HasConfig env
        , HasManager env
        )
    => m ()
downloadLoop = do
    -- get all commands that were sent
    -- and modify the download queue based on them
    commandM <- tryReadCommandChannel
    case commandM of
        Just command -> performCommand command
        Nothing -> return ()

    m <- getManager <$> ask
    settings <- getConfig <$> ask

    -- get all torrents from server to be filtered by TorrentID
    einfos <- callRTorrentWithBasicAuth m (sRTorrent settings) getTorrents
    case einfos of
        Left _ -> return ()
        Right infos -> do
            -- modify DQ based on completed torrents on the server
            updateQueue infos
    -- restart loop
    downloadLoop m settings dq' fire cc

updateQueue
    ::
        ( MonadIO m
        , MonadMask m
        , MonadDownloadQueue m
        , MonadReader env m
        , HasConfig env
        )
    -> [TorrentInfo]
    -> m ()
updateQueue infos = do
    dq <- readDownloadQueue
    settings <- sFTP <$> getConfig
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
        (fmap isJust . poll . fcPath)
        allCalculating

    -- Get the new paths pending download by
    -- waiting on the finished calculation
    newPendings <- V.forM willPend $ \(FTPSCalc i calc) -> do
        result <- waitCatch calc
        return $ case result of
            Left _ -> V.fromList []
            Right x -> FTPSPending i <$> (V.fromList x)

    -- Remove completed downloads
    -- TODO add these to a completed queue
    (_, stillDownloading) <- partitionM
        (fmap isJust . poll . fjResult)
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
    let len = length $ dqCurrent dq'
        maxConnections = ftpMaxConnections settings
    if len < maxConnections
        then enqueueFTPS
        else return ()

ftpsGetNewJobs :: FTPSettings -> TorrentInfo -> IO [String]
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
ftpsAllFiles :: FTP.Handle -> String -> IO [String]
ftpsAllFiles h dir = do
    fileInfo <- FTP.mlst h dir
    printM fileInfo
    let isType t = fromMaybe False
            . fmap (== t)
            . Map.lookup "type"
            . FTP.mrFacts
        isFile = isType "file"
        isDir = isType "dir"
        isntSpecialDir = \x -> (x /= ".") && (x /= "..")
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
            -- recursively find all files in directories we just found
            fileChildren <- mapM (ftpsAllFiles h) dirs
            return $ files <> join fileChildren
        else
            -- single files should return just themselves
            return $ if isFile fileInfo
                then [FTP.mrFilename fileInfo]
                else []

enqueueFTPS
    ::
        ( MonadMask m
        , MonadIO m
        , MonadDownloadQueue m
        , MonadReader env m
        , HasConfig env
        )
    => m ()
enqueueFTPS = do
    dq <- readDownloadQueue
    ftpSettings@FTPSettings{..} <- getConfig <$> ask
    let len = length $ dqCurrent dq
        n = ftpMaxConnections - len
        (enqueue, rest) = V.splitAt n (dqPending dq)
        ftransferring = dqCurrent dq
    started <- V.forM enqueue $ \(FTPSPending info path) -> do
        ftpsIORef <- liftIO $ newIORef 0
        a <- liftIO $ async $ ftpsDownloadPath ftpSettings path ftpsIORef
        return $ FTPSJob info ftpsIORef a
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
    => String
    -> RTorrentSettings
    -> Manager
    -> m (Either String TorrentInfo)
startTorrent filename rSettings m = do
    tfile <- liftIO $ BS.readFile filename
    case infoHash tfile of
        Nothing -> return $ Left "Invalid torrent file"
        Just hash -> do
            callRTorrentWithBasicAuth m rSettings (loadStartTorrentRaw tfile)
            callRTorrentWithBasicAuth m rSettings (getTorrent hash)

-- Placeholder!! TODO
-- This function will take a command and use it to modify the DownloadQueue
performCommand ::
    ( MonadDownloadQueue m
    , MonadIO m
    , MonadReader env m
    , HasConfig env
    , HasManager env
    )
    => DownloadCommand
    -> m ()
performCommand (Start filename)= do
    rtorrentConfig <- sRTorrent . getConfig <$> ask
    manager <- getManager <$> ask
    eStartedInfo <- startTorrent filename rtorrentConfig manager
    case eStartedInfo of
        Right startedInfo -> modifyDownloadQueue $ \dq -> dq {
            dqTorrents = dqTorrents dq <> [startedInfo]
        }
        Left err -> do
            putStrLn "Error starting torrent: "
            print err
performCommand c = undefined

-- TODO
-- This could be more efficient, consider a contribution to vector
partitionM :: Monad m => (a -> m Bool) -> Vector a -> m (Vector a, Vector a)
partitionM m xs = V.foldM foldFunc (V.fromList [], V.fromList []) xs
    where
        foldFunc (vas, vbs) x = do
            res <- m x
            return $ if res
                then (vas <> V.fromList [x], vbs)
                else (vas, vbs <> V.fromList [x])
