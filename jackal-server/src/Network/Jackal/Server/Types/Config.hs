{-# LANGUAGE OverloadedStrings #-}

module Network.Jackal.Server.Types.Config
    ( DownloadCommand(..)
    , Settings(..)
    , RTorrentSettings(..)
    , FTPSettings(..)
    ) where

import Data.Aeson
    ( FromJSON
    , ToJSON
    , parseJSON
    , withObject
    , (.:)
    , (.:?)
    , (.!=)
    )

-- TODO
-- Placeholder for sending commands to the download queue
-- Right now Start takes a filename and the others crash the app
data DownloadCommand = Pause | Stop | Start String
    deriving (Show)

data Settings = Settings {
    sRTorrent :: RTorrentSettings,
    sFTP :: FTPSettings
} deriving (Show)

instance FromJSON Settings where
    parseJSON = withObject "Settings" $ \v -> Settings
        <$> v .: "rtorrent"
        <*> v .: "ftp"

-- Info for connecting to RTorrent
data RTorrentSettings = RTorrentSettings {
    rtUser :: String,
    rtPass :: String,
    rtUrl :: String
} deriving (Show)

instance FromJSON RTorrentSettings where
    parseJSON = withObject "RTorrentSettings" $ \v -> RTorrentSettings
        <$> v .: "user"
        <*> v .: "password"
        <*> v .: "url"

-- Info for connecting to FTPS server
data FTPSettings = FTPSettings {
    ftpUser :: String,
    ftpPass :: String,
    ftpHost :: String,
    ftpPort :: Int,
    ftpHome :: String,
    ftpDestination :: String,
    ftpMaxConnections :: Int
} deriving (Show)

instance FromJSON FTPSettings where
    parseJSON = withObject "FTPSettings" $ \v -> FTPSettings
        <$> v .:  "user"
        <*> v .:  "password"
        <*> v .:  "host"
        <*> v .:? "port" .!= 21
        <*> v .:  "home"
        <*> v .:  "destination"
        <*> v .:  "maxConnections"
