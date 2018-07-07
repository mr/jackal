{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Jackal.Api
    ( JackalApi
    , jackalApi
    , TorrentProgress(..)
    , Pending(..)
    , Calculating(..)
    , FtpProgress(..)
    , JackalProgress(..)
    ) where

import Data.Aeson
import Data.ByteString (ByteString)
import Data.Monoid
import Servant

type JackalApi
    =    "start" :> ReqBody '[OctetStream] ByteString :> Post '[JSON] NoContent
    :<|> "progress" :> Get '[JSON] JackalProgress

jackalApi :: Proxy JackalApi
jackalApi = Proxy

data TorrentProgress = TorrentProgress {
    tpName :: String,
    tpCurrent :: Int,
    tpTotal :: Int
}

instance ToJSON TorrentProgress where
    toJSON tp = object
        [ "name" .= tpName tp
        , "current" .= tpCurrent tp
        , "total" .= tpTotal tp
        ]
    toEncoding tp = pairs
        $  "name" .= tpName tp
        <> "current" .= tpCurrent tp
        <> "total" .= tpTotal tp

instance FromJSON TorrentProgress where
    parseJSON = withObject "TorrentProgress" $ \v -> TorrentProgress
        <$> v .: "name"
        <*> v .: "current"
        <*> v .: "total"

data Pending = Pending {
    pName :: String
}

instance ToJSON Pending where
    toJSON p = object ["name" .= pName p]
    toEncoding p = pairs $ "name" .= pName p

instance FromJSON Pending where
    parseJSON = withObject "Pending" $ \v -> Pending <$> v .: "name"

data Calculating = Calculating {
    cName :: String
}

instance ToJSON Calculating where
    toJSON c = object ["name" .= cName c]
    toEncoding c = pairs $ "name" .= cName c

instance FromJSON Calculating where
    parseJSON = withObject "Calculating" $ \v -> Calculating <$> v .: "name"

data FtpProgress = FtpProgress {
    fpName :: String,
    fpCurrent :: Int,
    fpTotal :: Int
}

instance ToJSON FtpProgress where
    toJSON fp = object
        [ "name" .= fpName fp
        , "current" .= fpCurrent fp
        , "total" .= fpTotal fp
        ]
    toEncoding fp = pairs
        $  "name" .= fpName fp
        <> "current" .= fpCurrent fp
        <> "total" .= fpTotal fp

instance FromJSON FtpProgress where
    parseJSON = withObject "FtpProgress" $ \v -> FtpProgress
        <$> v .: "name"
        <*> v .: "current"
        <*> v .: "total"

data JackalProgress = JackalProgress {
    jpTorrent :: [TorrentProgress],
    jpPending :: [Pending],
    jpCalculating :: [Calculating],
    jpFtp :: [FtpProgress]
}

instance ToJSON JackalProgress where
    toJSON jp = object
        [ "torrents" .= jpTorrent jp
        , "pending" .= jpPending jp
        , "calculating" .= jpCalculating jp
        , "ftps" .= jpFtp jp
        ]
    toEncoding jp = pairs
        $  "torrents" .= jpTorrent jp
        <> "pending" .= jpPending jp
        <> "calculating" .= jpCalculating jp
        <> "ftps" .= jpFtp jp

instance FromJSON JackalProgress where
    parseJSON = withObject "JackalProgress" $ \v -> JackalProgress
        <$> v .: "torrents"
        <*> v .: "pending"
        <*> v .: "calculating"
        <*> v .: "ftps"
