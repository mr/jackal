{-# LANGUAGE ScopedTypeVariables #-}

module Network.Jackal.Server.RTorrent (callRTorrentWithBasicAuth) where

import qualified Control.Exception as E
import Control.Exception.Safe
    ( Handler(..)
    , catches
    )
import Control.Monad.Except
    ( ExceptT
    , runExceptT
    , throwError
    )
import Control.Monad.IO.Class
    ( liftIO
    , MonadIO
    )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBC
import Network.HTTP.Conduit
    ( applyBasicAuth
    , Manager
    , Request
    , RequestBody(..)
    , httpLbs
    , requestBody
    , responseBody
    , parseUrlThrow
    )
import qualified Network.RTorrent.Command.Internals as RC
import Network.RTorrent
    ( Command
    , Ret
    )
import Network.XmlRpc.Internals
    ( MethodResponse(..)
    , Value(..)
    , MethodCall(..)
    , renderCall
    , parseResponse
    , getType
    )

import Network.Jackal.Server.Types

callXml :: RC.RTMethodCall -> LBS.ByteString
callXml calls = renderCall $
    MethodCall "system.multicall" [RC.runRTMethodCall calls]

rtRequest :: Request -> RC.RTMethodCall -> Request
rtRequest req' calls = req' { requestBody = RequestBodyLBS $ callXml calls }

callRTorrentWithManagerRaw
    :: Manager
    -> Request
    -> RC.RTMethodCall
    -> ExceptT String IO Value
callRTorrentWithManagerRaw manager req' calls = do
    let req = rtRequest req' calls
    res <- liftIO $ httpLbs req manager
    response <- parseResponse $ LBC.unpack $ responseBody res
    case response of
        Return ret -> case ret of
            ValueArray arr -> return $ ValueArray arr
            val -> throwError $ "Got value of type " ++ show (getType val)
        Fault code err -> throwError $ show code ++ ": " ++ err

callRTorrentWithManager
    :: Command a
    => Manager
    -> Request
    -> a
    -> IO (Either String (Ret a))
callRTorrentWithManager m req command =
    runExceptT ( do
        ret <- callRTorrentWithManagerRaw m req (RC.commandCall command)
        RC.commandValue command ret
    ) `catches` [
        Handler (\(e :: E.IOException) -> return $ Left $ show e),
        Handler (\(e :: E.PatternMatchFail) -> return $ Left $ show e)
    ]

callRTorrentWithBasicAuth
    :: MonadIO m
    => Command a
    => Manager
    -> RTorrentSettings
    -> a
    -> m (Either String (Ret a))
callRTorrentWithBasicAuth m rTorrentSettings command = do
    let (RTorrentSettings user pass url) = rTorrentSettings
    req <- liftIO $ applyBasicAuth
        (BS.pack $ map (toEnum . fromEnum) user)
        (BS.pack $ map (toEnum . fromEnum) pass)
        <$> parseUrlThrow url
    liftIO $ callRTorrentWithManager m req command
