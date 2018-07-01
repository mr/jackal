module Network.Jackal.Server
    ( app
    ) where

import Data.ByteString (ByteString)
import Network.Jackal.Api
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

jackalApi :: Proxy JackalApi
jackalApi = Proxy

start :: ByteString -> Handler NoContent
start _ = return NoContent

progress :: Handler JackalProgress
progress = return $ JackalProgress
    [TorrentProgress 50 100, TorrentProgress 20 40]
    [Pending "butts"]
    [Calculating "more butts"]
    [FtpProgress 60 80, FtpProgress 70 90]

server :: Server JackalApi
server = start :<|> progress

app :: Application
app = serve jackalApi server
