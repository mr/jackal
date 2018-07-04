module Network.Jackal.Client where

import Data.ByteString (ByteString)
import Servant.API
import Servant.Client
import Network.Jackal.Api

start :: ByteString -> ClientM NoContent
progress :: ClientM JackalProgress
start :<|> progress = client jackalApi
