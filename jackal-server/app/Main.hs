module Main where

import Control.Concurrent.Async (link)
import Network.Jackal.Server.App (getDownloadThread)
import Network.Jackal.Server
import Network.Jackal.Server.Download
import Network.Wai.Handler.Warp

main :: IO ()
main = do
    env <- download
    -- crash when the download thread does
    link $ getDownloadThread env
    run 8080 $ app env
