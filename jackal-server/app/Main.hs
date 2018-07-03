module Main where

import Network.Jackal.Server
import Network.Jackal.Server.Download
import Network.Wai.Handler.Warp

main :: IO ()
main = do
    env <- download
    run 8080 $ app env
