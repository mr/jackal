module Main where

import Network.Jackal.Server
import Network.Wai.Handler.Warp

main :: IO ()
main = run 8080 app
