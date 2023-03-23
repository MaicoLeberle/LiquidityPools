{-# LANGUAGE RecordWildCards #-}

import Network.Wai.Handler.Warp

import Server (app)


main :: IO ()
main = run 8081 app
