import Network.Wai.Handler.Warp ( defaultSettings
                                , runSettings
                                , setPort
                                )

import Server.Server (app)


main :: IO ()
main = runSettings (setPort 8081 defaultSettings) app
