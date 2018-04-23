module Main where

import App
import Network.Wai.Handler.Warp
import Network.Wai
import System.IO

mkApp :: IO Application
mkApp = return app
 
main :: IO ()
main = do
  let port = 3000
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings
  runSettings settings =<< mkApp
