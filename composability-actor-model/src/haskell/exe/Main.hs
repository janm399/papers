{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Item as I
import qualified Schedule as S
import Network.Wai.Handler.Warp
import Network.Wai
import System.IO
import Servant
import CRUD
import qualified Proto.Cam.Messages as P

type Api = I.Api :<|> S.Api
server :: CRUDService P.ScheduleEntry -> Server Api
server scheduleSvc = I.server :<|> S.server scheduleSvc

api :: Proxy Api
api = Proxy

app :: IO Application 
app = serve api . server <$> newInMemoryService

main :: IO ()
main = do
  let port = 3000
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings
  runSettings settings =<< app
