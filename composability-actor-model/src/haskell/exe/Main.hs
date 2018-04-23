{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Item as I
import qualified Schedule as S
import qualified Data.ByteString.Lazy as B
import Network.Wai.Handler.Warp
import Network.Wai
import System.IO
import Servant

instance S.Ctx where
  findAll = return "all"
  save x = B.putStrLn x

mkApp :: IO Application
mkApp = return app

type Api = I.Api :<|> S.Api
server :: Server Api
server = I.server :<|> S.server

api :: Proxy Api
api = Proxy

app :: Application 
app = serve api server

main :: IO ()
main = do
  let port = 3000
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings
  runSettings settings =<< mkApp
