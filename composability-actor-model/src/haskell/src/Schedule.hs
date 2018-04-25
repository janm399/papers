{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}

module Schedule(Api, Ctx(..), api, server) where

import qualified Data.ByteString.Lazy as B
import Control.Monad.IO.Class
import Servant

class Ctx where
  -- |Finds all scheduled items in some kind of repository
  findAll :: IO B.ByteString
  -- |Adds a new scheduled item to
  save :: B.ByteString -> IO ()

type Api =
  "schedule" :> Get '[PlainText] B.ByteString :<|>
  "schedule" :> ReqBody '[OctetStream, PlainText] B.ByteString :> Post '[JSON] NoContent

api :: Proxy Api
api = Proxy

server :: Ctx => Server Api
server = listSchedules :<|> createSchedule

listSchedules :: Ctx => Handler B.ByteString
listSchedules = liftIO findAll

createSchedule :: Ctx => B.ByteString -> Handler NoContent
createSchedule schedule = do
  liftIO $ save schedule
  return NoContent

instance MimeRender PlainText B.ByteString where
  mimeRender _ val = val

instance MimeUnrender PlainText B.ByteString where
  mimeUnrender _ = Right
