{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Schedule(Api, Ctx(..), api, server) where

import qualified Data.ByteString.Lazy as B
import Control.Monad.IO.Class
import Servant
import qualified Proto.Cam.Messages as P
import Data.ProtoLens.Encoding

class Ctx where
  -- |Finds all scheduled items in some kind of repository
  findAll :: IO B.ByteString
  -- |Adds a new scheduled item to
  save :: B.ByteString -> IO ()

type Api =
  "schedule" :> Get '[PlainText] B.ByteString :<|>
  "schedule" :> ReqBody '[OctetStream, PlainText] B.ByteString :> Post '[OctetStream] B.ByteString

api :: Proxy Api
api = Proxy

server :: Ctx => Server Api
server = listSchedules :<|> createSchedule

listSchedules :: Ctx => Handler B.ByteString
listSchedules = liftIO findAll

createSchedule :: Ctx => B.ByteString -> Handler B.ByteString
createSchedule schedule = do
  liftIO $ save schedule
  return $ B.fromStrict $ encodeMessage (P.User "id" "01-01-2100" "Jan")

instance MimeRender PlainText B.ByteString where
  mimeRender _ val = val

instance MimeUnrender PlainText B.ByteString where
  mimeUnrender _ = Right
