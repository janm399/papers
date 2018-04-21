{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module App(app, api, server) where

import Control.Monad.Trans.Except
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Control.Monad.IO.Class
import Servant

-- * api

type ItemApi =
  "item" :> Get '[JSON] [Item] :<|>
  "item" :> Capture "itemId" Integer :> Get '[JSON] Item

type ScheduleApi =
  "schedule" :> Get '[PlainText] String :<|>
  "schedule" :> ReqBody '[OctetStream] B.ByteString :> Post '[PlainText] B.ByteString

type Api = ItemApi :<|> ScheduleApi

itemApi :: Proxy ItemApi
itemApi = Proxy

scheduleApi :: Proxy ScheduleApi
scheduleApi = Proxy

api :: Proxy Api
api = Proxy

-- * app

app :: Application
app = serve api server

server :: Server Api
server = itemServer :<|> scheduleServer

itemServer :: Server ItemApi
itemServer =
  getItems :<|>
  getItemById

getItems :: Handler [Item]
getItems = return [exampleItem]

getItemById :: Integer -> Handler Item
getItemById _ = return exampleItem
-- getItemById = \ case
--   0 -> return exampleItem
--   _ -> throwE err404

exampleItem :: Item
exampleItem = Item 0 "example item"


scheduleServer :: Server ScheduleApi
scheduleServer = listSchedules :<|> createSchedule

listSchedules:: Handler String
listSchedules = return "foo\n"

createSchedule :: B.ByteString -> Handler B.ByteString
createSchedule x = do
  -- liftIO (Data.ByteString.Lazy.putStrLn x)
  return $ "done "-- ++ (show $ B.length x)
  -- (Data.ByteString.Lazy.putStrLn x) >>= (const $ return "done")

instance MimeRender PlainText B.ByteString where
  mimeRender _ val = val

-- * item

data Item
  = Item {
    itemId :: Integer,
    itemText :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON Item
instance FromJSON Item
