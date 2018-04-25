{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Schedule(Api, Ctx(..), api, server) where

import qualified Data.ByteString.Lazy as B
import Control.Monad.IO.Class
import Servant
import qualified Proto.Cam.Messages as P
import Data.ProtoLens.Encoding
import Protobuf

class Ctx where
  -- |Finds all scheduled items in some kind of repository
  findAll :: IO [P.ScheduleEntry]
  -- |Adds a new scheduled item to
  save :: P.ScheduleEntry -> IO ()

type Api =
  "schedule" :> Get '[Protobuf] [P.ScheduleEntry] :<|>
  "schedule" :> ReqBody '[Protobuf] P.ScheduleEntry :> Post '[Protobuf] P.ScheduleEntry

api :: Proxy Api
api = Proxy

server :: Ctx => Server Api
server = listSchedules :<|> createSchedule

listSchedules :: Ctx => Handler [P.ScheduleEntry]
listSchedules = liftIO findAll

createSchedule :: Ctx => P.ScheduleEntry -> Handler P.ScheduleEntry
createSchedule schedule = do
  liftIO $ save schedule
  return schedule
