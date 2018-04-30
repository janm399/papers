{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Schedule.Service(Api, api, server) where

import Control.Monad.IO.Class
import Servant
import qualified Proto.Cam.Messages as P
import Protobuf
import CRUD

type Service = CRUDService P.ScheduleEntry

type Api =
  "schedule" :> Get '[Protobuf JSON] [P.ScheduleEntry] :<|>
  "schedule" :> ReqBody '[Protobuf OctetStream] P.ScheduleEntry :> Post '[Protobuf JSON] P.ScheduleEntry

api :: Proxy Api
api = Proxy

server :: Service -> Server Api
server svc = listSchedules svc :<|> createSchedule svc

listSchedules :: Service -> Handler [P.ScheduleEntry]
listSchedules svc = liftIO $ findAll svc

createSchedule :: Service -> P.ScheduleEntry -> Handler P.ScheduleEntry
createSchedule svc schedule = do
  liftIO $ save svc schedule
  return schedule
