module Device.Concrete where

import Schedule.Core
import Control.Concurrent.MVar

type URL = String

type State = Int
data Device = Device (Scheduler ScheduleEntry) State
type DeviceRef = MVar Device

data HttpEntry = HttpEntry URL

data ScheduleEntry = HealthCheck 
                   | ServiceDiscovery
                   | ServiceStateCheck
                   | ExecuteCommand
                   deriving Eq

newDevice :: URL -> IO DeviceRef
stopDevice :: DeviceRef -> IO ()

newDevice url = do
    scheduler <- newScheduler   -- Start the scheduler
    deviceRef <- newMVar $ Device scheduler 1

    -- Run the health checks every 5 seconds
    scheduleRepeated 5.0 HealthCheck (runScheduledItem deviceRef) scheduler

    return deviceRef
    where
        runScheduledItem :: DeviceRef -> ScheduleEntry -> IO ()
        runScheduledItem deviceRef HealthCheck = do
            putStrLn "Doing health check..."
            withDeviceState deviceRef (+1)    
            
        runScheduledItem _ _ = putStrLn "Not implemented yet"

        withDeviceState :: DeviceRef -> (State -> State) -> IO ()
        withDeviceState deviceRef f =
            modifyMVar_ deviceRef (\(Device scheduler state) -> return $ Device scheduler (f state))

stopDevice deviceRef = do
    (Device scheduler _) <- takeMVar deviceRef
    stopScheduler scheduler

-- newDevice initialState = do
--     scheduler <- newScheduler
--     addEntry (HealthCheck

-- newDevice props f = do
--     scheduler <- newScheduler
--     mapM (flip addEntry scheduler) (coreChecks props)
--     return $ f props

