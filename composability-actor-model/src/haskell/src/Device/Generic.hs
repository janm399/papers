module Device.Generic where

import Schedule.Core
import Control.Concurrent.MVar

class DeviceProps a where
    coreChecks :: (Executor c, Entry c) => a -> [c]
    modifyState :: a -> a

newDevice :: (DeviceProps a) => a -> (a -> m a) -> IO (m a)


newDevice props f = do
    scheduler <- newScheduler
    mapM (flip addEntry scheduler) (coreChecks props)
    return $ f props

