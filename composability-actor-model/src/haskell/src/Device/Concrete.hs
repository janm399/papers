module Device.Concrete where

import Schedule.Core
import Control.Concurrent.MVar

type URL = String
data HttpEntry = HttpEntry URL

-- type Device = Device {
--     addCommand :: IO ()
-- }

-- type State = String

-- newDevice :: State -> IO Device

-- newDevice initialState = do
--     scheduler <- newScheduler
--     addEntry (HealthCheck

-- newDevice props f = do
--     scheduler <- newScheduler
--     mapM (flip addEntry scheduler) (coreChecks props)
--     return $ f props

