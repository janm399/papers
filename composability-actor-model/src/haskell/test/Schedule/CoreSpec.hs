{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Schedule.CoreSpec where

import qualified Data.ByteString.Lazy as B
import Servant
import Test.Hspec
import Schedule.Core
import Control.Concurrent.MVar
import Test.Hspec.Wai
import Test.Hspec.Wai.Matcher
import Test.Hspec.Wai.JSON
import qualified Proto.Cam.Messages as P
import Data.ProtoLens
import Data.Time.Clock
import Data.Fixed
import Control.Concurrent.Async
import CRUD

data E = E (MVar UTCTime) UTCTime

instance Show E where
    show (E _ t) = "E fire at " ++ (show t)

instance Entry E where
    entryId = show
    fireAt (E _ t) = t

instance Executor E where
    execute (E et t) = do
      now <- getCurrentTime
      -- putStrLn $ "    > Scheduled for " ++ (show t) ++ ", executed at " ++ (show now)
      putMVar et now

spec :: Spec
spec = 
    describe "The scheduler" $ 
        it "schedules items" $ do
            scheduler <- newScheduler
            forConcurrently_ (replicate 300 10.0) (scheduleAfter scheduler)
    where
        scheduleAfter :: Scheduler E -> Double -> IO ()
        scheduleAfter scheduler seconds  = do
            now <- getCurrentTime
            let fireAt = addUTCTime (realToFrac seconds::NominalDiffTime) now
            waitFor <- newEmptyMVar :: IO (MVar UTCTime)
            addEntry (E waitFor fireAt) scheduler
            firedAt <- takeMVar waitFor
            let diff = fromRational $ toRational $ diffUTCTime firedAt fireAt
            -- putStrLn $ "    >> fireAt - firedAt = " ++ (show diff)
            diff `shouldSatisfy` (< 0.5)