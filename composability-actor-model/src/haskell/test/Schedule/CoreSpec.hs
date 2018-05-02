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
import Statistics.Sample
import qualified Data.Vector as V
import System.Random

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
            assertDistribution scheduler 1.0 1000
            assertDistribution scheduler 2.0 1000
            assertDistribution scheduler 5.0 1000
    where
        assertDistribution :: Scheduler E -> Double -> Int -> IO ()
        assertDistribution scheduler seconds count = do
            offsets <- randomList count seconds
            firingOffsetsL <- forConcurrently offsets (scheduleAfter scheduler)
            let firingOffsets = V.fromList firingOffsetsL
            let m = mean firingOffsets  
            let k = kurtosis firingOffsets
            let d = stdDev firingOffsets
            putStrLn $ "   > Mean = " ++ (show m) ++ ", kurtosis = " ++ (show k) ++ ", stddev = " ++ (show d)
            m `shouldSatisfy` (<0.5)
            (abs k) `shouldSatisfy` (<20)
            d `shouldSatisfy` (<1)

        randomList :: Int -> Double -> IO [Double]
        randomList 0 _ = return []
        randomList n max = do
            r  <- randomRIO (max, 2 * max)
            rs <- randomList (n - 1) max
            return (r:rs)

        scheduleAfter :: Scheduler E -> Double -> IO Double
        scheduleAfter scheduler seconds  = do
            now <- getCurrentTime
            let fireAt = addUTCTime (realToFrac seconds::NominalDiffTime) now
            waitFor <- newEmptyMVar :: IO (MVar UTCTime)
            addEntry (E waitFor fireAt) scheduler
            firedAt <- takeMVar waitFor
            let diff = fromRational $ toRational $ diffUTCTime firedAt fireAt
            return diff