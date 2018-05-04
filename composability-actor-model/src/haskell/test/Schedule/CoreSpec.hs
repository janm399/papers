{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Schedule.CoreSpec where

import qualified Data.ByteString.Lazy as B
import Test.Hspec
import Schedule.Core
import Control.Concurrent.MVar
import Control.Monad (replicateM)
import Data.Time.Clock
import Data.Fixed
import Control.Concurrent.Async
import Statistics.Sample
import qualified Data.Vector as V
import qualified Data.UUID.V4 as UUID
import System.Random

-- | E is the entry in the test schedule; it contains the id, firing time and the fired time
data E = E String (MVar UTCTime)

instance Eq E where
    (E id _) == (E id' _) = id == id'

instance Show E where
    show (E id _) = "Entry " ++ id

-- | Executing our `E` means to put current time to its fired time MVar.
instance Executor E where
    execute (E _ et) = do
      now <- getCurrentTime
      putMVar et now

spec :: Spec
spec =
    describe "The scheduler" $
        it "execute scheduled items accurately" $ do
            scheduler <- newScheduler
            assertSchedulingAccuracy scheduler 1.0 1000
            assertSchedulingAccuracy scheduler 2.0 1000
            assertSchedulingAccuracy scheduler 5.0 1000
            e <- activeEntries scheduler 
            e `shouldSatisfy` null
    where
        -- | Asserts sheduling accuracy for `count` items scheduled in the given
        --   scheduler for _around_ `seconds`s from now.
        --
        --   We measure the accuracy by looking at the mean & standard deviation of the
        --   firing time differences; we also expect the sharpness of the distribution to
        --   be high, which measures the number of outliers.
        assertSchedulingAccuracy :: Scheduler E -> Double -> Int -> IO ()
        assertSchedulingAccuracy scheduler seconds count = do
            offsets <- replicateM count (randomRIO (seconds, 2 * seconds))
            firingOffsets <- V.fromList `fmap` forConcurrently offsets (scheduleAndExecuteItemIn scheduler)
            let m = mean firingOffsets
            let k = kurtosis firingOffsets
            let d = stdDev firingOffsets
            putStrLn $ "   > Mean = " ++ (show m) ++ ", kurtosis = " ++ (show k) ++ ", stddev = " ++ (show d)

            -- we want the "steepness" of distribution of values to be as high as possible:
            -- * >= 2: hyperbolic secant distribution
            -- * >= 3: Laplace distribution, also known as the double exponential distribution
            k `shouldSatisfy` (>=2)

            -- the standard deviation and mean must also be [very] low
            d `shouldSatisfy` (<0.01)
            m `shouldSatisfy` (<0.01)

        -- | Schedules an item to be executed in `seconds`s in the given scheduler
        --   returns the IO of the difference between expected firing time and the
        --   actual firing time in seconds
        scheduleAndExecuteItemIn :: Scheduler E -> Double -> IO Double
        scheduleAndExecuteItemIn scheduler seconds  = do
            now <- getCurrentTime
            id <- UUID.nextRandom
            let fireAt = addUTCTime (realToFrac seconds::NominalDiffTime) now
            waitFor <- newEmptyMVar :: IO (MVar UTCTime)
            scheduleOnce fireAt (E (show id) waitFor) scheduler
            firedAt <- takeMVar waitFor
            return (fromRational $ toRational $ diffUTCTime firedAt fireAt)
