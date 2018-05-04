{-# LANGUAGE FlexibleInstances #-}
module Schedule.Core
    ( Executor(..)
    , Scheduler
    , newScheduler
    , scheduleOnce
    , removeEntry
    , activeEntries
    ) where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Data.List
import Data.Time
import Data.Time.Clock
import Data.Fixed

-- | Implements the action to execute the entry `a` when its time is up
class Executor a where
    -- | Called when it is time to execute the action in `a`
    execute :: a -> IO ()

-- | The scheduled entry associates a thread id with the entry `a`
data ScheduledEntry a = ScheduledEntry ThreadId a

-- | For clarity, `Schedule a` is a list of `ScheduleEntry a`s
type Schedule a = [ScheduledEntry a]
-- | For clarity and encapsulation, `Scheduler a` is simply an MVar of it
type Scheduler a = MVar (Schedule a)

-- | Creates a new `Scheduler a`
newScheduler :: IO (Scheduler a)

-- | Adds a new entry `a` to the `Scheduler a`
--   If the entry is already in the schedule, the schedule is left unmodified
scheduleOnce :: (Executor a, Eq a) => UTCTime     -- ^ The firing time
                                   -> a           -- ^ The thing to execute using Executor
                                   -> Scheduler a -- ^ The scheduler to add the entry to
                                   -> IO ()

-- | Adds a new entry `a` to the `Scheduler a`
--   If the entry is already in the schedule, the schedule is left unmodified
scheduleRepeated :: (Executor a, Eq a) => Double      -- ^ The period in seconds
                                       -> a           -- ^ The thing to execute using Executor
                                       -> Scheduler a -- ^ The scheduler to add the entry to 
                                       -> IO ()

-- | Returns the list of active entries in the schedule
activeEntries :: Scheduler a -> IO [a]

-- Private 
schedule :: (Executor a, Eq a) => Scheduler a -> a -> (a -> IO ()) -> IO ()
removeEntry :: Eq a => a -> Schedule a -> Schedule a
findEntry :: Eq a => a -> Schedule a -> Maybe (ScheduledEntry a)

newScheduler = newMVar []

scheduleOnce fireAt entry scheduler = 
    schedule scheduler entry run
    where
        run entry = do
            now <- getCurrentTime
            let diff = diffUTCTime fireAt now * 1000000
            threadDelay $ round diff
            execute entry

scheduleRepeated period entry scheduler =
    schedule scheduler entry run
    where
        run entry = forever $ do
            threadDelay $ round period * 1000000
            execute entry

-- removeEntry id scheduler = do
--     entries <- takeMVar scheduler
--     newEntries <- case findEntry id entries of
--                       Just (ScheduledEntry threadId entry) -> do
--                           killThread threadId
--                           return $ removeEntryId id entries
--                       Nothing ->
--                           return entries
--     putMVar scheduler newEntries

activeEntries scheduler =
    map (\ (ScheduledEntry _ entry) -> entry) <$> readMVar scheduler
    {- do
            entries <- readMVar scheduler
            return $ map (\(ScheduledEntry _ entry) -> entry) entries
    -}

findEntry entry = find (\(ScheduledEntry _ entry') -> entry' == entry)

removeEntry entry = filter (\(ScheduledEntry _ entry') -> entry' /= entry)

schedule scheduler entry f = do
    entries <- takeMVar scheduler
    newEntries <- case findEntry entry entries of
                      Just existing -> return entries
                      Nothing -> do
                          threadId <- forkIO $ run scheduler entry f
                          return (ScheduledEntry threadId entry : entries)
    putMVar scheduler newEntries
    where
        run scheduler entry f = do
            f entry
            modifyMVar_ scheduler (\entries -> return $ removeEntry entry entries)
