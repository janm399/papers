{-# LANGUAGE FlexibleInstances #-}
module Schedule.Core
    ( Entry(..)
    , EntryId
    , Executor(..)
    , Scheduler
    , newScheduler
    , addEntry
    , removeEntry
    , activeEntries
    ) where

import Control.Concurrent
import Control.Concurrent.MVar
import Data.List
import Data.Time
import Data.Time.Clock
import Data.Fixed

-- | EntryId is the alias for identifiers of entries in the schedule
type EntryId = String

-- | Defines functions that entries in the schedule must satisfy
class Entry a where
    -- | Returns the entry identifier
    entryId :: a -> EntryId
    -- | Returns the firing time
    fireAt :: a -> UTCTime

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

-- | Adds a new entry `a` to the `Scheduler a`; the entry must lie in the 
--   `Executor` and `Entry` typeclass.
--   If the entry is already in the schedule, the schedule is left unmodified
addEntry :: (Executor a, Entry a) => a -> Scheduler a -> IO ()

-- | Removes a (not-yet-executed) entry identified by its identity
--   If the entry is not in the schedule, the schedule is left unmodified
removeEntry :: Entry a => EntryId -> Scheduler a -> IO ()

-- | Returns the list of active entries in the schedule
activeEntries :: Scheduler a -> IO [a]

-- Private 
removeEntryId :: Entry a => EntryId -> Schedule a -> Schedule a
findEntry :: Entry a => EntryId -> Schedule a -> Maybe (ScheduledEntry a) 

newScheduler = newMVar []

addEntry entry scheduler = do
    entries <- takeMVar scheduler
    newEntries <- case findEntry (entryId entry) entries of
                      Just existing -> return entries
                      Nothing -> do
                          threadId <- forkIO $ run entry
                          return (ScheduledEntry threadId entry : entries)
    putMVar scheduler newEntries
    where
        run entry = do
            now <- getCurrentTime
            let diff = diffUTCTime (fireAt entry) now * 1000000
            threadDelay $ round diff
            let id = (entryId entry)
            execute entry
            modifyMVar_ scheduler (\entries -> return $ removeEntryId id entries)

removeEntry id scheduler = do
    entries <- takeMVar scheduler
    newEntries <- case findEntry id entries of
                      Just (ScheduledEntry threadId entry) -> do
                          killThread threadId
                          return $ removeEntryId id entries 
                      Nothing ->
                          return entries
    putMVar scheduler newEntries

activeEntries scheduler = 
    map (\ (ScheduledEntry _ entry) -> entry) <$> readMVar scheduler
    {- do
            entries <- readMVar scheduler
            return $ map (\(ScheduledEntry _ entry) -> entry) entries
    -}

findEntry id = find (\(ScheduledEntry _ entry) -> (entryId entry) == id) 

removeEntryId id = filter (\(ScheduledEntry _ entry) -> (entryId entry) /= id) 
    