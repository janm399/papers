{-# LANGUAGE FlexibleInstances #-}
module Schedule.Core where

import Control.Concurrent
import Control.Concurrent.MVar
import Data.List
import Data.Time

type EntryId = String

class Entry a where
    entryId :: a -> EntryId
    fireAt :: a -> UTCTime

class Executor a where
    execute :: a -> IO ()

data ScheduledEntry a = ScheduledEntry ThreadId a 

type Schedule a = [ScheduledEntry a]
type Scheduler a = MVar (Schedule a)

-- Public API
newScheduler :: Entry a => IO (Scheduler a)
addEntry :: (Executor a, Entry a) => a -> Scheduler a -> IO ()
removeEntry :: Entry a => EntryId -> Scheduler a -> IO ()
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
            threadDelay 10000000
                    --    ^s ^ ms  
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

activeEntries scheduler = do
    entries <- readMVar scheduler
    return $ map (\(ScheduledEntry _ entry) -> entry) entries

findEntry id schedule = find (\(ScheduledEntry _ entry) -> (entryId entry) == id) schedule

removeEntryId id = filter (\(ScheduledEntry _ entry) -> (entryId entry) /= id) 
    