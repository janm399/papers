module Schedule.Core where

import Control.Concurrent
import Control.Concurrent.MVar
import Data.List
import qualified Proto.Cam.Messages as P

type EntryId = String
data Entry = Entry {
    entryId :: String
} deriving (Show)

type ScheduledEntry = (ThreadId, Entry)

--data Scheduler = Scheduler (MVar [Entry])
type Scheduler = MVar [ScheduledEntry]

newScheduler :: IO Scheduler
addEntry :: Scheduler -> Entry -> IO ()
removeEntry :: Scheduler -> EntryId -> IO ()
activeEntries :: Scheduler -> IO [Entry]

newScheduler = newMVar []

addEntry scheduler entry = do
    entries <- takeMVar scheduler
    newEntries <- case find (\(t, e) -> (entryId e) == (entryId entry)) entries of
                      Just existing -> return entries
                      Nothing -> do
                          new <- forkIO $ run entry
                          return ((new, entry) : entries)
    putMVar scheduler newEntries
    where
        run entry = do
            threadDelay 10000000
                    --    ^s ^ ms  
            putStrLn $ "Running " ++ (entryId entry)
            modifyMVar_ scheduler (\entries -> return $ filter (\(_, e) -> (entryId e) /= (entryId entry)) entries)

removeEntry scheduler id = do
    entries <- takeMVar scheduler
    newEntries <- case find (\(_, e) -> (entryId e) == id) entries of
                      Just (t, existing) -> do
                          killThread t
                          return $ filter (\(_, e') -> (entryId existing) /= (entryId e')) entries
                      Nothing ->
                          return entries
    putMVar scheduler newEntries

activeEntries scheduler = do
    entries <- readMVar scheduler
    return $ map snd entries