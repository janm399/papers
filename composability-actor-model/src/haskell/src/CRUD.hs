module CRUD where

-- There is no need for the heavy-handed IORef, since
-- in the `newInMemoryService`, we're not doing any I/O
-- import Data.IORef

-- Instead, it's better to use software transactional memory
import Control.Concurrent.STM

data CRUDService a = CRUDService { 
      -- |Finds all scheduled items in some kind of repository
      findAll :: IO [a]
      -- |Adds a new scheduled item to
    , save :: a -> IO ()
    }

newInMemoryService :: IO (CRUDService a) 
newInMemoryService = do
    -- Again, instead of using IORef...
    -- s <- newIORef [] :: IO (IORef [a])

    -- it's better to get an `IO (TVar [a])` from the STM
    s <- atomically $ newTVar []

    -- The findAll and save functions then operate on the STM
    return $ CRUDService (findAll s) (save s)
    where
        findAll = atomically . readTVar -- readIORef
        save s item = atomically $ modifyTVar' s (\xs -> item : xs) -- modifyIORef'
