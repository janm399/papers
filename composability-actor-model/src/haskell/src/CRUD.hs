module CRUD where

import Data.IORef
    
data CRUDService a = 
    CRUDService { -- |Finds all scheduled items in some kind of repository
                  findAll :: IO [a]
                  -- |Adds a new scheduled item to
                , save :: a -> IO ()
                }

newInMemoryService :: IO (CRUDService a) 
newInMemoryService = do
    s <- newIORef [] :: IO (IORef [a])
    return $ CRUDService (findAll s) (save s)
    where
        findAll = readIORef
        save s item = modifyIORef' s (\xs -> item : xs)
