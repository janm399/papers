{-# LANGUAGE BangPatterns #-}
module Main where

import Data.Time
import Control.Concurrent
import System.IO
import qualified Data.ByteString as B

withIO :: String -> (a -> b) -> (a -> IO b)
withIO fn f = 
  \a -> wrap (f a)
  where
    wrap b = do
      withBinaryFile fn ReadMode read1M
      where 
        read1M handle = do 
          let size = 1024 * 1024
          hSetBuffering handle (BlockBuffering (Just size))
          _ <- B.hGetSome handle size
          -- idx <- randomRIO (1, size)
          -- let x = B.index nulls (idx - 1)
          -- if x /= 0 then
          --   error "foo"
          -- else
          return b

fb1 :: Int -> String
fb1 x 
  | (x `mod` 3 == 0) && (x `mod` 5 == 0) = "FizzBuzz"
  | x `mod` 3 == 0 = "Fizz"
  | x `mod` 5 == 0 = "Buzz"
  | otherwise = show x

fb2 :: Int -> String
fb2 x =
  case fb of
    Left m -> m
    Right m -> m
  where
    fb | (x `mod` 3 == 0) && (x `mod` 5 == 0) = Left "FizzBuzz"
       | x `mod` 3 == 0 = Right "Fizz"
       | x `mod` 5 == 0 = Right "Buzz"
       | otherwise = Right $ show x

data FB = Fizz | Buzz | FizzBuzz | Only Int

instance Show FB where
  show Fizz = "Fizz"
  show Buzz = "Buzz"
  show FizzBuzz = "FizzBuzz"
  show (Only x) = show x

fb3 :: Int -> String
fb3 = 
  show . fb 
  where
    fb x | (x `mod` 3 == 0) && (x `mod` 5 == 0) = FizzBuzz
         | x `mod` 3 == 0 = Fizz
         | x `mod` 5 == 0 = Buzz
         | otherwise = Only x

fb4 :: Int -> IO String
fb4 x = do
  m <- newEmptyMVar
  _ <- forkIO $ fb x m
  takeMVar m
  where
    fb :: Int -> (MVar String) -> IO ()
    fb xx m = putMVar m (fb1 xx)

timed' :: (Int -> IO String) -> IO (Double, Int)
timed' f = do
  now <- getCurrentTime
  let params = take 4000000 [1..]
  x <- mapM f params
  let !x' = map length x
  end <- getCurrentTime
  let diff = realToFrac (diffUTCTime end now) :: Double
  return ((diff * 1000), length x')

timed :: (Int -> String) -> IO Double
timed f = do
  now <- getCurrentTime
  let !_ = loop f 0
  end <- getCurrentTime
  let diff = realToFrac (diffUTCTime end now) :: Double
  return (diff * 1000)
  where
    loop :: (Int -> String) -> Int -> ()
    loop !fn !c 
      | c == 4000000 = ()
      | otherwise    = let !_ = fn c
                       in loop fn (c + 1)

main :: IO ()
main = do
  mapM_ loop ([1..10] :: [Int])
  where
    -- loop' _ = do
    --   (t5, _) <- timed' (withIO "/dev/zero" fb1) 
    --   putStrLn (show t5)

    loop _ = do
      t1 <- timed fb1
      t2 <- timed fb2
      t3 <- timed fb3
      (t4, _) <- timed' fb4 
      (t5, _) <- timed' (withIO "/dev/zero" fb1) 
      (t6, _) <- timed' (withIO "/tmp/zeros" fb1) 
      putStrLn (show t1)
      putStrLn (show t2)
      putStrLn (show t3)
      putStrLn (show t4)
      putStrLn (show t5)
      putStrLn (show t6)
      putStrLn ""
