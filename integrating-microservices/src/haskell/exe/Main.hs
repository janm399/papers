{-# LANGUAGE BangPatterns #-}
module Main where

import Data.Time
import Control.Concurrent

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
  mapM_ loop [1..10]
  where
    loop _ = do
      t1 <- timed fb1
      t2 <- timed fb2
      t3 <- timed fb3
      (t4, _) <- timed' fb4
      putStrLn (show t1)
      putStrLn (show t2)
      putStrLn (show t3)
      putStrLn (show t4)
