{-# LANGUAGE BangPatterns #-}
module Main where

import Data.Time

fb1 :: Int -> String
fb1 x 
  | (x `mod` 3 == 0) && (x `mod` 5 == 0) = "FizzBuzz"
  | x `mod` 3 == 0 = "Fizz"
  | x `mod` 5 == 0 = "Buzz"
  | otherwise = show x

fb2 :: Int -> String
fb2 x =
  ""

timed :: (Int -> String) -> IO Double
timed f = do
  now <- getCurrentTime
  let !_ = loop f 0
  end <- getCurrentTime
  let diff = realToFrac (diffUTCTime end now) :: Double
  return diff
  where
    loop :: (Int -> String) -> Int -> ()
    loop !fn !c 
      | c == 4000000 = ()
      | otherwise    = let !_ = fn c
                       in loop fn (c + 1)

main :: IO ()
main = do
    t <- timed fb1
    putStrLn (show t)
