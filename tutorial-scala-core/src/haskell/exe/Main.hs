module Main where

import Control.Monad.Except

data Expr a = Const a
            | Plus (Expr a) (Expr a)
            | Minus (Expr a) (Expr a)
            | Div (Expr a) (Expr a)
            | Mult (Expr a) (Expr a)

instance (Show a) => Show (Expr a) where
  show (Const a)   = show a
  show (Plus l r)  = show' l " + " r
  show (Minus l r) = show' l " - " r
  show (Div l r)   = show' l " / " r
  show (Mult l r)  = show' l " * " r

show' :: (Show a) => Expr a -> String -> Expr a -> String
show' l op r = "(" ++ show l ++ op ++ show r ++ ")"

eval :: (Integral a, Num a) => Expr a -> a
eval (Const a) = a
eval (Plus l r) = eval l + eval r
eval (Minus l r) = eval l - eval r
eval (Mult l r) = eval l * eval r
eval (Div l r) = eval l `div` eval r


type Result = Either String

eval' :: (Integral a, Num a, Show a) => Expr a -> Result a
eval' (Const a) = Right a
eval' e@(Div l r) = do
  l' <- eval' l
  r' <- eval' r
  divide l' r'
  where 
    divide :: (Integral a, Num a) => a -> a -> Result a
    divide _ 0   = throwError $ "The right-hand-side of " ++ (show e) ++ ", namely " ++ (show r) ++ " would cause / 0."
    divide l' r' = return $ l' `div` r'
eval' (Plus l r) = do
  l' <- eval' l
  r' <- eval' r
  return $ l' + r'
eval' (Minus l r) = do
  l' <- eval' l
  r' <- eval' r
  return $ l' - r'
eval' (Mult l r) = do
  l' <- eval' l
  r' <- eval' r
  return $ l' * r'

main :: IO ()
main = do
  let x = eval' (Div (Const 4) (Minus (Const 1) (Const 1))) :: Result Int
  putStrLn $ show x
