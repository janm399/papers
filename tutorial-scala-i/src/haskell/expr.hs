module Expr where

data Expr a = Const a
            | Plus (Expr a) (Expr a)
            | Minus (Expr a) (Expr a)
            | Div (Expr a) (Expr a)
            | Mult (Expr a) (Expr a)

eval :: (Integral a, Num a) => Expr a -> a
eval (Const a) = a
eval (Plus l r) = eval l + eval r
eval (Minus l r) = eval l - eval r
eval (Mult l r) = eval l * eval r
eval (Div l r) = eval l `div` eval r


eval' :: (Integral a, Num a, MonadError m) => Expr a -> m a
eval' (Const a) = a
eval' (Div l r) = do
  l <- eval' l
  r <- eval' r
  divide l r
  where 
    divide l 0 = throwError "fooz"
    divide l r = l `div` r
