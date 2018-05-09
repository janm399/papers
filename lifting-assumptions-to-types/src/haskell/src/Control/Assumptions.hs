{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
module Control.Assumptions where

data Nat = Zero | Succ Nat deriving Show

data SNat (n :: Nat) where
  SZero :: SNat Zero
  SSucc :: SNat n -> SNat (Succ n)

deriving instance Show (SNat n)

type family Add a b where
  Add Zero b = b
  Add (Succ a) b = Succ (Add a b)

data Vect (n :: Nat) a where
  Nil :: Vect Zero a
  Cons :: a -> Vect n a -> Vect (Succ n) a

deriving instance Show a => Show (Vect n a)

append :: Vect n a -> Vect m a -> Vect (Add n m) a
append Nil l = l
append (Cons x xs) ys = Cons x (append xs ys)

len :: Vect n a -> SNat n
len Nil = SZero
len (Cons _ xs) = SSucc (len xs)