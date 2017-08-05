{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lib where

import Data.Functor.Foldable
import Data.Functor.Classes

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data ExprF x
  = Val Int
  | Add x x
  deriving (Functor, Show)

instance Show1 ExprF where
  liftShowsPrec :: (Int -> a -> ShowS) -> ([a] -> ShowS) -> Int -> ExprF a -> ShowS
  liftShowsPrec _ _ d (Val int) = showsPrec d $ "Val " ++ show int
  liftShowsPrec sp _ d (Add x y) = showsBinaryWith sp sp "Add" d x y

type Expr = Mu ExprF

val :: Int -> Expr
val int = Mu $ \f -> f $ Val int

add :: Expr -> Expr -> Expr
add (Mu f1) (Mu f2) = Mu $ \f -> let b = Add (f1 f) (f2 f) in f b

wrapMu :: Functor f => f (Mu f) -> Mu f
wrapMu f = Mu $ \fa2a -> fa2a (fmap (\(Mu lala) -> lala fa2a) f)

proj :: forall f. Functor f => Mu f -> f (Mu f)
proj (Mu ex) = ex lala
  where
    lala :: f (f (Mu f)) -> f (Mu f)
    lala f = fmap wrapMu f
