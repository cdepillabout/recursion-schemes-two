{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}

module Test where

type Seconds = Integer

instance Num r => Num ((Integer -> r) -> r) where
  fromInteger :: Integer -> (Integer -> r) -> r
  fromInteger int f = f int

  (+) :: ((Integer -> r) -> r) -> ((Integer -> r) -> r) -> (Integer -> r) -> r
  (+) one two f = one f + two f

second :: Integer -> Integer
second int = int

minute :: Integer -> Integer
minute = (* 60) . second

hour :: Integer -> Integer
hour = (* 60) . minute

day :: Integer -> Integer
day = (* 24) . hour
