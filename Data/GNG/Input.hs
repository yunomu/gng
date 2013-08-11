{-# LANGUAGE FlexibleInstances #-}

module Data.GNG.Input
    ( Input(..)
    , move
    ) where

class Input a where
    len :: a -> Double
    add :: a -> a -> a
    mul :: a -> Double -> a
    diff :: a -> a -> a
    diff a b = add a $ mul b (-1)
    dist :: a -> a -> Double
    dist a b = len (diff a b)

instance Input Double where
    len = abs
    add = (+)
    diff = (-)
    mul = (*)

instance Input a => Input (a, a) where
    len (a, b) = sqrt (len a ^ (2::Int) + len b ^ (2::Int))
    add (a1, b1) (a2, b2) = (add a1 a2, add b1 b2)
    diff (a1, b1) (a2, b2) = (diff a1 a2, diff b1 b2)
    mul (a, b) x = (mul a x, mul b x)

move :: Input a => a -> a -> Double -> (a, Double)
move src tgt m = (add src d, len d)
  where
    d = diff tgt src `mul` m
