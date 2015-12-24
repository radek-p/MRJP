{-# LANGUAGE GADTs #-}
module Frontend.Utility.Other where


pref :: Int -> String
pref n = [ ' ' | _ <- [1..n] ]

