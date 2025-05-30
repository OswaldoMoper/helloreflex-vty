{-# LANGUAGE RecursiveDo #-}

module Main where

import Reflex
import Reflex.Vty

main :: IO ()
main = runVtyApp $ \vty -> do
    text "Hello, Reflex-VTY"