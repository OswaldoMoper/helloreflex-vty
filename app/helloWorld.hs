{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Text    (pack)
import qualified Graphics.Vty as V
import           Reflex
import           Reflex.Vty

main :: IO ()
main = mainWidget $ initManager_ $ do
  inp <- input
  simpleHelloWorld
  let quitEvent = fforMaybe inp $ \case
        V.EvKey V.KEsc [] -> Just ()
        V.EvKey (V.KChar 'c') [V.MCtrl] -> Just ()
        _ -> Nothing
  return quitEvent

simpleHelloWorld :: (HasDisplayRegion t m, HasImageWriter t m, HasTheme t m) => m ()
simpleHelloWorld = text ( pure ( pack $ concat (replicate 13 "\n") ++ replicate 43 ' ' ++ "¡Hello, Reflex-VTY!" ) )
