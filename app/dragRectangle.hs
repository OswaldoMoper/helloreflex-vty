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
  dragRectangle
  let quitEvent = fforMaybe inp $ \case
        V.EvKey V.KEsc [] -> Just ()
        V.EvKey (V.KChar 'c') [V.MCtrl] -> Just ()
        _ -> Nothing
  return quitEvent

dragRectangle :: (Reflex t, HasImageWriter t m) => m ()
dragRectangle = tellImages $ pure [V.string V.defAttr "¡Drag Rectangle!"]
