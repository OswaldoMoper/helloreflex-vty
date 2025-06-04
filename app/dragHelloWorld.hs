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
  dragHelloWorld inp
  let quitEvent = fforMaybe inp $ \case
        V.EvKey V.KEsc [] -> Just ()
        V.EvKey (V.KChar 'c') [V.MCtrl] -> Just ()
        _ -> Nothing
  return quitEvent

dragHelloWorld :: (HasDisplayRegion t m, HasImageWriter t m, HasTheme t m, PerformEvent t m, MonadHold t m, TriggerEvent t m) => Event t V.Event -> m ()
dragHelloWorld inp = do
  let textLabel = "¡Hello, Reflex-VTY!"
      initialPosition = (0, 0)
      textWidth = length textLabel

      posEvent = fmapMaybe (\case
        V.EvMouseDown x y _ _ -> Just (x, y)
        _                     -> Nothing) inp

      releaseEvent = fmapMaybe (\case
        V.EvMouseUp{} -> Just ()
        _             -> Nothing) inp

  lastValidPos <- holdDyn initialPosition posEvent
  let lastValidPosEvent = updated lastValidPos

      startDragEvent = fmapMaybe (\(clickPos, lastPos) ->
        if isClickOn clickPos lastPos textWidth
        then Just clickPos
        else Nothing) (attach (current lastValidPos) posEvent)
  dragging <- holdDyn False $ leftmost [True <$ startDragEvent, False <$ releaseEvent]
  let filteredMovement = gate (current dragging) posEvent
  finalPos <- holdDyn initialPosition filteredMovement
  text $ current $ fmap (\(x, y) -> pack $ concat (replicate y "\n") ++ replicate x ' ' ++ textLabel) finalPos

isClickOn :: (Int, Int) -> (Int, Int) -> Int -> Bool
isClickOn (clickX, clickY) (x_text, y_text) w_text =
  clickX >= x_text && clickX < (x_text + w_text) && clickY == y_text
