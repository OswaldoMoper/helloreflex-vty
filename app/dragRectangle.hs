{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Graphics.Vty as V
import           Reflex
import           Reflex.Vty

main :: IO ()
main = mainWidget $ initManager_ $ do
  inp <- input
  dragRectangle inp
  let quitEvent = fforMaybe inp $ \case
        V.EvKey V.KEsc [] -> Just ()
        V.EvKey (V.KChar 'c') [V.MCtrl] -> Just ()
        _ -> Nothing
  return quitEvent

dragRectangle :: (HasDisplayRegion t m, HasImageWriter t m, HasTheme t m, PerformEvent t m, MonadHold t m, TriggerEvent t m) => Event t V.Event -> m ()
dragRectangle inp = do
  let initialPosition = (0, 0)
      rectWidth = 10
      rectHeight = 5

      posEvent = fmapMaybe (\case
        V.EvMouseDown x y _ _ -> Just (x, y)
        _                     -> Nothing) inp

      releaseEvent = fmapMaybe (\case
        V.EvMouseUp{} -> Just ()
        _             -> Nothing) inp

  lastValidPos <- holdDyn initialPosition posEvent
  let startDragEvent = fmapMaybe (\(clickPos, lastPos) ->
        if isClickOn clickPos lastPos rectWidth rectHeight
        then Just clickPos
        else Nothing) (attach (current lastValidPos) posEvent)
  
  dragging <- holdDyn False $ leftmost [True <$ startDragEvent, False <$ releaseEvent]
  let filteredMovement = gate (current dragging) posEvent
  finalPos <- holdDyn initialPosition filteredMovement

  tellImages $ fmap (\(x, y) -> [drawRect x y rectWidth rectHeight]) (current finalPos)


drawRect :: Int -> Int -> Int -> Int -> V.Image
drawRect x y w h =
  let topBottom = V.string V.defAttr ("╭" ++ replicate (w - 2) '─' ++ "╮")
      middleRow = V.string V.defAttr ("│" ++ replicate (w - 2) ' ' ++ "│")
      bottomRow = V.string V.defAttr ("╰" ++ replicate (w - 2) '─' ++ "╯")
  in V.translate x y $ V.vertCat (topBottom : replicate (h - 2) middleRow ++ [bottomRow])

isClickOn :: (Int, Int) -> (Int, Int) -> Int -> Int -> Bool
isClickOn (clickX, clickY) (x_rect, y_rect) w_rect h_rect =
  clickX >= x_rect && clickX < (x_rect + w_rect) && clickY >= y_rect && clickY < (y_rect + h_rect)
