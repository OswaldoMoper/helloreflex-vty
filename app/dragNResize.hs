{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Graphics.Vty as V
import           Reflex
import           Reflex.Vty
import Control.Monad.Fix (MonadFix)

main :: IO ()
main = mainWidget $ initManager_ $ do
  inp <- input
  dragRectangle inp
  let quitEvent = fforMaybe inp $ \case
        V.EvKey V.KEsc [] -> Just ()
        V.EvKey (V.KChar 'c') [V.MCtrl] -> Just ()
        _ -> Nothing
  return quitEvent

dragRectangle ::
  ( HasDisplayRegion t m
  , HasImageWriter t m
  , HasTheme t m
  , PerformEvent t m
  , MonadHold t m
  , TriggerEvent t m
  , MonadFix m
  ) => Event t V.Event -> m ()
dragRectangle inp = do
  let initialPosition = (0, 0)
      initialWidth = 16
      initialHeight = 5

      posEvent = fmapMaybe (\case
        V.EvMouseDown x y _ _ -> Just (x, y)
        _                     -> Nothing) inp

      releaseEvent = fmapMaybe (\case
        V.EvMouseUp{} -> Just ()
        _             -> Nothing) inp

      sizeChangeEvent = fmapMaybe (\case
        V.EvKey (V.KChar '+') [] -> Just (1, 1)
        V.EvKey (V.KChar '-') [] -> Just (-1, -1)
        _ -> Nothing) inp

  widthDyn <- foldDyn (\(dw, _) w -> max 5 (w + dw)) initialWidth sizeChangeEvent
  heightDyn <- foldDyn (\(_, dh) h -> max 3 (h + dh)) initialHeight sizeChangeEvent

  lastValidPos <- holdDyn initialPosition posEvent
  let startDragEvent = fmapMaybe (\(clickPos, lastPos) ->
        if isClickOn clickPos lastPos initialWidth initialHeight
        then Just clickPos
        else Nothing) (attach (current lastValidPos) posEvent)
  
  dragging <- holdDyn False $ leftmost [True <$ startDragEvent, False <$ releaseEvent]
  let filteredMovement = gate (current dragging) posEvent
  finalPos <- holdDyn initialPosition filteredMovement

  tellImages $ current $ zipDynWith (\(x, y) (w, h) -> [drawRect x y w h]) finalPos (zipDyn widthDyn heightDyn)

drawRect :: Int -> Int -> Int -> Int -> V.Image
drawRect x y w h =
  let lText = "Hello World"
      topBottom = V.string V.defAttr ("╭" ++ replicate (w - 2) '─' ++ "╮")
      emptyRow = V.string V.defAttr ("│" ++ replicate (w - 2) ' ' ++ "│")
      textPaddingLeft = (w - 2 - length lText) `div` 2
      textPaddingRight = (w - 2 - length lText) - textPaddingLeft
      textRow = V.string V.defAttr ("│" ++ replicate textPaddingLeft ' ' ++ lText ++ replicate textPaddingRight ' ' ++ "│")
      bottomRow = V.string V.defAttr ("╰" ++ replicate (w - 2) '─' ++ "╯")
      textRowPosition = (h - 3) `div` 2
      rowsBefore = replicate textRowPosition emptyRow
      rowsAfter = replicate (h - 3 - textRowPosition) emptyRow
  in V.translate x y $ V.vertCat ([topBottom] ++ rowsBefore ++ [textRow] ++ rowsAfter ++ [bottomRow])

isClickOn :: (Int, Int) -> (Int, Int) -> Int -> Int -> Bool
isClickOn (clickX, clickY) (x_rect, y_rect) w_rect h_rect =
  clickX >= x_rect && clickX < (x_rect + w_rect) && clickY >= y_rect && clickY < (y_rect + h_rect)