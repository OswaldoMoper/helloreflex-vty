{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Graphics.Vty as V
import Reflex
import Reflex.Vty

main :: IO ()
main = mainWidget $ initManager_ $ do
  inp <- input
  resizeRectangle inp
  let quitEvent = fforMaybe inp $ \case
        V.EvKey V.KEsc [] -> Just ()
        V.EvKey (V.KChar 'c') [V.MCtrl] -> Just ()
        _ -> Nothing
  return quitEvent

resizeRectangle :: (HasDisplayRegion t m, HasImageWriter t m, HasTheme t m, PerformEvent t m, MonadHold t m, TriggerEvent t m) 
                => Event t V.Event -> m ()
resizeRectangle inp = do
  let initialX = 10
      initialY = 5
      initialWidth = 16
      initialHeight = 5

      mouseDownEvent = fmapMaybe (\case
        V.EvMouseDown _ y _ _ | y == (initialY + initialHeight - 1) -> Just y
        _ -> Nothing) inp
        
      mouseUpEvent = fmapMaybe (\case
          V.EvMouseUp{} -> Just ()
          _ -> Nothing) inp

  isDragging <- holdDyn False $ leftmost
    [ fmap (const True) mouseDownEvent
    , fmap (const False) mouseUpEvent
    ]

  let heightEventFiltered = gate (current isDragging) mouseDownEvent

  hDyn <- holdDyn initialHeight heightEventFiltered
  tellImages $ fmap (\h -> [drawRect initialX initialY initialWidth h]) (current hDyn)


drawRect :: Int -> Int -> Int -> Int -> V.Image
drawRect x y w h =
  let lText = "Hello World"
      topBottom = V.string V.defAttr ("╭" ++ replicate (w - 2) '─' ++ "╮")
      emptyRow = V.string V.defAttr ("│" ++ replicate (w - 2) ' ' ++ "│")
      bottomRow = V.string V.defAttr ("╰" ++ replicate (w - 2) '─' ++ "╯")

      textPaddingLeft = (w - 2 - length lText) `div` 2
      textPaddingRight = (w - 2 - length lText) - textPaddingLeft
      textRow = V.string V.defAttr ("│" ++ replicate textPaddingLeft ' ' ++ lText ++ replicate textPaddingRight ' ' ++ "│")

      textRowPosition = (h - 3) `div` 2
      rowsBefore = replicate textRowPosition emptyRow
      rowsAfter = replicate (h - 3 - textRowPosition) emptyRow
  in V.translate x y $ V.vertCat ([topBottom] ++ rowsBefore ++ [textRow] ++ rowsAfter ++ [bottomRow])
