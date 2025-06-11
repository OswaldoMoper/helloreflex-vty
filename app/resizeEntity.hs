{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

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
resizeRectangle inp = mdo
  let initialX = 10
      initialY = 5
      initialWidth = 16

      mouseUpEvent = fmapMaybe (\case
        V.EvMouseUp{} -> Just ()
        _ -> Nothing) inp
      mouseStopDragging = fmap (const False) mouseUpEvent
  hDyn <- holdDyn 5 $ heightChange inp
  isDragging <- holdDyn False $ leftmost [mouseStartDragging initialX initialY hDyn initialWidth inp, mouseStopDragging]
  let heightEventFiltered = gate (current isDragging) $
        fmapMaybe (\case
          V.EvMouseDown _ y _ _ -> Just y
          _ -> Nothing) inp
      heightChange
        = attachPromptlyDynWith
            (\ h y -> y - initialY + h) hDyn heightEventFiltered
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

mouseStartDragging initialX initialY hDyn initialWidth inp =
  True <$ attachPromptlyDynWith (\h ev -> fmapMaybe (\case
    V.EvMouseDown x y _ _ -> 
      if (y == initialY + h - 1) && (x > initialX && x < initialX + initialWidth) 
      then Just True
      else Nothing
    _ -> Nothing) (Just ev)) hDyn inp

-- mouseStartDragging initialX initialY initialHeight initialWidth inp = 
--   True <$ fmapMaybe (\case
--     V.EvMouseDown x y _ _ -> 
--       if (y == initialY + initialHeight - 1) && (x > initialX && x < initialX + initialWidth) then
--         Just y
--       else Nothing
--     _ -> Nothing) inp
