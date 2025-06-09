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
  let initialSize = (10, 5, 16, 5)

  lastValidSize <- holdDyn initialSize $ fmapMaybe (\case
      V.EvMouseDown mx my _ _ -> Just (mx, my, 16, 5)
      _                       -> Nothing) inp

  let startResizeEvent = fmapMaybe (\((x, y, w, h), (mx, my)) -> 
        case () of
          _ | my == y       -> Just (x, y - 1, w, h + 1)
            | my == y + h   -> Just (x, y, w, h - 1)
            | mx == x + w   -> Just (x, y, w + 1, h)
            | mx == x       -> Just (x, y, w - 1, h)
            | otherwise     -> Nothing) 
        (attach (current lastValidSize) (fmapMaybe (\case
          V.EvMouseDown mx my _ _ -> Just (mx, my)
          _                       -> Nothing) inp))

  resizing <- holdDyn False $ leftmost [True <$ startResizeEvent, False <$ fmapMaybe (\case V.EvMouseUp{} -> Just (); _ -> Nothing) inp]

  let filteredResize = gate (current resizing) startResizeEvent

  finalSize <- holdDyn initialSize filteredResize

  tellImages $ fmap (\(x, y, w, h) -> [drawRect x y w h]) (current finalSize)

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
