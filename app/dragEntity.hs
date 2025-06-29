{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}

module Main where

import           Control.Monad.Fix (MonadFix)
import qualified Graphics.Vty      as V
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

dragRectangle :: (HasDisplayRegion t m, HasImageWriter t m, HasTheme t m, PerformEvent t m, MonadHold t m, TriggerEvent t m, MonadFix m) => Event t V.Event -> m ()
dragRectangle inp = do
  let lText = "¡Hello, Reflex-VTY!"
      rectWidth = max 10 (length lText + 2)
      rectHeight = 5
      mouseUpEvent = fmapMaybe (\case
        V.EvMouseUp{} -> Just ()
        _ -> Nothing) inp
  rec
    currentPosDyn <- foldDyn const (0, 0) $
      attachPromptlyDynWith (\_basePos newPos -> newPos) basePosDyn posEvent
    basePosDyn <- foldDyn const (0, 0) $
      tag (current currentPosDyn) mouseUpEvent
    let mouseStartDragging =
            True <$ attachPromptlyDynWithMaybe
              (\(x0, y0) ev -> case ev of
                  V.EvMouseDown x y _ _ | isInside x y x0 y0 -> Just (x, y)
                  _                                          -> Nothing)
              basePosDyn
              inp
          where
            isInside x y x0 y0 =
              y > y0 && y < y0 + rectHeight && x > x0 && x < x0 + rectWidth
        mouseStopDragging = fmap (const False) mouseUpEvent
    isDragging <- holdDyn False $ leftmost [mouseStartDragging, mouseStopDragging]
    let posEvent = gate (current isDragging) $
          fmapMaybe (\case
            V.EvMouseDown x y _ _ -> Just (x, y)
            _ -> Nothing) inp
  tellImages $ fmap (\(x, y) -> [drawRect x y rectWidth rectHeight lText]) (current currentPosDyn)

drawRect :: Int -> Int -> Int -> Int -> String-> V.Image
drawRect x y w h lText =
  let topBottom        = V.string V.defAttr ("╭" ++ replicate (w - 2) '─' ++ "╮")
      emptyRow         = V.string V.defAttr ("│" ++ replicate (w - 2) ' ' ++ "│")
      bottomRow        = V.string V.defAttr ("╰" ++ replicate (w - 2) '─' ++ "╯")

      textRowPosition  = (h - 3) `div` 2
      rowsBefore       = replicate textRowPosition emptyRow
      rowsAfter        = replicate (h - 3 - textRowPosition) emptyRow

      textPaddingLeft  = (w - 2 - length lText) `div` 2
      textPaddingRight = (w - 2 - length lText) - textPaddingLeft
      textRow          = V.string V.defAttr ("│" ++ replicate textPaddingLeft ' ' ++ lText ++ replicate textPaddingRight ' ' ++ "│")
  in V.translate x y $ V.vertCat ([topBottom] ++ rowsBefore ++ [textRow] ++ rowsAfter ++ [bottomRow])
