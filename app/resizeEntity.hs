{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}

module Main where

import Control.Monad.Fix (MonadFix)
import qualified Graphics.Vty as V
import           Reflex
import           Reflex.Vty

main :: IO ()
main = mainWidget $ initManager_ $ do
  inp <- input
  resizeRectangle inp
  let quitEvent = fforMaybe inp $ \case
        V.EvKey V.KEsc [] -> Just ()
        V.EvKey (V.KChar 'c') [V.MCtrl] -> Just ()
        _ -> Nothing
  return quitEvent

resizeRectangle :: (HasDisplayRegion t m, HasImageWriter t m, HasTheme t m, PerformEvent t m, MonadHold t m, TriggerEvent t m, MonadFix m, MonadSample t (Performable m))
                => Event t V.Event -> m ()
resizeRectangle inp = do
  let lText = "¡Hello, Reflex-VTY!"
      initialX = 10
      initialY = 5
      initialWidth = max 16 (length lText + 2)
      mouseUpEvent = fmapMaybe (\case
        V.EvMouseUp{} -> Just ()
        _ -> Nothing) inp
  rec
  -- TODO: Code to handle resizing
  -- We need a way to resize rectangle dynamically
  -- For now, we will keep the width constant
  -- and allow height to change when the user clicks and drags
  -- the vertical edges of the rectangle.
    currentHDyn <- foldDyn const 5 $
      attachPromptlyDynWith (\_baseHeight newHeight -> newHeight) baseHeightDyn heightEvent
    baseHeightDyn <- foldDyn const 5 $
      tag (current currentHDyn) mouseUpEvent
    let mouseStartResizing =
          True <$ attachPromptlyDynWithMaybe
            (\h ev -> case ev of
                V.EvMouseDown x y _ _ | isOnEdge x y h -> Just ()
                _ -> Nothing)
            baseHeightDyn
            inp
          where
            isOnEdge x y h = y == initialY + h - 1 && x >= initialX && x < initialX + initialWidth
        mouseStopResizing = fmap (const False) mouseUpEvent
    isResizing <- holdDyn False $ leftmost [mouseStartResizing, mouseStopResizing]
    let heightEvent = gate (current isResizing) $
          fmapMaybe (\case
            V.EvMouseDown x y _ _ -> Just (max 3 (y - initialY + 1))
            _ -> Nothing) inp
  tellImages $ fmap (\h -> [drawRect initialX initialY initialWidth h lText]) (current currentHDyn)

drawRect :: Int -> Int -> Int -> Int -> String -> V.Image
drawRect x y w h lText =
  let topBottom = V.string V.defAttr ("╭" ++ replicate (w - 2) '─' ++ "╮")
      emptyRow = V.string V.defAttr ("│" ++ replicate (w - 2) ' ' ++ "│")
      bottomRow = V.string V.defAttr ("╰" ++ replicate (w - 2) '─' ++ "╯")

      textPaddingLeft = (w - 2 - length lText) `div` 2
      textPaddingRight = (w - 2 - length lText) - textPaddingLeft
      textRow = V.string V.defAttr ("│" ++ replicate textPaddingLeft ' ' ++ lText ++ replicate textPaddingRight ' ' ++ "│")

      textRowPosition = (h - 3) `div` 2
      rowsBefore = replicate textRowPosition emptyRow
      rowsAfter = replicate (h - 3 - textRowPosition) emptyRow
  in V.translate x y $ V.vertCat ([topBottom] ++ rowsBefore ++ [textRow] ++ rowsAfter ++ [bottomRow])
