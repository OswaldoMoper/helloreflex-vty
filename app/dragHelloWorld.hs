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
  dragHelloWorld inp
  let quitEvent = fforMaybe inp $ \case
        V.EvKey V.KEsc [] -> Just ()
        V.EvKey (V.KChar 'c') [V.MCtrl] -> Just ()
        _ -> Nothing
  return quitEvent

dragHelloWorld :: (HasDisplayRegion t m, HasImageWriter t m, HasTheme t m, PerformEvent t m, MonadHold t m, TriggerEvent t m, MonadFix m) => Event t V.Event -> m ()
dragHelloWorld inp = do
  let textLabel = "¡Hello, Reflex-VTY!"
      textWidth = length textLabel
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
              y > y0 - 1 && y < y0 + 1 && x > x0 && x < x0 + textWidth
        mouseStopDragging = fmap (const False) mouseUpEvent
    isDragging <- holdDyn False $ leftmost [mouseStartDragging, mouseStopDragging]
    let posEvent = gate (current isDragging) $
          fmapMaybe (\case
            V.EvMouseDown x y _ _ -> Just (x, y)
            _ -> Nothing) inp
  tellImages $ fmap (\(x, y) -> [drawText x y textLabel]) (current currentPosDyn)

drawText :: Int -> Int -> String -> V.Image
drawText x y lText =
  let textImage = V.string V.defAttr lText
  in V.translate x y textImage
