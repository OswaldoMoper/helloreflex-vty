{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}

module Main where

import Control.Monad.Fix (MonadFix)
import qualified Graphics.Vty as V
import           Reflex
import           Reflex.Vty
import           Data.Maybe (isJust)

data ResizeEdge = TopEdge 
                | BottomEdge 
                | LeftEdge
                | RightEdge
                deriving (Eq)

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
      minWidth = length lText + 2
      minHeight = 3
      mouseDownEvent = fmapMaybe (\case
        V.EvMouseDown x y _ _ -> Just (x, y)
        _ -> Nothing) inp
      mouseUpEvent = fmapMaybe (\case
        V.EvMouseUp{} -> Just ()
        _ -> Nothing) inp
  rec
    topDyn    <- foldDyn ($) 5 topUpdate
    heightDyn <- foldDyn ($) 5 heightUpdate
    leftDyn   <- foldDyn ($) 10 leftUpdate
    widthDyn  <- foldDyn ($) 21 widthUpdate
    let edgeClick = attachPromptlyDynWithMaybe
          (\(top, h, w, left) (x, y) ->
            if y == top && x >= left && x <= left + w 
            then Just (TopEdge, y)
            else if y == top + h && x >= left && x <= left + w
            then Just (BottomEdge, y)
            else if x == left && y >= top && y <= top + h
            then Just (LeftEdge, x)
            else if x == left + w && y >= top && y <= top + h
            then Just (RightEdge, x)
            else Nothing)
          (zipDynWith (\(top, h) (w, left) -> (top, h, w, left)) (zipDyn topDyn heightDyn) (zipDyn widthDyn leftDyn))
          mouseDownEvent
    resizingDyn <- holdDyn Nothing $
      leftmost
        [ Just <$> edgeClick
        , Nothing <$ mouseUpEvent 
        ]
    let dragging = fmap isJust resizingDyn
        resizing = gate (current dragging) mouseDownEvent
        heightUpdate = attachWithMaybe
          (\res (_, y) ->
            case res of
              Just (TopEdge, y0) ->
                let delta = y0 - y
                in if delta /=0
                   then Just $ \h ->
                     let newHeight = h + delta
                     in max minHeight newHeight
                   else Nothing
              Just (BottomEdge, y0) ->
                let delta = y - y0
                in if delta /= 0
                   then Just $ \h ->
                     let newHeight = h + delta
                     in max minHeight newHeight
                   else Nothing
              _ -> Nothing)
          (current resizingDyn) resizing
        topUpdate = attachWithMaybe
          (\res (_, y) ->
            case res of
              Just (TopEdge, y0) ->
                let delta = y - y0
                in if delta /= 0
                   then Just (+ delta)
                   else Nothing
              _ -> Nothing)
          (current resizingDyn) resizing
        widthUpdate = attachWithMaybe
          (\res (x, _) ->
            case res of
              Just (LeftEdge, x0) ->
                let delta = x0 - x
                in if delta /= 0
                   then Just $ \w -> max minWidth (w + delta)
                   else Nothing
              Just (RightEdge, x0) ->
                let delta = x - x0
                in if delta /= 0
                   then Just $ \w -> max minWidth (w + delta)
                   else Nothing
              _ -> Nothing)
          (current resizingDyn) resizing
        leftUpdate = attachWithMaybe
          (\res (x, _) ->
            case res of
              Just (LeftEdge, x0) ->
                let delta = x - x0
                in if delta /= 0
                   then Just (+ delta)
                   else Nothing
              _ -> Nothing)
          (current resizingDyn) resizing
  let drawDyn = zipDynWith
        (\(top, h) (w, left) -> drawRect left top w h lText)
        (zipDyn topDyn heightDyn) (zipDyn widthDyn leftDyn)
  tellImages $ fmap (:[]) (current drawDyn)

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
