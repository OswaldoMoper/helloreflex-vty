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

data ResizeOrDrag = TopEdge 
                  | BottomEdge 
                  | LeftEdge
                  | RightEdge
                  | OnText
                  | Inside
                  deriving (Eq)

main :: IO ()
main = mainWidget $ initManager_ $ do
  inp <- input
  dragNRezize inp
  let quitEvent = fforMaybe inp $ \case
        V.EvKey V.KEsc [] -> Just ()
        V.EvKey (V.KChar 'c') [V.MCtrl] -> Just ()
        _ -> Nothing
  return quitEvent

dragNRezize :: (HasDisplayRegion t m, HasImageWriter t m, HasTheme t m, PerformEvent t m, MonadHold t m, TriggerEvent t m, MonadFix m, MonadSample t (Performable m))
                => Event t V.Event -> m ()
dragNRezize inp = do
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
    topDyn         <- foldDyn ($) 5 topUpdate
    heightDyn      <- foldDyn ($) 5 heightUpdate
    leftDyn        <- foldDyn ($) 10 leftUpdate
    widthDyn       <- foldDyn ($) 21 widthUpdate
    textOffsetXDyn <- foldDyn ($) 0 textOffsetXUpdate
    textOffsetYDyn <- foldDyn ($) 0 textOffsetYUpdate
    let edgeClick = attachPromptlyDynWithMaybe
          (\(((top, h), (w, left)), (tx, ty)) (x, y) ->
            let textRowY = top + 1 + ((h - 3) `div` 2) + ty
                textColX = left + 1 + ((w - 2 - length lText) `div` 2) + tx
                textColXEnd = textColX + length lText
            in
              if y == textRowY && x >= textColX && x < textColXEnd
              then Just (OnText, x, y, w, h)
              else if x > left + 1 && x < left + w - 1 && y > top + 1 && y < top + h - 1
              then Just (Inside, x, y, w, h)
              else if y == top && x >= left && x <= left + w 
              then Just (TopEdge, x, y, w, h)
              else if y == top + h && x >= left && x <= left + w
              then Just (BottomEdge, x, y, w, h)
              else if x == left && y >= top && y <= top + h
              then Just (LeftEdge, x, y, w, h)
              else if x == left + w && y >= top && y <= top + h
              then Just (RightEdge, x, y, w, h)
              else Nothing)
          (zipDyn
            (zipDyn
              (zipDyn topDyn heightDyn)
              (zipDyn widthDyn leftDyn))
            (zipDyn textOffsetXDyn textOffsetYDyn))
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
              Just (TopEdge, _, y0, _, _) ->
                let delta = y0 - y
                in if delta /=0
                   then Just $ \h ->
                     let newHeight = h + delta
                     in max minHeight newHeight
                   else Nothing
              Just (BottomEdge, _, y0, _, _) ->
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
              Just (TopEdge, _, y0, _, _) ->
                let delta = y - y0
                in if delta /= 0
                   then Just (+ delta)
                   else Nothing
              Just (Inside, _, y0, _, _) ->
                let delta = y - y0
                in if delta /= 0
                   then Just (+ delta)
                   else Nothing
              _ -> Nothing)
          (current resizingDyn) resizing
        widthUpdate = attachWithMaybe
          (\res (x, _) ->
            case res of
              Just (LeftEdge, x0, _, _ , _) ->
                let delta = x0 - x
                in if delta /= 0
                   then Just $ \w -> max minWidth (w + delta)
                   else Nothing
              Just (RightEdge, x0, _, _, _) ->
                let delta = x - x0
                in if delta /= 0
                   then Just $ \w -> max minWidth (w + delta)
                   else Nothing
              _ -> Nothing)
          (current resizingDyn) resizing
        leftUpdate = attachWithMaybe
          (\res (x, _) ->
            case res of
              Just (LeftEdge, x0, _, _, _) ->
                let delta = x - x0
                in if delta /= 0
                   then Just (+ delta)
                   else Nothing
              Just (Inside, x0, _, _, _) ->
                let delta = x - x0
                in if delta /= 0
                   then Just (+ delta)
                   else Nothing
              _ -> Nothing)
          (current resizingDyn) resizing
        textOffsetXUpdate = attachWithMaybe
          (\res (x, _) ->
            case res of
              Just (OnText, x0, _, w, _) ->
                let delta = x - x0
                in if delta /= 0
                  then Just $ \offset -> 
                    absOffset ((w `div` 2) - 2) (2 - (w `div` 2)) (offset + delta)
                  else Nothing
              _ -> Nothing)
          (current resizingDyn) resizing
        textOffsetYUpdate = attachWithMaybe
          (\res (_, y) ->
            case res of
              Just (OnText, _, y0, _, h) ->
                let delta = y - y0
                in if delta /= 0
                  then Just $ \offset ->
                    absOffset ((h `div` 2) - 2) (2 - (h `div` 2)) (offset + delta) 
                  else Nothing
              _ -> Nothing)
          (current resizingDyn) resizing
  let drawDyn = zipDynWith
        (\(top, h, w, left) (tx, ty) -> drawRect left top w h tx ty lText)
        (zipDynWith (\(top, h) (w, left) -> (top, h, w, left))
            (zipDyn topDyn heightDyn)
            (zipDyn widthDyn leftDyn))
        (zipDyn textOffsetXDyn textOffsetYDyn)

  tellImages $ fmap (:[]) (current drawDyn)

drawRect :: Int -> Int -> Int -> Int -> Int -> Int -> String -> V.Image
drawRect x y w h offsetX offsetY lText =
  let topBottom = V.string V.defAttr ("╭" ++ replicate (w - 2) '─' ++ "╮")
      emptyRow = V.string V.defAttr ("│" ++ replicate (w - 2) ' ' ++ "│")
      bottomRow = V.string V.defAttr ("╰" ++ replicate (w - 2) '─' ++ "╯")

      textPaddingLeft = max 0 (min (w - 2 - length lText) ((w - 2 - length lText) `div` 2 + offsetX))
      textPaddingRight = w - 2 - length lText - textPaddingLeft
      textRow = V.string V.defAttr ("│" ++ replicate textPaddingLeft ' ' ++ lText ++ replicate textPaddingRight ' ' ++ "│")

      textRowPosition = max 0 (min (h - 3) ((h - 3) `div` 2 + offsetY))
      rowsBefore = replicate textRowPosition emptyRow
      rowsAfter = replicate (h - 3 - textRowPosition) emptyRow
  in V.translate x y $ V.vertCat ([topBottom] ++ rowsBefore ++ [textRow] ++ rowsAfter ++ [bottomRow])

absOffset :: Ord a => a -> a -> a -> a
absOffset maxO minO offset | maxO < offset = maxO
                           | minO > offset = minO
                           | otherwise     = offset
