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
      initialX = 10
      initialWidth = max 16 (length lText + 2)
      minHeight = 3
      mouseDownEvent = fmapMaybe (\case
        V.EvMouseDown x y _ _ -> Just (x, y)
        _ -> Nothing) inp
      mouseUpEvent = fmapMaybe (\case
        V.EvMouseUp{} -> Just ()
        _ -> Nothing) inp
  rec
  -- TODO: Code to handle resizing
  -- We need a way to dynamically resize the rectangle.
  -- For now, we can change the top position and height,
  -- meaning we can move the top and bottom edges.
  -- For the next change, we will leave the left edge 
  -- fixed (initialX) and want to be able to change the
  -- width when we drag the right edge.
    topDyn    <- foldDyn ($) 5 topUpdate
    heightDyn <- foldDyn ($) 5 heightUpdate
    let edgeClick = attachPromptlyDynWithMaybe
          (\(top, h) (x, y) ->
            if y == top
            then Just (TopEdge, y)
            else if y == top + h-1
            then Just (BottomEdge, y)
            else Nothing) (zipDyn topDyn heightDyn) mouseDownEvent
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
  let drawDyn = zipDynWith
        (\top h -> drawRect initialX top initialWidth h lText)
        topDyn heightDyn
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
