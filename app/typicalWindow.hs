{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}

module Main where

import           Control.Monad.Fix (MonadFix)
import           Data.Maybe        (isJust)
import           Debug.Trace       (traceShow)
import qualified Graphics.Vty      as V
import           Handler
import           Model
import           Reflex
import           Reflex.Vty
import           Reflex.Vty.Widget ()
import           Template

main :: IO ()
main = mainWidget $ initManager_ $ do
  inp <- input
  quitEventFromClick <- dragNRezize inp
  let quitEvent = leftmost
        [ fforMaybe inp $ \case
            V.EvKey V.KEsc [] -> Just ()
            V.EvKey (V.KChar 'c') [V.MCtrl] -> Just ()
            _ -> Nothing
        , quitEventFromClick
        ]
  return quitEvent

dragNRezize :: (HasDisplayRegion t m, HasImageWriter t m, HasTheme t m, PerformEvent t m, MonadHold t m, TriggerEvent t m, MonadFix m, MonadSample t (Performable m))
            => Event t V.Event -> m (Event t ())
dragNRezize inp = do
  let initialDims    = Dimensions 5 5 10 21 0 0 Windowed
      lText          = "¡Hello, Reflex-VTY!"
      minWidth       = length lText + 2
      minHeight      = 3
      mouseDownEvent = fmapMaybe (\case
        V.EvMouseDown x y _ _ -> Just (x, y)
        _ -> Nothing) inp
      mouseUpEvent   = fmapMaybe (\case
        V.EvMouseUp{} -> Just ()
        _ -> Nothing) inp

  (dimensionsDyn, quitClickEvent) <- handleWindowEvent inp lText minWidth minHeight initialDims

  drawDyn <- buildWindowDyn dimensionsDyn "Haskell" lText

  tellImages $ fmap (:[]) (current drawDyn)
  return quitClickEvent

buildWindowDyn :: ( Reflex t, MonadHold t m )
               => Dynamic t Dimensions
               -> String -- ^ Title
               -> String -- ^ Content
               -> m (Dynamic t V.Image)
buildWindowDyn dimensionsDyn title content =
  pure $ fmap (\d ->
    drawRect (dimLeft d) (dimTop d) (dimWidth d) (dimHeight d)
             (offsetX d) (offsetY d) title content (windowMode d))
    dimensionsDyn

handleWindowEvent :: ( Reflex t, PerformEvent t m, MonadHold t m, MonadFix m, TriggerEvent t m, MonadSample t (Performable m)
                     , HasDisplayRegion t m
                     )
                  => Event t V.Event
                  -> String -- ^ Central text
                  -> Int -- ^ minWidth
                  -> Int -- ^ minHeight
                  -> Dimensions
                  -> m (Dynamic t Dimensions, Event t ())
handleWindowEvent inp lText minWidth minHeight initialDims = mdo
  screenHeightDyn <- displayHeight
  screenWidthDyn  <- displayWidth
  mouseDownEvent <- pure $ fmapMaybe (\case
    V.EvMouseDown x y _ _ -> Just (x, y)
    _ -> Nothing) inp
  mouseUpEvent   <- pure $ fmapMaybe (\case
    V.EvMouseUp{} -> Just ()
    _ -> Nothing) inp
  dimensionsDyn <- foldDyn ($) initialDims dimensionsUpdate
  prevDimsDyn <- holdDyn initialDims $
    fmapMaybe
      (\d ->
        let mode = windowMode d
        in if mode /= FullScreen && mode /= Minimized
           then Just d
           else Nothing)
      (updated dimensionsDyn)
  let edgeClick = attachPromptlyDynWithMaybe
        (detectClickRegion (length lText))
        dimensionsDyn
        mouseDownEvent
      screenDimsDyn     = zipDyn screenHeightDyn screenWidthDyn
      resizingInputsDyn = zipDyn resizingDyn screenDimsDyn
      windowDimsDyn     = zipDyn dimensionsDyn prevDimsDyn
      fullContextDyn    = zipDyn resizingInputsDyn windowDimsDyn
      updateResult = attachWithMaybe
        (\((resM, (sh, sw)), (d, prevD)) mouse ->
          updateDimensions d prevD (sh - 1) sw resM mouse minHeight minWidth)
        (current fullContextDyn)
        resizing
      dimensionsUpdate = fst <$> updateResult
      nextClickInfo    = fmap snd updateResult
  resizingDyn <- holdDyn Nothing $
    leftmost
      [ Just <$> edgeClick
      , nextClickInfo
      , Nothing <$ mouseUpEvent
      ]
  let quitClickEvent = fmapMaybe (\case
        (Header Close, _, _, _, _) -> Just ()
        _                          -> Nothing) edgeClick
      dragging = fmap isJust resizingDyn
      resizing = gate (current dragging) mouseDownEvent
  return (dimensionsDyn, quitClickEvent)
