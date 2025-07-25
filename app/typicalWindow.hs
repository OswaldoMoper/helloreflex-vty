{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}

module Main where

import           Control.Monad.Fix (MonadFix)
import           Data.Maybe        (isJust)
import           Debug.Trace       (traceShow)
import qualified Graphics.Vty      as V
import           Reflex
import           Reflex.Vty
import           Reflex.Vty.Widget ()

data ClickAction = TopEdge | BottomEdge | LeftEdge | RightEdge
                 | TopLeft | TopRight | BottomLeft | BottomRight
                 | Content
                 | Header HeaderAction
                 deriving (Eq)

data HeaderAction = DragWindow
                  | Minimize
                  | Maximize
                  | Close
                  deriving (Eq)

data ContentAction = DragContent
                   | String
                   deriving (Eq, Show)

data WindowMode = Windowed
                | FullScreen
                | Minimized
                deriving (Eq, Show)

data Dimensions = Dimensions
  { dimTop     :: Int
  , dimHeight  :: Int
  , dimLeft    :: Int
  , dimWidth   :: Int
  , offsetX    :: Int
  , offsetY    :: Int
  , windowMode :: WindowMode
  } deriving (Show, Eq)

type ClickInfo = (ClickAction, Int, Int, Int, Int)

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

detectHeaderButton :: Int -> Int -> ClickAction
detectHeaderButton x w
  | x == w - 3 = Header Close
  | x == w - 6 = Header Maximize
  | x == w - 9 = Header Minimize
  | otherwise  = Header DragWindow

detectClickRegion :: Int -> Dimensions -> (Int, Int)
                  -> Maybe ClickInfo
detectClickRegion textLength d (x, y) =
  let top         = dimTop    d
      h           = dimHeight d
      w           = dimWidth  d
      left        = dimLeft   d
      tx          = offsetX   d
      ty          = offsetY   d
      textRowY    = top + 3 + ((h - 3) `div` 2) + ty
      textColX    = left + 1 + ((w - 2 - textLength) `div` 2) + tx
      textColXEnd = textColX + textLength
  in -- traceShow ("mouse: ", x, y, "box: ", left, top, w, h) $
     if x == left && y == top
     then Just (TopLeft, x, y, w, h)
     else if x == left + w && y == top
     then Just (TopRight, x, y, w, h)
     else if x == left && y == top + h
     then Just (BottomLeft, x, y, w, h)
     else if x == left + w && y == top + h
     then Just (BottomRight, x, y, w, h)
     else if y == textRowY && x >= textColX && x < textColXEnd
     then Just (Content, x, y, w, h)
     else if y > top && y < top + 2
     then Just (detectHeaderButton x (left + w), x, y, w, h)
     else if y == top && x >= left && x <= left + w
     then Just (TopEdge, x, y, w, h)
     else if y == top + h && x >= left && x <= left + w
     then Just (BottomEdge, x, y, w, h)
     else if x == left && y >= top && y <= top + h
     then Just (LeftEdge, x, y, w, h)
     else if x == left + w && y >= top && y <= top + h
     then Just (RightEdge, x, y, w, h)
     else Nothing

clamp :: Int -> Int -> Int -> Int
clamp minVal maxVal val = max minVal (min maxVal val)

applyBounds :: Int -> Int -> Int -> Int -> Dimensions -> Dimensions
applyBounds screenW screenH minW minH d =
  let w = max minW (min (dimWidth d) (screenW - dimLeft d))
      h = max minH (min (dimHeight d) (screenH - dimTop d))
      l = clamp 0 (screenW - w) (dimLeft d)
      t = clamp 0 (screenH - h) (dimTop d)
  in d { dimLeft = l, dimTop = t, dimWidth = w, dimHeight = h }

handleModeChange :: Dimensions
                 -> Dimensions
                 -> Maybe ClickInfo
                 -> Dimensions -> Dimensions
                 -> (Dimensions -> Dimensions)
                 -> Maybe (Dimensions -> Dimensions, Maybe ClickInfo)
handleModeChange d prev resM min full applyB =
  case windowMode d of
    FullScreen -> case resM of
      Just (Header Maximize, _, _, _, _)     -> Just (const prev, Nothing)
      Just (Header Minimize, _, _, _, _)     -> Just (const min, Nothing)
      Just (Header DragWindow, x0, y0, _, _) ->
        Just (applyB . const prev { dimLeft    = x0
                                  , dimTop     = y0
                                  , windowMode = Windowed}, resM)
      _                                    -> Nothing
    Minimized -> case resM of
      Just (Header Maximize, _, _, _, _) -> Just (const full, Nothing)
      Just _                             -> Just (const prev, Nothing)
      _                                  -> Nothing

updateDimensions :: Dimensions -- ^ Current dimensions
                 -> Dimensions -- ^ prevFullScreen Dimensions
                 -> Int -> Int -- ^ Screen height and width
                 -> Maybe ClickInfo
                 -> (Int, Int) -- ^ Mouse position (x, y)
                 -> Int -> Int -- ^ Minimum height and width
                 -> Maybe (Dimensions -> Dimensions, Maybe ClickInfo)
updateDimensions d prevFullScreen screenHeight screenWidth resM (x, y) minHeight minWidth =
  let minDims  = Dimensions
        { dimTop    = screenHeight - 2
        , dimHeight = 2
        , dimLeft   = 0
        , dimWidth  = minWidth
        , offsetX   = 0
        , offsetY   = 0
        , windowMode = Minimized
        }
      fullDims = Dimensions
        { dimTop    = 0
        , dimHeight = screenHeight
        , dimLeft   = 0
        , dimWidth  = screenWidth
        , offsetX   = 0
        , offsetY   = 0
        , windowMode = FullScreen
        }
      applyB   = applyBounds screenWidth screenHeight minWidth minHeight
  in
    if windowMode prevFullScreen /= windowMode d
    then handleModeChange d prevFullScreen resM minDims fullDims applyB
    else case resM of
      Just (Header Maximize, _, _, _, _) ->
        Just (const fullDims, Nothing)
      Just (Header Minimize, _, _, _, _) ->
        Just (const minDims, Nothing)
      Just (action, x0, y0, w, h) -> Just (fDim, actionM) where
        deltaX  = x0 - x
        deltaY  = y0 - y
        deltaX' = x  - x0
        deltaY' = y  - y0
        actionM = resM
        fDim = case action of
          TopLeft | deltaX' /= 0 || deltaY' /= 0 ->
            applyB . \d -> d
              { dimLeft  = dimLeft d + deltaX'
              , dimTop   = dimTop d + deltaY'
              , dimWidth = max minWidth (dimWidth d - deltaX')
              , dimHeight = max minHeight (dimHeight d - deltaY')
              }
          TopRight | deltaX' /= 0 || deltaY' /= 0 ->
            applyB . \d -> d
              { dimTop    = dimTop d - deltaY
              , dimWidth  = max minWidth (dimWidth d - deltaX)
              , dimHeight = max minHeight (dimHeight d - deltaY')
              }
          BottomLeft | deltaX' /= 0 || deltaY' /= 0 ->
            applyB . \d -> d
              { dimLeft   = dimLeft d + deltaX'
              , dimWidth  = max minWidth (dimWidth d - deltaX')
              , dimHeight = max minHeight (dimHeight d - deltaY)
              }
          BottomRight | deltaX' /= 0 || deltaY' /= 0 ->
            applyB . \d -> d
              { dimWidth  = max minWidth (dimWidth d - deltaX)
              , dimHeight = max minHeight (dimHeight d - deltaY)
              }
          TopEdge | deltaY' /= 0 ->
            applyB . \d -> d
            { dimTop    = dimTop d + deltaY'
            , dimHeight = max minHeight (dimHeight d - deltaY')
            }
          BottomEdge | deltaY /= 0 ->
            applyB . \d -> d
              { dimHeight = max minHeight (dimHeight d - deltaY)
              }
          LeftEdge | deltaX' /= 0 ->
            applyB . \d -> d
              { dimLeft  = dimLeft d + deltaX'
              , dimWidth = max minWidth (dimWidth d - deltaX')
              }
          RightEdge | deltaX /= 0 ->
            applyB . \d -> d
              { dimWidth = max minWidth (dimWidth d - deltaX)
              }
          Header DragWindow | deltaX' /= 0 || deltaY' /= 0 ->
            \d -> d
              { dimLeft = dimLeft d + deltaX'
              , dimTop  = dimTop d + deltaY'
              }
          Content | deltaX' /= 0 || deltaY' /= 0 ->
            let clampX = absOffset ((w `div` 2) - 2) (2 - (w `div` 2))
                clampY = absOffset ((h `div` 2) - 2) (2 - (h `div` 2))
            in \d -> d
                { offsetX = clampX (offsetX d + deltaX')
                , offsetY = clampY (offsetY d + deltaY')
                }
          _ -> const d
      Nothing -> Nothing

drawRect :: Int -> Int -> Int -> Int -> Int -> Int -> String -> String -> WindowMode -> V.Image
drawRect x y w h offsetTextX offsetTextY titleText contentText modeWindow
  | modeWindow == Minimized = V.translate x y $ V.vertCat
      [ drawTopBorder w
      , drawTitleRow w titleText modeWindow ""
      ]
  | otherwise = V.translate x y $ V.vertCat $
      [ drawTopBorder w
      , drawTitleRow w titleText modeWindow contentText
      , drawSeparatorRow w
      ]
      ++ drawContentArea w h contentText offsetTextX offsetTextY
      ++ [drawBottomBorder w]

drawTopBorder :: Int -> V.Image
drawTopBorder w = V.string V.defAttr $ "┌" ++ replicate (w - 2) '─' ++ "┐"

drawBottomBorder :: Int -> V.Image
drawBottomBorder w = V.string V.defAttr $ "└" ++ replicate (w - 2) '─' ++ "┘"

drawEmptyRow :: Int -> V.Image
drawEmptyRow w = V.string V.defAttr $ "│" ++ replicate (w - 2) ' ' ++ "│"

drawSeparatorRow :: Int -> V.Image
drawSeparatorRow w = V.string V.defAttr $ "├" ++ replicate (w - 2) '─' ++ "┤"

headerButtons :: WindowMode -> String
headerButtons FullScreen = "  -  🗗  X "
headerButtons Minimized  = "  ▢  X "
headerButtons Windowed   = "  -  ▢  X "

drawTitleRow :: Int -> String -> WindowMode -> String -> V.Image
drawTitleRow w titleText modeWindow contentText =
  let buttons = headerButtons modeWindow
      availableWidth = w - 2
      maxTitleLength = availableWidth - length buttons
      trimmedTitle   = take maxTitleLength titleText
      titlePadding   = max 0 (availableWidth - length trimmedTitle - length buttons) `div` 2
      titlePaddingR  = w - 2 - titlePadding - length trimmedTitle - length buttons
      rowStr         = "│" ++ replicate titlePadding ' ' ++ trimmedTitle
                      ++ replicate titlePaddingR ' ' ++ buttons ++ "│"
  in V.string V.defAttr rowStr

drawContentRow :: Int -> String -> Int -> V.Image
drawContentRow w content offsetTextX =
  let availableWidth     = w - 2
      contentPaddingLeft = max 0 (min (availableWidth - length content)
                                 ((availableWidth - length content) `div` 2 + offsetTextX))
      contentPaddingRight = availableWidth - contentPaddingLeft - length content
  in V.string V.defAttr $ "│" ++ replicate contentPaddingLeft ' '
                              ++ content ++ replicate contentPaddingRight ' ' ++ "│"

drawContentArea :: Int -> Int -> String -> Int -> Int -> [V.Image]
drawContentArea w h content offsetTextX offsetTextY =
  let contentHeight     = h - 4
      contentPaddingTop = max 0 (min contentHeight ((contentHeight `div` 2) + offsetTextY))
      rowsBefore        = replicate contentPaddingTop (drawEmptyRow w)
      rowsAfter         = replicate (contentHeight - contentPaddingTop) (drawEmptyRow w)
  in rowsBefore ++ [drawContentRow w content offsetTextX] ++ rowsAfter

absOffset :: Ord a => a -> a -> a -> a
absOffset maxO minO offset | maxO < offset = maxO
                           | minO > offset = minO
                           | otherwise     = offset
