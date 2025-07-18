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

data ClickAction = TopEdge
                 | BottomEdge
                 | LeftEdge
                 | RightEdge
                 | Content
                 | Header HeaderAction
                 deriving (Eq)

data HeaderAction = DragWindow
                  | Minimize
                  | FullScreen
                  | Close
                  deriving (Eq)

data ContentAction = DragContent
                   | String
                   deriving (Eq, Show)

data Dimensions = Dimensions
  { dimTop    :: Int
  , dimHeight :: Int
  , dimLeft   :: Int
  , dimWidth  :: Int
  , offsetX   :: Int
  , offsetY   :: Int
  } deriving (Show, Eq)

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
  screenHeightDyn <- displayHeight
  screenWidthDyn  <- displayWidth
  let initialDims    = Dimensions 5 5 10 21 0 0
      lText          = "¡Hello, Reflex-VTY!"
      minWidth       = length lText + 2
      minHeight      = 3
      mouseDownEvent = fmapMaybe (\case
        V.EvMouseDown x y _ _ -> Just (x, y)
        _ -> Nothing) inp
      mouseUpEvent   = fmapMaybe (\case
        V.EvMouseUp{} -> Just ()
        _ -> Nothing) inp
  rec
    dimensionsDyn <- foldDyn ($) initialDims dimensionsUpdate
    prevDimsDyn <- holdDyn initialDims $
      attachPromptlyDynWithMaybe
        (\mode d ->
          if mode /= "FullScreen" && mode /= "Minimized"
          then Just d
          else Nothing)
        modeDyn
        (updated dimensionsDyn)
    let edgeClick = attachPromptlyDynWithMaybe
          (detectClickRegion (length lText))
          dimensionsDyn
          mouseDownEvent

    resizingDyn <- holdDyn Nothing $
      leftmost
        [ Just <$> edgeClick
        , Nothing <$ mouseUpEvent
        ]

    modeDyn <- foldDyn updateMode "Windowed" edgeClick
    let quitClickEvent = fmapMaybe (\case
          (Header Close, _, _, _, _) -> Just ()
          _                          -> Nothing) edgeClick

    let dragging = fmap isJust resizingDyn
        resizing = gate (current dragging) mouseDownEvent
        dimensionsUpdate = attachWithMaybe
          (\((resM, (screenHeight, screenWidth)), (isFS, (d, prevD))) mouse ->
            updateDimensions d prevD (screenHeight - 1) screenWidth isFS resM mouse minHeight minWidth)
          (current $ zipDyn (zipDyn resizingDyn (zipDyn screenHeightDyn screenWidthDyn))
                            (zipDyn modeDyn (zipDyn dimensionsDyn prevDimsDyn)))
          resizing
  let drawDyn = fmap (\(d, isFS) -> drawRect (dimLeft d) (dimTop d) (dimWidth d) (dimHeight d)
                                        (offsetX d) (offsetY d) "Haskell" lText isFS)
                   (zipDyn dimensionsDyn modeDyn)

  tellImages $ fmap (:[]) (current drawDyn)
  return quitClickEvent

updateMode :: (ClickAction, Int, Int, Int, Int) -> String -> String
updateMode (Header FullScreen, _, _, _, _) "FullScreen" = "Windowed"
updateMode (Header FullScreen, _, _, _, _) _            = "FullScreen"
updateMode (Header Minimize,   _, _, _, _) _            = "Minimized"
updateMode (Header DragWindow, _, _, _, _) "Minimized"  = "Windowed"
updateMode _                               m            = m

detectClickRegion :: Int -> Dimensions -> (Int, Int)
                  -> Maybe (ClickAction, Int, Int, Int, Int)
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
     if y == textRowY && x >= textColX && x < textColXEnd
     then Just (Content, x, y, w, h)
     else if y > top && y < top + 2
     then if x == left + w - 3
           then Just (Header Close, x, y, w, h)
           else if x == left + w - 6
           then Just (Header FullScreen, x, y, w, h)
           else if x == left + w - 9
           then Just (Header Minimize, x, y, w, h)
           else Just (Header DragWindow, x, y, w, h)
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

updateDimensions :: Dimensions -- ^ Current dimensions
                 -> Dimensions -- ^ prevFullScreen Dimensions
                 -> Int -> Int -- ^ Screen height and width
                 -> String     -- ^ isFullScreen, Windowed or Minimized
                 -> Maybe (ClickAction, Int, Int, Int, Int)
                 -> (Int, Int) -- ^ Mouse position (x, y)
                 -> Int -> Int -- ^ Minimum height and width
                 -> Maybe (Dimensions -> Dimensions)
updateDimensions d prevFullScreen screenHeight screenWidth isMode resM (x, y) minHeight minWidth
  | isMode == "FullScreen"
  = case resM of
      Just (Header FullScreen, _, _, _, _) ->
        Just $ const Dimensions
          { dimTop    = 0
          , dimHeight = screenHeight
          , dimLeft   = 0
          , dimWidth  = screenWidth
          , offsetX   = 0
          , offsetY   = 0
          }
      Just (Header Minimize, _, _, _, _) ->
        Just $ const prevFullScreen
      _ -> Nothing
  | isMode == "Minimized"
  = case resM of
      Just (Header Minimize, _, _, _, _) ->
        Just $ const Dimensions
          { dimTop    = screenHeight - 2
          , dimHeight = 2
          , dimLeft   = 0
          , dimWidth  = minWidth
          , offsetX   = 0
          , offsetY   = 0
          }
      Just (Header DragWindow, _, _, _, _) ->
        Just $ const prevFullScreen
      _ -> Nothing
  | otherwise = case resM of
      Just (Header FullScreen, _, _, _, _) ->
        Just $ const prevFullScreen
      Just (Header Minimize, _, _, _, _) ->
        Just $ const prevFullScreen
      Just (action, x0, y0, w, h) ->
        let deltaX  = x0 - x
            deltaY  = y0 - y
            deltaX' = x  - x0
            deltaY' = y  - y0
            applyB  = applyBounds screenWidth screenHeight minWidth minHeight
        in case action of
          TopEdge | deltaY' /= 0 ->
            Just $ applyB . \d -> d
            { dimTop    = dimTop d + deltaY'
            , dimHeight = max minHeight (dimHeight d - deltaY')
            }
          BottomEdge | deltaY /= 0 ->
            Just $ applyB . \d -> d
              { dimHeight = max minHeight (dimHeight d - deltaY)
              }
          LeftEdge | deltaX' /= 0 ->
            Just $ applyB . \d -> d
              { dimLeft  = dimLeft d + deltaX'
              , dimWidth = max minWidth (dimWidth d - deltaX')
              }
          RightEdge | deltaX /= 0 ->
            Just $ applyB . \d -> d
              { dimWidth = max minWidth (dimWidth d - deltaX)
              }
          Header DragWindow | deltaX' /= 0 || deltaY' /= 0 ->
            Just $ \d -> d
              { dimLeft = dimLeft d + deltaX'
              , dimTop  = dimTop d + deltaY'
              }
          Content | deltaX' /= 0 || deltaY' /= 0 ->
            let clampX = absOffset ((w `div` 2) - 2) (2 - (w `div` 2))
                clampY = absOffset ((h `div` 2) - 2) (2 - (h `div` 2))
            in Just $ \d -> d
                { offsetX = clampX (offsetX d + deltaX')
                , offsetY = clampY (offsetY d + deltaY')
                }
          _ -> Nothing
      Nothing -> Nothing

drawRect :: Int -> Int -> Int -> Int -> Int -> Int -> String -> String -> String -> V.Image
drawRect x y w h offsetTextX offsetTextY titleText contentText modeWindow
  | h <= 2 =
      let topBorder    = V.string V.defAttr ("╭" ++ replicate (w - 2) '─' ++ "╮")
          buttonsText    = "  ▢  X "
          availableWidth = w - 2
          maxTitleLen    = availableWidth - length buttonsText
          trimmedTitle   = take maxTitleLen titleText
          titlePadding   = max 0 (availableWidth - length trimmedTitle - length buttonsText) `div` 2
          titlePaddingR  = w - 2 - titlePadding - length buttonsText - length trimmedTitle
          titleRow       = V.string V.defAttr ("│" ++ replicate titlePadding ' ' ++ trimmedTitle
                                              ++ replicate titlePaddingR ' ' ++ buttonsText ++ "│")
      in V.translate x y $ V.vertCat [topBorder, titleRow]
  | otherwise =
      let topBorder    = V.string V.defAttr ("╭" ++ replicate (w - 2) '─' ++ "╮")
          emptyRow     = V.string V.defAttr ("│" ++ replicate (w - 2) ' ' ++ "│")
          bottomBorder = V.string V.defAttr ("╰" ++ replicate (w - 2) '─' ++ "╯")

          buttonsText    = if modeWindow == "FullScreen"
                           then "  -  🗗  X "
                           else "  -  ▢  X "
          availableWidth = w - 2
          maxTitleLen    = availableWidth - length buttonsText
          trimmedTitle   = take maxTitleLen titleText
          titlePadding   = max 0 (availableWidth - length trimmedTitle) `div` 2
          centerTitle    | length buttonsText > titlePadding = titlePadding - length buttonsText
                        | otherwise                         = 0
          titlePaddingR  = w - 2 - titlePadding - length buttonsText - length trimmedTitle - centerTitle
          titleRow       = V.string V.defAttr ("│" ++ replicate (titlePadding + centerTitle) ' '
                                              ++ trimmedTitle ++ replicate titlePaddingR ' '
                                              ++ buttonsText ++ "│")
          separatorRow   =
            V.string V.defAttr ("├" ++ replicate (w - 2) '─' ++ "┤")

          contentPaddingLeft  = max 0 (min (w - 2 - length contentText) ((w - 2 - length contentText) `div` 2 + offsetTextX))
          contentPaddingRight = w - 2 - length contentText - contentPaddingLeft
          contentRow          = V.string V.defAttr ("│" ++ replicate contentPaddingLeft ' '
                                                  ++ contentText ++ replicate contentPaddingRight ' ' ++ "│")

          contentHeight = h - 4
          contentPaddingTop = max 0 (min contentHeight ((contentHeight `div` 2) + offsetTextY))
          rowsBefore = replicate contentPaddingTop emptyRow
          rowsAfter = replicate (contentHeight - contentPaddingTop) emptyRow
      in V.translate x y $ V.vertCat ([topBorder, titleRow, separatorRow] ++ rowsBefore ++ [contentRow] ++ rowsAfter ++ [bottomBorder])

absOffset :: Ord a => a -> a -> a -> a
absOffset maxO minO offset | maxO < offset = maxO
                           | minO > offset = minO
                           | otherwise     = offset
