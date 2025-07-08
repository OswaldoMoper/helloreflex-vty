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
    let edgeClick = attachPromptlyDynWithMaybe
          (detectClickRegion (length lText))
          dimensionsDyn
          mouseDownEvent
        resizeClick = fmapMaybe
          (\(action, x, y, w, h) ->
             case action of
               Header Close      -> Nothing
               Header Minimize   -> Nothing
               Header FullScreen -> Just (Header FullScreen, x, y, w, h)
               _                 -> Just (action, x, y, w, h))
          edgeClick

    resizingDyn <- holdDyn Nothing $
      leftmost
        [ Just <$> resizeClick
        , Nothing <$ mouseUpEvent
        ]

    isFullScreenDyn <- toggle False $
      fmapMaybe (\case (Header FullScreen, _, _, _, _) -> Just () ; _ -> Nothing) edgeClick

    let quitClickEvent = fmapMaybe (\case
          (Header Close, _, _, _, _) -> Just ()
          _                          -> Nothing) edgeClick

    let dragging = fmap isJust resizingDyn
        resizing = gate (current dragging) mouseDownEvent
        dimensionsUpdate = attachWithMaybe
          (\((resM, (screenHeight, screenWidth)), (isFS, d)) mouse ->
            updateDimensions d screenWidth screenHeight isFS resM mouse minHeight minWidth)
          (current $ zipDyn (zipDyn resizingDyn (zipDyn screenWidthDyn screenHeightDyn))
                            (zipDyn isFullScreenDyn dimensionsDyn))
          resizing
  let drawDyn = fmap (\d -> drawRect (dimLeft d) (dimTop d) (dimWidth d) (dimHeight d)
                                        (offsetX d) (offsetY d) "Haskell" lText)
                   dimensionsDyn

  tellImages $ fmap (:[]) (current drawDyn)
  return quitClickEvent

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

updateDimensions :: Dimensions
                 -> Int -> Int -- ^ Screen height and width
                 -> Bool       -- ^ isFullScreen
                 -> Maybe (ClickAction, Int, Int, Int, Int)
                 -> (Int, Int) -- ^ Mouse position (x, y)
                 -> Int -> Int -- ^ Minimum height and width
                 -> Maybe (Dimensions -> Dimensions)
updateDimensions d screenHeight screenWidth isFS resM (x, y) minHeight minWidth =
  case resM of
    Just (Header FullScreen, _, _, _, _) ->
      Just $ const $
        if isFS
        then d
        else Dimensions 0 0 screenWidth screenHeight 0 0
    Just (action, x0, y0, w, h) ->
      let deltaX  = x0 - x
          deltaY  = y0 - y
          deltaX' = x  - x0
          deltaY' = y  - y0
      in case action of
        TopEdge | deltaY' /= 0 ->
          Just $ \d -> d
          { dimTop    = dimTop d + deltaY'
          , dimHeight = max minHeight (dimHeight d - deltaY')
          }
        BottomEdge | deltaY /= 0 ->
          Just $ \d -> d
            { dimHeight = max minHeight (dimHeight d - deltaY)
            }
        LeftEdge | deltaX' /= 0 ->
          Just $ \d -> d
            { dimLeft  = dimLeft d + deltaX'
            , dimWidth = max minWidth (dimWidth d - deltaX')
            }
        RightEdge | deltaX /= 0 ->
          Just $ \d -> d
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

drawRect :: Int -> Int -> Int -> Int -> Int -> Int -> String -> String -> V.Image
drawRect x y w h offsetTextX offsetTextY titleText contentText
  | h <= 2 =
      let topBorder    = V.string V.defAttr ("╭" ++ replicate (w - 2) '─' ++ "╮")
          buttonsText    = "▢  X "
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

          buttonsText    = "  -  ▢  X "
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
