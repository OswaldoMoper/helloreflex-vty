
module Handler where

import           Model

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
      Just (action, x0, y0, w, h) -> Just (transform action x0 y0 w h, resM) where
        dX                  = x0 - x
        dY                  = y0 - y
        dNotZero            = dX /= 0 || dY /= 0
        transform a _ _ _ _ =
          let f = case a of
                    TopLeft | dNotZero           -> \d' -> d'
                      { dimLeft   = dimLeft d' - dX, dimTop = dimTop d' - dY
                      , dimWidth  = max minWidth (dimWidth d' + dX)
                      , dimHeight = max minHeight (dimHeight d' + dY) }
                    TopRight | dNotZero          -> \d' -> d'
                      { dimTop    = dimTop d' - dY
                      , dimWidth  = max minWidth (dimWidth d' - dX)
                      , dimHeight = max minHeight (dimHeight d' + dY) }
                    BottomLeft | dNotZero        -> \d' -> d'
                      { dimLeft   = dimLeft d' - dX
                      , dimWidth  = max minWidth (dimWidth d' + dX)
                      , dimHeight = max minHeight (dimHeight d' - dY) }
                    BottomRight | dNotZero       -> \d' -> d'
                      { dimWidth  = max minWidth (dimWidth d' - dX)
                      , dimHeight = max minHeight (dimHeight d' - dY) }
                    TopEdge | dY /= 0            -> \d' -> d'
                      { dimTop    = dimTop d' - dY
                      , dimHeight = max minHeight (dimHeight d' + dY) }
                    BottomEdge | dY /= 0         -> \d' -> d'
                      { dimHeight = max minHeight (dimHeight d' - dY) }
                    LeftEdge | dX /= 0           -> \d' -> d'
                      { dimLeft  = dimLeft d' - dX
                      , dimWidth = max minWidth (dimWidth d' + dX) }
                    RightEdge | dX /= 0          -> \d' -> d'
                      { dimWidth = max minWidth (dimWidth d' - dX) }
                    Header DragWindow | dNotZero -> \d' -> d'
                      { dimLeft = dimLeft d' - dX
                      , dimTop  = dimTop d' - dY }
                    Content | dNotZero           ->
                      let clampX = absOffset ((w `div` 2) - 2) (2 - (w `div` 2))
                          clampY = absOffset ((h `div` 2) - 2) (2 - (h `div` 2))
                      in \d' -> d'
                         { offsetX = clampX (offsetX d' - dX)
                         , offsetY = clampY (offsetY d' - dY) }
                    _                            -> id
          in applyB . f
      Nothing -> Nothing

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

detectHeaderButton :: Int -> Int -> ClickAction
detectHeaderButton x w
  | x == w - 3 = Header Close
  | x == w - 6 = Header Maximize
  | x == w - 9 = Header Minimize
  | otherwise  = Header DragWindow

clamp :: Int -> Int -> Int -> Int
clamp minVal maxVal val = max minVal (min maxVal val)

applyBounds :: Int -> Int -> Int -> Int -> Dimensions -> Dimensions
applyBounds screenW screenH minW minH d =
  let w = max minW (min (dimWidth d) (screenW - dimLeft d))
      h = max minH (min (dimHeight d) (screenH - dimTop d))
      l = clamp 0 (screenW - w) (dimLeft d)
      t = clamp 0 (screenH - h) (dimTop d)
  in d { dimLeft = l, dimTop = t, dimWidth = w, dimHeight = h }

absOffset :: Ord a => a -> a -> a -> a
absOffset maxO minO offset | maxO < offset = maxO
                           | minO > offset = minO
                           | otherwise     = offset
