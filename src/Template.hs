
module Template where

import qualified Graphics.Vty      as V
import Model

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
