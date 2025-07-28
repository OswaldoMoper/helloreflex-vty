{-# LANGUAGE DeriveGeneric #-}

module Model where

import           GHC.Generics (Generic)

data Dimensions = Dimensions
  { dimTop     :: Int
  , dimHeight  :: Int
  , dimLeft    :: Int
  , dimWidth   :: Int
  , offsetX    :: Int
  , offsetY    :: Int
  , windowMode :: WindowMode
  } deriving (Show, Eq, Generic)

data WindowMode = Windowed
                | FullScreen
                | Minimized
                deriving (Eq, Show, Generic)

data ClickAction = TopEdge | BottomEdge | LeftEdge | RightEdge
                 | TopLeft | TopRight | BottomLeft | BottomRight
                 | Content
                 | Header HeaderAction
                 deriving (Eq, Show, Generic)

data HeaderAction = DragWindow
                  | Minimize
                  | Maximize
                  | Close
                  deriving (Eq, Show, Generic)

data ContentAction = DragContent
                   | String
                   deriving (Eq, Show, Generic)

type ClickInfo = (ClickAction, Int, Int, Int, Int)
