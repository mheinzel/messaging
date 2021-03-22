{-# LANGUAGE LambdaCase #-}

module System.Console.ANSI.Declarative.Widget.Split where

import Data.Foldable (fold)
import System.Console.ANSI.Declarative.Widget.Render

split :: (IsWidget a, IsWidget b) => SplitDir -> SplitPos -> a -> b -> Split
split pos dir a b = Split pos dir (SomeWidget a) (SomeWidget b)

splitTop,
  splitBottom,
  splitLeft,
  splitRight ::
    (IsWidget a, IsWidget b) => Int -> a -> b -> Split
splitTop n = split TopBottom (FromStart n)
splitBottom n = split TopBottom (FromEnd n)
splitLeft n = split LeftRight (FromStart n)
splitRight n = split LeftRight (FromEnd n)

data Split = Split SplitDir SplitPos SomeWidget SomeWidget
  deriving (Show)

data SplitDir
  = TopBottom
  | LeftRight
  deriving (Show)

data SplitPos
  = FromStart Int
  | FromEnd Int
  | Ratio Float
  deriving (Show)

instance IsWidget Split where
  renderWidget = renderSplit

renderSplit :: Split -> Render Result
renderSplit (Split TopBottom pos top bot) = do
  size <- availableSize
  height <- availableHeight
  let n = splitOffset height pos
  fmap fold . sequenceA $
    [ offsetBy mempty {positionRow = 0} $
        withSize size {sizeRows = n} $
          renderWidget top,
      offsetBy mempty {positionRow = n} $
        withSize size {sizeRows = height - n} $
          renderWidget bot
    ]
renderSplit (Split LeftRight pos top bot) = do
  size <- availableSize
  width <- availableWidth
  let n = splitOffset width pos
  fmap fold . sequenceA $
    [ offsetBy mempty {positionColumn = 0} $
        withSize size {sizeColumns = n} $
          renderWidget top,
      offsetBy mempty {positionColumn = n} $
        withSize size {sizeColumns = width - n} $
          renderWidget bot
    ]

splitOffset :: Int -> SplitPos -> Int
splitOffset size = clampToSize . findSplit
  where
    clampToSize = max 0 . min size
    findSplit = \case
      FromStart n -> n
      FromEnd n -> size - n
      Ratio r -> floor (r * fromIntegral size)
