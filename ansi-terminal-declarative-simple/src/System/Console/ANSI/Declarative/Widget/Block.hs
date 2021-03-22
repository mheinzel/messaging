{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Console.ANSI.Declarative.Widget.Block where

import Control.Monad (zipWithM_)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (traverse_)
import Data.List (unfoldr)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text (putStr)
import Data.Vector (Vector)
import qualified System.Console.ANSI as Ansi
import System.Console.ANSI.Declarative.Widget.Render

block :: Vector StyledLine -> Block
block = Block AlignTop AlignLeft

alignTop, alignBottom, alignMiddle, alignLeft, alignRight, alignCenter :: Block -> Block
alignTop b = b {alignHorizontal = AlignTop}
alignBottom b = b {alignHorizontal = AlignBottom}
alignMiddle b = b {alignHorizontal = AlignMiddle}
alignLeft b = b {alignVertical = AlignLeft}
alignRight b = b {alignVertical = AlignRight}
alignCenter b = b {alignVertical = AlignCenter}

data Block = Block
  { alignHorizontal :: AlignHorizontal,
    alignVertical :: AlignVertical,
    blockContent :: Vector StyledLine
  }
  deriving (Show)

data AlignHorizontal
  = AlignTop
  | AlignBottom
  | AlignMiddle
  deriving (Show)

data AlignVertical
  = AlignLeft
  | AlignRight
  | AlignCenter
  deriving (Show)

-------------------------------------------------------------------------------

-- Not 100% happy with how this section turned out, might want to refactor it.
newtype StyledLine = StyledLine {styledSegments :: [StyledSegment]}
  deriving stock (Show)
  deriving newtype (Semigroup, Monoid)

unstyled :: Text -> StyledLine
unstyled = styled []

styled :: [Ansi.SGR] -> Text -> StyledLine
styled style = StyledLine . pure . StyledSegment style . Text.unwords . Text.lines

lineLength :: StyledLine -> Int
lineLength = sum . map segmentLength . styledSegments

data StyledSegment = StyledSegment
  { segmentStyle :: [Ansi.SGR],
    segmentText :: Text
  }
  deriving stock (Show)

segmentLength :: StyledSegment -> Int
segmentLength = Text.length . segmentText

-------------------------------------------------------------------------------

instance IsWidget Block where
  renderWidget = renderBlock

-- | Wraps lines. If there is not enough vertical space available, the last
-- lines will be shown.
renderBlock :: Block -> Render Result
renderBlock (Block alignH alignV content) = do
  height <- availableHeight
  width <- availableWidth
  let wrapped = foldMap (wrapStyledLine width) content
  let paddingTop = paddingAmountHorizontal alignH (length wrapped) height
  let visible = take height $ drop (negate paddingTop) wrapped
  zipWithM_ (renderLine alignV) [max 0 paddingTop ..] visible
  pure mempty

renderLine :: AlignVertical -> Int -> StyledLine -> Render ()
renderLine align offset line = do
  origin <- currentOrigin
  width <- availableWidth
  let paddingLeft = paddingAmountVertical align (lineLength line) width
  let row = positionRow origin + offset
  let column = positionColumn origin + paddingLeft
  liftIO $ Ansi.setCursorPosition row column
  liftIO $ traverse_ renderSegment $ reverse $ styledSegments line
  where
    renderSegment (StyledSegment style text) = do
      Ansi.setSGR style
      Text.putStr text

paddingAmountHorizontal :: AlignHorizontal -> Int -> Int -> Int
paddingAmountHorizontal align used available =
  case align of
    AlignTop -> 0
    AlignBottom -> available - used
    AlignMiddle -> (available - used) `div` 2

paddingAmountVertical :: AlignVertical -> Int -> Int -> Int
paddingAmountVertical align used available =
  case align of
    AlignLeft -> 0
    AlignRight -> available - used
    AlignCenter -> (available - used) `div` 2

wrapStyledLine :: Int -> StyledLine -> [StyledLine]
wrapStyledLine width = unfoldr $ \case
  StyledLine [] -> Nothing
  line -> Just (splitStyledLine width line)

splitStyledLine :: Int -> StyledLine -> (StyledLine, StyledLine)
splitStyledLine width line = case styledSegments line of
  [] ->
    (mempty, mempty)
  seg : rest ->
    let widthAfter = width - Text.length (segmentText seg)
     in if widthAfter >= 0
          then
            let (pre, post) = splitStyledLine widthAfter (StyledLine rest)
             in (pre <> StyledLine [seg], post)
          else
            let (segmentPre, segmentPost) = splitStyledSegment width seg
             in (StyledLine [segmentPre], StyledLine (segmentPost : rest))

splitStyledSegment :: Int -> StyledSegment -> (StyledSegment, StyledSegment)
splitStyledSegment width (StyledSegment style text) =
  let (pre, post) = Text.splitAt width text
   in (StyledSegment style pre, StyledSegment style post)
