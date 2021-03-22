module System.Console.ANSI.Declarative.Widget.Border where

import Data.Foldable (fold)
import Data.Functor (void)
import qualified Data.List as List
import qualified Data.Text as Text
import System.Console.ANSI.Declarative.Widget.Block (block)
import System.Console.ANSI.Declarative.Widget.Padding (padAll, padding)
import System.Console.ANSI.Declarative.Widget.Render

border :: IsWidget a => BorderCharacters -> a -> Border
border chars = Border chars . SomeWidget

data Border = Border
  { borderCharacters :: BorderCharacters,
    borderChild :: SomeWidget
  }
  deriving (Show)

data BorderCharacters = BorderCharacters
  { borderCharTop :: Char,
    borderCharBottom :: Char,
    borderCharLeft :: Char,
    borderCharRight :: Char,
    borderCharCornerTopLeft :: Char,
    borderCharCornerTopRight :: Char,
    borderCharCornerBottomLeft :: Char,
    borderCharCornerBottomRight :: Char
  }
  deriving (Show)

-- | Very basic look, but since all corners look the same, this version will
-- work better when connecting or overlapping multiple borders.
asciiChars :: BorderCharacters
asciiChars =
  BorderCharacters
    { borderCharTop = '-',
      borderCharBottom = '-',
      borderCharLeft = '|',
      borderCharRight = '|',
      borderCharCornerTopLeft = '+',
      borderCharCornerTopRight = '+',
      borderCharCornerBottomLeft = '+',
      borderCharCornerBottomRight = '+'
    }

unicodeChars :: BorderCharacters
unicodeChars =
  BorderCharacters
    { borderCharTop = '─',
      borderCharBottom = '─',
      borderCharLeft = '│',
      borderCharRight = '│',
      borderCharCornerTopLeft = '┌',
      borderCharCornerTopRight = '┐',
      borderCharCornerBottomLeft = '└',
      borderCharCornerBottomRight = '┘'
    }

instance IsWidget Border where
  renderWidget = renderBorder

renderBorder :: Border -> Render Result
renderBorder (Border chars sub) = do
  renderBorderOutline chars
  renderWidget $ padding (padAll 1) sub

renderBorderOutline :: BorderCharacters -> Render ()
renderBorderOutline chars = do
  size <- availableSize
  height <- availableHeight
  width <- availableWidth

  -- LEFT
  void . offsetBy mempty $
    withSize size {sizeColumns = 1} $
      renderPlain . List.replicate height $
        Text.singleton (borderCharLeft chars)
  -- RIGHT
  void . offsetBy mempty {positionColumn = width - 1} $
    withSize size {sizeColumns = 1} $
      renderPlain . List.replicate height $
        Text.singleton (borderCharRight chars)
  -- TOP
  void . offsetBy mempty $
    withSize size {sizeRows = 1} $
      renderPlain . pure . fold $
        [ Text.singleton (borderCharCornerTopLeft chars),
          Text.replicate (width - 2) (Text.singleton (borderCharTop chars)),
          Text.singleton (borderCharCornerTopRight chars)
        ]
  -- BOTTOM
  void . offsetBy mempty {positionRow = height - 1} $
    withSize size {sizeRows = 1} $
      renderPlain . pure . fold $
        [ Text.singleton (borderCharCornerBottomLeft chars),
          Text.replicate (width - 2) (Text.singleton (borderCharBottom chars)),
          Text.singleton (borderCharCornerBottomRight chars)
        ]
  where
    renderPlain =
      renderWidget . block . Text.unlines
