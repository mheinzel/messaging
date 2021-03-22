module System.Console.ANSI.Declarative.Widget.Padding where

import System.Console.ANSI.Declarative.Widget.Render

padding :: IsWidget a => PaddingStyle -> a -> Padding
padding style = Padding style . SomeWidget

data Padding = Padding
  { paddingStyle :: PaddingStyle,
    paddingChild :: SomeWidget
  }
  deriving (Show)

data PaddingStyle = PaddingStyle
  { paddingTop :: Int,
    paddingBottom :: Int,
    paddingLeft :: Int,
    paddingRight :: Int
  }
  deriving (Show)

instance Semigroup PaddingStyle where
  PaddingStyle t1 b1 l1 r1 <> PaddingStyle t2 b2 l2 r2 =
    PaddingStyle (t1 + t2) (b1 + b2) (l1 + l2) (r1 + r2)

instance Monoid PaddingStyle where
  mempty = PaddingStyle 0 0 0 0

padAll :: Int -> PaddingStyle
padAll n = padHorizontal n <> padVertical n

padHorizontal :: Int -> PaddingStyle
padHorizontal n = padTop n <> padBottom n

padVertical :: Int -> PaddingStyle
padVertical n = padLeft n <> padRight n

padTop :: Int -> PaddingStyle
padTop n = mempty {paddingTop = n}

padBottom :: Int -> PaddingStyle
padBottom n = mempty {paddingBottom = n}

padLeft :: Int -> PaddingStyle
padLeft n = mempty {paddingLeft = n}

padRight :: Int -> PaddingStyle
padRight n = mempty {paddingRight = n}

instance IsWidget Padding where
  renderWidget = renderPadding

renderPadding :: Padding -> Render Result
renderPadding (Padding style sub) = do
  let offset =
        Position
          { positionRow = paddingTop style,
            positionColumn = paddingLeft style
          }
      endOffset =
        Position
          { positionRow = paddingBottom style,
            positionColumn = paddingRight style
          }
  -- Need to reduce twice, spacing on both sides.
  size <- reduceSize offset . reduceSize endOffset <$> availableSize
  offsetBy offset $
    withSize size $
      renderWidget sub
