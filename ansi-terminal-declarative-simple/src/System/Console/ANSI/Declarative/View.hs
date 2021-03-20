{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Console.ANSI.Declarative.View
  ( -- * View components
    View (..),
    AlignHorizontal (..),
    AlignVertical (..),
    SplitDir (..),
    SplitPos (..),
    PaddingStyle (..),
    padAll,
    padHorizontal,
    padVertical,
    padTop,
    padBottom,
    padLeft,
    padRight,
    BorderCharacters (..),
    asciiChars,

    -- * Block content
    StyledLine,
    unstyled,
    styled,

    -- * Rendering
    render,
  )
where

import Control.Monad (void, zipWithM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader, ask, asks, local)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.Foldable (fold, traverse_)
import Data.List (unfoldr)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text (putStr)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified System.Console.ANSI as Ansi
import qualified System.Console.Terminal.Size as Term

data View
  = Empty
  | Block AlignHorizontal AlignVertical (Vector StyledLine)
  | Split SplitDir SplitPos View View
  | Padding PaddingStyle View
  | Border BorderCharacters View
  deriving (Show)

data Align = Align
  { alignHorizontal :: AlignHorizontal,
    alignVertical :: AlignVertical
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

data SplitDir
  = Horizontal
  | Vertical
  deriving (Show)

data SplitPos
  = FromStart Int
  | FromEnd Int
  | Ratio Float
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

-------------------------------------------------------------------------------

render :: View -> IO ()
render view = do
  window <- Term.size
  Ansi.hideCursor
  Ansi.setSGR [Ansi.Reset]
  Ansi.clearScreen
  Ansi.setCursorPosition 0 0
  let size =
        Size
          { sizeRows = maybe 24 Term.height window,
            sizeColumns = maybe 72 Term.width window
          }
  let screenSpace = ScreenSpace (Position 0 0) size
  result <- runRender screenSpace $ renderView view

  Ansi.setCursorPosition 0 0
  -- Not exactly sure why, but without this, no output is visible.
  putStrLn ""

  case resultCursors result of
    -- Just take first available cursor for now.
    cursor : _ -> do
      Ansi.setCursorPosition (positionRow cursor) (positionColumn cursor)
      Ansi.showCursor
    [] -> do
      -- We already hid the cursor, nothing to do.
      pure ()

newtype Render a = Render {getRender :: ReaderT ScreenSpace IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader ScreenSpace)

runRender :: ScreenSpace -> Render a -> IO a
runRender outline = flip runReaderT outline . getRender

availableSize :: Render Size
availableSize = do
  asks spaceSize

availableHeight :: Render Int
availableHeight = do
  asks (sizeRows . spaceSize)

availableWidth :: Render Int
availableWidth = do
  asks (sizeColumns . spaceSize)

-------------------------------------------------------------------------------

data ScreenSpace = ScreenSpace
  { spaceOrigin :: Position,
    spaceSize :: Size
  }
  deriving (Show)

data Position = Position
  { positionRow :: Int,
    positionColumn :: Int
  }
  deriving (Show)

instance Semigroup Position where
  p1 <> p2 =
    Position
      { positionRow = positionRow p1 + positionRow p2,
        positionColumn = positionColumn p1 + positionColumn p2
      }

instance Monoid Position where
  mempty = Position 0 0

negatePosition :: Position -> Position
negatePosition (Position row col) = Position (negate row) (negate col)

data Size = Size
  { sizeRows :: Int,
    sizeColumns :: Int
  }
  deriving (Show)

reduceSize :: Position -> Size -> Size
reduceSize reduction s =
  Size
    { sizeRows = sizeRows s - positionRow reduction,
      sizeColumns = sizeColumns s - positionColumn reduction
    }

-------------------------------------------------------------------------------

newtype Result = Result
  { resultCursors :: [Position]
  }
  deriving (Show, Semigroup, Monoid)

offsetResultFrom :: Position -> Result -> Result
offsetResultFrom pos (Result cursors) =
  Result (map (<> negatePosition pos) cursors)

-------------------------------------------------------------------------------

renderView :: View -> Render Result
renderView = \case
  Empty ->
    pure mempty
  Block alignH alignV block ->
    renderLines (Align alignH alignV) block
  Split Horizontal pos top bot -> do
    size <- availableSize
    height <- availableHeight
    let n = splitSize height pos
    fmap fold . sequenceA $
      [ offsetBy mempty {positionRow = 0} $
          withSize size {sizeRows = n} $
            renderView top,
        offsetBy mempty {positionRow = n} $
          withSize size {sizeRows = height - n} $
            renderView bot
      ]
  Split Vertical pos top bot -> do
    size <- availableSize
    width <- availableWidth
    let n = splitSize width pos
    fmap fold . sequenceA $
      [ offsetBy mempty {positionColumn = 0} $
          withSize size {sizeColumns = n} $
            renderView top,
        offsetBy mempty {positionColumn = n} $
          withSize size {sizeColumns = width - n} $
            renderView bot
      ]
  Padding style sub ->
    renderWithPadding style $ renderView sub
  Border chars sub -> do
    renderBorder chars
    renderWithPadding (padAll 1) $ renderView sub

renderBorder :: BorderCharacters -> Render ()
renderBorder chars = do
  size <- availableSize
  height <- availableHeight
  width <- availableWidth

  -- LEFT
  void . offsetBy mempty $
    withSize size {sizeColumns = 1} $
      renderPlain $
        Vector.replicate height (Text.singleton (borderCharLeft chars))
  -- RIGHT
  void . offsetBy mempty {positionColumn = width - 1} $
    withSize size {sizeColumns = 1} $
      renderPlain $
        Vector.replicate height (Text.singleton (borderCharRight chars))
  -- TOP
  void . offsetBy mempty $
    withSize size {sizeRows = 1} $
      renderPlain . Vector.singleton . fold $
        [ Text.singleton (borderCharCornerTopLeft chars),
          Text.replicate (width - 2) (Text.singleton (borderCharTop chars)),
          Text.singleton (borderCharCornerTopRight chars)
        ]
  -- BOTTOM
  void . offsetBy mempty {positionRow = height - 1} $
    withSize size {sizeRows = 1} $
      renderPlain . Vector.singleton . fold $
        [ Text.singleton (borderCharCornerBottomLeft chars),
          Text.replicate (width - 2) (Text.singleton (borderCharBottom chars)),
          Text.singleton (borderCharCornerBottomRight chars)
        ]
  where
    renderPlain =
      renderView . Block AlignTop AlignLeft . fmap unstyled

renderWithPadding :: PaddingStyle -> Render Result -> Render Result
renderWithPadding style renderContent = do
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
    withSize size renderContent

splitSize :: Int -> SplitPos -> Int
splitSize size = clampToSize . split
  where
    clampToSize = clamp (0, size)
    split = \case
      FromStart n -> n
      FromEnd n -> size - n
      Ratio r -> floor (r * fromIntegral size)

clamp :: Ord a => (a, a) -> a -> a
clamp (lo, hi) = max lo . min hi

offsetBy :: Position -> Render Result -> Render Result
offsetBy offset =
  fmap (offsetResultFrom offset)
    . local (\sp -> sp {spaceOrigin = offset <> spaceOrigin sp})

withSize :: Size -> Render Result -> Render Result
withSize size =
  local (\sp -> sp {spaceSize = size})

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

-- | Wraps lines. If there is not enough vertical space available, the last
-- lines will be shown.
renderLines :: Align -> Vector StyledLine -> Render Result
renderLines Align {alignHorizontal, alignVertical} block = do
  height <- availableHeight
  width <- availableWidth
  let wrapped = foldMap (wrapStyledLine width) block
  let paddingTop = paddingAmountHorizontal alignHorizontal (length wrapped) height
  let visible = take height $ drop (negate paddingTop) wrapped
  zipWithM_ (renderLine alignVertical) [max 0 paddingTop ..] visible
  pure mempty

renderLine :: AlignVertical -> Int -> StyledLine -> Render ()
renderLine align offset line = do
  origin <- spaceOrigin <$> ask
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
