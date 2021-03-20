{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Console.ANSI.Declarative.View
  ( render,
    View (..),
    SplitDir (..),
    SplitPos (..),
    StyledLine,
    unstyled,
    styled,
  )
where

import Control.Monad (zipWithM_)
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
  | Block (Vector StyledLine)
  | Split SplitDir SplitPos View View
  deriving (Show)

data SplitDir
  = Horizontal
  deriving (Show)

data SplitPos
  = FromStart Int
  | FromEnd Int
  | Ratio Float
  deriving (Show)

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

addPosition :: Position -> Position -> Position
addPosition p1 p2 =
  Position
    { positionRow = positionRow p1 + positionRow p2,
      positionColumn = positionColumn p1 + positionColumn p2
    }

subtractPosition :: Position -> Position -> Position
subtractPosition p1 p2 =
  Position
    { positionRow = positionRow p1 - positionRow p2,
      positionColumn = positionColumn p1 - positionColumn p2
    }

data Size = Size
  { sizeRows :: Int,
    sizeColumns :: Int
  }
  deriving (Show)

minSize :: Size -> Size -> Size
minSize s1 s2 =
  Size
    { sizeRows = sizeRows s1 `min` sizeRows s2,
      sizeColumns = sizeColumns s1 `min` sizeColumns s2
    }

-------------------------------------------------------------------------------

newtype Result = Result
  { resultCursors :: [Position]
  }
  deriving (Show, Semigroup, Monoid)

offsetResultFrom :: Position -> Result -> Result
offsetResultFrom pos (Result cursors) = Result (map (`subtractPosition` pos) cursors)

-------------------------------------------------------------------------------

renderView :: View -> Render Result
renderView = \case
  Empty ->
    pure mempty
  Block block ->
    renderLines block
  Split Horizontal pos top bot -> do
    size <- availableSize
    height <- availableHeight
    let n = splitSize height pos
    let zero = Position 0 0
    fmap fold . sequenceA $
      [ offsetBy zero {positionRow = 0} $
          clipSizeTo size {sizeRows = n} $
            renderView top,
        offsetBy zero {positionRow = n} $
          clipSizeTo size {sizeRows = 1} $
            renderBarHorizontal '-',
        offsetBy zero {positionRow = n + 1} $
          clipSizeTo size {sizeRows = height - n - 1} $
            renderView bot
      ]

splitSize :: Int -> SplitPos -> Int
splitSize size = clampToSize . split
  where
    clampToSize = clamp (0, size)
    split = \case
      FromStart n -> n
      FromEnd n -> size - (n + 1)
      Ratio r -> floor (r * fromIntegral size)

clamp :: Ord a => (a, a) -> a -> a
clamp (lo, hi) = max lo . min hi

offsetBy :: Position -> Render Result -> Render Result
offsetBy offset =
  fmap (offsetResultFrom offset)
    . local (\sp -> sp {spaceOrigin = addPosition offset (spaceOrigin sp)})

clipSizeTo :: Size -> Render Result -> Render Result
clipSizeTo size =
  local (\sp -> sp {spaceSize = minSize size (spaceSize sp)})

renderBarHorizontal :: Char -> Render Result
renderBarHorizontal char = do
  width <- availableWidth
  let barText = Text.replicate width (Text.singleton char)
  renderLines . pure $ unstyled barText

-------------------------------------------------------------------------------

-- Not 100% happy with how this section turned out, might want to refactor it.
newtype StyledLine = StyledLine {styledSegments :: [StyledSegment]}
  deriving stock (Show)
  deriving newtype (Semigroup, Monoid)

unstyled :: Text -> StyledLine
unstyled = styled []

styled :: [Ansi.SGR] -> Text -> StyledLine
styled style = StyledLine . map (StyledSegment style) . Text.lines

data StyledSegment = StyledSegment
  { segmentStyle :: [Ansi.SGR],
    segmentText :: Text
  }
  deriving stock (Show)

-- | Wraps lines. If there is not enough vertical space available, the last
-- lines will be shown.
renderLines :: Vector StyledLine -> Render Result
renderLines block = do
  height <- availableHeight
  width <- availableWidth
  -- We know each element needs at least one line, so no we can discard some
  -- immediately.
  let candidates = Vector.drop (length block - height) block
  let wrapped = foldMap (wrapStyledLine width) candidates
  let visible = drop (length wrapped - height) wrapped
  let paddingTop = height - length wrapped
  zipWithM_ renderLine [paddingTop ..] visible
  pure mempty

renderLine :: Int -> StyledLine -> Render ()
renderLine offset line = do
  origin <- spaceOrigin <$> ask
  liftIO $ Ansi.setCursorPosition (positionRow origin + offset) (positionColumn origin)
  liftIO $ traverse_ renderSegment $ reverse $ styledSegments line
  where
    renderSegment (StyledSegment style text) = do
      Ansi.setSGR style
      Text.putStr text

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
