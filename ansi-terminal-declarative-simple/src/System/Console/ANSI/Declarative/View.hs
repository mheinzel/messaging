{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Console.ANSI.Declarative.View
  ( render
  , View (..)
  , SplitDir (..)
  , SplitPos (..)
  , StyledLine
  , unstyled
  , styled
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader, ask, local)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.Foldable (traverse_)
import Data.List (unfoldr)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text (putStr)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified System.Console.ANSI as Ansi
import qualified System.Console.Terminal.Size as Term

data View
  = Block (Vector StyledLine)
  | Split SplitDir SplitPos View View
  | BarAtTop Char View
  deriving (Show)

data SplitDir
  = Horizontal
  deriving (Show)

data SplitPos
  = FromBeginning Int
  | FromEnd Int
  deriving (Show)

-------------------------------------------------------------------------------

render :: View -> IO ()
render view = do
  window <- Term.size
  Ansi.clearScreen
  Ansi.setCursorPosition 0 0
  let outline =
        Outline
          { heightFrom = 0,
            widthFrom = 0,
            heightTo = maybe 24 Term.height window,
            widthTo = maybe 72 Term.width window
          }
  runRender outline $ renderView view
  Ansi.setSGR [Ansi.Reset]

newtype Render a = Render {getRender :: ReaderT Outline IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader Outline)

runRender :: Outline -> Render () -> IO ()
runRender outline = flip runReaderT outline . getRender

availableHeight :: Render Int
availableHeight = do
  outline <- ask
  pure $ heightTo outline - heightFrom outline

availableWidth :: Render Int
availableWidth = do
  outline <- ask
  pure $ widthTo outline - widthFrom outline

-------------------------------------------------------------------------------

data Outline = Outline
  { heightFrom :: Int,
    widthFrom :: Int,
    heightTo :: Int,
    widthTo :: Int
  }
  deriving (Show)

splitOutlineHorizontal :: SplitPos -> Outline -> (Outline, Outline)
splitOutlineHorizontal pos Outline {heightFrom, widthFrom, heightTo, widthTo} =
  let heightSplitUnchecked = case pos of
        FromBeginning n -> heightFrom + n
        FromEnd n -> heightTo - n
      heightSplit = clamp (heightFrom, heightTo) heightSplitUnchecked
      top = Outline heightFrom widthFrom heightSplit widthTo
      bot = Outline heightSplit widthFrom heightTo widthTo
   in (top, bot)

clamp :: Ord a => (a, a) -> a -> a
clamp (lo, hi) = max lo . min hi

-------------------------------------------------------------------------------

renderView :: View -> Render ()
renderView = \case
  Split Horizontal pos top bot ->
    splitHorizontal pos (renderView top) (renderView bot)
  BarAtTop char block ->
    splitHorizontal (FromBeginning 1) (renderBar char) (renderView block)
  Block block ->
    renderLines block

splitHorizontal :: SplitPos -> Render () -> Render () -> Render ()
splitHorizontal splitPos renderTop renderBot = do
  (outlineTop, outlineBot) <- splitOutlineHorizontal splitPos <$> ask
  local (const outlineTop) renderTop
  local (const outlineBot) renderBot

renderBar :: Char -> Render ()
renderBar char = do
  width <- availableWidth
  let barText = Text.replicate width (Text.singleton char)
  renderLine $ StyledLine [StyledSegment [] barText]

-------------------------------------------------------------------------------

-- Not 100% happy with how this section turned out, might want to refactor it.
newtype StyledLine = StyledLine {styledSegments :: [StyledSegment]}
  deriving stock (Show)
  deriving newtype (Semigroup, Monoid)

unstyled :: Text -> StyledLine
unstyled = styled []

styled :: [Ansi.SGR] -> Text -> StyledLine
styled style = StyledLine . pure . StyledSegment style

data StyledSegment = StyledSegment
  { segmentStyle :: [Ansi.SGR],
    segmentText :: Text
  }
  deriving stock (Show)

-- | Wraps lines. If there is not enough vertical space available, the last
-- lines will be shown.
renderLines :: Vector StyledLine -> Render ()
renderLines block = do
  height <- availableHeight
  width <- availableWidth
  -- We know each element needs at least one line, so no we can discard some
  -- immediately.
  let unwrapped = Vector.drop (length block - height) block
  let wrapped = foldMap (wrapStyledLine width) unwrapped
  let visible = drop (length wrapped - height) wrapped
  let padded = replicate (height - length wrapped) mempty <> visible
  outline <- ask
  liftIO $ Ansi.setCursorPosition (heightFrom outline) (widthFrom outline)
  traverse_ renderLine padded

renderLine :: StyledLine -> Render ()
renderLine line = do
  liftIO $ traverse_ renderSegment $ reverse $ styledSegments line
  liftIO $ Text.putStr "\n"
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
