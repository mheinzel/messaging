{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module System.Console.ANSI.Declarative.Widget.Render where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader.Class (MonadReader, asks, local)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import qualified System.Console.ANSI as Ansi
import qualified System.Console.Terminal.Size as Term

class Show a => IsWidget a where
  renderWidget :: a -> Render Result

data SomeWidget where
  SomeWidget :: IsWidget a => a -> SomeWidget

deriving stock instance Show SomeWidget

instance IsWidget SomeWidget where
  renderWidget (SomeWidget widget) = renderWidget widget

-------------------------------------------------------------------------------

renderToTerminal :: IsWidget a => a -> IO ()
renderToTerminal widget = do
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
  result <- runRender screenSpace $ renderWidget widget

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

-------------------------------------------------------------------------------

newtype Render a = Render {getRender :: ReaderT ScreenSpace IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader ScreenSpace)

runRender :: ScreenSpace -> Render a -> IO a
runRender outline = flip runReaderT outline . getRender

currentOrigin :: Render Position
currentOrigin = do
  asks spaceOrigin

availableSize :: Render Size
availableSize = do
  asks spaceSize

availableHeight :: Render Int
availableHeight = do
  asks (sizeRows . spaceSize)

availableWidth :: Render Int
availableWidth = do
  asks (sizeColumns . spaceSize)

offsetBy :: Position -> Render Result -> Render Result
offsetBy offset =
  fmap (offsetResultFrom offset)
    . local (\sp -> sp {spaceOrigin = offset <> spaceOrigin sp})

withSize :: Size -> Render Result -> Render Result
withSize size =
  local (\sp -> sp {spaceSize = size})

-------------------------------------------------------------------------------

newtype Result = Result
  { resultCursors :: [Position]
  }
  deriving (Show, Semigroup, Monoid)

offsetResultFrom :: Position -> Result -> Result
offsetResultFrom pos (Result cursors) =
  Result (map (<> negatePosition pos) cursors)

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
