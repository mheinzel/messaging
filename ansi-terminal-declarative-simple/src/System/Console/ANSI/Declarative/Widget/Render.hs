{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module System.Console.ANSI.Declarative.Widget.Render where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader.Class (MonadReader, asks, local)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import qualified System.Console.ANSI as Ansi
import qualified System.IO as IO

class Show a => IsWidget a where
  renderWidget :: a -> Render Result

data SomeWidget where
  SomeWidget :: IsWidget a => a -> SomeWidget

deriving stock instance Show SomeWidget

instance IsWidget SomeWidget where
  renderWidget (SomeWidget widget) = renderWidget widget

-------------------------------------------------------------------------------

renderToTerminal :: IsWidget a => Size -> a -> IO ()
renderToTerminal size widget = do
  Ansi.hideCursor
  Ansi.setSGR [Ansi.Reset]
  Ansi.clearScreen
  Ansi.setCursorPosition 0 0
  let screenSpace = ScreenSpace (Position 0 0) size
  result <- runRender screenSpace $ renderWidget widget

  case resultCursors result of
    -- Just take first available cursor for now.
    cursor : _ -> do
      Ansi.setCursorPosition (positionRow cursor) (positionColumn cursor)
      Ansi.showCursor
    [] -> do
      -- We already hid the cursor, nothing to do.
      pure ()

  -- This is necessary because of output buffering.
  IO.hFlush IO.stdout

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
  local (\sp -> sp {spaceOrigin = offset <> spaceOrigin sp})

withSize :: Size -> Render Result -> Render Result
withSize size =
  local (\sp -> sp {spaceSize = size})

withCursor :: Position -> Render Result -> Render Result
withCursor pos = fmap (Result [pos] <>)

-------------------------------------------------------------------------------

newtype Result = Result
  { resultCursors :: [Position]
  }
  deriving (Show, Semigroup, Monoid)

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
