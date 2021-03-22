module System.Console.ANSI.Declarative.Widget.Editor where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Zipper as Zipper
import System.Console.ANSI.Declarative.Input (KeyboardArrow (..), KeyboardInput (..))
import System.Console.ANSI.Declarative.Widget.Block (block)
import System.Console.ANSI.Declarative.Widget.Render

editor :: Text -> Editor
editor initial = Editor $ Zipper.textZipper (Text.lines initial) Nothing

-- | Limitation: Doesn't handle multi-width Unicode characters well.
-- Single-width should be fine, though.
newtype Editor = Editor {getZipper :: Zipper.TextZipper Text}
  deriving (Show)

editorContent :: Editor -> [Text]
editorContent = Zipper.getText . getZipper

handleInput :: KeyboardInput -> Editor -> Editor
handleInput input = Editor . edit input . getZipper
  where
    edit (Printable c) = Zipper.insertChar c
    edit Enter = Zipper.breakLine
    edit Tab = id
    edit Backspace = Zipper.deletePrevChar
    edit Delete = Zipper.deleteChar
    edit PageUp = id
    edit PageDown = id
    edit Home = Zipper.gotoBOL
    edit End = Zipper.gotoEOL
    edit Escape = id
    edit (Arrow ArrowUp) = Zipper.moveUp
    edit (Arrow ArrowDown) = Zipper.moveDown
    edit (Arrow ArrowLeft) = Zipper.moveLeft
    edit (Arrow ArrowRight) = Zipper.moveRight

instance IsWidget Editor where
  renderWidget = renderEditor

renderEditor :: Editor -> Render Result
renderEditor ed = do
  origin <- currentOrigin
  size <- availableSize
  let (row, col) = clampTo size $ Zipper.cursorPosition $ getZipper ed
  withCursor (origin <> Position row col) $
    renderWidget $
      block . Text.unlines $
        editorContent ed
  where
    clampTo (Size height width) (row, col) =
      let row' = row + col `div` width
       in if row' >= height
            then (height - 1, width - 1) -- stick to end/corner
            else (row', col `mod` width)
