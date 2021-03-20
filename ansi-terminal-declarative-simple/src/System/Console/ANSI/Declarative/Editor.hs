module System.Console.ANSI.Declarative.Editor where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Zipper as Zipper
import qualified Data.Vector as Vector
import System.Console.ANSI.Declarative.Input
import System.Console.ANSI.Declarative.View

newtype Editor = Editor {getZipper :: Zipper.TextZipper Text}
  deriving (Show)

editor :: Text -> Editor
editor initial = Editor $ Zipper.textZipper (Text.lines initial) Nothing

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

viewEditor :: Editor -> View
viewEditor =
  Block AlignTop AlignLeft
    . Vector.fromList
    . fmap unstyled
    . editorContent
