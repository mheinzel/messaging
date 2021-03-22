{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Console.ANSI.Declarative.Widget.Block where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text (putStr)
import qualified Prettyprinter as PP
import qualified Prettyprinter.Render.Terminal as PP
import qualified Prettyprinter.Render.Terminal.Internal as PP.Internal
import qualified Prettyprinter.Render.Util.Panic as PP.Panic
import qualified System.Console.ANSI as Ansi
import System.Console.ANSI.Declarative.Widget.Render

block :: Text -> Block
block = prettyBlock . PP.pretty

-- | To be used with the @prettyprinter@ and @prettyprinter-ansi-terminal@
-- packages.
prettyBlock :: PP.Doc PP.AnsiStyle -> Block
prettyBlock = Block AlignTop AlignLeft

-- | TODO: Currently, these don't really work.
alignTop, alignBottom, alignMiddle:: Block -> Block
alignTop b = b {alignTopBottom = AlignTop}
alignBottom b = b {alignTopBottom = AlignBottom}
alignMiddle b = b {alignTopBottom = AlignMiddle}

-- | TODO: Currently, these don't really work.
alignLeft, alignRight, alignCenter :: Block -> Block
alignLeft b = b {alignLeftRight = AlignLeft}
alignRight b = b {alignLeftRight = AlignRight}
alignCenter b = b {alignLeftRight = AlignCenter}

data Block = Block
  { alignTopBottom :: AlignTopBottom,
    alignLeftRight :: AlignLeftRight,
    blockContent :: PP.Doc PP.AnsiStyle
  }
  deriving (Show)

data AlignTopBottom
  = AlignTop
  | AlignBottom
  | AlignMiddle
  deriving (Show)

data AlignLeftRight
  = AlignLeft
  | AlignRight
  | AlignCenter
  deriving (Show)

instance IsWidget Block where
  renderWidget = renderBlock

-- | Wraps lines. If there is not enough vertical space available, the last
-- lines will be shown.
renderBlock :: Block -> Render Result
renderBlock (Block alignTB alignLR content) = do
  width <- availableWidth
  let options = PP.LayoutOptions (PP.AvailablePerLine width 1.0)
  let docStream = PP.layoutSmart options content
  renderDocStream alignTB alignLR docStream

-- | We are rolling our own, because we need a few things 'PP.renderIO' doesn't
-- offer:
--
-- * print with a vertical offset
-- * really make sure to stop printing when available width is used up
--   (not sure if a layouting can sometimes still produce lines that are too long)
-- * stop printing lines when when available height is used up
renderDocStream ::
  AlignTopBottom ->
  AlignLeftRight ->
  PP.SimpleDocStream PP.AnsiStyle ->
  Render Result
renderDocStream _alignTB _alignLR docStream = do
  origin <- currentOrigin
  size <- availableSize
  -- Currently just starting at the top, no horizonal aligment.
  let rowOffset = 0

  liftIO $ mempty <$ renderDocStreamRaw origin size rowOffset docStream

renderDocStreamRaw ::
  Position ->
  Size ->
  -- | row offset
  Int ->
  PP.SimpleDocStream PP.AnsiStyle ->
  IO ()
renderDocStreamRaw origin size initRowOffset initDocStream = do
  Ansi.setCursorPosition
    (positionRow origin)
    (positionColumn origin)
  let initColOffset = 0
  let initStyles = pure mempty :: NonEmpty PP.AnsiStyle
  renderRaw initRowOffset initColOffset initStyles initDocStream
  where
    inRange row col =
      and [row >= 0, col >= 0, row < sizeRows size, col < sizeColumns size]
    renderRaw row col styles = \case
      PP.SFail -> PP.Panic.panicUncaughtFail
      PP.SEmpty -> pure ()
      PP.SChar c rest -> do
        when (inRange row col) $
          putChar c
        renderRaw row (col + 1) styles rest
      PP.SText len txt rest -> do
        let remainingLength = sizeColumns size - col
        when (inRange row col) $
          Text.putStr $ Text.take remainingLength txt
        renderRaw row (col + min len remainingLength) styles rest
      PP.SLine indent rest -> do
        Ansi.setCursorPosition
          (positionRow origin + row + 1)
          (positionColumn origin + indent)
        renderRaw (row + 1) indent styles rest
      PP.SAnnPush pushedStyle rest -> do
        let newStyle = pushedStyle <> NonEmpty.head styles
        Ansi.setSGR $ ansiStyleSGRs newStyle
        renderRaw row col (NonEmpty.cons newStyle styles) rest
      PP.SAnnPop rest -> do
        case NonEmpty.nonEmpty (NonEmpty.tail styles) of
          Nothing -> PP.Panic.panicUnpairedPop
          Just styles' -> do
            Ansi.setSGR $ ansiStyleSGRs $ NonEmpty.head styles'
            renderRaw row col styles' rest

-- | This is unfortunately not exposed by 'pretty-printer-ansi-terminal', so we
-- need to copy it.
ansiStyleSGRs :: PP.AnsiStyle -> [Ansi.SGR]
ansiStyleSGRs style =
  catMaybes
    [ Just Ansi.Reset,
      flip fmap (PP.Internal.ansiForeground style) $ \(int, c) ->
        Ansi.SetColor Ansi.Foreground (convertIntensity int) (convertColor c),
      flip fmap (PP.Internal.ansiBackground style) $ \(int, c) ->
        Ansi.SetColor Ansi.Background (convertIntensity int) (convertColor c),
      flip fmap (PP.Internal.ansiBold style) $ \_ ->
        Ansi.SetConsoleIntensity Ansi.BoldIntensity,
      flip fmap (PP.Internal.ansiItalics style) $ \_ ->
        Ansi.SetItalicized True,
      flip fmap (PP.Internal.ansiUnderlining style) $ \_ ->
        Ansi.SetUnderlining Ansi.SingleUnderline
    ]
  where
    convertIntensity :: PP.Intensity -> Ansi.ColorIntensity
    convertIntensity = \case
      PP.Vivid -> Ansi.Vivid
      PP.Dull -> Ansi.Dull

    convertColor :: PP.Color -> Ansi.Color
    convertColor = \case
      PP.Black -> Ansi.Black
      PP.Red -> Ansi.Red
      PP.Green -> Ansi.Green
      PP.Yellow -> Ansi.Yellow
      PP.Blue -> Ansi.Blue
      PP.Magenta -> Ansi.Magenta
      PP.Cyan -> Ansi.Cyan
      PP.White -> Ansi.White
