{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Messaging.Client.Terminal.View where

import Data.Foldable (toList)
import Data.Text (Text)
import qualified Messaging.Client.Core.State as Core
import Messaging.Client.Terminal.State
import qualified Messaging.Shared.Conversation as Conv
import qualified Messaging.Shared.User as User
import Prettyprinter ((<+>))
import qualified Prettyprinter as PP
import qualified Prettyprinter.Render.Terminal as PP
import qualified System.Console.ANSI.Declarative.Widget as Widget

viewState :: State -> Widget.SomeWidget
viewState state =
  Widget.SomeWidget $
    Widget.prettyBlock $ PP.pretty $ show $_inputs state
