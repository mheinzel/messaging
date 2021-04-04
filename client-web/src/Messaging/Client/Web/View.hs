{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Messaging.Client.Web.View where

import Data.Foldable (toList)
import qualified Data.JSString as JSString
import qualified Data.Map as Map
import Data.Maybe (isJust, isNothing)
import qualified Data.Text as Text
import qualified Messaging.Client.Core.State as Core
import Messaging.Client.Web.State
import Messaging.Client.Web.Update
import qualified Messaging.Shared.User as User
import qualified Miso
import Miso.Html
import Miso.String (MisoString, fromMisoString, toMisoString)

{- HLINT ignore "Redundant $" -}

viewModel :: Model -> View Action
viewModel model =
  div_ [class_ "container", style_ $ Map.singleton "height" "100vh"] $
    [ div_ [class_ "columns"] $
        [ link_ [rel_ "stylesheet", href_ "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.4.3/css/bulma.min.css"],
          div_ [class_ "column is-narrow"] $
            [viewSidebar model],
          div_ [class_ "column"] $
            [viewMain model]
        ]
    ]

viewSidebar :: Model -> View Action
viewSidebar model =
  div_ [class_ "box", style_ $ Map.singleton "width" "300px"] $
    [ h4_ [class_ "title is-4"] [text "Messaging Web Client"],
      h6_ [class_ "subtitle is-6"] [a_ [href_ "https://github.com/mheinzel/messaging"] [text "(GitHub)"]],
      case model of
        Chatting chat -> viewConversationList chat
        _ -> div_ [] []
    ]

viewMain :: Model -> View Action
viewMain = \case
  LoggingIn login ->
    maybe NoOp LoginAction <$> viewLogin login
  WaitingForAuth waiting ->
    viewWaiting waiting
  Chatting chat ->
    maybe NoOp ChatAction <$> viewChat chat

-- Login ----------------------------------------------------------------------

viewLogin :: Login -> View (Maybe LoginAction)
viewLogin login =
  div_ [class_ "columns"] $
    [ div_ [class_ "column is-half"] $
        [viewLoginBox login],
      div_ [class_ "column is-half"] $
        map viewLoginError (errors login)
    ]

viewLoginBox :: Login -> View (Maybe LoginAction)
viewLoginBox login =
  div_ [class_ "box"] $
    [ div_ [class_ "field"] $
        [ label_ [class_ "label"] [text "Backend URL"],
          div_ [class_ "control"] $
            [ input_
                [ class_ "input",
                  type_ "url",
                  value_ (backendUrl login),
                  onInput $ Just . UpdateBackendUrl,
                  onEnter StartLogin
                ]
            ]
        ],
      div_ [class_ "field"] $
        [ label_ [class_ "label"] [text "Username"],
          div_ [class_ "control"] $
            [ input_
                [ class_ $ "input" <> if userNameInvalid then " is-danger" else "",
                  type_ "text",
                  autofocus_ True,
                  value_ (userName login),
                  onInput $ Just . UpdateUserName,
                  onEnter StartLogin
                ]
            ],
          case userNameError of
            Nothing -> p_ [] []
            Just err -> p_ [class_ "help is-danger"] [text err]
        ],
      button_
        [ class_ "button is-primary",
          disabled_ userNameInvalid,
          onClick $ Just StartLogin
        ]
        [text "Connect"]
    ]
  where
    name = fromMisoString (userName login)
    userNameInvalid = Text.null name || isJust userNameError
    userNameError =
      if not (Text.null name) && isNothing (User.mkUserName name)
        then Just "Usernames must have 3 to 24 characters (letters, numbers, dash or underscore)"
        else Nothing

viewLoginError :: MisoString -> View a
viewLoginError err =
  article_ [class_ "message is-danger"] $
    [ div_ [class_ "message-header"] [text "Error"],
      div_ [class_ "message-body"] [text err]
    ]

-- Waiting --------------------------------------------------------------------

viewWaiting :: Waiting -> View a
viewWaiting _waiting =
  div_ [class_ "block"] $
    [ div_ [class_ "block"] $
        [ text "Connecting..."
        ],
      div_ [class_ "block"] $
        [ text "Maybe the URL is wrong or the server is not running?"
        ]
    ]

-- Chat -----------------------------------------------------------------------

viewConversationList :: Chat -> View a
viewConversationList _chat =
  nav_ [class_ "panel"] $
    [ p_ [class_ "panel-heading"] [text "Conversations"],
      a_ [class_ "panel-block is-active"] [text "#general"]
    ]

viewChat :: Chat -> View (Maybe ChatAction)
viewChat chat =
  case currentConversation chat of
    Nothing ->
      div_ [class_ "block"] [text "No conversation selected"]
    Just conv ->
      viewConversation conv (editor chat)

-- #outer {
--   display: flex;
--   flex-flow: column;
--   height: 100%;
-- }
--
-- #inner_fixed {
--   height: 100px;
--   background-color: grey;
-- }
--
-- #inner_remaining {
--   background-color: #DDDDDD;
--   flex-grow : 1;
-- }
viewConversation :: Core.ConversationState -> MisoString -> View (Maybe ChatAction)
viewConversation conv edit =
  div_ [] $
    [ div_
        [ class_ "box",
          style_ . Map.fromList $
            [ -- Use most vertical space available, leave some for editor.
              ("height", "calc(100vh - 100px)"),
              ("overflow-y", "auto"),
              -- To make scrolling stick to bottom.
              ("display", "flex"),
              ("flex-direction", "column-reverse")
            ]
        ]
        [ viewConversationHistory (Core._conversationHistory conv)
        ],
      viewEditor edit
    ]

viewConversationHistory :: Core.ConversationHistory -> View a
viewConversationHistory history =
  div_ [] $
    fmap viewHistoryEntry $
      toList $ Core._historyEntries history

viewHistoryEntry :: Core.ConversationHistoryEntry -> View a
viewHistoryEntry entry =
  div_ [] $
    case entry of
      Core.Message sender msg ->
        [ colored primary $ User.userNameText sender,
          text $ ": " <> toMisoString msg
        ]
      Core.UserJoined user ->
        [ colored info $ User.userNameText user <> " joined"
        ]
      Core.UserLeft user ->
        [ colored info $ User.userNameText user <> " left"
        ]
  where
    colored c txt =
      span_ [style_ $ Map.singleton "color" c] [text $ toMisoString txt]
    primary = "hsl(171, 100%, 41%)"
    info = "hsl(204, 36%, 63%)"

viewEditor :: MisoString -> View (Maybe ChatAction)
viewEditor txt =
  div_ [class_ "field is-grouped"] $
    [ p_ [class_ "control is-expanded"] $
        [ input_
            [ class_ "input is-primary",
              value_ txt,
              onInput $ Just . UpdateEditor,
              if editorEmpty
                then autofocus_ True -- dummy
                else onEnter $ SendMessage txt
            ]
        ],
      p_ [class_ "control"] $
        [ a_
            [ class_ "button is-primary",
              disabled_ editorEmpty,
              onClick $ Just $ SendMessage txt
            ]
            [text "Send"]
        ]
    ]
  where
    editorEmpty = JSString.null $ JSString.strip txt

onEnter :: a -> Attribute (Maybe a)
onEnter action = onKeyDownWithInfo $ \case
  Miso.KeyInfo {Miso.keyCode = Miso.KeyCode 13, Miso.shiftKey = False} ->
    Just action
  _ ->
    Nothing
