{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Messaging.Client.Web.View where

import Data.Either (isLeft)
import Data.Foldable (toList)
import qualified Data.JSString as JSString
import qualified Data.Map as Map
import Data.Maybe (isNothing)
import qualified Messaging.Client.Core.State as Core
import Messaging.Client.Web.State
import Messaging.Client.Web.Update
import qualified Messaging.Shared.Conversation as Conv
import qualified Messaging.Shared.User as User
import qualified Miso
import Miso.Html
import Miso.String (MisoString, fromMisoString, toMisoString)
import Prelude hiding (div)

{- HLINT ignore "Redundant $" -}

viewModel :: Model -> View Action
viewModel model =
  div_ [class_ "container", style_ $ Map.singleton "height" "100vh"] $
    [ div_ [class_ "columns"] $
        [ link_ [rel_ "stylesheet", href_ "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.9.2/css/bulma.min.css"],
          div_ [class_ "column is-narrow"] $
            [viewSidebar model],
          div_ [class_ "column"] $
            [viewMain model]
        ]
    ]

viewSidebar :: Model -> View Action
viewSidebar model =
  div_ [class_ "box", style_ $ Map.singleton "width" "380px"] $
    [ h3_ [class_ "title is-3 has-text-centered"] $
        [ a_
            [ href_ "https://github.com/mheinzel/messaging",
              style_ $ Map.singleton "color" "#00d1b2"
            ]
            [text "Messaging"],
          text " Web Client"
        ],
      h6_ [class_ "subtitle is-6 has-text-centered"] $
        [ text "Built using ",
          a_ [href_ "https://github.com/ghcjs/ghcjs"] [text "GHCJS"],
          text ", ",
          a_ [href_ "https://haskell-miso.org/"] [text "Miso"],
          text " and ",
          a_ [href_ "https://bulma.io/"] [text "Bulma"],
          text "."
        ],
      case model of
        Chatting chat ->
          maybe NoOp ChatAction <$> viewConversationNames chat
        _ ->
          div_ [] []
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
        [ label_ [class_ "label"] [text "Server URL"],
          p_ [class_ "control"] $
            [ input_
                [ class_ "input",
                  type_ "url",
                  value_ (serverUrl login),
                  onInput $ Just . UpdateServerUrl,
                  onEnter StartLogin
                ]
            ]
        ],
      div_ [class_ "field"] $
        [ label_ [class_ "label"] [text "Username"],
          p_ [class_ "control"] $
            [ input_
                [ class_ $
                    "input"
                      <> if isLeft validatedUserName then " is-danger" else "",
                  type_ "text",
                  maxlength_ "24",
                  autofocus_ True,
                  value_ (userName login),
                  onInput $ Just . UpdateUserName,
                  onEnter StartLogin
                ]
            ],
          case validatedUserName of
            Left err
              | not (JSString.null (userName login)) ->
                p_ [class_ "help is-danger"] [text err]
            _ ->
              p_ [] []
        ],
      button_
        [ class_ "button is-primary",
          disabled_ $ isLeft validatedUserName,
          onClick $ Just StartLogin
        ]
        [text "Connect"]
    ]
  where
    validatedUserName =
      case User.mkUserName (fromMisoString (userName login)) of
        Just n -> Right n
        Nothing -> Left "Usernames must have 3 to 24 characters (letters, numbers, dash or underscore)"

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

viewConversationNames :: Chat -> View (Maybe ChatAction)
viewConversationNames chat =
  div_ [] $
    [ nav_ [class_ "panel"] $
        [p_ [class_ "panel-heading"] [text "Conversations"]]
          <> map
            (viewConversationName (currentConversationName chat))
            (conversationNames chat),
      addConversation
    ]
  where
    addConversation =
      div_ [class_ "field has-addons"] $
        [ p_ [class_ "control"] $
            [ a_ [class_ "button is-static"] [text "#"]
            ],
          p_ [class_ "control"] $
            [ input_
                [ class_ "input",
                  type_ "text",
                  maxlength_ "24",
                  value_ $ newConversation chat,
                  onInput $ Just . UpdateNewConversation,
                  onEnter JoinConversation
                ]
            ],
          p_ [class_ "control"] $
            [ button_
                [ class_ "button is-primary",
                  disabled_ newConversationInvalid,
                  onClick $ Just JoinConversation
                ]
                [text "Join"]
            ]
        ]

    newConversationInvalid =
      isNothing newConversationName
    newConversationName =
      Conv.mkConversationName $ fromMisoString $ newConversation chat

viewConversationName ::
  Maybe Conv.ConversationName ->
  Conv.ConversationName ->
  View (Maybe ChatAction)
viewConversationName focusedConv convName =
  if focusedConv == Just convName
    then
      div_
        [ class_ "panel-block is-active has-background-danger-light",
          style_ $ Map.singleton "white-space" "nowrap"
        ]
        [ p_ [class_ "control"] $
            [ button_
                [ class_ "button is-danger is-small",
                  onClick $ Just $ LeaveConversation convName
                ]
                [text "Leave"]
            ],
          text $ toMisoString $ "#" <> Conv.conversationNameText convName
        ]
    else
      a_
        [ class_ "panel-block",
          style_ $ Map.singleton "white-space" "nowrap",
          onClick $ Just $ SwitchConversation convName
        ]
        [ p_ [class_ "control"] $
            [ button_
                [ class_ "button is-invisible is-small"
                ]
                [text "Leave"]
            ],
          text $ toMisoString $ "#" <> Conv.conversationNameText convName
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
                then defaultValue_ "" -- dummy
                else onEnter $ SendMessage txt
            ]
        ],
      p_ [class_ "control"] $
        [ button_
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
