{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module to display the initial view of the game, i.e. the one
-- that shows up when the game starts.
-- |
module WelcomeView (viewWelcomeModel) where

import Constants
import qualified Data.Map.Strict as Map
import Miso
import Miso.String hiding (length)
import Miso.Util ((=:))
import Model (WelcomeModel (..))
import Update
import ViewInternal

-- | Constructs a virtual DOM from a welcome model
viewWelcomeModel :: WelcomeModel -> View Action
viewWelcomeModel _ =
  div_
    [style_ style]
    [torchesDiv zpp, div_ [style_ flexColumnStyle] [titleDiv, buttonsDiv]]
  where
    (z, zpp) = (0, z + 1)
    style =
      Map.union (zpltwh z Relative 0 0 welcomePixelWidth welcomePixelHeight) $
        Map.fromList
          [("background-image", assetsUrl "welcome.png")]
    -- The top level flex, layout things in a column
    textStyle = Map.fromList textRawStyle
    titleDiv = div_ [style_ titleStyle] [text gameTitle]
    titleFontSize = cellPixelSize + (cellPixelSize `div` 2)
    titleStyle =
      textStyle
        <> "font-size" =: (ms titleFontSize <> "px")
        <> "z-index" =: ms zpp
        <> "margin-top" =: (ms cellPixelSize <> "px")
    topMarginAttr = style_ $ "margin-top" =: (ms titleFontSize <> "px")
    -- A flex right below the top level, layout things in a line
    -- It has two cells: ["single player"; "start"]
    buttonsDiv =
      div_
        [ style_ $
            flexLineStyle
              <> "justify-content" =: "center"
              <> "width" =: (ms welcomePixelWidth <> "px")
              <> "margin-top" =: (ms (titleFontSize * 2) <> "px")
        ]
        [singlePlayerButtonDiv, buttonDiv]
    singlePlayerButtonDiv =
      div_
        [style_ textStyle, style_ $ marginhv cellPixelSize 0]
        [text "Single Player"]
    buttonDiv =
      button_
        [ onClick $ WelcomeAction' WelcomeStart,
          style_ $ marginhv cellPixelSize 0,
          buttonStyle
        ]
        [div_ [style_ textStyle] [text "Start"]]

torchesDiv :: Int -> View Action
torchesDiv z =
  div_
    []
    [ nodeHtml
        "style"
        []
        ["@keyframes torch { 100% { background-position: -48px; } }"],
      torch True,
      torch False
    ]
  where
    torch x = div_ [style_ $ style x] []
    style x =
      Map.union
        (tilerb z Absolute 0 (14 + (if x then 1 else -1)))
        ("background-image" =: assetsUrl "torchs.png")
        <> ("animation" =: ("torch " <> duration x <> " steps(2) infinite"))
    duration x = if x then "2s" else "2.5s"
