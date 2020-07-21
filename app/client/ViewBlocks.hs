{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- This module defines the basic building blocks used by views,
-- so that they have a uniform look'n'feel
-- |
module ViewBlocks (GUI (..)) where

import Constants (borderSize)
import Data.List
import qualified Data.Map.Strict as Map
import Miso
import Miso.String
import Miso.Util ((=:))
import ViewInternal

class GUI a where
  -- | argument #1 is the z-index
  -- | argument #2 is whether the button is enabled
  -- | argument #3 is the style
  -- | argument #4 is the button's text
  textButton :: Int -> Bool -> [Attribute a] -> MisoString -> [View a]

-- | Implementation where text buttons have a continuous shrink/grow border
instance GUI a where
  textButton z enabled attrs text =
    keyframed
      (\as -> builder z enabled (as ++ attrs) text)
      "box-shadow: 0 0 0 0 rgba(0,255,0,1);"
      ("box-shadow: 0 0 0 " <> ms borderSize <> "px rgba(0,255,0,1);")
      ("pulse", "infinite", "ease-in-out")
      (Just "alternate")
      Nothing
    where
      builder z enabled attrs text =
        button_ (buttonStyle enabled False : attrs) [stytextzhv z text 0 0]

--- | Implementation where text buttons have a simple non moving borde
instance GUI a where
  textButton z enabled attrs text =
    [ button_
      (buttonStyle enabled True : attrs)
      [stytextzhv z text 0 0]
    ]

disabledColor :: MisoString
disabledColor = "#CCCCCC"

buttonStyle :: Bool -> Bool -> Attribute a
buttonStyle enabled border =
  style_ $
    ("border" =: (px borderSize <> " solid " <> borderColor))
      <> "background-color" =: "transparent" -- no background
      <> "outline" =: "none" -- don't highlight that it has been pressed
      <> Map.fromList textRawStyle
  where
    borderColor = if enabled then textMainColor else disabledColor
    borderSize = if border then 2 else 0
