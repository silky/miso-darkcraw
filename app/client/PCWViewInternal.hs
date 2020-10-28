{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- |
-- This module contains generic things to be used in *View.hs
-- and *ViewInternal.hs files. Contrary to 'ViewInternal', it contains
-- things that are specific to the game; not solely pure HTML/CSS stuff.
-- |
module PCWViewInternal
  ( cardBoxShadowStyle,
    cardCreature,
    cardCreatureUI,
    cardPositionStyle,
    cardPositionStyle',
    viewFrame,
    CardDrawStyle (..),
    DisplayMode (..),
  )
where

import Card
import Cinema (Actor (..), ActorKind (..), ActorState (..), Direction, Element (..), Frame (..), defaultDirection, spriteToKind)
import Constants
import Data.Function ((&))
import qualified Data.Map.Strict as Map
import Data.Maybe
import Miso
import Miso.String (MisoString, ms)
import Miso.Util ((=:))
import SharedModel
import Update (Action)
import ViewInternal

data DisplayMode = NormalMode | DebugMode

data CardDrawStyle = CardDrawStyle
  { -- | Whether the card should fade in
    fadeIn :: Bool,
    -- | Whether the card is being hovered
    hover :: Bool
  }

instance Semigroup CardDrawStyle where
  CardDrawStyle fadeIn1 hover1 <> CardDrawStyle fadeIn2 hover2 =
    CardDrawStyle (fadeIn1 || fadeIn2) (hover1 || hover2)

instance Monoid CardDrawStyle where
  mempty = CardDrawStyle False False

cardBoxShadowStyle ::
  -- | The (r, g, b) of the border
  (Int, Int, Int) ->
  -- | The width of the border
  Int ->
  -- | The timing-function of the transition
  MisoString ->
  Attribute a
cardBoxShadowStyle (r, g, b) width timingFunction =
  style_ $
    Map.fromList
      [ ("box-shadow", "0 0 0 " <> ms width <> "px " <> rgba r g b),
        ("transition", "box-shadow"),
        ("transition-duration", "0.15s"),
        ("transition-timing-function", timingFunction)
      ]

-- | A convenience wrapper over [cardCreature]
cardCreatureUI ::
  -- | The z index
  Int ->
  SharedModel ->
  -- | Whether a card should be drawn or solely a placeholder for drag target
  Creature UI ->
  CardDrawStyle ->
  Styled (View Action)
cardCreatureUI z shared ui cdsty =
  cardCreature z shared (Just (core, filepath ui)) cdsty
  where
    core = unliftCreature ui

-- | Div displaying a card
cardCreature ::
  -- | The z index
  Int ->
  SharedModel ->
  -- | Whether a card should be drawn or solely a placeholder for drag target
  Maybe (Creature Core, Filepath) ->
  CardDrawStyle ->
  Styled (View Action)
cardCreature z _ Nothing cdsty = pure $ cardBackground z cdsty
cardCreature z shared (Just (creature, filepath)) cdsty@CardDrawStyle {fadeIn} =
  if fadeIn
    then
      keyframed
        builder
        (keyframes (animDataName animData) "opacity: 0;" [] "opacity: 1;")
        animData
    else pure $ builder []
  where
    (topMargin, leftMargin) = (cps `div` 4, topMargin)
    pictureStyle =
      zplt (z + 1) Absolute ((cardPixelWidth - cps) `div` 2) topMargin
    pictureCell = imgCell $ ms $ filepathToString filepath
    statsTopMargin = topMargin * 2 + cps
    statsStyle = zpltwh (z + 1) Absolute leftMargin statsTopMargin width cps
    fontSize = cps `div` 2
    fontStyle =
      "font-size" =: px fontSize
        <> "font-family" =: "serif"
    width = cardPixelWidth - (topMargin * 2)
    inStatsStyle = flexLineStyle <> fontStyle
    statsCell =
      div_
        [style_ inStatsStyle]
        [ text $ ms $ hp creature,
          imgCell assetFilenameHeart,
          text $ ms $ attack creature,
          imgCell assetFilenameSword
        ]
    skills = fromMaybe [] $ Card.skills creature
    skillsTopMargin = statsTopMargin + fontSize + (fontSize `div` 2)
    skillsHeight = cps * length skills
    skillsStyle =
      "display" =: "flex"
        <> "flex-direction" =: "column"
        <> "align-items" =: "flex-start" -- left
        <> zpltwh (z + 1) Absolute leftMargin skillsTopMargin width skillsHeight
        <> fontStyle
    skillDiv skill = div_ [] [liftSkill shared skill & skillTitle & ms & text]
    skillsDivs = map skillDiv skills
    builder attrs =
      div_ attrs $
        [div_ [style_ pictureStyle] [pictureCell]]
          ++ [div_ [style_ statsStyle] [statsCell]]
          ++ [div_ [style_ skillsStyle] skillsDivs]
          ++ [cardBackground z cdsty]
    animData =
      (animationData "handCardFadein" "1s" "ease")
        { animDataFillMode = Just "forwards"
        }

cardBackground ::
  -- | The z index
  Int ->
  CardDrawStyle ->
  View Action
cardBackground z cdsty =
  div_
    [style_ cardStyle']
    [ img_
        [ src_ $ assetsPath assetFilenameBeigeBG,
          width_ $ ms cardPixelWidth,
          height_ $ ms cardPixelHeight,
          noDrag
        ]
    ]
  where
    cardStyle = zpwh z Absolute cardPixelWidth cardPixelHeight
    cardStyle' =
      if hover cdsty
        then Map.insert "outline" (ms borderSize <> "px solid red") cardStyle
        else cardStyle

cardPositionStyle ::
  -- | The horizontal offset from the enclosing container, in number of cells
  Int ->
  -- | The vertical offset from the enclosing container, in number of cells
  Int ->
  Map.Map MisoString MisoString
cardPositionStyle xCellsOffset yCellsOffset =
  cardPositionStyle' (xCellsOffset * cps) (yCellsOffset * cps)

cardPositionStyle' ::
  -- | The horizontal offset from the enclosing container, in pixels
  Int ->
  -- | The vertical offset from the enclosing container, in pixels
  Int ->
  Map.Map MisoString MisoString
cardPositionStyle' xPixelsOffset yPixelsOffset =
  pltwh Absolute xPixelsOffset yPixelsOffset cardPixelWidth cardPixelHeight

-- Now come functions that are about building the view of a Scene
data Context = Context
  { z :: Int,
    paths :: Map.Map CreatureID (Direction -> MisoString),
    shared :: SharedModel
  }

createContext :: Int -> SharedModel -> Context
createContext z shared@SharedModel {..} =
  Context {..}
  where
    paths = map f sharedCards & catMaybes & Map.fromList
    f (CreatureCard Creature {..}) = Just (creatureId, dirToFilename filepath)
    f _ = Nothing
    -- Leave 24x24_3_0.png untouched if direction is ToLeft
    dirToFilename filepath dir
      | dir == defaultDirection =
        ms $ filepathToString filepath
    -- Return 24x24_3_1.png untouched if direction is ToRight. This is
    -- where we rely on the right version of a sprite to be one line below
    -- its left version
    dirToFilename f@Filepath {..} _ =
      dirToFilename f {fpY = fpY + 1} defaultDirection

viewFrame :: DisplayMode -> Int -> SharedModel -> Frame -> View a
viewFrame mode z smodel (Frame mapping) =
  div_
    []
    $ mapping
      & Map.toList
      & map (uncurry (viewEntry mode context))
      & concat
  where
    context = createContext z smodel

stateToAttribute :: Int -> ActorState -> Attribute a
stateToAttribute z ActorState {x, y} =
  style_ $
    pltwh Absolute (x * cps) (y * cps) cps cps
      <> "z-index" =: ms z

viewEntry :: DisplayMode -> Context -> Element -> Actor -> [View a]
viewEntry _ _ _ (Actor _ ActorState {sprite = Nothing}) = []
viewEntry mode Context {..} element (Actor mname state@ActorState {direction, telling, sprite = Just sprite}) =
  [ nodeHtmlKeyed
      "div"
      (Key (ms (show element)))
      ([stateToAttribute (zFor sprite) state] ++ nameTooltip)
      [imgCell path]
  ]
    ++ [ div_ [bubbleStyle state] [text $ ms $ fromJust telling]
         | isJust telling
       ]
  where
    (zpp, zpppp, zppPP) = (z + 1, zpp + 1, zpppp + 1)
    path = case sprite of
      Left cid ->
        case paths Map.!? cid of
          Nothing -> error $ "CreatureID has no corresponding filename: " ++ show cid
          Just dirToPath -> dirToPath direction
      Right tile -> tileToFilepath shared tile & filepathToString & ms
    bubbleStyle ActorState {x, y} =
      style_ $
        "position" =: "absolute"
          <> "left" =: px ((x * cps) + cps `div` 2)
          <> "top" =: px ((y - 1) * cps)
          <> "transform" =: "translate(-50%, -50%)" -- Center element
          <> "background-color" =: beigeHTML
          <> "border-radius" =: px 2 -- rounded corners
          <> "z-index" =: ms zppPP -- on top of everything
          <> "width" =: "fit-content" -- make box exactly the size of the text
    zFor sprite =
      case spriteToKind sprite of
        CreatureKind -> zpppp
        TileKind -> zpp
    nameTooltip
      | Just name <- mname, DebugMode <- mode = [title_ (ms name)]
      | otherwise = []
