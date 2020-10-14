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
  )
where

import Card (Card (CreatureCard), Creature (..), CreatureID, Filepath (..), Phase (..), filepath, filepathToString, unliftCreature)
import Cinema (ActorKind (..), ActorState (..), Direction, Element (..), Frame (..), Frame (..), defaultDirection, spriteToKind)
import Constants
import Data.Function ((&))
import qualified Data.Map.Strict as Map
import Data.Maybe
import Miso
import Miso.String (MisoString, ms)
import Miso.Util ((=:))
import SharedModel (SharedModel (..), tileToFilepath)
import Update (Action)
import ViewInternal

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
  -- | Whether a card should be drawn or solely a placeholder for drag target
  Creature UI ->
  -- | Whether this card is being hovered
  Bool ->
  View Action
cardCreatureUI z ui hover =
  cardCreature z (Just (core, filepath ui)) hover
  where
    core = unliftCreature ui

-- | Div displaying a card
cardCreature ::
  -- | The z index
  Int ->
  -- | Whether a card should be drawn or solely a placeholder for drag target
  Maybe (Creature Core, Filepath) ->
  -- | Whether this card is being hovered
  Bool ->
  View Action
cardCreature z Nothing hover = cardBackground z hover
cardCreature z (Just (creature, filepath)) hover =
  div_
    []
    $ [div_ [style_ pictureStyle] [pictureCell]]
      ++ [div_ [style_ statsStyle] [statsCell]]
      ++ [cardBackground z hover]
  where
    topMargin = cps `div` 4
    pictureStyle =
      zplt (z + 1) Absolute ((cardPixelWidth - cps) `div` 2) topMargin
    pictureCell = imgCell $ ms $ filepathToString filepath
    statsStyle = zpltwh (z + 1) Absolute topMargin top width cps
      where
        width = cardPixelWidth - (topMargin * 2)
        top = topMargin + cps + topMargin
    inStatsStyle =
      flexLineStyle
        <> "font-size" =: (ms (cps `div` 2) <> "px")
        <> "font-family" =: "serif"
    statsCell :: View Action =
      div_
        [style_ inStatsStyle]
        [ text $ ms $ hp creature,
          imgCell assetFilenameHeart,
          text $ ms $ attack creature,
          imgCell assetFilenameSword
        ]

cardBackground ::
  -- | The z index
  Int ->
  -- | Whether the card is being hovered
  Bool ->
  View Action
cardBackground z hover =
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
      if hover
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

viewFrame :: Int -> SharedModel -> Frame -> View a
viewFrame z smodel (Frame mapping) =
  div_
    []
    $ mapping
      & Map.toList
      & map (uncurry (viewEntry context))
      & concat
  where
    context = createContext z smodel

stateToAttribute :: Int -> ActorState -> Attribute a
stateToAttribute z ActorState {x, y} =
  style_ $
    pltwh Absolute (x * cps) (y * cps) cps cps
      <> "z-index" =: ms z

viewEntry :: Context -> Element -> ActorState -> [View a]
viewEntry _ _ ActorState {sprite = Nothing} = []
viewEntry Context {..} _ state@ActorState {direction, telling, sprite = Just sprite} =
  [div_ [stateToAttribute (zFor sprite) state] [imgCell path]]
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
