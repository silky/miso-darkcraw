{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- |
-- Module to display an instance of the main view, i.e.
-- the battle between two players. It's the only module that
-- can depend on 'GameViewInternal'.
-- |
module GameView where

import Board
import Card
import Configuration (hashless)
import Constants
import Control.Lens
import Control.Monad.Except
import Data.Generics.Labels ()
import Data.Generics.Product
import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Text as Text
import Debug.Trace (traceShow)
import Event
import qualified Game
import GameViewInternal
import Miso hiding (at)
import Miso.String hiding (concat, intersperse, map)
import Model
import PCWViewInternal
import SharedModel
import Update
import ViewBlocks (dummyOn)
import ViewInternal

-- | Constructs a virtual DOM from a game model
viewGameModel :: GameModel -> Styled (View Action)
viewGameModel model@GameModel {board, interaction, playingPlayer} = do
  boardDiv <- boardDivM
  handDiv <- handDivM
  return $ div_ [] [boardDiv, handDiv]
  where
    (z, zpp) = (0, z + 1)
    enemySpot = otherPlayerSpot playingPlayer
    boardCardsM = boardToInPlaceCells zpp model dragTargetType
    boardDivM = do
      let errs = errView model zpp & maybeToList
      stacks <-
        if hashless
          then traverse (stackView model z enemySpot GameViewInternal.Board) [Discarded, Stacked]
          else return []
      turn <- turnView model zpp
      let scores = scoreViews model zpp
      boardCards <- boardCardsM
      return $ div_ [style_ boardStyle] $ concat stacks ++ [turn] ++ errs ++ scores ++ boardCards
    boardStyle =
      zpltwh z Relative 0 0 boardPixelWidth boardPixelHeight
        <> "background-image" =: assetsUrl "forest.png"
    handDivM = do
      cells <- boardToInHandCells zpp model
      return $ div_ [style_ handStyle] cells
    handStyle =
      zpltwh z Relative 0 0 handPixelWidth handPixelHeight
        <> "background-image" =: assetsUrl "forest-hand.png"
    dragTargetType =
      case interaction of
        GameDragInteraction (Dragging (HandIndex i) _) ->
          case boardToHand board playingPlayer & flip lookupHand i & runExcept of
            Left err -> traceShow (Text.unpack err) Nothing
            Right id -> Just $ idToTargetType id
        _ -> Nothing

boardToInPlaceCells ::
  -- | The z index
  Int ->
  GameModel ->
  -- | The target to which the card being dragged applies (if any)
  Maybe TargetType ->
  Styled [View Action]
boardToInPlaceCells z m@GameModel {board, playingPlayer} dragTargetType = do
  emitTopLevelStyle $ bumpKeyFrames True
  emitTopLevelStyle $ bumpKeyFrames False
  main <- mainM
  let playerTargets = [boardToPlayerTarget playerTargetZ m dragTargetType pSpot | pSpot <- allPlayersSpots]
  return [div_ [] $ main ++ playerTargets]
  where
    mainM =
      sequence
        [ boardToInPlaceCell z m dragTargetType pSpot cSpot
          | (pSpot, cSpot, _) <- boardToHoleyInPlace board
        ]
    bumpAnim upOrDown = ms $ "bump" ++ (if upOrDown then "Up" else "Down")
    bumpInit = "transform: translateY(0px);"
    sign upOrDown = if upOrDown then "-" else ""
    bump50 upOrDown = "transform: translateY(" ++ sign upOrDown ++ show cellPixelSize ++ "px);"
    bumpKeyFrames upOrDown =
      keyframes (bumpAnim upOrDown) bumpInit [(50, bump50 upOrDown)] bumpInit
    playerTargetActive = borderWidth m (Game.PlayerTarget playingPlayer) > 0
    playerTargetZ = if playerTargetActive then z + 1 else z - 1

boardToInPlaceCell ::
  Int ->
  GameModel ->
  -- | The target to which the card being dragged applies (if any)
  Maybe TargetType ->
  PlayerSpot ->
  CardSpot ->
  Styled (View Action)
boardToInPlaceCell z m@GameModel {anims, board, gameShared, interaction} dragTargetType pSpot cSpot =
  nodeHtmlKeyed
    "div"
    (Key $ ms key)
    ( [ style_ $
          Map.fromList bounceStyle
            <> cardPositionStyle x y -- Absolute positioning
            <> "z-index" =: ms z,
        class_ "card",
        cardBoxShadowStyle rgb (borderWidth m target) "ease-in-out"
      ]
        ++ inPlaceEnterLeaveAttrs GameAction'
        ++ (dragDropEvents <$> dragTarget & concat)
    )
    <$> do
      death <- deathFadeout attackEffect x y
      heart <- heartWobble (z + 1) attackEffect x y
      cards <- sequence $
        case maybeCreature of
          Nothing -> Nothing
          Just creature ->
            let cdsty =
                  mempty
                    { hover = beingHovered,
                      PCWViewInternal.fadeIn = Board.fadeIn attackEffect
                    }
             in Just $ cardView z gameShared (CreatureCard creature) cdsty
      return $ maybeToList cards ++ death ++ heart
  where
    key = intersperse "_" ["inPlace", show pSpot, show cSpot] & concat
    maybeCreature = boardToInPlaceCreature board pSpot cSpot
    inPlaceEnterLeaveAttrs lift =
      -- If dragging we don't need to handle in place hovering
      case (maybeCreature, dragTarget) of
        (Just _, Nothing) ->
          [ onMouseEnter' "card" $ lift $ GameInPlaceMouseEnter target,
            onMouseLeave' "card" $ lift $ GameInPlaceMouseLeave target
          ]
        _ -> []
    target = Game.CardTarget pSpot cSpot
    bumpAnim upOrDown = ms $ "bump" ++ (if upOrDown then "Up" else "Down")
    upOrDown = case pSpot of PlayerTop -> False; PlayerBot -> True
    (x, y) = cardCellsBoardOffset pSpot cSpot
    beingHovered = interaction == GameHoverInPlaceInteraction target
    attackEffect =
      anims ^. spotToLens pSpot . field' @"inPlace" . #unInPlaceEffects . ix cSpot
    bounceStyle =
      [ ("animation", bumpAnim upOrDown <> " 0.5s ease-in-out")
        | attackBump attackEffect
      ]
    rgb = borderRGB interaction target
    dragTarget =
      case (dragTargetType, maybeCreature) of
        (Just (CardTargetType Hole), Nothing) -> Just target
        (Just (CardTargetType Occupied), Just _) -> Just target
        _ -> Nothing

boardToPlayerTarget ::
  Int ->
  GameModel ->
  Maybe TargetType ->
  PlayerSpot ->
  View Action
boardToPlayerTarget z m@GameModel {interaction} dragTargetType pSpot =
  nodeHtmlKeyed
    "div"
    (Key $ ms key)
    ( [ style_ $ posStyle x y <> "z-index" =: ms z, -- Absolute positioning
        cardBoxShadowStyle rgb bwidth "ease-in-out"
      ]
        ++ (dragDropEvents <$> dragTarget & concat)
    )
    []
  where
    key = show pSpot ++ "-target"
    (x, y) = cardCellsBoardOffset pSpot cSpot
    (w, h) = (cardCellWidth * 3 + cardHCellGap * 2, cardCellHeight * 2 + cardVCellGap)
    posStyle x y = pltwh Absolute (x * cps) (y * cps) (w * cps) (h * cps)
    cSpot = case pSpot of PlayerTop -> TopLeft; PlayerBot -> BottomRight
    target = Game.PlayerTarget pSpot
    (rgb, bwidth) = (borderRGB interaction target, borderWidth m target)
    dragTarget =
      case dragTargetType of
        Just PlayerTargetType -> Just target
        _ -> Nothing

-- | The events for placeholders showing drag targets
dragDropEvents :: Game.Target -> [Attribute Action]
dragDropEvents target =
  [ onDragEnter $ lift $ GameDragEnter target,
    onDragLeave $ lift $ GameDragLeave target,
    onDrop (AllowDrop True) $ lift GameDrop,
    dummyOn "dragover"
  ]
  where
    lift = GameAction'

borderRGB :: GameInteraction -> Game.Target -> (Int, Int, Int)
borderRGB interaction target =
  case interaction of
    GameDragInteraction Dragging {dragTarget} | dragTarget == Just target -> yellowRGB
    _ -> greenRGB

boardToInHandCells ::
  -- | The z index
  Int ->
  GameModel ->
  Styled [View Action]
boardToInHandCells z m@GameModel {board, playingPlayer, gameShared} = do
  stacks <- traverse (stackView m z playingPlayer Hand) [Discarded, Stacked]
  cards <- traverse (boardToInHandCell z m) icreatures
  return $ cards ++ concat stacks
  where
    cards =
      boardToHand board playingPlayer
        & map (unliftCard . unsafeIdentToCard gameShared)
    icreatures = Prelude.zip cards [HandIndex 0 ..]

boardToInHandCell ::
  -- | The z index
  Int ->
  GameModel ->
  (Card Core, HandIndex) ->
  Styled (View Action)
boardToInHandCell z GameModel {anims, interaction, gameShared, playingPlayer} (card, i) = do
  card <- cardView z gameShared card cdsty
  return $ div_ attrs [card | not beingDragged]
  where
    pixelsXOffset i
      | i == 0 = (boardPixelWidth - cardPixelWidth) `div` 2 -- center
      | i == 1 = pixelsXOffset 0 - xshift -- shift to the left compared to the center
      | i == 2 = pixelsXOffset 0 + xshift -- shift to the right compared to the center
      | i `mod` 2 == 0 = xshift + pixelsXOffset (i - 2) -- iterate
      | otherwise = pixelsXOffset (i - 2) - xshift -- iterate
      where
        xshift = cardCellWidth * cellPixelSize + (cardHCellGap * cellPixelSize) `div` 2
    (beingHovered, beingDragged) =
      case interaction of
        GameHoverInteraction Hovering {hoveredCard} ->
          (hoveredCard == i, False)
        GameDragInteraction Dragging {draggedCard} ->
          (False, draggedCard == i)
        GameShowErrorInteraction _ -> (False, False)
        _ -> (False, False)
    x = pixelsXOffset (unHandIndex i)
    y = 2 * cellPixelSize
    fadeIn = unHandIndex i `elem` boardToHand anims playingPlayer
    attrs =
      [ style_ $ cardPositionStyle' x y,
        prop "draggable" True,
        onDragStart $ GameAction' $ GameDragStart i,
        onDragEnd $ GameAction' GameDragEnd,
        class_ "card",
        onMouseEnter' "card" $ GameAction' $ GameInHandMouseEnter i,
        onMouseLeave' "card" $ GameAction' $ GameInHandMouseLeave i
      ]
    cdsty = mempty {hover = beingHovered, PCWViewInternal.fadeIn = fadeIn}

cardCellsBoardOffset :: PlayerSpot -> CardSpot -> (Int, Int)
cardCellsBoardOffset PlayerTop cardSpot =
  (offsetx + x, offsety + y)
  where
    (offsetx, offsety) =
      (boardToLeftCardCellsOffset, 3) -- offset from background corner
    botyShift = cardCellHeight + cardVCellGap -- offset from top to bottom line
    xtop = cardCellWidth + cardHCellGap -- offset from left to middle
    xtopright = xtop * 2 -- offset from left to right
    (x, y) = case cardSpot of
      TopLeft -> (0, 0)
      Top -> (xtop, 0)
      TopRight -> (xtopright, 0)
      BottomLeft -> (0, botyShift)
      Bottom -> (xtop, botyShift)
      BottomRight -> (xtopright, botyShift)
cardCellsBoardOffset PlayerBot cardSpot =
  (offsetx + x, offsety + y)
  where
    (offsetx, offsety) =
      ( boardToLeftCardCellsOffset,
        3 + (2 * cardCellHeight) + cardVCellGap + teamsVCellGap -- offset from background corner
      )
    topyShift = cardCellHeight + cardVCellGap
    xtop = cardCellWidth + cardHCellGap -- offset from left to middle
    xtopright = xtop * 2 -- offset from left to right
    (x, y) = case cardSpot of
      TopLeft -> (xtopright, topyShift) -- TopLeft is bottom right in bottom part
      Top -> (xtop, topyShift) -- Top is Bottom in bottom part
      TopRight -> (0, topyShift) -- TopRght is bottom left in bottom part
      BottomLeft -> (xtopright, 0) -- BottomLeft is top right in bottom part
      Bottom -> (xtop, 0) -- Bottom is Top in bottom part
      BottomRight -> (0, 0) -- BottomRight is Top Left in bottom part

handCell :: View Action
handCell =
  img_
    [ width_ $ ms handPixelWidth,
      src_ $ assetsPath "forest-hand.png",
      noDrag
    ]
