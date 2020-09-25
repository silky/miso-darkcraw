{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Module containing the base for animating scenes in lobbies
module Cinema
  ( ActorChange (Leave),
    Direction (..),
    DirectionChange,
    Element (..),
    Frame (..),
    ActorState (..),
    Scene (..),
    Sprite (),
    StayChange,
    SpriteChange,
    TellingChange,
    TimedFrame (..),
    (=:),
    (|||),
    at,
    at',
    creatureSprite,
    defaultDirection,
    display,
    down,
    initial,
    left,
    mkChange,
    patch,
    patchActorState,
    right,
    shutup,
    tell,
    tileSprite,
    up,
    while,
  )
where

import Card (CreatureID)
import Data.Function ((&))
import Data.Kind (Constraint, Type)
import Data.List (find)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, fromMaybe, isJust)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import SharedModel (SharedModel (..))
import Tile (Tile)

data Direction = ToLeft | ToRight -- Suffix with 'To' to avoid clashing with Either
  deriving (Eq, Generic, Ord, Show)

defaultDirection :: Direction
defaultDirection = ToLeft -- How sprite are in oryx' set

creatureSprite :: CreatureID -> Sprite
creatureSprite = Left

tileSprite :: Tile -> Sprite
tileSprite = Right

-- Sufficient data to obtain a sprite
type Sprite = Either CreatureID Tile

data ActorState = ActorState
  { -- | In which direction the sprite is looking at
    direction :: Direction,
    -- | How to draw this actor
    sprite :: Sprite,
    -- | Whether the element says something
    telling :: Maybe String,
    -- | 0 means on the left
    x :: Int,
    -- | 0 means top
    y :: Int
  }
  deriving (Eq, Generic, Ord, Show)

defaultActorState :: Sprite -> ActorState
defaultActorState sprite =
  ActorState
    { direction = defaultDirection,
      sprite = sprite,
      telling = Nothing,
      x = 0,
      y = 0
    }

-- An actor's unique identifier
newtype Element = Element {unElement :: Int}
  deriving (Eq, Generic, Ord, Show)

newtype Frame a = Frame {unFrame :: Map.Map Element a}
  deriving (Eq, Ord, Show, Generic)

data TimedFrame a = TimedFrame
  { -- | The duration of a frame, in tenth of seconds
    duration :: Int,
    -- | The frame
    frame :: Frame a
  }
  deriving (Eq, Ord, Show, Generic)

newtype Scene a = Scene {frames :: [TimedFrame a]}
  deriving (Eq, Ord, Show, Generic)

instance Semigroup (Scene a) where
  (Scene frames1) <> (Scene frames2) = Scene (frames1 <> frames2)

instance Monoid (Scene a) where
  mempty = Scene []

data DirectionChange = TurnRight | TurnLeft | NoDirectionChange
  deriving (Eq, Generic, Ord, Show)

instance Semigroup DirectionChange where
  TurnLeft <> TurnRight = NoDirectionChange
  TurnLeft <> _ = TurnLeft
  TurnRight <> TurnLeft = NoDirectionChange
  TurnRight <> _ = TurnRight
  NoDirectionChange <> change = change

instance Monoid DirectionChange where
  mempty = NoDirectionChange

data TellingChange = Tell String | ShutUp | NoTellingChange
  deriving (Eq, Generic, Ord, Show)

-- last change wins
instance Semigroup TellingChange where
  change <> NoTellingChange = change
  _ <> change = change

instance Monoid TellingChange where
  mempty = NoTellingChange

data SpriteChange = SetSprite Sprite | NoSpriteChange
  deriving (Eq, Generic, Ord, Show)

-- | The change to an actor's 'ActorState'
data StayChange = StayChange
  { -- | The change to 'direction'
    turn :: DirectionChange,
    -- | The change to the sprite
    spriteChange :: SpriteChange,
    -- | The change to 'telling'
    tellingChange :: TellingChange,
    -- | The change to 'x'
    xoffset :: Int,
    -- | The change to 'y'
    yoffset :: Int
  }
  deriving (Eq, Generic, Ord, Show)

instance Semigroup SpriteChange where
  s <> NoSpriteChange = s
  _ <> s = s

instance Monoid SpriteChange where
  mempty = NoSpriteChange

instance Semigroup StayChange where
  (StayChange turn1 sprite1 tell1 dx1 dy1) <> (StayChange turn2 sprite2 tell2 dx2 dy2) =
    StayChange (turn1 <> turn2) (sprite1 <> sprite2) (tell1 <> tell2) (dx1 + dx2) (dy1 + dy2)

instance Monoid StayChange where
  mempty = StayChange mempty mempty mempty 0 0

data ActorChange
  = -- | Actor leaves the stage
    Leave
  | -- | Actor stays on stage, but moves or says something
    Stay StayChange
  deriving (Eq, Generic, Ord, Show)

instance Semigroup ActorChange where
  _ <> change = change

instance Monoid ActorChange where
  mempty = Stay mempty

mkChange :: Sprite -> DirectionChange -> TellingChange -> Int -> Int -> ActorChange
mkChange sprite turn tellingChange xoffset yoffset =
  Stay StayChange {spriteChange = SetSprite sprite, ..}

-- | Use this function to initialize an 'Element'
at :: Sprite -> Int -> Int -> ActorChange
at sprite x y = Stay mempty {spriteChange = SetSprite sprite, xoffset = x, yoffset = y}

-- | Use this function to initialize an 'Element'
at' :: Sprite -> Direction -> Int -> Int -> ActorChange
at' sprite dir x y =
  Stay mempty {spriteChange = SetSprite sprite, turn = turnFrom dir, xoffset = x, yoffset = y}
  where
    turnFrom ToRight = TurnRight
    turnFrom ToLeft = TurnLeft

shift :: Int -> Int -> ActorChange
shift x y = Stay mempty {xoffset = x, yoffset = y}

down :: ActorChange
down = shift 0 1

left :: ActorChange
left = shift (-1) 0

right :: ActorChange
right = shift 1 0

shutup :: ActorChange
shutup = Stay mempty {tellingChange = ShutUp}

tell :: String -> ActorChange
tell s = Stay mempty {tellingChange = Tell s}

up :: ActorChange
up = shift 0 (-1)

initial :: TimedFrame ActorChange -> TimedFrame ActorState
initial tframe = patch (TimedFrame 0 (Frame mempty)) tframe

patch :: TimedFrame ActorState -> TimedFrame ActorChange -> TimedFrame ActorState
patch
  TimedFrame {frame = Frame prev}
  TimedFrame {duration, frame = Frame diff} =
    -- Take duration from Change: ignore old duration
    TimedFrame {frame = Frame frame'', ..}
    where
      -- Compute elements that make it to the next scene
      elements = Map.keys prev ++ Map.keys diff & Set.fromList
      frame' = Set.map buildActorState elements & Set.toList & Map.fromList
      buildActorState e =
        ( e,
          case (prev Map.!? e, diff Map.!? e) of
            (_, Just Leave) ->
              Nothing
            (Nothing, Just c) ->
              -- No previous state, consider diff as absolute
              patchActorState (defaultActorState $ unsafeSpriteOf c) c
            (Just p, Just c) ->
              -- Apply diff
              patchActorState p c
            (p, Nothing) ->
              -- No diff: keep previous state
              p
        )
      frame'' = Map.mapMaybe id frame'
      unsafeSpriteOf Leave = error "Sprite of leaving actor should not be requested"
      unsafeSpriteOf (Stay StayChange {spriteChange}) =
        case spriteChange of
          SetSprite sprite -> sprite
          NoSpriteChange -> error "Actor has no initial Sprite"

patchActorState :: ActorState -> ActorChange -> Maybe ActorState
patchActorState s@ActorState {..} (Stay StayChange {..}) =
  Just $
    s
      { direction = applyDirectionChange direction turn,
        telling = applyTellingChange telling tellingChange,
        x = x + xoffset,
        y = y + yoffset
      }
  where
    applyDirectionChange dir NoDirectionChange = dir
    applyDirectionChange _ TurnRight = ToRight
    applyDirectionChange _ TurnLeft = ToLeft
    applyTellingChange telling NoTellingChange = telling
    applyTellingChange _ (Tell s) = Just s
    applyTellingChange _ ShutUp = Nothing
patchActorState _ Leave = Nothing

(=:) :: Element -> ActorChange -> Frame ActorChange
k =: v = Frame (Map.singleton k v)

instance Semigroup (Frame ActorChange) where
  (Frame m1) <> (Frame m2) = Frame (Map.unionWith (<>) m1 m2)

instance Monoid (Frame ActorChange) where
  mempty = Frame mempty

-- | Given a duration and a frame, builds a 'TimedFrame'
while :: Int -> Frame a -> Scene a
while i m = Scene [TimedFrame {duration = i, frame = m}]

type Date = Int

type DatedFrames =
  ( [(Date, Frame ActorChange)],
    Date -- end date of the last diff
  )

(|||) :: Scene ActorChange -> Scene ActorChange -> Scene ActorChange
(Scene ss1) ||| (Scene ss2) = Scene (fromDates (merge (toDates ss1) (toDates ss2)))
  where
    toDates :: [TimedFrame ActorChange] -> DatedFrames
    toDates = go [] 0
      where
        go acc t [] = (reverse acc, t)
        go acc t (TimedFrame {duration, frame} : scenes) = go ((t, frame) : acc) (t + duration) scenes
    fromDates :: DatedFrames -> [TimedFrame ActorChange]
    fromDates (frames, endDate) = go frames
      where
        go [] = []
        go [(t, frame)] = [TimedFrame (endDate - t) frame]
        go ((t1, frame) : frames@((t2, _) : _)) = TimedFrame (t2 - t1) frame : go frames
    merge :: DatedFrames -> DatedFrames -> DatedFrames
    merge (ms1, endDate1) (ms2, endDate2) = (go ms1 ms2, max endDate1 endDate2)
      where
        go ms1 [] = ms1
        go [] ms2 = ms2
        go ms1@((t1, m1) : ms1') ms2@((t2, m2) : ms2')
          | t1 < t2 = (t1, m1) : go ms1' ms2
          | t1 > t2 = (t2, m2) : go ms1 ms2'
          | otherwise = (t1, m1 <> m2) : go ms1' ms2'

-- | Builds a scene of states from a scene of changes. Interprets the
-- | first diff as an absolute scene (i.e. not as a diff).
display :: Scene ActorChange -> Scene ActorState
display (Scene []) = Scene []
display (Scene (absolute : diffs)) =
  Scene (firstActorState : display' firstActorState diffs)
  where
    firstActorState = initial absolute
    display' _ [] = []
    display' display (firstChange : nextChanges) =
      let nextActorState = patch display firstChange
       in nextActorState : display' nextActorState nextChanges
