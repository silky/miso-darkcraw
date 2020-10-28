{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- This module contains the subset of the model that is needed by 'Game'
-- I didn't want to make 'Game' depend on 'Model', because it seemed overkill
-- and dangerous cyclewise in the long run. Hence I introduced this module.
-- |
module SharedModel
  ( identToCard,
    identToCreature,
    liftCard,
    liftCreature,
    SharedModel (..),
    tileToFilepath,
    unsafeIdentToCard,
    unsafeLiftCard,
    unsafeLiftCreature,
    liftSkill,
  )
where

import Card
import Data.Foldable (asum, find)
import Data.Function ((&))
import Data.Maybe
import GHC.Generics (Generic)
import System.Random
import Tile (Tile, TileUI (..))

instance Eq StdGen where
  std1 == std2 = show std1 == show std2

-- | The part of the model that is likely to be used by all pages
-- | i.e. all possible models
data SharedModel = SharedModel
  { -- | Data obtained at load time, that never changes
    sharedCards :: [Card UI],
    sharedSkills :: [SkillUI],
    sharedTiles :: [TileUI],
    -- | RNG obtained at load time, to be user whenever randomness is needed
    sharedStdGen :: StdGen
  }
  deriving (Eq, Generic, Show)

identToCard :: SharedModel -> CardIdentifier -> Maybe (Card UI)
identToCard SharedModel {sharedCards} cid =
  asum $ map (identToCard' cid) sharedCards

-- Could type level computations make this function superseded
-- by identToCard? I mean if you pass a 'IDC' CardIdentifier to identToCard,
-- you're sure to get a Just (CreatureCard _) value
-- (Just (NeutralCard _) is impossible)
identToCreature :: SharedModel -> CreatureID -> Maybe (Creature UI)
identToCreature SharedModel {sharedCards} cid =
  asum $ map (identToCreature' cid) sharedCards

unsafeIdentToCard :: SharedModel -> CardIdentifier -> Card UI
unsafeIdentToCard smodel ci = identToCard smodel ci & fromJust

identToCard' :: CardIdentifier -> Card p -> Maybe (Card p)
identToCard' cid card =
  case (cid, card) of
    (IDC cid2, CreatureCard Creature {creatureId = cid1}) | cid1 == cid2 -> Just card
    (IDN n2, NeutralCard n1) | n1 == n2 -> Just card
    (IDI i2, ItemCard i1) | i1 == i2 -> Just card
    _ -> Nothing

identToCreature' :: CreatureID -> Card p -> Maybe (Creature p)
identToCreature' cid (CreatureCard c@Creature {creatureId = cid1}) | cid1 == cid = Just c
identToCreature' _ _ = Nothing

liftCard :: SharedModel -> Card Core -> Maybe (Card UI)
liftCard shared = \case
  CreatureCard creature -> CreatureCard <$> liftCreature shared creature
  NeutralCard n -> Just $ NeutralCard n
  ItemCard i -> Just $ ItemCard i

liftCreature :: SharedModel -> Creature Core -> Maybe (Creature UI)
liftCreature shared creature =
  identToCreature shared cid
  where
    IDC cid = creatureToIdentifier creature

liftSkill :: SharedModel -> Skill -> SkillUI
liftSkill SharedModel {sharedSkills} skill =
  fromMaybe
    default_
    $ find (\SkillUI {skill = sk} -> sk == skill) sharedSkills
  where
    default_ = SkillUI {skill = HitFromBack, ..}
    skillText = show skill ++ " not found!"
    skillTitle = skillText

unsafeLiftCard :: SharedModel -> Card Core -> Card UI
unsafeLiftCard s c = liftCard s c & fromJust

unsafeLiftCreature :: SharedModel -> Creature Core -> Creature UI
unsafeLiftCreature s c = liftCreature s c & fromJust

tileToFilepath :: SharedModel -> Tile -> Filepath
tileToFilepath SharedModel {sharedTiles} tile =
  case find (\TileUI {tile = t} -> t == tile) sharedTiles of
    Nothing -> Card.default24Filepath
    Just TileUI {Tile.filepath} -> filepath
