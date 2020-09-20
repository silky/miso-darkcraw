{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

-- |
-- This module contains the subset of the model that is needed by 'Game'
-- I didn't want to make 'Game' depend on 'Model', because it seemed overkill
-- and dangerous cyclewise in the long run. Hence I introduced this module.
-- |
module SharedModel
  ( identToCard,
    identToCreature,
    liftCreature,
    SharedModel (..),
    unsafeIdentToCard,
  )
where

import Card (Card (..), CardIdentifier (..), CardIdentifier, Creature (..), Creature, CreatureID, Phase (..), creatureToIdentifier)
import Data.Foldable (asum)
import Data.Function ((&))
import Data.Maybe (fromJust)
import GHC.Generics (Generic)
import System.Random

instance Eq StdGen where
  std1 == std2 = show std1 == show std2

-- | The part of the model that is likely to be used by all pages
-- | i.e. all possible models
data SharedModel = SharedModel
  { -- | Data obtained at load time, that never changes
    sharedCards :: [Card UI],
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

liftCreature :: SharedModel -> Creature Core -> Maybe (Creature UI)
liftCreature shared creature =
  identToCreature shared cid
  where
    IDC cid = creatureToIdentifier creature
