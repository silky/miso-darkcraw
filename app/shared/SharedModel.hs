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
    idToCreature,
    liftCard,
    liftCreature,
    SharedModel,
    tileToFilepath,
    unsafeGet,
    unsafeIdentToCard,
    unsafeLiftCard,
    unsafeLiftCreature,
    liftSkill,
    shuffle,
    create,
    SharedModel.getStdGen,
    getCards,
    withStdGen,
    getCardIdentifiers,
    cardToFilepath,
  )
where

import Card
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Random
import Data.Foldable (find)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import GHC.Generics (Generic)
import Json (loadJson)
import System.Random.Shuffle (shuffleM)
import Tile (Filepath, Tile, TileUI (..), default24Filepath)

instance Eq StdGen where
  std1 == std2 = show std1 == show std2

-- | The part of the model that is likely to be used by all pages
-- | i.e. all possible models
data SharedModel = SharedModel
  -- XXX @smelc, turn those into maps, for efficiency
  { -- | Data obtained at load time, that never changes
    sharedCards :: Map CardIdentifier (Card UI),
    sharedSkills :: Map Skill SkillUI,
    sharedTiles :: Map Tile TileUI,
    -- | RNG obtained at load time, to be user whenever randomness is needed
    sharedStdGen :: StdGen
  }
  deriving (Eq, Generic, Show)

create :: [Card UI] -> [SkillUI] -> [TileUI] -> StdGen -> SharedModel
create cards skills tiles sharedStdGen =
  SharedModel {..}
  where
    groupBy f l = map (\e -> (f e, e)) l & Map.fromList
    sharedCards = groupBy cardToIdentifier cards
    sharedSkills = groupBy skill skills
    sharedTiles = groupBy Tile.tile tiles

cardToFilepath :: SharedModel -> Card UI -> Filepath
cardToFilepath SharedModel {..} = \case
  CreatureCard Creature {tile} ->
    fromMaybe default24Filepath $ sharedTiles Map.!? tile <&> filepath
  NeutralCard _ -> error "NeutralCard not supported"
  ItemCard _ -> error "ItemCard not supported"

getCardIdentifiers :: SharedModel -> [CardIdentifier]
getCardIdentifiers SharedModel {sharedCards} = Map.keys sharedCards

getCards :: SharedModel -> [Card UI]
getCards SharedModel {sharedCards} = Map.elems sharedCards

getStdGen :: SharedModel -> StdGen
getStdGen SharedModel {sharedStdGen} = sharedStdGen

withStdGen :: SharedModel -> StdGen -> SharedModel
withStdGen shared stdgen = shared {sharedStdGen = stdgen}

-- | An instance of 'SharedModel' that is fine for debugging. Don't use
-- it in production!
unsafeGet :: SharedModel
unsafeGet =
  case loadJson of
    Left err -> error err
    Right (cards, skills, tiles) -> create cards skills tiles $ mkStdGen 42

identToCard :: SharedModel -> CardIdentifier -> Maybe (Card UI)
identToCard SharedModel {sharedCards} cid = sharedCards Map.!? cid

-- Could type level computations make this function superseded
-- by identToCard? I mean if you pass a 'IDC' CardIdentifier to identToCard,
-- you're sure to get a Just (CreatureCard _) value
-- (Just (NeutralCard _) is impossible)
idToCreature :: SharedModel -> CreatureID -> Maybe (Creature UI)
idToCreature SharedModel {sharedCards} cid =
  sharedCards Map.!? IDC cid >>= cardToCreature

identToNeutral :: SharedModel -> Neutral -> Maybe (NeutralObject UI)
identToNeutral SharedModel {sharedCards} n =
  sharedCards Map.!? IDN n >>= cardToNeutralObject

liftCard :: SharedModel -> Card Core -> Maybe (Card UI)
liftCard shared = \case
  CreatureCard creature -> CreatureCard <$> liftCreature shared creature
  NeutralCard n -> NeutralCard <$> liftNeutralObject shared n
  ItemCard i -> Just $ ItemCard i

liftNeutralObject :: SharedModel -> NeutralObject Core -> Maybe (NeutralObject UI)
liftNeutralObject shared no =
  identToNeutral shared n
  where
    IDN n = neutralToIdentifier no

liftCreature :: SharedModel -> Creature Core -> Maybe (Creature UI)
liftCreature shared creature =
  idToCreature shared cid
  where
    IDC cid = creatureToIdentifier creature

liftSkill :: SharedModel -> Skill -> SkillUI
liftSkill SharedModel {sharedSkills} skill =
  fromMaybe
    default_
    $ find (\SkillUI {skill = sk} -> sk == skill) sharedSkills
  where
    default_ = SkillUI {skill = LongReach, ..}
    skillText = show skill ++ " not found!"
    skillTitle = skillText

shuffle :: SharedModel -> [a] -> (SharedModel, [a])
shuffle shared@SharedModel {sharedStdGen} l =
  (shared {sharedStdGen = stdgen'}, l')
  where
    (l', stdgen') = shuffleM l & flip runRandT sharedStdGen & runIdentity

tileToFilepath :: SharedModel -> Tile -> Filepath
tileToFilepath SharedModel {sharedTiles} tile =
  case find (\TileUI {tile = t} -> t == tile) sharedTiles of
    Nothing -> default24Filepath
    Just TileUI {Tile.filepath} -> filepath

unsafeIdentToCard :: SharedModel -> CardIdentifier -> Card UI
unsafeIdentToCard smodel ci = identToCard smodel ci & fromJust

unsafeLiftCard :: SharedModel -> Card Core -> Card UI
unsafeLiftCard s c = liftCard s c & fromJust

unsafeLiftCreature :: SharedModel -> Creature Core -> Creature UI
unsafeLiftCreature s c = liftCreature s c & fromJust
