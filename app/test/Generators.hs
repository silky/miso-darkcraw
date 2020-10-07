{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Generators where

import Card
import Cinema
import GHC.Generics
import Generic.Random
import Test.QuickCheck
import Tile

newtype SceneAst = SceneAst [SceneAction]
  deriving (Eq, Show, Generic)

data SceneAction
  = NewActor
  | During Int FrameDiffAst
  | SceneAst :|||: SceneAst
  | Fork SceneAst
  deriving (Eq, Show, Generic)

data FrameDiffAction = FrameDiffAction Int ActorChange
  deriving (Eq, Show, Generic)

newtype FrameDiffAst = FrameDiffAst [FrameDiffAction]
  deriving (Eq, Show, Generic)

instance Arbitrary CreatureKind where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary Team where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary CreatureID where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary Tile where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary SpriteChange where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary DirectionChange where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary TellingChange where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary ActorChange where
  arbitrary = genericArbitraryU
  shrink = genericShrink

genFrameDiffAction :: Int -> Gen FrameDiffAction
genFrameDiffAction n = FrameDiffAction <$> choose (0, n -1) <*> arbitrary

instance Arbitrary FrameDiffAction where
  arbitrary = error "Arbitrary instance only exists to provide shrink"
  shrink = genericShrink

genFrameDiffAst :: Int -> Gen FrameDiffAst
genFrameDiffAst 0 = return (FrameDiffAst [])
genFrameDiffAst n = FrameDiffAst <$> listOf (genFrameDiffAction n)

instance Arbitrary FrameDiffAst where
  arbitrary = error "Arbitrary instance only exists to provide shrink"
  shrink = genericShrink

genSceneAst :: Int -> Gen SceneAst
genSceneAst i = do
  n <- getSize
  k <- choose (0, n)
  SceneAst <$> go k i
  where
    go :: Int -> Int -> Gen [SceneAction]
    go 0 _ = return []
    go n i =
      oneof
        [ (NewActor :) <$> go (n -1) (i + 1),
          (:)
            <$> (During <$> (getPositive <$> arbitrary) <*> genFrameDiffAst i)
            <*> go (n -1) i,
          (:)
            <$> ((:|||:) <$> subSequence i <*> subSequence i)
            <*> go (n -1) i,
          (:)
            <$> (Fork <$> subSequence i)
            <*> go (n -1) i
        ]
      where
        subSequence :: Int -> Gen SceneAst
        subSequence i = scale (`div` 2) (genSceneAst i)

instance Arbitrary SceneAction where
  arbitrary = error "Arbitrary instance only exists to provide shrink"
  shrink = genericShrink

deleteOrphanInstructions :: SceneAst -> SceneAst
deleteOrphanInstructions (SceneAst actions) = SceneAst (go 0 actions)
  where
    go i (NewActor : k) =
      NewActor : go (i + 1) k
    go i (During duration (FrameDiffAst frameActions) : k) =
      During duration (FrameDiffAst (concatMap (prune i) frameActions)) : go i k
    go i ((SceneAst actions1 :|||: SceneAst actions2) : k) =
      (SceneAst (go i actions1) :|||: SceneAst (go i actions2)) : go i k
    go i (Fork (SceneAst actions1) : k) =
      Fork (SceneAst (go i actions1)) : go i k
    go _ [] = []
    prune i frame@(FrameDiffAction j _)
      | j < i = [frame]
      | otherwise = []

instance Arbitrary SceneAst where
  arbitrary = genSceneAst 0

  -- This is a bit hacky: genericShrink will generate instructions of the form
  -- actor += change, where actor has not been declared yet, we simply delete
  -- these instructions. A better solution would be to write a manual shrink
  -- that only produces well-formed scenes.
  shrink = map deleteOrphanInstructions . genericShrink

astToFrameDiff :: [Element] -> FrameDiffAst -> FrameDiff ()
astToFrameDiff actors (FrameDiffAst actions) = mapM_ interpret actions
  where
    interpret (FrameDiffAction i actorChange) = (actors !! i) += actorChange

astToScene :: SceneAst -> Scene ()
astToScene (SceneAst actions) = go [] actions
  where
    go _ [] = return ()
    go actors (NewActor : k) = do
      actor <- newActor
      -- actors without sprites won't render
      during 0 (dress actor (tileSprite Heart))
      go (actor : actors) k
    go actors (During duration frameDiffAst : k) = do
      during duration (astToFrameDiff actors frameDiffAst)
      go actors k
    go actors ((SceneAst actions1 :|||: SceneAst actions2) : k) = do
      go actors actions1 ||| go actors actions2
      go actors k
    go actors (Fork (SceneAst actions1) : k) = do
      fork (go actors actions1)
      go actors k
