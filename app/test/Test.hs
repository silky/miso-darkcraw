{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import AI
import Board
import Card
import Cinema
import Constants
import Control.Lens hiding (at, (+=))
import Control.Lens.Extras
import Control.Monad
import Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Set as Set
import qualified Game (Event (..), PolyResult (..), Target (..), attackOrder, placePrimeToHandIndex, play, playAll)
import Generators
import Json
import qualified Match
import Movie
import Pretty
import SceneEquivalence
import SharedModel (SharedModel)
import qualified SharedModel
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Turn

creatureSum :: [Creature p] -> (Creature p -> Int) -> Int
creatureSum cards getter = sum (map getter cards)

getAllDecks :: [Card UI] -> [[Card Core]]
getAllDecks cards = [teamDeck cards t | t <- allTeams]

testBalance :: [Card UI] -> Int
testBalance cards =
  undefined
  where
    _humanDeck = teamDeck cards Human
    _undeadDeck = teamDeck cards Undead

-- XXX smelc group AI tests together

-- | Tests that the AI treats 'Ranged' correctly.
testAIRanged :: SharedModel -> Turn -> Board Core
testAIRanged shared turn =
  case Game.playAll shared board events of
    Left _ -> error "AI failed"
    Right (Game.Result board' () _) -> board'
  where
    (t, teams) = (Undead, Teams Undead Undead)
    archer = IDC $ CreatureID Archer t
    board = boardAddToHand (emptyBoard teams) (turnToPlayerSpot turn) archer
    events = AI.play SharedModel.unsafeGet board turn

-- This test fails for now. It acts as a witness of the bug that playing
-- the last card of the hand prints the following in the console:
-- "Invalid hand index: 4. Hand has 4 card(s)."
-- FIXME the test fails because of too many discards at the moment!
testPlayLastHandCard :: SharedModel -> SpecWith ()
testPlayLastHandCard shared =
  describe
    "Play the last card of the hand"
    $ prop "doesn't yield an error" $
      \(Pretty board, Pretty turn) ->
        let event = AI.placeCards shared board turn & keepLast board turn
         in isJust event
              ==> Game.play shared board (fromJust event) `shouldSatisfy` isRight
  where
    isRight (Right _) = True
    isRight (Left _) = False
    keepLast (board :: Board 'Core) turn events = go events
      where
        pSpot = turnToPlayerSpot turn
        lastIdx = (boardToHand board pSpot & length) - 1
        go [] = Nothing
        go (e@Game.Place' {} : tl) =
          case Game.placePrimeToHandIndex board e of
            Nothing -> error "Unexpected case"
            Just (HandIndex i) | i == lastIdx -> Just e
            _ -> go tl
        go (e : _) = error $ "Only Place' events should have been generated, but received: " ++ show e

testNeighbors :: SpecWith ()
testNeighbors =
  describe "Neighbors doesn't return doublons" $
    prop "forall n::Neighborhood cSpot::CardSpot, neighbors n cSpot doesn't have doublons" $
      \(n, cSpot) ->
        let res = neighbors n cSpot
         in (res & sort) `shouldBe` (res & Set.fromList & Set.toList & sort)

testSceneInvariant :: Int -> TimedFrame -> Spec
testSceneInvariant idx TimedFrame {..} =
  -- Check no two Element are in the same spot
  it ("Scene Change invariant " ++ show idx) $
    values `shouldBe` values'
  where
    values = unFrame frame & Map.elems & map actorState & filter isCreature & map toPos & List.sort
    values' = values & Set.fromList & Set.toList & List.sort
    isCreature ActorState {sprite = Just s} | spriteToKind s == CreatureKind = True
    isCreature _ = False
    toPos ActorState {x, y} = (x, y)

testScenesInvariant :: String -> Scene () -> Spec
testScenesInvariant name scene =
  describe ("Scene ActorChange " ++ name) $
    zipWithM_ testSceneInvariant [0 ..] (render scene)

testForkScene :: Spec
testForkScene =
  describe "Cinema.fork" $
    it "interleaves events as expected" $
      actualScene ~= expectedScene
  where
    scene1 :: Element -> Element -> Scene ()
    scene1 w0 w1 = do
      during 1 (up w0)
      fork $ do
        during 1 (down w1)
        during 1 (down w1)
        during 1 (down w1)
      during 1 (up w0)
    scene2 :: Element -> Scene ()
    scene2 w0 =
      during 1 (up w0)
    actualScene :: Scene ()
    actualScene = do
      w0 <- newHiddenActor "w0"
      w1 <- newHiddenActor "w1"
      scene1 w0 w1
      scene2 w0
    expectedScene :: Scene ()
    expectedScene = do
      w0 <- newHiddenActor "w0"
      w1 <- newHiddenActor "w1"
      during 1 (up w0)
      during 1 (do up w0; down w1)
      during 1 (do up w0; down w1)
      during 1 (down w1)

testParallelSceneComposition :: Spec
testParallelSceneComposition =
  describe "Cinema.|||" $
    it "interleaves events in the expected order" $
      actualMergedScene ~= expectedMergedScene
  where
    scene1 :: Element -> Scene ()
    scene1 w0 = do
      during 1 (w0 & moveTo 0 0)
      during 3 (right w0)
      during 1 (left w0)
    scene2 :: Element -> Scene ()
    scene2 w1 = do
      during 2 (w1 & moveTo 1 1)
      during 4 (right w1)
    actualMergedScene :: Scene ()
    actualMergedScene = do
      w0 <- newHiddenActor "w0"
      w1 <- newHiddenActor "w1"
      scene1 w0 ||| scene2 w1
    expectedMergedScene :: Scene ()
    expectedMergedScene = do
      w0 <- newHiddenActor "w0"
      w1 <- newHiddenActor "w1"
      during 1 (do w0 & moveTo 0 0; w1 & moveTo 1 1)
      during 1 (right w0)
      during 2 (right w1)
      during 2 (left w0)

{- HLINT ignore testSceneReturn -}
testSceneReturn :: SpecWith ()
testSceneReturn =
  modifyMaxSize (const 35) $
    describe "Scene.return" $ do
      prop "left neutral for >>" $
        \(astToScene -> scene) ->
          (return () >> scene) ~= scene
      prop "right neutral for >>" $
        \(astToScene -> scene) ->
          (scene >> return ()) ~= scene
      prop "left neutral for |||" $
        \(astToScene -> scene) ->
          (return () ||| scene) ~= scene
      prop "right neutral for |||" $
        \(astToScene -> scene) ->
          (scene ||| return ()) ~= scene

testGetActorState =
  describe "getActorState" $
    it "should read the state of the actor" $
      actualScene ~= expectedScene
  where
    skeleton = creatureSprite $ CreatureID Skeleton Undead
    actualScene = do
      a1 <- newActorAt "a1" skeleton 1 2
      a1x <- a1 `dot` x
      a1y <- a1 `dot` y
      newActorAt "a2" skeleton a1x a1y
      wait 1
    expectedScene = do
      newActorAt "a1" skeleton 1 2
      newActorAt "a2" skeleton 1 2
      wait 1

testAIPlace shared =
  describe "AI.placeCards" $ do
    prop "placeCards returns events whose spots differ" $
      \board turn -> allDiff $ AI.placeCards shared board turn
    prop "placeCards return events that commute (modulo Discipline)" $
      \(Pretty board) (Pretty turn) ->
        let events = AI.placeCards shared board turn & filter (not . hasDiscipline)
         in (length events >= 2)
              ==> forAll (Test.QuickCheck.elements (permutations events))
              $ \events' ->
                Pretty (ignoreErrMsg (Game.playAll shared board events)) `shouldBe` Pretty (ignoreErrMsg (Game.playAll shared board events'))
  where
    ignoreErrMsg (Left _) = Nothing
    ignoreErrMsg (Right (Game.Result board' () _)) = Just board'
    spotsDiffer (Game.Place' (Game.CardTarget pSpot1 cSpot1) _) (Game.Place' (Game.CardTarget pSpot2 cSpot2) _) =
      pSpot1 /= pSpot2 || cSpot1 /= cSpot2
    spotsDiffer _ _ = error "Only Place' events should have been generated"
    allDiff [] = True
    allDiff (event : events) = all (spotsDiffer event) events && allDiff events
    hasDiscipline (Game.Place' _ (IDC id)) =
      Discipline `elem` (SharedModel.idToCreature shared id & fromJust & skills)
    hasDiscipline (Game.Place' _ _) = False
    hasDiscipline _ = error "Only Place' events should have been generated"

{- HLINT ignore testInPlaceEffectsMonoid -}
testInPlaceEffectsMonoid =
  describe "InPlaceEffects is a well behaved Monoid" $ do
    prop "mempty <> effects == effect" $
      \(effect :: InPlaceEffects) ->
        mempty <> effect `shouldBe` effect
    prop "effects <> mempty == effect" $
      \(effect :: InPlaceEffects) ->
        effect <> mempty `shouldBe` effect
    prop "effects <> effects' == effects' <> effects" $
      \(effect :: InPlaceEffects, effect') ->
        effect <> effect' `shouldBe` effect' <> effect

testNoPlayEventNeutral shared =
  describe "Game.NoPlayEvent is neutral w.r.t Game.playAll" $
    prop "Game.playAll $ [NoPlayEvent] ++ es == Game.playAll $ es ++ [NoPlayEvent]" $
      \(board, events) ->
        let left = [Game.NoPlayEvent] ++ events
         in let right = events ++ [Game.NoPlayEvent]
             in Game.playAll shared board left `shouldBe` Game.playAll shared board right

testPlayScoreMonotonic shared =
  describe "boardScore is monotonic w.r.t. Game.play" $
    prop "forall b :: Board, let b' = Game.play b (AI.aiPlay b); score b' is better than score b" $
      \(board, turn) ->
        let (pSpot, score) = (turnToPlayerSpot turn, flip boardScore pSpot)
         in let (initialScore, events) = (score board, AI.play shared board turn)
             in let nextScore = Game.playAll shared board events & takeBoard <&> score
                 in monotonic initialScore nextScore
  where
    takeBoard (Left _) = Nothing
    takeBoard (Right (Game.Result b _ _)) = Just b
    monotonic _ Nothing = True -- Nothing to test
    monotonic i (Just j) = j <= i -- Better score is smaller score

main :: IO ()
main = hspec $ do
  let eitherCardsNTiles = loadJson
  let (cards, _, _) = eitherCardsNTiles ^?! _Right
  let allDecks = getAllDecks cards
  let allCreatures = mapMaybe cardToCreature $ concat allDecks
  describe "initial state is correct" $ do
    it "cards can be loaded from json" $
      is _Right eitherCardsNTiles -- should be the first test, others depend on it
    it "all decks are initially of the same size" $
      all (\l -> length l == length (head allDecks)) allDecks
    it "all hit points are initially > 0" $
      all (\c -> hp c > 0) allCreatures
    it "all attacks are initially >= 0" $
      all (\c -> attack c >= 0) allCreatures
    it "lobbies and board backgrounds agree in width" $
      boardPixelWidth `shouldBe` lobbiesPixelWidth
    it "lobbies and board backgrounds agree in height" $
      boardPixelHeight `shouldBe` lobbiesPixelHeight
  describe "exactly all spots are used" $
    it "attackOrder" $
      all
        (\pSpot -> length allCardsSpots == length (Game.attackOrder pSpot))
        allPlayersSpots
  let shared = SharedModel.unsafeGet
  describe "AI.hs" $
    it "AI puts Ranged creature in back line" $
      let occupiedSpots =
            boardToHoleyInPlace (testAIRanged shared initialTurn)
              & filter (\(_, _, maybeCreature) -> isJust maybeCreature)
       in all (\(_, cSpot, _) -> inTheBack cSpot) occupiedSpots
            && not (null occupiedSpots)
  testScenesInvariant "welcomeMovie" welcomeMovie
  testParallelSceneComposition
  describe "Cinema.|||" $
    it "does not create artificial intermediary frames" $
      (wait 1 ||| wait 2) ~= wait 2
  testForkScene
  modifyMaxSize (const 35) $
    describe "Cinema.fork" $
      prop "should behave like ||| with rest of program" $
        \(astToScene -> scene1) (astToScene -> scene2) (astToScene -> scene3) ->
          (do scene1; fork scene2; scene3) ~= (do scene1; scene2 ||| scene3)
  modifyMaxSize (const 35) $
    describe "Cinema.>>" $
      prop "should be associative" $
        \(astToScene -> scene1) (astToScene -> scene2) (astToScene -> scene3) ->
          ((scene1 >> scene2) >> scene3) ~= (scene1 >> (scene2 >> scene3))
  modifyMaxSize (const 35) $
    describe "Cinema.|||" $
      prop "should be associative" $
        \(astToScene -> scene1) (astToScene -> scene2) (astToScene -> scene3) ->
          ((scene1 ||| scene2) ||| scene3) ~= (scene1 ||| (scene2 ||| scene3))
  testSceneReturn
  testGetActorState
  testAIPlace shared
  testInPlaceEffectsMonoid
  testNoPlayEventNeutral shared
  testPlayScoreMonotonic shared
  Match.main shared
