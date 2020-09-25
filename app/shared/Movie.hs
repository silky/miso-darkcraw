{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Module containing concrete lists of scenes for lobbies.
-- A movie is a list of scenes, built from the smart operators
-- from 'Cinema'.
module Movie (welcomeMovie) where

import Card (CreatureID (..), CreatureKind (..), Team (..))
import Cinema
import Constants
import Tile

cid0 :: CreatureID
cid0 = CreatureID General Human

w0 :: Element
w0 = Element 0

cid01 :: CreatureID
cid01 = CreatureID Spearman Human

w01 :: Element
w01 = Element 2

cid02 :: CreatureID
cid02 = CreatureID Archer Human

w02 :: Element
w02 = Element 3

allw0right :: Frame ActorChange
allw0right = w0 =: right <> w01 =: right <> w02 =: right

cid1 :: CreatureID
cid1 = CreatureID Vampire Undead

w1 :: Element
w1 = Element 1

cid10 :: CreatureID
cid10 = CreatureID Skeleton Undead

w10 :: Element
w10 = Element 4

whiteAppears :: Int -> Int -> Int -> [Frame ActorChange]
whiteAppears i x y =
  map f [WhiteAppears0, WhiteAppears1, WhiteAppears2, WhiteAppears3, WhiteAppears4]
  where
    f tile = Element i =: at (tileSprite tile) x y

welcomeGhostMovie1 :: Scene ActorChange
welcomeGhostMovie1 =
  mconcat
    [ while 9 $ g =: at' (creatureSprite cid) ToRight 1 0,
      while 8 $ g =: down,
      while 15 $ g =: right,
      while 8 $ g =: down,
      while 12 $ g =: right,
      while 7 $ g =: down,
      while 8 $ g =: right,
      while 10 $ g =: down,
      while 20 $ g =: down,
      while 15 $ g =: right,
      while 8 $ g =: right,
      while 15 $ g =: right
    ]
  where
    cid = CreatureID Ghost Undead
    g = Element 5

welcomeGhostMovie2 :: Scene ActorChange
welcomeGhostMovie2 =
  mconcat
    [ while 15 $ g =: at (creatureSprite cid) (lobbiesCellWidth - 3) 0,
      while 10 $ g =: down,
      while 12 $ g =: left,
      while 18 $ g =: down,
      while 12 $ g =: left,
      while 7 $ g =: down,
      while 8 $ g =: left,
      while 10 $ g =: left,
      while 10 $ g =: up,
      while 14 $ g =: up,
      while 20 $ g =: up,
      while 15 $ g =: left,
      while 8 $ g =: right,
      while 15 $ g =: right
    ]
  where
    cid = CreatureID Ghost Undead
    g = Element 6

welcomeFightMovie :: Scene ActorChange
welcomeFightMovie =
  foldMap
    (while 10) -- 10 tenth of seconds: 1 second
    [ w0 =: at' (creatureSprite cid0) ToRight 0 15 <> w1 =: at (creatureSprite cid1) (lobbiesCellWidth - 1) 11,
      w0 =: right <> w1 =: left,
      w0 =: right <> w0 =: tell "Come on guys!" <> w1 =: left,
      w0 =: right <> w0 =: shutup <> w01 =: at' (creatureSprite cid01) ToRight 0 15 <> w1 =: tell "Fresh meat!",
      w0 =: right <> w01 =: right <> w02 =: at' (creatureSprite cid02) ToRight 0 15,
      w1 =: shutup,
      w0 =: right <> w01 =: right <> w01 =: up <> w02 =: right,
      allw0right,
      allw0right,
      allw0right,
      allw0right <> w1 =: left,
      allw0right <> w1 =: left,
      allw0right <> w1 =: left,
      allw0right <> w1 =: left,
      w0 =: up <> w01 =: right <> w02 =: right,
      w0 =: up <> w01 =: right <> w01 =: up <> w02 =: right <> w02 =: up
    ]
    <> while 5 (w1 =: tell "iugp9b7")
    <> while 1 (w1 =: shutup)
    <> foldMap (while 2) (whiteAppears 7 12 11)
    <> while 10 (w10 =: at (creatureSprite cid10) 12 11)

welcomeMovie :: Scene ActorChange
welcomeMovie = welcomeGhostMovie1 ||| welcomeGhostMovie2 ||| welcomeFightMovie
