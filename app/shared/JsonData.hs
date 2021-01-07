{- ORMOLU_DISABLE -}

{-# LANGUAGE OverloadedStrings #-}

-- This module is generated by `./th/main.py --install` (whenever
-- th/data.json changes). Hence do not edit this file!
module JsonData where

import Data.Text

jsonData :: Text
jsonData = "{\n\
         \  \"creatures\": [\n\
         \    { \"id\": { \"name\":\"spearman\",  \"team\":\"human\" },  \"title\":\"Spearman\",  \"hp\":2, \"attack\":1, \"moral\":3, \"victory_points\":2, \"tile\":\"HumanSpearman\",  \"skills\":[\"Discipline\", \"LongReach\"] },\n\
         \    { \"id\": { \"name\":\"swordsman\", \"team\":\"human\" },  \"title\":\"swordsman\", \"hp\":2, \"attack\":1, \"moral\":3, \"victory_points\":2, \"tile\":\"HumanSwordsman\", \"skills\":[\"Discipline\"] },\n\
         \    { \"id\": { \"name\":\"archer\",    \"team\":\"human\" },  \"title\":\"archer\",    \"hp\":2, \"attack\":1, \"moral\":3, \"victory_points\":2, \"tile\":\"HumanArcher\",    \"skills\":[\"Ranged\"] },\n\
         \    { \"id\": { \"name\":\"general\",   \"team\":\"human\" },  \"title\":\"general\",   \"hp\":3, \"attack\":3, \"moral\":5, \"victory_points\":5, \"tile\":\"HumanGeneral\",   \"skills\":[\"Unique\"] },\n\
         \    { \"id\": { \"name\":\"knight\",    \"team\":\"human\" },  \"title\":\"Knight\",    \"hp\":2, \"attack\":2, \"moral\":4, \"victory_points\":5, \"tile\":\"HumanKnight\",    \"skills\":[\"Blow\"] },\n\
         \    { \"id\": { \"name\":\"skeleton\",  \"team\":\"undead\" }, \"title\":\"skeleton\",  \"hp\":1, \"attack\":1, \"victory_points\":1, \"tile\":\"UndeadSkeleton\" },\n\
         \    { \"id\": { \"name\":\"vampire\",   \"team\":\"undead\" }, \"title\":\"vampire\",   \"hp\":3, \"attack\":3, \"victory_points\":5, \"tile\":\"UndeadVampire\"  },\n\
         \    { \"id\": { \"name\":\"archer\",    \"team\":\"undead\" }, \"title\":\"archer\",    \"hp\":1, \"attack\":1, \"victory_points\":1, \"tile\":\"UndeadArcher\", \"skills\":[\"Ranged\"]   },\n\
         \    { \"id\": { \"name\":\"mummy\",     \"team\":\"undead\" }, \"title\":\"mummy\",     \"hp\":5, \"attack\":2, \"victory_points\":2, \"tile\":\"UndeadMummy\"    },\n\
         \    { \"id\": { \"name\":\"necromancer\", \"team\":\"undead\" }, \"title\":\"necromancer\", \"hp\":1, \"attack\":0, \"victory_points\":0, \"tile\":\"UndeadNecromancer\", \"skills\":[\"DrawCard\"] },\n\
         \    { \"id\": { \"name\":\"warrior\",   \"team\":\"undead\" }, \"title\":\"warrior\",   \"hp\":1, \"attack\":2, \"victory_points\":0, \"tile\":\"UndeadWarrior\"  },\n\
         \    { \"id\": { \"name\":\"ghost\",     \"team\":\"undead\" }, \"title\":\"ghost\",     \"hp\":1, \"attack\":0, \"victory_points\":0, \"tile\":\"UndeadGhost\"    },\n\
         \    { \"id\": { \"name\":\"shade\",     \"team\":\"undead\" }, \"title\":\"shade\",     \"hp\":1, \"attack\":2, \"victory_points\":0, \"tile\":\"UndeadShade\"    }\n\
         \  ],\n\
         \  \"neutral\": [\n\
         \    { \"name\":\"infernalhaste\", \"title\":\"Haste\",  \"tile\":\"SkullRedEyes\", \"text\":\"All creatures attack now!\", \"teams\": [\"undead\"] },\n\
         \    { \"name\":\"health\",        \"title\":\"Health\", \"tile\":\"RedPotion\",    \"text\":\"Gain 1 HP\"                , \"teams\": [\"human\"]  },\n\
         \    { \"name\":\"life\",          \"title\":\"Life\",   \"tile\":\"GreenPotion\",  \"text\":\"Gain 3 HP\"                , \"teams\": [\"human\"]  }\n\
         \  ],\n\
         \  \"items\": [\n\
         \    { \"name\":\"crown\", \"title\":\"Crown\", \"tile\":\"crown\", \"text\":\"Leadership +2\" }\n\
         \  ],\n\
         \  \"tiles\": [\n\
         \    { \"tile\":\"BlackAppears0\", \"filepath\": { \"root\": \"24x24\", \"x\":0, \"y\":6 } },\n\
         \    { \"tile\":\"BlackAppears1\", \"filepath\": { \"root\": \"24x24\", \"x\":1, \"y\":6 } },\n\
         \    { \"tile\":\"BlackAppears2\", \"filepath\": { \"root\": \"24x24\", \"x\":2, \"y\":6 } },\n\
         \    { \"tile\":\"BlackAppears3\", \"filepath\": { \"root\": \"24x24\", \"x\":3, \"y\":6 } },\n\
         \    { \"tile\":\"Blood0\",        \"filepath\": { \"root\": \"24x24\", \"x\":0, \"y\":7 } },\n\
         \    { \"tile\":\"Blood1\",        \"filepath\": { \"root\": \"24x24\", \"x\":1, \"y\":7 } },\n\
         \    { \"tile\":\"Blood2\",        \"filepath\": { \"root\": \"24x24\", \"x\":2, \"y\":7 } },\n\
         \    { \"tile\":\"Blood3\",        \"filepath\": { \"root\": \"24x24\", \"x\":3, \"y\":7 } },\n\
         \    { \"tile\":\"Bones0\",        \"filepath\": { \"root\": \"24x24\", \"x\":0, \"y\":8 } },\n\
         \    { \"tile\":\"Bones1\",        \"filepath\": { \"root\": \"24x24\", \"x\":1, \"y\":8 } },\n\
         \    { \"tile\":\"Bones2\",        \"filepath\": { \"root\": \"24x24\", \"x\":2, \"y\":8 } },\n\
         \    { \"tile\":\"Bones3\",        \"filepath\": { \"root\": \"24x24\", \"x\":3, \"y\":8 } },\n\
         \    { \"tile\":\"Bones4\",        \"filepath\": { \"root\": \"24x24\", \"x\":4, \"y\":8 } },\n\
         \    { \"tile\":\"Bones5\",        \"filepath\": { \"root\": \"24x24\", \"x\":5, \"y\":8 } },\n\
         \    { \"tile\":\"Bones6\",        \"filepath\": { \"root\": \"24x24\", \"x\":6, \"y\":8 } },\n\
         \    { \"tile\":\"Heart\",         \"filepath\": { \"root\": \"16x16\", \"x\":0, \"y\":0 } },\n\
         \    { \"tile\":\"HumanSwordsman\",\"filepath\": { \"root\": \"24x24\", \"x\":0, \"y\":0 } },\n\
         \    { \"tile\":\"HumanSpearman\", \"filepath\": { \"root\": \"24x24\", \"x\":1, \"y\":0 } },\n\
         \    { \"tile\":\"HumanArcher\",   \"filepath\": { \"root\": \"24x24\", \"x\":2, \"y\":0 } },\n\
         \    { \"tile\":\"HumanGeneral\",  \"filepath\": { \"root\": \"24x24\", \"x\":3, \"y\":0 } },\n\
         \    { \"tile\":\"HumanKnight\",   \"filepath\": { \"root\": \"24x24\", \"x\":4, \"y\":0 } },\n\
         \    { \"tile\":\"SkullRedEyes\",  \"filepath\": { \"root\": \"16x16\", \"x\":1, \"y\":2 } },\n\
         \    { \"tile\":\"Sword1\",        \"filepath\": { \"root\": \"16x16\", \"x\":1, \"y\":0 } },\n\
         \    { \"tile\":\"Sword2\",        \"filepath\": { \"root\": \"16x16\", \"x\":3, \"y\":1 } },\n\
         \    { \"tile\":\"RedPotion\",     \"filepath\": { \"root\": \"16x16\", \"x\":2, \"y\":0 } },\n\
         \    { \"tile\":\"GreenPotion\",   \"filepath\": { \"root\": \"16x16\", \"x\":3, \"y\":0 } },\n\
         \    { \"tile\":\"Crown\",         \"filepath\": { \"root\": \"16x16\", \"x\":0, \"y\":1 } },\n\
         \    { \"tile\":\"Loupe\",         \"filepath\": { \"root\": \"16x16\", \"x\":4, \"y\":1 } },\n\
         \    { \"tile\":\"UndeadSkeleton\",\"filepath\": { \"root\": \"24x24\", \"x\":0, \"y\":3 } },\n\
         \    { \"tile\":\"UndeadVampire\", \"filepath\": { \"root\": \"24x24\", \"x\":1, \"y\":3 } },\n\
         \    { \"tile\":\"UndeadArcher\",  \"filepath\": { \"root\": \"24x24\", \"x\":2, \"y\":3 } },\n\
         \    { \"tile\":\"UndeadMummy\",   \"filepath\": { \"root\": \"24x24\", \"x\":3, \"y\":3 } },\n\
         \    { \"tile\":\"UndeadWarrior\", \"filepath\": { \"root\": \"24x24\", \"x\":4, \"y\":3 } },\n\
         \    { \"tile\":\"UndeadGhost\",   \"filepath\": { \"root\": \"24x24\", \"x\":5, \"y\":3 } },\n\
         \    { \"tile\":\"UndeadShade\",   \"filepath\": { \"root\": \"24x24\", \"x\":6, \"y\":3 } },\n\
         \    { \"tile\":\"UndeadNecromancer\", \"filepath\": { \"root\": \"24x24\", \"x\":7, \"y\":3 } },\n\
         \    { \"tile\":\"WhiteAppears0\", \"filepath\": { \"root\": \"24x24\", \"x\":0, \"y\":5 } },\n\
         \    { \"tile\":\"WhiteAppears1\", \"filepath\": { \"root\": \"24x24\", \"x\":1, \"y\":5 } },\n\
         \    { \"tile\":\"WhiteAppears2\", \"filepath\": { \"root\": \"24x24\", \"x\":2, \"y\":5 } },\n\
         \    { \"tile\":\"WhiteAppears3\", \"filepath\": { \"root\": \"24x24\", \"x\":3, \"y\":5 } },\n\
         \    { \"tile\":\"WhiteAppears4\", \"filepath\": { \"root\": \"24x24\", \"x\":4, \"y\":5 } }\n\
         \  ],\n\
         \  \"skills\": [\n\
         \    { \"skill\":\"Blow\",        \"title\":\"Blow\",       \"text\":\"+2 :crossed_swords: during first turn\"},\n\
         \    { \"skill\":\"Discipline\",  \"title\":\"Discipline\", \"text\":\"Upon arrival, neighbors with discipline get +1 :heart: and +1 :crossed_swords:\"},\n\
         \    { \"skill\":\"DrawCard\",    \"title\":\"Librarian\",  \"text\":\"Draw an additional card at beginning of your turn\"},\n\
         \    { \"skill\":\"LongReach\",   \"title\":\"Long reach\", \"text\":\"Hits 2 cells away when in the back line\"},\n\
         \    { \"skill\":\"Ranged\",      \"title\":\"Ranged\",     \"text\":\"Hits any enemy in its column\"},\n\
         \    { \"skill\":\"Unique\",      \"title\":\"Unique\",     \"text\":\"Never goes back to the stack\"}\n\
         \  ]\n\
         \}\n"
