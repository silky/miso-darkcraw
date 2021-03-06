{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Constants where

import Miso.String

assetsDir :: MisoString
assetsDir = "assets"

assetFilenameBeigeBG :: MisoString
assetFilenameBeigeBG = "24x24_0_2.png"

assetFilenameCrown :: MisoString
assetFilenameCrown = "16x16_0_1.png"

assetFilenameHeart :: MisoString
assetFilenameHeart = "16x16_0_0.png"

assetFilenameSkull :: MisoString
assetFilenameSkull = "16x16_1_1.png"

assetFilenameSword :: MisoString
assetFilenameSword = "16x16_1_0.png"

assetSlotBlue :: MisoString
assetSlotBlue = "blue_slot.png"

assetSlotRed :: MisoString
assetSlotRed = "red_slot.png"

assetsPath :: MisoString -> MisoString
assetsPath filename = assetsDir <> "/" <> filename

assetsUrl :: MisoString -> MisoString
assetsUrl filename = "url(" <> assetsPath filename <> ")"

-- | The number of vertical cells in a board
boardCellHeight :: Int
boardCellHeight = 26

-- | The number of horizontal cells in a board
boardCellWidth :: Int
boardCellWidth = 21

-- | The number of cells from the left of the board to leftmost cards
boardToLeftCardCellsOffset :: Int
boardToLeftCardCellsOffset = 5

-- | The board's width, in pixels
boardPixelWidth :: Int
boardPixelWidth = boardCellWidth * cps

-- | The board's height , in pixels
boardPixelHeight :: Int
boardPixelHeight = boardCellHeight * cps

-- | The additional attack granted by the Blow skill when available
blowAmount :: Int
blowAmount = 2

-- | The size of borders around cards
borderSize :: Int
borderSize = 3

-- | The height of the hand in the build view, in pixels
buildPixelHeight = cps * 6

-- | The number of cards to draw at the beginning of a turn
nbCardsToDraw :: Int
nbCardsToDraw = 3

gameTitle :: MisoString
gameTitle = "Pixel Card Wars"

-- | The hands's height, in pixels
handPixelHeight :: Int
handPixelHeight = 192

-- | The hands's width, in pixels
handPixelWidth :: Int
handPixelWidth = boardPixelWidth

-- | The initial number of cards in the hand
initialHandSize :: Int
initialHandSize = 7 -- For testing, FIXME @smelc revert me to 5

-- The yellowish background of a card
cardBackground :: MisoString -> MisoString
cardBackground team = "card-bg-" <> team <> ".png"

-- | A card's height, in cells
cardCellHeight :: Int
cardCellHeight = 4

-- | A card's width, in cells
cardCellWidth :: Int
cardCellWidth = 3

-- | A card's height, in pixels
cardPixelHeight :: Int
cardPixelHeight = cellPixelSize * cardCellHeight

-- | A card's width, in pixels
cardPixelWidth :: Int
cardPixelWidth = cellPixelSize * cardCellWidth

-- | The horizontal spacing between two cards, in cells
cardHCellGap :: Int
cardHCellGap = 1

-- | The vertical spacing between two cards, in cells
cardVCellGap :: Int
cardVCellGap = 1

-- | The URL of the game on itch.io
itchURL :: MisoString
itchURL = "https://hgames.itch.io/pixel-card-wars"

-- | The vertical spacing between the two teams, in cells
teamsVCellGap :: Int
teamsVCellGap = 2

-- | The turn widget height, in pixels
turnPixelHeight :: Int
turnPixelHeight = 6 * cellPixelSize

-- | The turn widget width, in pixels
turnPixelWidth :: Int
turnPixelWidth = 4 * cellPixelSize

-- | The size of a cell, in pixels
cellPixelSize :: Int
cellPixelSize = 24

-- | The size of a cell, in pixels
cps :: Int
cps = cellPixelSize

-- | The default font size in pixels. It's actually the same as the default of browsers.
-- | See e.g. https://www.w3schools.com/css/css_font_size.asp
defaultFontSize :: Int
defaultFontSize = 16

-- | The size of smaller individual assets (items, heart, etc.)
seize :: Int
seize = 16

-- | The size of subtitles
subtitleFontSize :: Int
subtitleFontSize = (cps + titleFontSize) `div` 3

-- | The size of titles
titleFontSize :: Int
titleFontSize = cps + (cps `div` 2)

-- | The width of the backgrounds of non-board view, in cells
lobbiesCellWidth :: Int
lobbiesCellWidth = boardCellWidth

-- | The width of the backgrounds of non-board view, in pixels
lobbiesPixelWidth :: Int
lobbiesPixelWidth = lobbiesCellWidth * cps

-- | The height of the backgrounds of non-board view, in pixels
lobbiesPixelHeight :: Int
lobbiesPixelHeight = 624

beigeHTML :: MisoString
beigeHTML = "#F6E795"

greenHTML :: MisoString
greenHTML = "#00FF00"

greenRGB :: (Int, Int, Int)
greenRGB = (0, 255, 0)

redHTML :: MisoString
redHTML = "#FF0000"

yellowHTML :: MisoString
yellowHTML = "#FFFF00"

yellowRGB :: (Int, Int, Int)
yellowRGB = (255, 255, 0)
