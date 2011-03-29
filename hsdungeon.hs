-- HsDungeon
-- Andrew Pennebaker
-- 29 Mar 2011

module HsDungeon where

import Data.List (intercalate)

data StairDirection = UpStair | DownStair deriving (Eq)

data Stair = Stair {
		stairDirection :: StairDirection,
		stairLocked :: Bool
	} deriving (Eq)

data Door = Door {
		doorClosed :: Bool,
		doorLocked :: Bool
	} deriving (Eq)

data Tile
	= TileStair Stair
	| TileEmpty
	| TileFloor
	| TileWall
	| TileDoor Door deriving (Eq)

instance Show Tile where
	show (TileStair stair) = case stairDirection stair of
		UpStair -> "<"
		DownStair -> ">"

	show TileEmpty = " "
	show TileFloor = "."
	show TileWall = "#"

	show (TileDoor door) = case doorClosed door of
		True -> "+"
		False -> "/"

data Level = Level [[Tile]] deriving (Eq)

instance Show Level where
	show (Level level) = intercalate "\n" $ map (intercalate "" . map show) level

type Dungeon = [Level]

data Player = Player {
		pos :: (Int, Int),
		health :: Int
	} deriving (Eq, Show)

data Game = Game {
		dungeon :: Dungeon,
		player :: Player
	} deriving (Eq, Show)

initGame :: IO Game
initGame = do
	let d = replicate 1 $ Level $ replicate 24 $ replicate 80 TileFloor
	-- let d = replicate 26 $ Level $ replicate 24 $ replicate 80 TileFloor
	let p = Player { pos = (0, 0), health = 15 }
	return Game { dungeon = d, player = p }