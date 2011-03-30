-- HsDungeon
-- Andrew Pennebaker
-- 29 Mar 2011

module Dungeon where

import Data.List (intercalate)

import Random (randomRIO)

pick :: [a] -> IO a
pick xs = do
	r <- randomRIO (0, (length xs - 1))
	return (xs !! r)

data StairDirection = UpStair | DownStair

data Stair = Stair {
		stairDirection :: StairDirection,
		stairLocked :: Bool
	}

data Door = Door {
		doorClosed :: Bool,
		doorLocked :: Bool
	}

data TileType
	= TileStair Stair
	| TileEmpty
	| TileFloor
	| TileWall
	| TileDoor Door

instance Show TileType where
	show (TileStair stair) = case stairDirection stair of
		UpStair -> "<"
		DownStair -> ">"

	show TileEmpty = " "
	show TileFloor = "."
	show TileWall = "#"

	show (TileDoor door) = case doorClosed door of
		True -> "+"
		False -> "/"

data Item = Item {
		itemName :: String,
		itemShow :: String,
		itemAction :: Game -> IO Game
	}

data Tile = Tile {
		tileType :: TileType,
		tileContents :: [Item],
		tileHidden :: Bool
	}

instance Show Tile where
	show t = case tileHidden t of
		True -> " "
		False -> case null $ tileContents t of
			False -> itemShow $ head $ tileContents t
			True -> show $ tileType t

defaultFloor :: Tile
defaultFloor = Tile { tileType = TileFloor, tileContents = [], tileHidden = False }

data Level = Level [[Tile]]

instance Show Level where
	show (Level level) = intercalate "\n" $ map (intercalate "" . map show) level

type Dungeon = [Level]

type Pos = (Int, Int)

data Player = Player {
		pos :: Pos
	} deriving (Eq)

instance Show Player where
	show player = "@"

data Game = Game {
		dungeon :: Dungeon,
		player :: Player
	}

dist :: Pos -> Pos -> Int
dist p1 p2 = floor $ sqrt $ fromIntegral (dx * dx + dy * dy)
	where
		dx = fst p1 - fst p2
		dy = snd p1 - snd p2

separate :: Level -> Int -> Pos -> Pos -> IO Pos
separate (Level lev) m p1 p2 = case dist p1 p2 < m of
	False -> return p2
	True -> do
		let (rows, cols) = (length lev, length (lev !! 0))

		x <- pick [0 .. cols - 1]
		y <- pick [0 .. rows - 1]

		p2' <- separate (Level lev) m p1 (x, y)

		return p2'

-- putTile :: Level -> Tile -> Pos -> Level
-- putTile (Level lev) t p = -- ...

addStairs :: Level -> Stair -> Stair -> IO Level
addStairs (Level lev) s1 s2 = do
	let (rows, cols) = (length lev, length (lev !! 0))

	p1X <- pick [0 .. cols - 1]
	p1Y <- pick [0 .. rows - 1]

	p2X <- pick [0 .. cols - 1]
	p2Y <- pick [0 .. rows - 1]

	let p1 = (p1X, p1Y)
	let p2 = (p2X, p2Y)

	p2' <- separate (Level lev) (cols `div` 3) p1 p2

	let lev' = [[defaultFloor]]

	-- ...

	return $ Level lev'

initGame :: IO Game
initGame = do
	let rows = 24
	let cols = 80
	let levels = 26

	let d = replicate levels $ Level $ replicate rows $ replicate cols defaultFloor
	let p = Player { pos = (0, 0) }

	return Game { dungeon = d, player = p }