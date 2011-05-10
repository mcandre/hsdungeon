-- HsDungeon
-- Andrew Pennebaker
-- 29 Mar 2011

module Dungeon where

import Data.Set (fromList, toList, difference)
import Data.List (intercalate)

import Random (randomRIO)

pick :: [a] -> IO a
pick xs = (randomRIO (0, length xs - 1)) >>= (return . (xs !!))

rows :: Int
rows = 24

cols :: Int
cols = 80

levels :: Int
levels = 26

data StairDirection = UpStair | DownStair deriving (Eq)

data Stair = Stair {
		stairDirection :: StairDirection,
		stairLocked :: Bool
	} deriving (Eq)

data Door = Door {
		doorClosed :: Bool,
		doorLocked :: Bool
	} deriving (Eq)

data TileType
	= TileStair Stair
	| TileEmpty
	| TileFloor
	| TileWall
	| TileDoor Door deriving (Eq)

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

data ItemFunc = ItemFunc (Game -> IO Game)

instance Eq ItemFunc where
	ItemFunc x == ItemFunc y = True

data Item = Item {
		itemName :: String,
		itemShow :: String,
		itemAction :: ItemFunc
	} deriving (Eq)

player :: Item
player = Item {
		itemName = "Player",
		itemShow = "@",
		itemAction = ItemFunc $ return . id
	}

data Tile = Tile {
		tileType :: TileType,
		tileContents :: [Item],
		tileHidden :: Bool
	} deriving (Eq)

defaultUpStair :: Tile
defaultUpStair = Tile {
		tileType = TileStair $ Stair { stairDirection = UpStair, stairLocked = False },
		tileContents = [],
		tileHidden = False
	}

defaultDownStair :: Tile
defaultDownStair = Tile {
		tileType = TileStair $ Stair { stairDirection = DownStair, stairLocked = False },
		tileContents = [],
		tileHidden = False
	}

defaultDoor :: Tile
defaultDoor = Tile {
		tileType = TileDoor $ Door { doorClosed = True, doorLocked = False },
		tileContents = [],
		tileHidden = False
	}

defaultFloor :: Tile
defaultFloor = Tile {
		tileType = TileFloor,
		tileContents = [],
		tileHidden = False
	}

defaultWall :: Tile
defaultWall = Tile {
		tileType = TileWall,
		tileContents = [],
		tileHidden = False
	}

instance Show Tile where
	show t = case tileHidden t of
		True -> " "
		False -> case null $ tileContents t of
			False -> itemShow $ head $ tileContents t
			True -> show $ tileType t

data Level = Level [[Tile]] deriving (Eq)

instance Show Level where
	show (Level level) = unlines $ map (intercalate "" . map show) level

data Pos = Pos (Int, Int) deriving (Eq, Ord, Show)

-- cartesian ordering
instance Enum Pos where
	succ (Pos (x, y))
		| y == 0 = Pos (0, x + 1)
		| otherwise = Pos (x + 1, y - 1)

	pred (Pos (x, y))
		| x == 0 = Pos (y - 1, 0)
		| otherwise = Pos (x - 1, y + 1)

	toEnum i = acc i (Pos (0, 0))
		where
			acc 0 p = p
			acc n p = acc (n - 1) (succ p)
	
	fromEnum p = dec 0 p
		where
			dec n (Pos (0, 0)) = n
			dec n p = dec (n + 1) (pred p)

getTile :: Level -> Pos -> Tile
getTile (Level lev) (Pos (x, y)) = (lev !! y) !! x

dist :: Pos -> Pos -> Int
dist (Pos p1) (Pos p2) = floor $ sqrt $ fromIntegral (dx * dx + dy * dy)
	where
		dx = fst p1 - fst p2
		dy = snd p1 - snd p2

type Dungeon = [Level]

data Game = Game {
		dungeon :: Dungeon,
		playerLevel :: Int,
		playerPos :: Pos
	} deriving (Eq)

instance Show Game where
	show g = let
			lev = (dungeon g) !! (playerLevel g)
			pPos = playerPos g
			lev' = putItem lev player pPos
		in
			show lev'

putTile :: Level -> Tile -> Pos -> Level
putTile (Level lev) t (Pos (x, y)) = let
		(rowsBefore, rowsCur:rowsAfter) = splitAt y lev
		(colsBefore, colsCur:colsAfter) = splitAt x rowsCur
	in
		Level $ rowsBefore ++ [colsBefore ++ [t] ++ colsAfter] ++ rowsAfter

putItem :: Level -> Item -> Pos -> Level
putItem (Level lev) i (Pos (x, y)) = let
		t = getTile (Level lev) (Pos (x, y))
		t' = t { tileContents = i : tileContents t }
	in
		putTile (Level lev) t' (Pos (x, y))

randomPos :: [Int] -> [Int] -> IO Pos
randomPos colBounds rowBounds= do
	x <- pick colBounds
	y <- pick rowBounds
	return $ Pos (x, y)

makeStairs :: Level -> IO (Pos, Pos)
makeStairs (Level lev) = do
	let colBounds = [0 .. cols - 1]
	let rowBounds = [0 .. rows - 1]

	Pos (x1, y1) <- randomPos colBounds rowBounds

	-- Ensure stairs do not overlap
	let colBounds' = (toList . flip difference (fromList [x1]) . fromList) colBounds
	let rowBounds' = (toList . flip difference (fromList [y1]) . fromList) rowBounds

	Pos (x2, y2) <- randomPos colBounds' rowBounds'

	return (Pos (x1, y1), Pos (x2, y2))

allPos :: [Pos]
allPos = [Pos (0,0) .. Pos (rows - 1, cols - 1)]

findTile :: (Tile -> Bool) -> Level -> Pos
findTile f (Level lev) = head $ filter (f . getTile (Level lev)) allPos

moveUp :: Game -> Game
moveUp g = let
		Pos (x, y) = playerPos g
		p' = case y > 0 of
			False -> Pos (x, y)
			True -> Pos (x, y - 1)
		g' = g { playerPos = p' }
	in
		g'

moveDown :: Game -> Game
moveDown g = let
		Pos (x, y) = playerPos g
		p' = case y < rows - 1 of
			False -> Pos (x, y)
			True -> Pos (x, y + 1)
		g' = g { playerPos = p' }
	in
		g'

moveLeft :: Game -> Game
moveLeft g = let
		Pos (x, y) = playerPos g
		p' = case x < 0 of
			False -> Pos (x, y)
			True -> Pos (x - 1, y)
		g' = g { playerPos = p' }
	in
		g'

moveRight :: Game -> Game
moveRight g = let
		Pos (x, y) = playerPos g
		p' = case x < cols - 1 of
			False -> Pos (x, y)
			True -> Pos (x + 1, y)
		g' = g { playerPos = p' }
	in
		g'

initGame :: IO Game
initGame = do
	let lev = Level $ replicate rows $ replicate cols defaultWall

	(s1Pos, s2Pos) <- makeStairs lev

	let lev' = putTile lev defaultUpStair s1Pos
	let lev'' = putTile lev' defaultDownStair s2Pos

	return Game { dungeon = [lev''], playerLevel = 0, playerPos = s1Pos }