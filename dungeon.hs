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
	show (Level level) = intercalate "\n" $ map (intercalate "" . map show) level

data Pos = Pos (Int, Int) deriving (Eq, Ord, Show)

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

tileAt :: Level -> Pos -> Maybe Tile
tileAt (Level lev) (Pos (x, y)) = let
		(rows, cols) = (length lev, length (lev !! 0))
	in
		case (x >= cols || y >= rows) of
			True -> Nothing
			False -> Just $ (lev !! y) !! x

dist :: Pos -> Pos -> Int
dist (Pos p1) (Pos p2) = floor $ sqrt $ fromIntegral (dx * dx + dy * dy)
	where
		dx = fst p1 - fst p2
		dy = snd p1 - snd p2

type Dungeon = [Level]

data Game = Game {
		dungeon :: Dungeon,
		playerLevel :: Int
	} deriving (Eq)

instance Show Game where
	show g = show $ (dungeon g) !! (playerLevel g)

putTile :: Level -> Tile -> Pos -> Level
putTile (Level lev) t (Pos (x, y)) = let
		(rowsBefore, rowsCur:rowsAfter) = splitAt y lev
		(colsBefore, colsCur:colsAfter) = splitAt x rowsCur
	in
		Level $ rowsBefore ++ [colsBefore ++ [t] ++ colsAfter] ++ rowsAfter

addStairs :: Level -> Tile -> Tile -> IO Level
addStairs (Level lev) s1 s2 = do
	let (rows, cols) = (length lev, length (lev !! 0))

	x1 <- pick [0 .. cols - 1]
	y1 <- pick [0 .. rows - 1]

	x2 <- pick [0 .. cols - 1]
	y2 <- pick [0 .. rows - 1]

	let p1 = Pos (x1, y1)
	let p2 = Pos (x2, y2)

	let Level lev' = putTile (Level lev) s1 p1
	let Level lev'' = putTile (Level lev') s2 p2

	return $ Level lev''

findTile :: (Maybe Tile -> Bool) -> Level -> Maybe Pos
findTile f (Level lev) = let
		(rows, cols) = (length lev, length (lev !! 0))
		matches = filter (f . tileAt (Level lev)) [Pos (0,0) .. Pos (rows - 1, cols - 1)]
	in
		case null matches of
			True -> Nothing
			False -> Just $ head matches

addPlayer :: Level -> Item -> IO Level
addPlayer (Level lev) player = do
	let (rows, cols) = (length lev, length (lev !! 0))

	putStrLn $ "Finding starting position..."

	let startPos = case findTile (== Just defaultUpStair) (Level lev) of
		Nothing -> Pos (0, 0) -- Will only occur if addStairs is not run beforehand
		Just p -> p

	putStrLn $ "Starting position: " ++ show startPos

	putStrLn $ "Accessing start tile..."

	let startTile = case tileAt (Level lev) startPos of
		Nothing -> defaultFloor -- Will only occur if addStairs is not run beforehand
		Just t -> t

	putStrLn $ "Start tile: " ++ show startTile

	putStrLn $ "Adding player to start tile..."

	let startTile' = startTile { tileContents = player : tileContents startTile}

	putStrLn $ "New start tile: " ++ show startTile'

	putStrLn $ "Reinserting start tile..."

	let lev' =  putTile (Level lev) startTile' startPos

	return lev'

initGame :: IO Game
initGame = do
	let rows = 24
	let cols = 80
	let levels = 26

	putStrLn $ "Adding walls..."

	let lev0 = Level $ replicate rows $ replicate cols defaultWall

	putStrLn $ show lev0

	putStrLn $ "Adding stairs..."

	lev0' <- addStairs lev0 defaultUpStair defaultDownStair

	putStrLn $ show lev0'

	putStrLn $ "Adding player..."

	lev0'' <- addPlayer lev0' player

	putStrLn $ show lev0''

	putStrLn $ "Adding more levels..."

	let d = lev0'' : replicate (levels - 1) lev0'

	putStrLn $ "Returning game..."

	return Game { dungeon = d, playerLevel = 0 }