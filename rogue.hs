#!/usr/bin/env runhaskell

module Main where

import UI.HSCurses.Curses
import Control.Exception (handle, AsyncException(UserInterrupt))
import Control.Monad (when)
import Data.List.Split (unintercalate)
import Dungeon

drawGame :: Game -> IO ()
drawGame g = do
	let levString = show g
	let rowStrings = unintercalate "\n" levString
	
	wclear stdScr
	mapM (\row -> mvWAddStr stdScr row 0 (rowStrings !! row)) [0 .. rows - 1]
	refresh

act :: Game -> Key -> IO Game
act g k = do
	-- putStrLn $ "Key: " ++ show k

	let g' = case k of
		cKEY_UP -> moveUp g
		cKEY_DOWN -> moveDown g
		cKEY_LEFT -> moveLeft g
		cKEY_RIGHT -> moveRight g
		otherwise -> g

	return g'

loopGame :: Game -> IO ()
loopGame g = do
	drawGame g
	c <- getCh
	g' <- act g c
	loopGame g'

main :: IO ()
main = do
	handle (\e -> when (e == UserInterrupt) endWin) $ do

	initCurses
	cBreak True
	cursSet CursorInvisible
	echo False
	nl False

	g <- initGame

	loopGame g

	endWin