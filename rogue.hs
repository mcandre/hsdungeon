#!/usr/bin/env runhaskell

module Main where

import UI.HSCurses.Curses
import Control.Exception (handle, AsyncException(UserInterrupt))
import Control.Monad (when)
import Data.List.Split (unintercalate)
import Dungeon

drawGame :: Game -> IO ()
drawGame g = do
	let lev = (dungeon g) !! (playerLevel g)
	let levString = show lev

	-- putStrLn levString

	let rowStrings = unintercalate "\n" levString
	
	wclear stdScr
	mapM (\row -> mvWAddStr stdScr row 0 (rowStrings !! row)) [0 .. rows - 1]
	refresh

loopGame :: Game -> IO ()
loopGame g = do
	-- ...

	loopGame g

main :: IO ()
main = do
	handle (\e -> when (e == UserInterrupt) endWin) $ do

	initCurses
	cBreak True
	cursSet CursorInvisible
	echo False
	-- nl False

	g <- initGame

	drawGame g

	loopGame g

	endWin