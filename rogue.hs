#!/usr/bin/env runhaskell

module Main where

import UI.NCurses
import Control.Exception (handle, AsyncException(UserInterrupt))
import Control.Monad (when)
import Data.List.Split (unintercalate)
import Data.Text (pack)
import Dungeon

drawRow :: Window -> Int -> String -> IO ()
drawRow w r s = do
	updateWindow w $ moveCursor (fromIntegral r) 0
	updateWindow w $ (drawText . pack) s

drawGame :: Window -> Game -> IO ()
drawGame w g = do
	let levString = show g
	let rowStrings = unintercalate "\n" levString

	mapM (\row -> drawRow w row (rowStrings !! row)) [0 .. rows - 1]
	render

act :: Game -> (Maybe Event) -> IO Game
act g e = do
	let g' = case e of
		Nothing -> g
		EventCharacter k -> case k of
			KeyUpArrow -> moveUp g
			KeyDownArrow -> moveDown g
			KeyLeftArrow -> moveLeft g
			KeyRightArrow -> moveRight g
			otherwise -> g

	return g'

loopGame :: Window -> Game -> IO ()
loopGame w g = do
	drawGame w g
	e <- getEvent w Nothing
	g' <- act g e
	loopGame g'

main :: IO ()
main = do
	runCurses
	w <- defaultWindow

	handle (\e -> when (e == UserInterrupt) closeWindow w) $ do
		runCurses
		setCBreak True
		setAttribute AttributeInvisible
		setEcho False

		g <- initGame

		loopGame w g

		closeWindow w