#!/usr/bin/env runhaskell

module Main where

import UI.NCurses
import Control.Exception (handle, AsyncException(UserInterrupt))
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.List.Split (unintercalate)
import Data.Text (pack)
import Dungeon

drawRow :: Window -> Int -> String -> Curses ()
drawRow w r s = do
	updateWindow w $ moveCursor (fromIntegral r) 0
	updateWindow w $ (drawText . pack) s

drawGame :: Window -> Game -> Curses ()
drawGame w g = do
	let levString = show g
	let rowStrings = unintercalate "\n" levString

	mapM (\row -> drawRow w row (rowStrings !! row)) [0 .. rows - 1]
	render

act :: Game -> Event -> Curses Game
act g e = return g'
	where
	g' = case e of
		-- EventCharacter 'q' -> return g
		EventSpecialKey KeyUpArrow -> moveUp g
		EventSpecialKey KeyDownArrow -> moveDown g
		EventSpecialKey KeyLeftArrow -> moveLeft g
		EventSpecialKey KeyRightArrow -> moveRight g
		otherwise -> g

loopGame :: Window -> Game -> Curses ()
loopGame w g = do
	drawGame w g
	me <- getEvent w Nothing

	g' <- case me of
		Nothing -> return g
		Just e -> act g e

	loopGame w g'

main :: IO ()
main = runCurses $ do
		w <- defaultWindow

		setCBreak True
		setEcho False

		g <- liftIO initGame

		loopGame w g

		closeWindow w