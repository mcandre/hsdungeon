#!/usr/bin/env runhaskell

module Rogue where

import Dungeon

main :: IO ()
main = do
	g <- initGame

	let d = dungeon g
	let lev1 = d !! 0

	putStrLn $ show lev1