#!/usr/bin/env runhaskell

module Rogue where

import Dungeon

main :: IO ()
main = do
	g <- initGame
	putStrLn $ show g