#!/usr/bin/env runhaskell

module Rogue where

import Dungeon

main :: IO ()
main = do
	putStrLn $ "Creating game..."

	g <- initGame

	return ()

	-- putStrLn $ "Printing game..."
	-- 
	-- putStrLn $ show g