module Main where

import Control.Monad.Writer (runWriter)
import Data.List (replicate)

import System.IO.CodePage (withCP65001)

import Dominoes (Board, solve)
import Dominoes.Board (makeBoard)

main :: IO ()
-- On Windows, code page 65001 must be set to display the unicode characters
-- used to show the orientation of a placed tile.
main = withCP65001 $ do
  example "Example 1" testBoard1
  example "Example 2" testBoard2

-- |Example taken from Logic Problems Issue No. 407 published by Puzzler Media
-- Ltd, page 28.
testBoard1 :: Board
testBoard1 = makeBoard (8, 7)
  [ 1, 0, 0, 6, 4, 6, 5, 6
  , 1, 3, 3, 5, 1, 1, 1, 6
  , 5, 6, 6, 2, 4, 2, 0, 4
  , 3, 3, 1, 0, 0, 3, 2, 3
  , 5, 4, 4, 1, 2, 0, 4, 5
  , 3, 4, 5, 5, 1, 3, 2, 0
  , 6, 2, 0, 2, 6, 4, 2, 5 ]

-- |Example taken from
-- https://www.puzzler.com/FileDepository/Puzzles/Samples/Dominoes.pdf.
testBoard2 :: Board
testBoard2 = makeBoard (8, 7)
  [ 1, 2, 1, 4, 6, 5, 4, 0
  , 1, 5, 6, 0, 3, 4, 5, 6
  , 6, 4, 1, 0, 2, 4, 3, 0
  , 6, 2, 5, 1, 0, 3, 6, 5
  , 3, 6, 5, 0, 0, 4, 4, 2
  , 3, 2, 2, 3, 5, 1, 3, 0
  , 1, 3, 1, 5, 2, 6, 2, 4 ]

-- |Process an example.
example :: String -> Board -> IO ()
example title b = do
  putStrLn title
  putStrLn $ replicate (length title) '-'
  putStrLn ""
  print b
  putStrLn ""
  let (mPts, msgs) = runWriter $ solve b
  mapM_ putStrLn msgs
  putStrLn ""
  case mPts of
    Nothing -> putStrLn "Failed"
    Just pts -> print pts
  putStrLn ""
