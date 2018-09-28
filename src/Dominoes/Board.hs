{-|
Module exporting helper functions for type 'Board'.
-}
module Dominoes.Board
  ( makeBoard
  , boardSize
  , allLocations
  ) where

import Dominoes.Types (Board (..), Location)

import Data.Array (bounds, listArray)
import Data.Ix (Ix (range))
import Data.List (transpose)

-- |Given a board size (width, height) and a list of spots (as presented on the
-- page, from top left to bottom right), create a board. For example:
--
-- >>> :{
-- >>> makeBoard (3, 2)
-- >>> [ 0, 0, 1
-- >>> , 0, 1, 1 ]
-- >>> :}
-- +-+-+-+
-- |0|0|1|
-- +-+-+-+
-- |0|1|1|
-- +-+-+-+
makeBoard :: (Int, Int)  -- ^Size of board: (width, height)
          -> [Int]       -- ^List of spots, as presented on the page, from top
                         -- left to bottom right.
          -> Board
makeBoard (w, h) spots = Board (listArray b spots')
 where
  b = ((1, 1), (w, h))
  spots' = concat $ transpose $ rows [] spots
  rows rs [] = rs
  rows rs ss = let (r, ss') = splitAt w ss
               in  rows (r : rs) ss'

-- |The size of a board (width, height).
boardSize :: Board -> (Int, Int)
boardSize (Board b) = (xu - xl + 1, yu - yl + 1)
 where
  ((xl, yl), (xu, yu)) = bounds b

-- |All the locations on a board.
allLocations :: Board -> [Location]
allLocations (Board b) = range $ bounds b
