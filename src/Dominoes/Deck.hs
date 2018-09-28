{-|
Module exporting helper functions for a deck of domino tiles.
-}

module Dominoes.Deck
  ( deck
  , deckSize
  ) where

import Dominoes.Types (Tile, makeTile)

-- |A complete deck of domino tiles.
deck :: Int     -- ^Maximum number of spots
     -> [Tile]
deck n
  | n < 0 = []
  | otherwise = [ makeTile s1 s2 | s1 <- [0 .. n]
                                 , s2 <- [0 .. s1]
                ]
-- |Given a valid number of domino tiles, the maximum number of spots on a
-- tile. For example:
--
-- >>> deckSize 28
-- Right 6
deckSize :: Int  -- ^Number of tiles
         -> Either String Int
deckSize t
  | t <= 0 = Left $ "Invalid number of tiles " ++ show t ++ "; cannot have \
                    \non-positive number of tiles"
  | n' < fromIntegral n =
      Left $ "Invalid number of tiles " ++ show t ++ "; cannot use part of a \
             \deck. A deck of up to " ++ show (n - 1) ++ " spots would have " ++
             show (n * (n + 1) `div` 2) ++ " tiles"
  | otherwise = Right $ n - 1
 where
  t' = fromIntegral t :: Double
  n' = (sqrt (1 + 8 * t') - 1.0) / 2.0
  n = ceiling n'
