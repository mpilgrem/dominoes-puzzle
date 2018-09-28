{-|
Solve a dominoes puzzle.

A dominoes puzzle is a rectangular board divided into square cells, each
cell containing a number of spots. A valid puzzle can be solved by placing a
complete deck of domino tiles onto the board.

A standard domino deck has 28 unique tiles, from double blank (no spots) to
double six. 28 tiles require 56 cells, such as those provided by a 7 x 8 board.
-}
module Dominoes
  ( Board
  , PlacedTile
  , solve
  ) where

import Control.Monad.Writer (Writer, tell, writer)

import Data.Array ((!))
import Data.List (foldl', intercalate, nub, sortBy)
import Data.Maybe (fromJust, fromMaybe, mapMaybe)

import Dominoes.Deck (deck, deckSize)
import Dominoes.Board (allLocations, boardSize)
import Dominoes.Location (oppDir, otherLocation, placements)
import Dominoes.Types (Board (..), Dir (..), Location, PlacedTile (..),
  Placement (..), Tile, TilePlaces (..), makeTile, ptLocation)

-- |Attempt to solve a board, logging progress. A solution is a list of placed
-- domino tiles. For example:
--
-- >>> :{
-- >>> let board = makeBoard (8, 7)
-- >>>       [ 1, 2, 1, 4, 6, 5, 4, 0
-- >>>       , 1, 5, 6, 0, 3, 4, 5, 6
-- >>>       , 6, 4, 1, 0, 2, 4, 3, 0
-- >>>       , 6, 2, 5, 1, 0, 3, 6, 5
-- >>>       , 3, 6, 5, 0, 0, 4, 4, 2
-- >>>       , 3, 2, 2, 3, 5, 1, 3, 0
-- >>>       , 1, 3, 1, 5, 2, 6, 2, 4 ]
-- >>> in  fromMaybe [] $ fst $ runWriter $ solve board
-- >>> :}
-- +-+-+-+-+-+-+-+-+
-- |1|2 1|4 6|5 4|0|
-- + +-+-+-+-+-+-+ +
-- |1|5 6|0|3|4|5|6|
-- +-+-+-+ + + + +-+
-- |6|4 1|0|2|4|3|0|
-- + +-+-+-+-+-+-+ +
-- |6|2|5|1 0|3 6|5|
-- +-+ + +-+-+-+-+-+
-- |3|6|5|0|0 4|4|2|
-- + +-+-+ +-+-+ + +
-- |3|2 2|3|5|1|3|0|
-- +-+-+-+-+ + +-+-+
-- |1 3|1 5|2|6|2 4|
-- +-+-+-+-+-+-+-+-+
solve :: Board -> Writer [String] (Maybe [PlacedTile])
solve b
  | even s
    = case mn of
        Left msg -> logFail msg
        Right n  -> do
         tell ["Starting to solve ..."]
         solve' b [] (deck n)
  | otherwise = logFail $ "Invalid board sized " ++ show (w, h) ++
                          "; cannot have odd number of squares"
 where
  (w, h) = boardSize b
  s = w * h
  mn = deckSize (s `div` 2)

-- |Solve a board, given already placed tiles and a list of tiles not yet
-- placed.
solve' :: Board         -- ^Board
       -> [PlacedTile]  -- ^Placed tiles
       -> [Tile]        -- ^Remaining tiles
       -> Writer [String] (Maybe [PlacedTile])
solve' _ pts [] = pure $ Just pts  -- No remaining tiles? Solved.
solve' b pts ts
  | noPlacements ps  -- No placements for some remaining tiles?
    = logFail $ "No solution for some remaining tiles. Tiles: " ++ show ts ++
                " Places: " ++ show ps
  | null unique = do  -- No unique placements?
      tell ["Trying alternatives for tile " ++ show t]
      trySolve b pts ts pts'
  | otherwise = do
      tell ["Only one place for " ++ intercalate "; " (map show unique)]
      solve' b (unique ++ pts) (removePlacedTiles ts unique)
 where
  fls = freeLocations b pts
  unique = nub $ ufls ++ ups
  ufls = uniqueFreeLocation b fls
  ps = tilePlaces ts $ freeTiles b fls
  ps' = sortOptions ps
  opt = head ps'
  t = tpTile opt
  pts' = map (PlacedTile t) (tpPlaces opt)
  ups = uniquePlacements ps

-- |Try to solve a board, given already placed tiles, a list of tiles not yet
-- placed and a list of possible alternative placements
trySolve :: Board
         -> [PlacedTile]  -- ^Placed tiles
         -> [Tile]        -- ^Remaining tiles
         -> [PlacedTile]  -- ^Alternative tile placements to try
         -> Writer [String] (Maybe [PlacedTile])
trySolve _ pts [] _ = pure $ Just pts  -- No tiles remaining?
trySolve _ _ _ [] = logFail "Ran out of options"  -- No alteratives remaining?
trySolve b pts ts (pt':pts') = do
  tell ["Trying " ++ show pt']
  result <- solve' b (pt':pts) (removePlacedTiles ts [pt'])  -- Try alternative
  case result of
    Just pts'' -> writer (Just pts'', [show pt' ++ " worked!"])
    Nothing -> do
      tell [show pt' ++ " did not work!"]
      trySolve b pts ts pts'  -- Try with remaining alternatives

-- |Log a failure to the standard output channel.
logFail :: String -> Writer [String] (Maybe a)
logFail msg = writer (Nothing, [msg])

-- |Sort list of possible tile placements by number of possible places
sortOptions :: [TilePlaces] -> [TilePlaces]
sortOptions = sortBy order
 where
  order opts1 opts2 =
    compare (length $ tpPlaces opts1) (length $ tpPlaces opts2)

-- |Remove a list of placed tiles from a list of times
removePlacedTiles :: [Tile]        -- ^List of tiles
                  -> [PlacedTile]  -- ^Placed tiles to remove
                  -> [Tile]
removePlacedTiles = foldl' removePlacedTile

-- |Remove a placed tile from a list of tiles
removePlacedTile :: [Tile]      -- ^List of tiles
                 -> PlacedTile  -- ^Placed tile to remove
                 -> [Tile]
removePlacedTile ts pt = filter (/= tile pt) ts

-- |Given a board and tiles placed on it, a list of all available locations.
freeLocations :: Board
              -> [PlacedTile]  -- ^Tiles placed on the board
              -> [Location]
freeLocations b pts = filter (`notElem` ptLocations) (allLocations b)
 where
  ptLocations = concatMap ptLocation pts

-- |Given a board and the available locations on it, a list of all possible tile
-- placements.
freeTiles :: Board
          -> [Location]    -- ^Available locations on the board
          -> [PlacedTile]
freeTiles b fls =
  mapMaybe (placeTile' b) $ concatMap (validPlacements fls) fls

-- |Given the remaining tiles and all possible tile placements, the possible
-- places (if any) for those tiles
tilePlaces :: [Tile]        -- ^Remaining tiles
           -> [PlacedTile]  -- ^Possible tile placements
           -> [TilePlaces]
tilePlaces ts pts = map (options vpts) ts
 where
  vpts = validPlacedTiles ts pts

-- |Given the remaining tiles and all possible tile placements, the possible
-- tile placements corresponding to those tiles.
validPlacedTiles :: [Tile]        -- ^Remaining tiles
                 -> [PlacedTile]  -- ^Possible tile placements
                 -> [PlacedTile]
validPlacedTiles ts = filter (\pt -> tile pt `elem` ts)

-- |Filter possible placements for unique placements
uniquePlacements :: [TilePlaces] -> [PlacedTile]
uniquePlacements = mapMaybe uniquePlacement

-- |Equate a single possible place with a placed tile
uniquePlacement :: TilePlaces -> Maybe PlacedTile
uniquePlacement opts = case tpPlaces opts of
  [p] -> Just $ PlacedTile (tpTile opts) p
  _   -> Nothing

-- |Any no possible places?
noPlacements :: [TilePlaces] -> Bool
noPlacements = any noPlacement

-- |No possible places?
noPlacement :: TilePlaces -> Bool
noPlacement opts = null (tpPlaces opts)

-- |Given possible tile placements and a tile, possible places for that tile
options :: [PlacedTile]  -- ^Tile placements
        -> Tile          -- ^Tile
        -> TilePlaces
options pts t = TilePlaces t $ map place $ filter (\pt -> t == tile pt) pts

-- |Given the available locations on the board, those tiles that would uniquely
-- cover a location.
uniqueFreeLocation :: Board
                   -> [Location]    -- ^Available locations on the board
                   -> [PlacedTile]
uniqueFreeLocation b fls =
  nub $ mapMaybe (uniqueFreeLocation' b fls) fls

-- |Is there a single tile placement at a location?
uniqueFreeLocation' :: Board -> [Location] -> Location -> Maybe PlacedTile
uniqueFreeLocation' b fls l = case validPlacements fls l of
  [p] -> Just $ placeTile b p
  _   -> Nothing

-- |Given available locations, valid placements for a specific available
-- location
validPlacements :: [Location]   -- ^Available locations on the board
                -> Location     -- ^Specific available location
                -> [Placement]
validPlacements fls l = filter (validPlacement fls) $ placements l

-- |Given available locations, is a placement at an available location valid?
validPlacement :: [Location]  -- ^Available locations
               -> Placement   -- ^Placement at an available location
               -> Bool
validPlacement fls p = otherLocation p `elem` fls

-- |Given a board and a placement, place a tile (rotating tile, if required).
placeTile :: Board -> Placement -> PlacedTile
placeTile b p = fromMaybe pt' $ placeTile' b p
 where
  pt' = fromJust $ placeTile' b p'
  p' = Placement (otherLocation p) (oppDir (dir p))

-- |Given a board and a placement, is a placed tile possible (without rotation)?
placeTile' :: Board -> Placement -> Maybe PlacedTile
placeTile' (Board b) p = if valid
  then Just $ PlacedTile t p
  else Nothing
 where
  l = pos p
  d = dir p
  valid = s > s' || (s == s' && (d == RightDir || d == UpDir))
  s = b ! l
  l' = otherLocation p
  s' = b ! l'
  t = makeTile s s'
