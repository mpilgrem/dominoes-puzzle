{-|
Module exporting (or rexporting) types and helper functions.
-}
module Dominoes.Types
  ( Location
  , Board (..)
  , Tile (..)
  , makeTile
  , spots
  , Dir (..)
  , Placement (..)
  , TilePlaces (..)
  , PlacedTile (..)
  , ptLocation
  ) where

import Data.Array (Array, (!), bounds)
import Data.List (foldl', intercalate)
import Data.Maybe (fromJust)

import Dominoes.Location (Dir (..), Location, Placement (..), otherLocation,
  oppDir)

-- |A board, represented as an array of spots ('Int') indexed by location.
newtype Board = Board (Array Location Int)

instance Show Board where
  show (Board b) =
    concatMap (\y -> hGridLine ++
      concatMap (\x -> "|" ++ show (b ! (x, y))) w ++ "|\n") (reverse h) ++
      hGridLine
   where
    ((xl, yl), (xu, yu)) = bounds b
    h = [yl .. yu]
    w = [xl .. xu]
    n = xu - xl + 1
    hGridLine = concatMap (const "+-") [1 .. n] ++ "+\n"

-- |A domino tile. The first number of spots is greater than or equal to the
-- second number of spots.
data Tile = Tile { spot1 :: !Int
                 , spot2 :: !Int }
          deriving Eq

instance Show Tile where
  show t = "[" ++ show (spot1 t) ++ ":" ++ show (spot2 t) ++ "]"

-- |Construct a domino.
makeTile :: Int -> Int -> Tile
makeTile s s' = if s >= s'
  then Tile s s'
  else Tile s' s

-- |Given a domino tile, return both its spots (first, second).
spots :: Tile -> (Int, Int)
spots (Tile s1 s2) = (s1, s2)

-- |Possible placements of a particular tile
data TilePlaces = TilePlaces { tpTile   :: !Tile
                             , tpPlaces :: ![Placement] }

instance Show TilePlaces where
  show tp = show (tpTile tp) ++ " at: " ++
    intercalate "; " (map show (tpPlaces tp))

-- |A placement of a particular tile
data PlacedTile = PlacedTile { tile   :: !Tile
                             , place  :: !Placement }
                deriving Eq

instance Show PlacedTile where
  show pt = show (tile pt) ++ " at " ++ show (place pt)

  showList [] = id
  showList pts = (++
    concatMap (\y ->
      concatMap (\x -> case lookup (x, y) cs of
                         Nothing -> "  "
                         Just (_, d) -> case d of
                                          UpDir -> "+ "
                                          _     -> "+-") [xl .. xu] ++
      case lookup (xu, y) cs of
        Nothing -> " "
        _       -> "+"
      ++ "\n" ++
      concatMap (\x -> case lookup (x, y) cs of
                         Nothing -> "  "
                         Just (s, d) -> (case d of
                             LeftDir -> " "
                             _       -> "|")
                           ++ show s) [xl .. xu]
      ++ (case lookup (xu, y) cs of
           Nothing -> " "
           Just (_, d) -> case d of
                             RightDir -> " "
                             _        -> "|")
      ++ "\n") [yu, yu - 1 .. yl] ++
    concatMap (\x -> case lookup (x, yl) cs of
      Nothing -> "  "
      Just (_, d) -> case d of
        DownDir -> "+ "
        _     -> "+-") [xl .. xu] ++
    (case lookup (xu, yl) cs of
      Nothing -> " "
      _       -> "+")
    ++ "\n")
   where
    ((xl, yl), (xu, yu)) = fromJust $ ptBounds pts
    cs = concatMap ptCells pts

-- |Given a list of placed tiles, the bounds of the smallest rectangle enclosing
-- the locations they cover. Equal to 'Nothing' if the list is empty.
ptBounds :: [PlacedTile] -> Maybe (Location, Location)
ptBounds [] = Nothing
ptBounds pts = lBounds (concatMap ptLocation pts)

-- |Given a list of locations, the bounds of the smallest rectangle enclosing
-- them. Equal to 'Nothing' if the list is empty.
lBounds :: [Location] -> Maybe (Location, Location)
lBounds [] = Nothing
lBounds ls = Just ((xMin, yMin), (xMax, yMax))
 where
  (l:ls') = ls
  (x0, y0) = l
  (xMin, yMin, xMax, yMax) = foldl' process (x0, y0, x0, y0) ls'
  process (xMin', yMin', xMax', yMax') (x, y) =
    (min xMin' x, min yMin' y, max xMax' x, max yMax' y)

-- |Given a placed tile, an association list of its cells (spots, orientation)
-- indexed by their location.
ptCells :: PlacedTile -> [(Location, (Int, Dir))]
ptCells pt = c2 : [c1]
 where
  t = tile pt
  (s1, s2) = spots t
  location1 = (pos . place) pt
  location2 = otherLocation $ place pt
  dir1 = (dir . place) pt
  dir2 = oppDir dir1
  c1 = (location1, (s1, dir1))
  c2 = (location2, (s2, dir2))

-- |Given a placed tile, a list of the locations it covers.
ptLocation :: PlacedTile -> [Location]
ptLocation pt = location1 : [location2]
 where
  location1 = (pos . place) pt
  location2 = otherLocation $ place pt
