{-|
Module exporting the type 'Placement', related types and synonyms and helper
functions.
-}
module Dominoes.Location
  ( Location
  , Dir (..)
  , Placement (..)
  , step
  , otherLocation
  , oppDir
  , placements
  ) where

-- |A location on the board.
type Location = (Int, Int)

-- |An orientation of a tile.
data Dir = RightDir | UpDir | LeftDir | DownDir
  deriving (Enum, Eq)

instance Show Dir where
  show RightDir = "▶"
  show UpDir    = "▲"
  show LeftDir  = "◀"
  show DownDir  = "▼"

-- |A placement of a tile, being a location on the board and an orientation of
-- the tile.
data Placement = Placement { pos :: !Location
                           , dir :: !Dir }
               deriving Eq

instance Show Placement where
  show p = show (pos p) ++ " " ++ show (dir p)

-- |Given an orientation, its equivalent vector.
step :: Dir -> (Int, Int)
step RightDir = ( 1,  0)
step UpDir    = ( 0,  1)
step LeftDir  = (-1,  0)
step DownDir  = ( 0, -1)

-- |Gvien a placement, its other location.
otherLocation :: Placement -> Location
otherLocation (Placement (x, y) d) = let (dx, dy) = step d
                                     in  (x + dx, y + dy)

-- |Given an orientation, the opposite orientation.
oppDir :: Dir -> Dir
oppDir RightDir = LeftDir
oppDir UpDir    = DownDir
oppDir LeftDir  = RightDir
oppDir DownDir  = UpDir

-- |Given a location, all placements at that location.
placements :: Location -> [Placement]
placements l = map (Placement l) [RightDir .. DownDir]
