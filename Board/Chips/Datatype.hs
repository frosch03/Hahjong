module Board.Chips.Datatype
       ( Position(..), Positions
       , Positioning
       , Chip(..), Chips
       , Row(..), Spot, Level
       , Id
       , Suit(..), Wind(..), Dragon(..), Flower(..), Season(..), Value(..)
       , theRow
       , isZ, zCond
       , isChip
       , isSub
       , emptyRow
       , leftAttPos, rightAttPos
       , leftEnd, rightEnd
       , positions
       )
where

import Board.Chips.Positions

type Chips
     = [Chip]

data Chip
     = Chip 
       { id    :: Id
       , value :: Value
       } 

instance Show Chip where 
  show (Chip id val) = show id ++ ": " ++ show val


data Suit
     = Bamboos
     | Characters
     | Circles
     deriving (Show)

data Wind      
     = East 
     | South 
     | West
     | North
     deriving (Show)

data Dragon
     = Red
     | Green
     | White
     deriving (Show)
       
data Flower
     = Plum
     | Orchid
     | Chrysanthemum
     | Bamboo
     deriving (Show)       
       
data Season       
     = Spring
     | Summer
     | Autumn
     | Winter
     deriving (Show)

data Value
     = Simples Suit Int
     | Winds   Wind
     | Dragons Dragon
     | Flowers Flower
     | Seasons Season


instance Show Value where
  show (Simples suit nr) 
      =  show suit 
      ++ "("
      ++ show nr
      ++ ")"
  show (Winds   wind)   = show wind
  show (Dragons dragon) = show dragon
  show (Flowers flower) = show flower
  show (Seasons season) = show season
       


