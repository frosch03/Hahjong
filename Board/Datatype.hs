module Board.Datatype
       ( Id
       , Row(..), Spot, Level
       , Position(..), Positions
       , Positioning
       , Board(..)
       , Chip(..), Chips         
       , theRow
       , isZ, zCond
       , isChip
       , isSub
       , emptyRow
       , leftAttPos, rightAttPos
       , leftEnd, rightEnd
       , pickChip
       , positions
       , chips
       )
where

import Board.Chips

data Board 
     = Board Chips Positioning

