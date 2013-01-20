module Board
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
       , posSetable
       , setChip
       , addChip
       , getChipAt
       , subChip
       , neigChips
       , positions
       , chips
       )
where

import Board.Datatype hiding (pickChip)
import qualified Board.Datatype as BD

import Prelude hiding (id)
import Data.List  (nub)



pickChip = BD.pickChip posSetable

posSetable :: Position -> Positions -> Bool
posSetable p ps
  | (not (emptyRow p ps) && (isSub p ps)
    &&
    (  (spot $ leftEnd  $ theRow p ps) -1 == (spot p)
    || (spot $ rightEnd $ theRow p ps) +1 == (spot p)
    )
    )
  = True

  | ((emptyRow p ps) && (isSub p ps))
  = True

  | ((isZ p) && (zCond p ps))
  = True

  | ((isZ p) && (not (zCond p ps)))
  = True

  | otherwise
  = False

setChip :: ([(Position, Chip)], Board) -> (Position, Chip) -> ([(Position, Chip)], Board)
setChip (act, board) (p, c)
  = (  ((p, c):act)
    ,  if (row p == Z)
          then case (spot p) of
               0  -> addChip board (c, newZ0Poss)
               1  -> addChip board (c, newZ1Poss)
               13 -> addChip board (c, newZ13Poss)
               14 -> addChip board (c, newZ14Poss)
          else addChip board (c, newPoss)
    )
  where newZ1Poss = map (\x -> (id c, id x))
                     $ (getChipAt act (Pos D 6 4)
                        : getChipAt act (Pos D 7 4)
                        : getChipAt act (Pos E 6 4)
                        : getChipAt act (Pos E 7 4)
                        : [])
        newZ0Poss = map (\x -> (id c, id x))
                     $ (getChipAt act (Pos D 1 1)
                        : getChipAt act (Pos E 1 1)
                        : [])
        newZ13Poss = map (\x -> (id c, id x))
                      $ (getChipAt act (Pos D 12 1)
                        : getChipAt act (Pos E 12 1)
                        : [])
        newZ14Poss = map (\x -> (id c, id x))
                      $ (getChipAt act (Pos Z 13 1)
                        : [])
        newPoss  = map (\x -> (id c, id x)) $ (subChip act p) ++ (neigChips act p)
        newBoard = addChip board (c, newPoss)


addChip :: Board -> (Chip, Positioning) -> Board
addChip (Board cs ps) (c, p) = (Board (c:cs) (nub (p ++ ps)))

getChipAt :: [(Position, Chip)] -> Position -> Chip
getChipAt pcs p
  = snd.head $ filter (\((Pos r s l), _) -> r == row p && s == spot p && l == level p) pcs

subChip :: [(Position, Chip)] -> Position -> [Chip]
subChip pcs p
  = map snd $ filter (\((Pos r s l), _) -> r == row p && s == spot p && (l+1) == level p) pcs


neigChips :: [(Position, Chip)] -> Position -> [Chip]
neigChips pcs p
  = map snd $ filter (\((Pos r s l), _) -> r == row p && ((s+1) == spot p || (s-1) == spot p) && l == level p) pcs

