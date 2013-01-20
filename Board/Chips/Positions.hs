module Board.Chips.Positions
       ( Position(..), Positions
       , Positioning
       , Row(..), Spot, Level
       , Id
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

import Board.Chips.Positions.Datatype

positions :: Positions
positions =    concat ( concat positions )
            ++ [ Pos Z 0  1
               , Pos Z 1  5
               , Pos Z 13 1
               , Pos Z 14 1
               ]
  where rows    = [ A, B, C, D, E, F, G, H ]
        positions = map (\(r, x) -> map (\(y, s) -> map (Pos r s)(enumFromTo 1 y)) (zip x [1..])) (zip rows basic)
        special   = [ 1, 5, 1, 1 ]
        basic     = [ [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ]
                    , [      1, 2, 2, 2, 2, 2, 2, 1       ]
                    , [   1, 1, 2, 3, 3, 3, 3, 2, 1, 1    ]
                    , [1, 1, 1, 2, 3, 4, 4, 3, 2, 1, 1, 1 ]
                    , [1, 1, 1, 2, 3, 4, 4, 3, 2, 1, 1, 1 ]
                    , [   1, 1, 2, 3, 3, 3, 3, 2, 1, 1    ]
                    , [      1, 2, 2, 2, 2, 2, 2, 1       ]
                    , [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ]
                    ]


theRow :: Position -> Positions -> Positions
theRow p ps
  = filter (\(Pos r _ l) -> r == row p && l == level p) ps

isZ :: Position -> Bool
isZ (Pos r _ _) = r == Z

isChip :: Position -> Positions -> Bool
isChip p ps = (>0) $ length $ filter (==p) ps



zCond :: Position -> Positions -> Bool
zCond p ps
  | spot p == 0
  = (isChip (Pos D 1 1) ps) && (isChip (Pos E 1 1) ps)
  | spot p == 1
  = (isChip (Pos D 6 4) ps) && (isChip (Pos D 7 4) ps) && (isChip (Pos E 6 4) ps) && (isChip (Pos E 7 4) ps) 
  | spot p == 13
  = (isChip (Pos D 12 1) ps) && (isChip (Pos E 12 1) ps) 
  | spot p == 14
  = (isChip (Pos Z 13 1) ps)


_AttPos :: (Position -> Positions -> Position) -> Position -> Positions -> Bool
_AttPos o p ps
  = p == leftMost theRow
    where theRow = filter (\(Pos r' _ l') -> r' == row p && l' == level p) ps
          leftMost :: Positions -> Position
          leftMost tr 
            | length tr == 1
            = head tr
            | length tr == 2
            = (tr!!0) `o` [(tr!!1)]
            | otherwise
            = (head tr) `o` (tail tr)

leftAttPos :: Position -> Positions -> Bool
leftAttPos  = _AttPos leftOf

rightAttPos :: Position -> Positions -> Bool
rightAttPos = _AttPos rightOf          

emptyRow :: Position -> Positions -> Bool
emptyRow p ps
  = (==0) $ length $ filter (\(Pos r _ l) -> (r == (row p)) && (l == (level p))) ps

isSub :: Position -> Positions -> Bool
isSub (Pos r s l) act
  | l == 1
  = True

  | otherwise
  = isChip (Pos r s (l-1)) act

_End :: (Spot -> Spot -> Bool) -> Positions -> Position
_End o (p:ps)
  = foldl f p ps
  where f (Pos r s l) (Pos r' s' l')
          | r' == r && l' == l && s' `o` s
          = (Pos r' s' l')

          | otherwise
          = (Pos r s l)

leftEnd :: Positions -> Position
leftEnd = _End (<)

rightEnd :: Positions -> Position
rightEnd = _End (>)

_Of :: (Spot -> Spot -> Bool) ->  Position -> Positions -> Position
_Of _ p [] = p
_Of o p ps
  = foldl f p ps
  where f (Pos r s l) (Pos r' s' l')
          | r == r' && l == l' && s `o` s'
          = (Pos r' s' l')

          | otherwise
          = (Pos r s l)

leftOf  = _Of (>)
rightOf = _Of (<)

