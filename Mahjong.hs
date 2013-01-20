module Mahjong where

import Stack
import Data.Maybe (fromJust, isJust)
import Data.List  (nub)
import Prelude hiding (id)

data Board 
     = Board Chips Positioning

type Id = Int
       
type Positioning
     = [(Id, Id)]

type Chips
     = [Chip]

data Row      = Z | A | B | C | D | E | F | G | H deriving (Show, Eq, Ord)
type Spot     = Int
type Level    = Int

type Positions = [Position]

data Position
      = Pos { row   :: Row
            , spot  :: Spot
            , level :: Level
            }
      deriving (Eq)

instance Show Position where
  show (Pos r s l) = show r ++ show s ++ ('^': show l)

data Ruler
     = Ruler
       { position :: Position
       , stoneId  :: Id
       }

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
       


chips :: Chips
chips = zipWith (flip Chip)
        (  concat ( replicate 4 ( concatMap (\x ->  zipWith Simples (replicate 9 x) [1..9]) suits ) )
        ++ concat ( replicate 4 $ map Winds winds ) 
        ++ concat ( replicate 4 $ map Dragons dragons ) 
        ++ map Flowers flowers
        ++ map Seasons seasons
        )
        [1..]
  where suits   = [ Bamboos, Characters, Circles ] 
        winds   = [ East, South, West, North ]
        dragons = [ Red,  Green, White ]
        flowers = [ Plum, Orchid, Chrysanthemum, Bamboo ]
        seasons = [ Spring, Summer, Autumn, Winter ] 
        

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

twoPositions :: Positions -> (Position, Position)
twoPositions poss = (head lowest, head (tail lowest))
  where lowest = foldl (flip takePosIfLower) [] poss

lowerPos :: Position -> Position -> Position
lowerPos x@(Pos _ _ l) y@(Pos _ _ l')
  = if l < l' then x else y

staple :: (Row, Spot) -> Positions -> Positions
staple (r, s) poss
  = filter (\x@(Pos r' s' _) -> r == r' && s == s') poss

takePosIfLower :: Position -> Positions -> Positions
takePosIfLower n [] = [n]
takePosIfLower n@(Pos r s l) (p@(Pos r' s' l'):ps)
  | r == r' && s == s' && l < l' = n:ps
  | r == r' && s == s' && l > l' = p:ps
  | otherwise                    = p:(takePosIfLower n ps)


type Rand = [Int]

randoms :: [Int]
randoms = [20,25,62,11,8,22,4,141,121,26,138,140,79,83,18,135,120,70,101,131,113,134,125,44,61,37,123,100,10,33,142,55,54,139,110,2,24,41,66,128,21,64,84,17,73,127,56,130,28,35,36,87,1,30,23,57,3,69,12,77,7,112,47,88,60,96,102,31,42,75,68,124,129,143,103,144,74,40,118,105,119,136,126,34,104,81,122,14,58,29,107,97,111,85,6,115,133,16,15,116,19,72,5,59,43,106,92,82,9,78,80,27,94,52,63,71,109,117,39,51,114,95,45,91,137,86,98,99,108,53,76,93,32,67,13,48,90,38,50,65,132,46,49,89]


randoms1 :: [Int]
randoms1 = [116,128,134,46,130,77,22,135,98,131,55,45,82,107,125,117,85,52,80,81,78,119,47,24,138,57,51,43,141,31,41,96,74,92,86,32,97,108,19,124,72,15,64,115,21,94,109,67,132,100,39,79,8,2,113,139,1,83,33,136,61,34,106,114,112,50,20,87,69,118,38,4,110,88,35,121,30,137,140,70,59,10,11,123,127,40,23,73,37,102,101,63,142,54,36,95,42,84,62,5,120,49,25,16,65,7,3,12,44,26,29,144,75,6,48,122,99,66,103,111,18,76,9,105,93,143,68,104,129,89,27,56,58,28,60,126,71,133,91,90,14,17,53,13]

makeRandPoss :: Rand -> Positions -> Positions
makeRandPoss rnd poss
  = sort tmp
    where tmp = zip rnd poss
          sort (x:xs) = [ snd l | l <- xs, fst l < fst x ] ++ (snd x : [ snd r | r <- xs, fst r >= fst x ] )

type RandomPositions = Positions

layOrd :: RandomPositions -> Positions
layOrd rand = layOrd' [] rand

layOrd' :: Positions -> Positions -> Positions
layOrd' lay (x:y:[]) = lay ++ (x:y:[])
layOrd' lay left = layOrd' (lay ++ nextLay) (filter (\x -> foldl (&&) True (map (x /=) nextLay)) left)
  where nextLay = maybe [] (\(x, y) -> [x, y]) $ layTwo lay left

layTwo :: Positions -> RandomPositions -> Maybe (Position, Position)
layTwo act rnd
  | ((length rnd) <  2)
  = Nothing
  | ((length rnd) == 2)
  = Just (rnd!!0, rnd!!1)
  | ((length rnd) > 2)
  = Just (pos1, pos2)
    where (act', pos1) = let (act', erg) = foldl pickChip (act, Nothing) rnd
                         in  (act', fromJust erg)
          pos2 =   fromJust $ snd $ foldl pickChip (act', Nothing)
                 $ filter (\(Pos r s _) -> r /= row pos1 || s /= spot pos1) rnd


       

pickChip :: (Positions, Maybe Position) -> Position -> (Positions, Maybe Position)
pickChip (act, Just x) _ = (act, Just x)
pickChip (act, Nothing)  cand
  | posSetable cand act
  = ((cand:act),Just cand)

  | otherwise
  = (act, Nothing)

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

    

theRow :: Position -> Positions -> Positions
theRow p ps
  = filter (\(Pos r _ l) -> r == row p && l == level p) ps

isZ :: Position -> Bool
isZ (Pos r _ _) = r == Z

isChip :: Position -> Positions -> Bool
isChip p ps = (>0) $ length $ filter (==p) ps


-- 0 1 13 14
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

leftAttPos  = _AttPos leftOf
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

leftEnd = _End (<)
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

layOrder :: RandomPositions -> [(Position, Position)]
layOrder [] = []
layOrder poss
  = (p1, p2) : (layOrder [ p | p <- poss, p /= p1 && p /= p2 ])
    where (p1, p2) = twoPositions poss


-- leftmost :: Row -> Level -> Board -> [(Id, Id)] -> Id
-- leftmost r l es
--   =
--     where tmp = filter (\(

oneRow :: Row -> Level -> [(Position, Position)] -> Maybe (Spot, [Either () ()])
oneRow r l ps
  = if ([] == layrow) then Nothing else Just (spot first, layit)
    where tmp = reverse $ foldl (\xs (x2, x1) -> x1:x2:xs) [] ps
          layrow = filter (\(Pos r' _ l') -> r' == r && l' == l) tmp
          first = head layrow
          layit = foldl (\xs (Pos _ s _) -> if s < (spot first) then (Left ()):xs else (Right ()):xs) [] (tail layrow)

genGame :: Board
genGame
  = snd $ foldl setChip ([], (Board [] [])) (zip lay chips)
  where lay = layOrd $ makeRandPoss randoms positions


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

-- type Dict = [(Row, Level), (Spot, [Either () ()])]

-- blub 
--   = error
--     where chipstack = zip chips lay
--           dict = map concat
--                  .   (\((r, l), x) -> if isJust x then [((r,l),fromJust x)] else [])
--                  .   (\(r, l) -> ((r, l), oneRow r l lay))
--                  $   [ (r, l) | r <- [A, B, C, D, E, F, G, H],  l <- [1..100] ]
--           lay' = layOrder $ makeRandPoss randomsS positions
--           lay  = reverse $ foldl (\xs (x2, x1) -> x1:x2:xs) [] lay'

-- connectH :: Chip -> Position -> (Board, Ruler) -> (Spot, [Either () ()]) -> (Board, Ruler)
-- connectH c p ((Board cs ps), rl) (s, lo)
--   = if cnt == 0
--     then ((Board (c:cs) ps), (p { spot = s } , id c):rl)
--     else 
--   where sInRow = filter (\(Pos r _ l) -> r == row p && l == layer p).position rl
--         cnt    = length sInRow
--         sL     =   foldl (\(ms, mi) (ns, ni) -> if ms < ns then (ms,mi) else (ns,ni) ) []
--                  $ map (\x -> (spot.position $ x, stoneId x)) sInRow
--         sR     =   foldl (\(ms, mi) (ns, ni) -> if ms > ns then (ms,mi) else (ns,ni) ) []
--                  $ map (\x -> (spot.position $ x, stoneId x)) sInRow
--         nextP  = either (\_ -> sL-1) (\_ -> sR+1) $ lo!!(cnt-1)

          
-- -- genEdges :: Chips -> [(Position, Position)] -> [(Id, Id)]
-- genEdges (c1:c2:cs) (x@(p1,p2):xs)
--   = 
