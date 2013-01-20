module Board.Chips
       ( Chip(..), Chips
       , Id
       , Row(..), Spot, Level
       , Position(..), Positions
       , Positioning 
       , Suit(..), Wind(..), Dragon(..), Flower(..), Season(..), Value(..)
       , theRow
       , isZ, zCond
       , isChip
       , isSub
       , emptyRow
       , leftAttPos, rightAttPos
       , leftEnd, rightEnd
       , positions
       , chips
       , pickChip
       )
where

import Board.Chips.Datatype


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


pickChip :: (Position -> Positions -> Bool) -> (Positions, Maybe Position) -> Position -> (Positions, Maybe Position)
pickChip _ (act, Just x) _ = (act, Just x)
pickChip f (act, Nothing)  cand
  | f cand act
  = ((cand:act),Just cand)

  | otherwise
  = (act, Nothing)
