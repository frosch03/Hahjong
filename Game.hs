module Game where

import Board
import MahRandom

import Data.Maybe (fromJust, isJust)

import Prelude hiding (id)




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

genGame :: Board
genGame
  = snd $ foldl setChip ([], (Board [] [])) (zip lay chips)
  where lay = layOrd $ makeRandPoss randoms positions


