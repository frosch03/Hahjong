module Board.Chips.Positions.Datatype
       ( Id
       , Row(..), Spot, Level
       , Position(..), Positions
       , Positioning
       )
where

type Id = Int

data Row      = Z | A | B | C | D | E | F | G | H deriving (Show, Eq, Ord)
type Spot     = Int
type Level    = Int

type Positioning
     = [(Id, Id)]

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
        

