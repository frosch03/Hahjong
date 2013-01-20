module Stack where

import Data.Maybe

type Height = Int

data Stack a
     = Stack Height [a]
     deriving (Show)

newStack :: Int -> Stack a
newStack i = Stack i []

push :: a -> Stack a -> Maybe (Stack a)
push _ (Stack 0 es) = Nothing
push e (Stack i es) = Just (Stack (i-1) (e:es))

pop :: Stack a -> Maybe a
pop (Stack _ []) = Nothing
pop (Stack _ es) = Just $ head es

height :: Stack a -> Int
height (Stack i _) = i

