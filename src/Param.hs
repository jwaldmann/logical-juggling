module Param where

p0 :: Param
p0 = Param { persons = 4, period = 3 }

data Param =
  Param { persons :: Int
        , period :: Int
        }
  deriving (Show)

