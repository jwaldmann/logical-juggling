{-# language OverloadedStrings #-}

module Syntax where

import Prelude hiding (Either(..))
import Data.Text
import Data.String
import Data.Hashable

f0 :: Formula
f0 = Boolean And
  [ Quantified Forall Throw "t"
    $ Boolean Or [ Atom Eq [ Constant Time 3
                           , Apply Minus [ Apply End [Ref "t"]
                                       , Apply Begin [Ref "t"]
                                       ]
                           ]
                 ]
  ]

data Formula
  = Atom Rel [Term]
  | Quantified Quant Sort Name Formula
  | Boolean Boo [Formula]
    deriving (Eq, Ord, Show)

data Sort = Throw | Place | Person | Time | Hand
  deriving (Eq, Ord, Show)

data Rel = Eq | Neq | Lt | Gt | Leq | Geq
         | Pass | Self | Right | Left
  deriving (Eq, Ord, Show)

data Quant
  = Forall | Exists
  | Atleast Int | Atmost Int | Exactly Int
  deriving (Eq, Ord, Show)

data Name = Name { hc :: ! Int, contents :: ! Text }
  deriving (Eq, Ord, Show)

name :: Text -> Name
name s = Name { hc = hash s, contents = s }

instance Hashable Name where 
  hashWithSalt = undefined
  hash = hc

instance IsString Name where
  fromString = name . fromString  

data Boo = Not | And | Or | Implies | Xor
  deriving (Eq, Ord, Show)

data Term = Ref Name
          | Constant Sort Int
          | Apply Fun [Term]
  deriving (Eq, Ord, Show)

data Fun = Next | Prev | Plus | Minus
         | Begin | End | Height
         | From | To
  deriving (Eq, Ord, Show)

