{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# language GeneralizedNewtypeDeriving #-}

module Data where

import Param
import Prelude hiding ((||),(&&),and,or,not)
import Ersatz (Boolean(..), Codec(..), assert, exists)

import Control.Monad (forM, forM_, guard)
import qualified Data.Array as A

newtype Time = Time Int
  deriving (Eq, Ord, Show, Enum, A.Ix, Num, Real, Integral)
newtype Person = Person Int
  deriving (Eq, Ord, Show, Enum, A.Ix, Num, Real, Integral)
type Place = (Person,Time)

type Throw = (Place,Place)

begin ((_,t),_) = t
end (_,(_,t)) = t
from ((p,_),_) = p
to (_,(p,_)) = p


newtype Plan b = Plan (A.Array Throw b)
            deriving (Show)

instance Codec p => Codec (Plan p) where
  type Decoded (Plan p) = Plan (Decoded p)
  decode s (Plan p) = Plan <$> decode s p

make p = do
  let bot = (Person 0, Time 0)
      top = ( Person $ pred $ Param.persons p
            , Time $ pred $ Param.period p
            )
      bnd = ((bot,bot),(top,top))
  a <- A.array bnd
      <$> ( forM (A.range bnd) $ \ i -> (i,) <$> exists )
  let points = A.range (bot,top)
  forM_ points $ \ from -> do
    assert $ exactly_one $ map (\ to -> a A.! (from,to)) points
  forM_ points $ \ to -> do
    assert $ exactly_one $ map (\ from -> a A.! (from,to)) points
  return $ Plan a  

exactly_one [] = false
exactly_one [x] = x
exactly_one xs =
  let (ys,zs) = splitAt (div (length xs) 2) xs
  in     exactly_one ys && not (or zs)
      || not (or ys) && exactly_one zs

bounds (Plan p) =
  let ((bot,_),(top,_)) = A.bounds p
  in (bot,top)

assocs (Plan p) = A.assocs p

period p = let ((0,0),(_,t1)) = bounds p in t1 + 1
persons p = let ((0,0),(h1,_)) = bounds p in h1 + 1 

all_times   p = let ((0,0),(_,t1)) = bounds p in A.range (0,t1)
all_persons p = let ((0,0),(h1,_)) = bounds p in A.range (0,h1)
all_places p = A.range $ bounds p

height t = end t - begin t
direction t = (from t, to t)

person p = return $ ['A' .. ] !! fromIntegral p
  
pplan p = unlines $ do
  let ((h0,t0),(h1,t1)) = bounds p
  h <- [h0 .. h1]
  return $ person h ++ " : " ++ do
    t <- [t0 .. t1]
    let [(h',t')] = targets p (h,t)
    let th = ((h,t),(h',t'))
        d = show
          $ (fromIntegral  :: Time -> Integer)
          $ head $ filter (>= 3)
            [ height th, height th + Data.period p .. ]
        e = if from th == to th
            then "   "
            else "->"
                 ++ person (to th)
    d ++ e ++ " , "

targets plan p = do
  ((from,to),True) <- assocs plan
  guard $ from == p
  return to

