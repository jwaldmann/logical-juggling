{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Prelude hiding (and, or, not, (||), (&&))
import qualified Prelude as P
import Ersatz
import qualified Data.Array as A
import Control.Monad (forM, forM_, guard)


newtype Time = Time Int
  deriving (Eq, Ord, Show, Enum, A.Ix, Num, Real, Integral)
newtype Person = Person Int
  deriving (Eq, Ord, Show, Enum, A.Ix, Num, Real, Integral)
type Place = (Person,Time)

each_throw :: Plan Bit -> (Throw -> Bool) -> Bit
each_throw p pred = and $ do
  (throw,t) <- assocs p
  return $ t ==> encode (pred throw)

some_throw :: Plan Bit -> (Throw -> Bool) -> Bit
some_throw p pred = or $ do
  (throw,t) <- assocs p
  return $ t && encode (pred throw)

each_person :: Plan Bit -> (Person -> Bit) -> Bit
each_person p pred = and $ do
  let ((p0,_),(p1,_)) = bounds p
  per <- [p0 .. p1]
  return $ pred per  

main = do
  (Satisfied, Just (p :: Plan Bool)) <- solveWith minisat $ do
    p :: Plan Bit <- plan (Person 4) (Time 3)
    assert $ heights p [3,4]
    assert $ each_person p $ \ e -> some_throw p $ \ t ->
           from t == e
      P.&& to t == e
      P.&& eq_mod (period p) 3 (height t)
    assert $ passes p [(0,2),(0,3),(1,2),(1,3)
                      ,(2,0),(2,1),(3,0),(3,1)
                      ]
    return p
  putStrLn $ pplan p


eq_mod m a b = 0 == mod (a - b) m

heights p hs = allowed_heights p hs && required_heights p hs

required_heights p hs = and $ do
  h <- hs
  return $ or $ do
    (throw,t) <- assocs p
    return $ t && encode ( eq_mod (period p) h (height throw))

allowed_heights p hs = and $ do
  (throw,t) <- assocs p
  return $ t ==> (encode $ P.or $ do
    a <- hs
    return $ 0 == mod (height throw - a) (period p)
    )

from ((p0,t0),_) = p0
to (_,(p1,t1)) = p1
start ((p0,t0),_) = t0
land (_,(p1,t1)) = t1

isself throw = let (h0,h1) = direction throw in h0 == h1

passes p ds = allowed_passes p ds && required_passes p ds

required_passes p ds = and $ do
  d <- ds
  return $ or $ do
    (throw,t) <- assocs p
    return $ t && encode ( direction throw == d )

allowed_passes p ds = and $ do
  (throw,t) <- assocs p
  return $ t ==> (encode $ P.or $ isself throw : do
    d <- ds
    return $ direction throw == d )
  
  
height t = land t - start t
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
            [ height th, height th + period p .. ]
        e = if from th == to th
            then "   "
            else "->"
                 ++ person (to th)
    d ++ e ++ " , "

targets plan p = do
  ((from,to),True) <- assocs plan
  guard $ from == p
  return to

type Throw = (Place,Place)
    
newtype Plan p = Plan (A.Array Throw  p)
            deriving (Show)

bounds (Plan p) =
  let ((bot,_),(top,_)) = A.bounds p
  in (bot,top)

assocs (Plan p) = A.assocs p

period p = let ((_,t0),(_,t1)) = bounds p in t1 - t0
  
instance Codec p => Codec (Plan p) where
  type Decoded (Plan p) = Plan (Decoded p)
  decode s (Plan p) = Plan <$> decode s p

plan persons time = do
  let bot = (Person 0, Time 0) ; top = (pred persons, pred time)
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

