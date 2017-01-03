{-# language OverloadedStrings #-}
{-# language LambdaCase #-}
{-# language TupleSections #-}
{-# language ConstraintKinds #-}

module Semantics where

import Syntax as S
import Parse ()
import Data
import Param

import Prelude hiding (and, or, not, (||), (&&))
import qualified Prelude as P
import Ersatz (Bit,Boolean(..),Result(..)
              , Orderable(..), Equatable(..)
              , assert, encode, sumBits)
import qualified Ersatz as E
import Ersatz.Solver

import qualified Data.Text as T
-- import qualified Data.Map.Strict as M
import qualified Data.HashMap.Strict as M
import Data.HashMap.Strict (HashMap)
import Data.Hashable
import Control.Monad.Except

type Map k v = HashMap k v

type Mappable k = (Eq k, Hashable k)

solve f p = semantics f p >>= \ case
  Just s -> putStrLn s

semantics :: Formula -> Param -> IO (Maybe String)
semantics f p = do
  (stats,(status,out)) <- solveWithStats minisat $ do
    plan <- Data.make p
    case models f plan of
      P.Left err -> error $ show err
      P.Right bit -> do
        assert bit
        return plan
  print stats      
  case status of
    Satisfied -> return $ pplan <$> out
    _ -> error $ show status

models :: Formula -> Plan Bit -> Either T.Text Bit
models f p = formula p M.empty f

newtype Encoded a = Encoded (Map a Bit)
  deriving Show

equals (Encoded x) (Encoded y) =
  or $ M.elems $ M.intersectionWith (===) x y

data Value = VThrow (Encoded Throw)
           | VPlace (Encoded Place)
           | VTime (Encoded Time)
           | VPerson (Encoded Person)
  deriving ( Show)

elements :: Plan b -> Sort -> [Value]
elements p s = case s of
  S.Person -> map (\x -> VPerson $ known x ) $ all_persons p
  S.Time   -> map (\x -> VTime   $ known x ) $ all_times   p
  S.Place  -> map (\x -> VPlace  $ known x ) $ all_places   p

known e = Encoded $ M.singleton e true  

formula :: Plan Bit -> Map Name Value -> Formula -> Either T.Text Bit
formula p env f = case f of
  Atom rel ts -> do
    vs <- forM ts $ term p env
    case (rel,vs) of
       (Eq, [ VTime x, VTime y ]) -> return $ equals x y
       (Eq, [ VPerson x, VPerson y ]) -> return $ equals x y
       (Eq, [ VPlace x, VPlace y ]) -> return $ equals x y
       (Neq, [ VTime x, VTime y ]) -> return $ not $ equals x y
       (Neq, [ VPerson x, VPerson y ]) -> return $ not $ equals x y
       (Neq, [ VPlace x, VPlace y ]) -> return $ not $ equals x y
       (Pass, [ VThrow x ]) ->
         return $ not $ equals (apply1 from x) (apply1 to x)
       (Self, [ VThrow x ]) ->
         return $ equals (apply1 from x) (apply1 to x)
       (S.Right, [VTime x]) -> return $ rel1 even x
       (S.Left, [VTime x]) -> return $ rel1 odd x
  Quantified q Throw name f -> do
    pairs <- forM (assocs p) $ \ (k,v) -> 
      (v,) <$> formula p (M.insert name (VThrow $ Encoded $ M.singleton k v) env) f
    let combi =   map (uncurry (&&)) pairs
    return $ case q of
      Forall -> and $ map (uncurry (==>)) pairs
      Exists -> or combi
      Atmost k -> atmost k combi
      Exactly k -> exactly k combi
      Atleast k -> atleast k combi
  Quantified q sort name f -> do
    vs <- forM (elements p sort) $ \ v ->
      formula p (M.insert name v env) f
    return $ case q of
      Exists -> or vs
      Forall -> and vs
      Atleast k -> atleast k vs
      Atmost k -> atmost k vs
      Exactly k -> exactly k vs
  Boolean bop fs -> do
    vs <- forM fs $ formula p env
    case (bop,vs) of
      (And,_) -> return $ and vs
      (Or,_) -> return $ or vs
      (Implies,[p,q]) -> return $ p ==> q
      (Not,[p]) -> return $ not p
      (Xor,_) -> return $ P.foldr xor false vs
      _ -> throwError $ T.unwords
        [ "Semantics.models:", T.pack $ show f ]

term :: Plan Bit -> Map
 Name Value -> Term -> Either T.Text Value
term p env t = case t of
  Constant S.Person i -> return $ VPerson $ known
     $ mod (fromIntegral i) (Data.persons p)
  Constant S.Time   i -> return $ VTime   $ known
     $ mod (fromIntegral i) (Data.period p)
  Ref n | Just v <- M.lookup n env -> return v
  Apply op ts -> do
    vs <- forM ts $ term p env
    case (op,vs) of
      (Begin,[VThrow x]) -> return $ VTime $ apply1 begin x
      (End,[VThrow x]) -> return $ VTime $ apply1 end x
      (From,[VThrow x]) -> return $ VPerson $ apply1 from x
      (To,[VThrow x]) -> return $ VPerson $ apply1 to x
      (Height,[VThrow x]) -> return $ VTime $ apply1 ( \ t -> mod (end t - begin t) (Data.period p) ) x
      (Next, [VTime x]) -> return $ VTime $ apply1 (next_time p) x
      (Next, [VPerson x]) -> return $ VPerson $ apply1 (next_person p) x
      (Prev, [VTime x]) -> return $ VTime $ apply1 (prev_time p) x
      (Prev, [VPerson x]) -> return $ VPerson $ apply1 (prev_person p) x
      (Plus, [VTime x,VTime y]) -> return $ VTime $ apply2 (plus_time p) x y
      (Minus, [VTime x,VTime y]) -> return $ VTime $ apply2 (minus_time p) x y
      _ -> throwError $ T.unwords [ "Semantics.term.Apply:", T.pack (show t) ]
  _ -> throwError $ T.unwords [ "Semantics.term:", T.pack (show t) ]

next_time p x = mod (succ x) (Data.period p)
next_person p x = mod (succ x) (Data.persons p)

prev_time p x = mod (pred x) (Data.period p)
prev_person p x = mod (pred x) (Data.persons p)

plus_time p x y = mod (x + y) (Data.period p)
minus_time p x y = mod (x - y) (Data.period p)

rel1 :: (a -> Bool) -> Encoded a -> Bit
rel1 r (Encoded x) = or $ do
  (k,v) <- M.toList x
  guard $ r k
  return v

apply1 :: Mappable b => (a -> b) -> Encoded a -> Encoded b
apply1 f (Encoded x) = Encoded $ M.fromListWith (||) $ do
  (k,v) <- M.toList x
  return (f k, v)

apply2 :: Mappable c => (a -> b -> c) -> Encoded a -> Encoded b -> Encoded c
apply2 f (Encoded x) (Encoded y) = Encoded $ M.fromListWith (||) $ do
  (kx,vx) <- M.toList x
  (ky,vy) <- M.toList y
  return (f kx ky, vx && vy)

atleast, atmost, exactly :: Int -> [Bit] -> Bit
atleast k xs = encode (fromIntegral k) <=? sumBits xs
atmost k xs = encode (fromIntegral k) >=? sumBits xs
exactly k xs = encode (fromIntegral k) E.=== sumBits xs
