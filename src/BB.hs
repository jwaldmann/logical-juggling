{-# language NoMonomorphismRestriction #-}
{-# language TypeFamilies #-}

module BB where

import Prelude hiding (and, or, not, (||), (&&))
import qualified Prelude as P
import Ersatz (Boolean(..), Bit)
import qualified Ersatz as E

data BB = Known Bool | Unknown Bit
  deriving Show

instance E.Codec BB where
  type Decoded BB = Bool
  decode s (Known c) = return c
  decode s (Unknown c) = E.decode s c

instance Boolean BB where
  bool c = Known c

  not (Known c) = Known (not c)
  not (Unknown u) = Unknown (not u)
  
  Known False && y = Known False
  Known True && y = y
  x && Known False = Known False
  x && Known True = x
  Unknown x && Unknown y = Unknown (x && y)
  
  x || y = not (not x && not y)

  all f args = foldr (\ x y -> f x && y) true args
  any f args = foldr (\ x y -> f x || y) false args

  xor (Known False) y = y
  xor (Known True) y = not y
  xor x (Known False) = x
  xor x (Known True) = not x
  xor (Unknown x) (Unknown y) = Unknown (xor x y)
  
x === y = xor x (not y)

sumBit :: [BB] -> E.Bits
sumBit xs = E.sumBit $ map unpack xs

unpack (Known k) = bool k
unpack (Unknown u) = u

exists = Unknown <$> E.exists
assert (Known c) = E.assert $ bool c
assert (Unknown c) = E.assert c
