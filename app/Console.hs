{-# language LambdaCase #-}

import qualified Param as P
import Parse
import Syntax
import qualified Semantics 

import System.Environment
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.IO (stdin)

main = getArgs >>= \ case
  [] -> T.hGetContents stdin >>= work "-"
  [f] -> T.readFile f >>= work f

work _ s = case Parse.run Parse.source 0 s of
  Prelude.Left msg -> error $ "\n" ++ T.unpack msg
  Prelude.Right (i,_) -> do
    print i
    Semantics.solve (Boolean And $ constraints i)
      $ P.Param { P.persons = persons i, P.period = period i }
