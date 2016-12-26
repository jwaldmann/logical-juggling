{-# language NoMonomorphismRestriction #-}
{-# language FlexibleContexts #-}

module Parse where

import Prelude hiding (Either (..))
import qualified Prelude as P
import Syntax
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec
import Text.Parsec.Expr
import qualified Data.Text as T

instance Read Formula where readsPrec = run formula
instance Read Term where readsPrec = run term

test0 :: Formula
test0 = read "forall throw t : time 3 == end t - begin t"

run p prec s =   
    case runParser ( (,) <$> p <*> getInput ) () "-" s of
      P.Right (x,t) -> [(x,t)]
      P.Left err -> error $ show err

formula = quantified <|> boolean

quantified = Quantified
  <$> quant 
  <*> sort
  <*> ( name <* keyword ":" )
  <*> formula

quant =
 (fromList [("atleast",Atleast),("atmost",Atmost),("exactly",Exactly)]
        <*> number )
  <|> fromList [("forall", Forall), ("exists", Exists)]


boolean = buildExpressionParser
    [ [ prefix "!" (\ x -> Boolean Not [x]) ]
    , [ binary "&&" (\ x y -> Boolean And [x,y]) AssocLeft ]
    , [ binary "||" (\ x y -> Boolean Or  [x,y]) AssocLeft ]
    , [ binary "==>" (\ x y -> Boolean Implies [x,y]) AssocRight ]
    ] ( parens formula <|> atom )

atom = atom1 <|> atom2

atom1 = ( \ r t -> Atom r [t] )
  <$> fromList [ ("pass", Pass), ("self", Self), ("left", Left), ("right",Right)]
  <*> aterm

atom2 = ( \ l op r -> Atom op [l,r] )
  <$> term
  <*> fromList [ ("==" , Eq ), ("!=", Neq)
               , ("<", Lt), (">", Gt)
               , ("<=", Leq), (">=", Geq) ]
  <*> term

sort = fromList
  [ ("throw", Throw), ("place", Place), ("person", Person)
  , ("time", Time), ("hand", Hand)
  ]

term = buildExpressionParser
      [ [ binary "+" (\x y -> Apply Plus [x,y]) AssocLeft 
        , binary "-" (\x y -> Apply Minus [x,y]) AssocLeft
        ]
      ] application

application =  ( Constant <$> sort <*> number )
  <|> ( ( \ f a -> Apply f [a]) <$> function <*> aterm )
  <|> aterm

aterm = parens term 
  <|> ( Ref <$> name )

fromList = foldr1 (<|>) . map (\(s,v) -> keyword s *> return v)

function = fromList
  [ ("next", Next), ("prev", Prev)
  , ("begin", Begin), ("end", End), ("height", Height)
  , ("from", From), ("to", To)
  ]

name = (Name . T.pack)
   <$> ( (:) <$> letter <*> many alphaNum <* spaces)

number =
  ( foldl (\x y -> x*10 + fromEnum y - fromEnum '0') 0 )
  <$> many1 digit <* spaces

parens p = keyword "(" *> p <* keyword ")"

keyword s = try (string s) <* spaces

prefix s f = Prefix ( keyword  s *> return f )
binary s f a = Infix ( keyword s *> return f ) a
