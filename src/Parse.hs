{-# language NoMonomorphismRestriction #-}
{-# language FlexibleContexts #-}
{-# language OverloadedStrings #-}

module Parse where

import Prelude hiding (Either (..))
import qualified Prelude as P
import Syntax
import Text.Parsec
import Text.Parsec.Text
import Text.Parsec.Char
import Text.Parsec.Expr
import Text.Parsec.Pos
import Text.Parsec.Error
import qualified Data.Text as T
import Control.Monad.Except
import Data.Monoid



instance Read Formula where
  readsPrec prec s = force $ run formula prec $ T.pack s
instance Read Term where
  readsPrec prec s = force $ run term prec $ T.pack s

force e = case e of
  P.Right (x,s) -> [(x,s)]
  P.Left err -> error $ T.unpack err

test0 :: Formula
test0 = read "forall throw t : time 3 == end t - begin t"

run :: Parser a -> Int -> T.Text -> P.Either T.Text (a, String)
run p prec s =   
    case runParser ( (,) <$> (whitespace *> p) <*> getInput ) () "-" s of
      P.Right (x,t) -> return (x,T.unpack t)
      P.Left err ->
        let pos = errorPos err
            (pre, this: post) = splitAt (sourceLine pos - 1)
               $  T.lines s
            under = T.replicate (sourceColumn pos - 1) "~" <> "^"
        in  throwError $ T.unlines $
         [  this, under , T.pack $ show err ]

data Source =
  Source { persons :: Int, period :: Int , constraints :: [Formula] }
  deriving Show

source = Source
 <$> ( keyword "persons" *> number )
 <*> ( keyword "period" *> number )
 <*> many formula
 <* eof

formula = quantified <|> boolean

quantified = Quantified
  <$> quant 
  <*> sort
  <*> ( Parse.name <* keyword ":" )
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
  <|> ( Ref <$> Parse.name )

fromList = foldr1 (<|>) . map (\(s,v) -> keyword s *> return v)

function = fromList
  [ ("next", Next), ("prev", Prev)
  , ("begin", Begin), ("end", End), ("height", Height)
  , ("from", From), ("to", To)
  ]

name = (Syntax.name . T.pack)
   <$> ( (:) <$> letter <*> many alphaNum <* whitespace)

number =
  ( foldl (\x y -> x*10 + fromEnum y - fromEnum '0') 0 )
  <$> many1 digit <* whitespace

parens p = keyword "(" *> p <* keyword ")"

keyword s = try (string s) <* whitespace

prefix s f = Prefix ( keyword  s *> return f )
binary s f a = Infix ( keyword s *> return f ) a

whitespace = many $ void space
  <|> void ( try (string "//") >> manyTill anyChar endOfLine )
  <|> void ( try (string "/*") >> manyTill anyChar (try $ string "*/") )
