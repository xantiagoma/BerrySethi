module Parser(parseRegExprNumWithTerm, toRegExprNum) where

import Automaton
import RegExpr
import NumSym
import Text.ParserCombinators.Parsec
import Data.Either.Combinators
import Data.Map(Map)
import Data.Set(Set)
import qualified Data.Set  as Set
import qualified Data.Map  as Map

pA :: GenParser Char st Char
pA = do l <- alphaNum
        return $ (l)

parseRegExpr :: SourceName -> String -> Either ParseError (RegExpr Char)
parseRegExpr source "" = parse pEmpty source " "
parseRegExpr source st = parse pRegExpr source st    

pSym :: GenParser Char st (RegExpr Char)
pSym = Sym <$> pA

pEmpty :: GenParser Char st (RegExpr a)
pEmpty = Empty <$ char ' '

pOpKleene :: GenParser Char st (RegExpr Char -> RegExpr Char)
pOpKleene = Kleene <$ char '*' 
         <?> "Expected '*'"

pKleene :: GenParser Char st (RegExpr Char)
pKleene = try (Kleene <$> (pFactor <* char '*'))
  <|> pFactor

pOpUnion :: GenParser Char st (RegExpr Char -> RegExpr Char -> RegExpr Char)
pOpUnion = Union <$ char '|'
         <?> "Expected '|'"

pConcat :: GenParser Char st (RegExpr Char)
pConcat = pKleene `chainl1` pOpConcat

pOpConcat :: GenParser Char st (RegExpr Char -> RegExpr Char -> RegExpr Char)
pOpConcat = Con <$ char '.'
        <?> "Expected '.'"

pRegExpr :: GenParser Char st (RegExpr Char)
pRegExpr = pConcat `chainl1` pOpUnion

pFactor :: GenParser Char st (RegExpr Char)
pFactor = Sym <$> pA
       <|> pParens pRegExpr

pParens = between (char '(') (char ')')

toRegExprNum :: (Int,RegExpr a) -> (Int, RegExpr (NumSym a))
toRegExprNum (i,Empty)      = (i, Sym (NEp) )
toRegExprNum (i,Sym a)      = (i+1, Sym (NS i a) )
toRegExprNum (i, Con l r)   = let (i',l') = toRegExprNum (i,l)
                                  (i'',r') = toRegExprNum (i',r)
                                  in (i'', Con l' r')
toRegExprNum (i, Union l r) = let (i',l') = toRegExprNum (i,l)
                                  (i'',r') = toRegExprNum (i',r)
                                  in (i'', Union l' r')
toRegExprNum (i, Kleene h)  = let (i', h') = toRegExprNum (i,h)
                                  in (i', Kleene h')

parseRegExprNum :: SourceName -> String -> RegExpr (NumSym Char)
parseRegExprNum source st = snd $ toRegExprNum (1,noNum)
                              where noNum = fromRight' (parseRegExpr source st)

parseRegExprNumWithTerm :: SourceName -> String -> RegExpr (NumSym Char)
parseRegExprNumWithTerm source st = Con q (Sym NTerm)
                                      where q = parseRegExprNum source st