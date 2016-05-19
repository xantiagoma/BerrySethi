module Parser(parseRegExpr) where

import RegExpr
import NumSym
import Text.ParserCombinators.Parsec
import Data.Either.Combinators
import Data.Set(Set)
import qualified Data.Set as Set


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

pParens = between(char '(') (char ')')

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

nulo :: RegExpr a -> Bool
nulo Empty        = True --PREGUNTAR
nulo (Sym a)      = False
nulo (Con e e')   = (nulo e) && (nulo e')
nulo (Union e e') = (nulo e) || (nulo e')
nulo (Kleene e)   = True

inicial :: (Ord a) => RegExpr a -> (Set a)
inicial Empty        = Set.empty
inicial (Sym a)      = Set.fromList (a:[])
inicial (Union e e') = Set.union (inicial e) (inicial e')
inicial (Con e e')   = if nulo(e)
                        then Set.union (inicial e) (inicial e')
                        else (inicial e)
inicial (Kleene e)   = inicial e

final :: (Ord a) => RegExpr a -> (Set a)
final Empty = Set.empty
final (Sym a) = Set.fromList (a:[])
final (Union e e') = Set.union (final e) (final e')
final (Con e e')  = if nulo(e')
                      then Set.union (final e) (final e')
                      else (final e')
final (Kleene e) = final e

cartesian :: (Ord a, Ord b) => Set a -> Set b -> Set (a,b)
cartesian x y = Set.fromList [(i,j)| i <- (Set.toList x), j <- (Set.toList y) ] 


dig :: (Ord a, Ord b) => RegExpr a -> (Set (a,b))
dig Empty        = cartesian Set.empty Set.empty
dig (Sym a)      = cartesian Set.empty Set.empty
dig (Union e e') = Set.union (dig e) (dig e')
dig (Con e e')   = Set.union (dig e) (cartesian final(e) inicial(e'))
                     