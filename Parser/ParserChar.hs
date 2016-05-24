module Parser(parseRegExpr) where

import Automaton
import RegExpr
import NumSym
import Text.ParserCombinators.Parsec
import Data.Either.Combinators
import Data.Map(Map)
import Data.Set(Set)
import qualified Data.Set  as Set
import qualified Data.Map  as Map
import qualified Data.List as List

fstt :: (a,b,c) -> a
fstt (a,b,c) = a

sndt :: (a,b,c) -> b
sndt (a,b,c) = b

trdt :: (a,b,c) -> c
trdt (a,b,c) = c

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

parseRegExprNumWithTerm :: SourceName -> String -> RegExpr (NumSym Char)
parseRegExprNumWithTerm source st = Con q (Sym NTerm)
                                      where q = parseRegExprNum source st

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


dig :: (Ord a) => RegExpr a -> Set (a,a)
dig Empty        = Set.empty
dig (Sym a)      = Set.empty
dig (Union e e') = Set.union (dig e) (dig e')
dig (Con e e')   = Set.union (dig e) (Set.union (dig e') (cartesian (final(e)) (inicial(e'))))
dig (Kleene e)   = Set.union (dig e) (cartesian (final(e)) (inicial(e)))
                     
siguientes :: (Ord a) => Set (a,a) -> Map.Map a (Set a)
siguientes s = Set.foldr f Map.empty s where
               f (k,v) = Map.insertWith (Set.union) k (Set.singleton v)


berrySethi :: (Ord a) => RegExpr a -> Automaton Estado Char
berrySethi reg = undefined
q0 :: (Ord a) => RegExpr a -> Set (NumSym a)
q0 reg = inicial (snd $ (toRegExprNum (1,reg)))


fin :: (Ord a) => RegExpr a -> Set (NumSym a)
fin reg = final (snd $ (toRegExprNum (1,reg)))


ab :: (Ord a) => RegExpr a -> Set a
ab (Con a b) = Set.union (ab a) (ab b)
ab (Union a b) = Set.union (ab a) (ab b)
ab (Kleene a) = ab a
ab (Sym a) = Set.fromList (a:[])

inLoop :: (Ord a) => RegExpr a -> Set (NumSym a, NumSym a)
inLoop = undefined


type Estado = Set (NumSym Char)
type Arco   = (Estado,Set (Char,Estado))
type Follows = Map (NumSym Char) (Set (NumSym Char))

checkSym :: Char -> NumSym Char -> Bool
checkSym a (NS i b) = a==b
checkSym a NEp = True
checkSym a NTerm = True

getAlphas  :: [NumSym Char] -> [Char]
getAlphas x = Set.toList (Set.fromList (getAlphas' x))

getAlphas' :: [NumSym Char] -> [Char]
getAlphas' [] = []
getAlphas' (x:xs) = (getAlpha x):(getAlphas xs)

getAlpha :: NumSym Char -> Char
getAlpha (NS i l) = l
getAlpha NEp      = '€'
getAlpha NTerm    = '˧'

qM :: Follows -> (Set Estado, Set Estado, Set Arco) -> (Set Estado, Set Estado, Set Arco)
qM sig mQ = if Set.null (sndt mQ)
          then mQ
          else qM sig mQ'
            where mQ' = q' (getAlphas (Set.toList (Set.elemAt 0 (sndt mQ)))) sig mQ

q' :: [Char] -> Follows -> (Set Estado, Set Estado, Set Arco) -> (Set Estado, Set Estado, Set Arco)
q' [] _ mQ = (Set.union (fstt mQ) (Set.singleton fstNV),Set.deleteAt 0 (sndt mQ), tercero)
              where fstNV = Set.elemAt 0 (sndt mQ)
                    tercero = Set.fromList (unionTableArcos (groupBy' (Set.toList (trdt mQ))))
q' (b:bs) fol mQ = if Set.member q'' (fstt mQ) || Set.member q'' (sndt mQ)
                      then q' bs fol (fstt mQ, sndt mQ, Set.union (trdt mQ) (Set.singleton (fstNV, Set.singleton (b,q''))) ) --Agregar Arco
                      else q' bs fol (fstt mQ, Set.union (sndt mQ) (Set.singleton q''),
                                              Set.union (trdt mQ) (Set.singleton (fstNV, Set.singleton (b,q''))))
                        where q'' = unionFb' (filterChar b (Set.toList fstNV)) fol
                              fstNV = Set.elemAt 0 (sndt mQ)

filterChar ::  Char -> [NumSym Char] -> [NumSym Char]
filterChar c q = filter (checkSym c) q 

unionFb' :: [NumSym Char] -> Follows -> Estado
unionFb' [] sigs = Set.empty
unionFb' (x:xs) sigs = if ((x == NTerm) || (x ==NEp))
                       then Set.empty
                       else Set.union (sigs Map.! x) (unionFb' xs sigs) 

groupBy' :: [Arco] -> [[Arco]]
groupBy' arcos = List.groupBy (\a b -> fst a == fst b) arcos

mergeRightArcos :: [Arco] -> Set (Char, Estado)
mergeRightArcos [x] = snd x
mergeRightArcos (x:xs) = Set.union (snd x) (mergeRightArcos xs)

unionTableArcos :: [[Arco]] -> [Arco]
unionTableArcos [xs] =  (fst (head xs), mergeRightArcos xs):[]
unionTableArcos (xs:xss) = (fst (head xs), mergeRightArcos xs):(unionTableArcos xss)


--Vars
e' = parseRegExprNumWithTerm   "" "(a|b.b)*.(a.c).(a.c)*"
mQ = (Set.empty, Set.singleton (inicial e'), Set.empty)
q = Set.elemAt 0 (sndt mQ)
