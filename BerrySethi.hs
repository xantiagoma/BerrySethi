module BerrySethi(berrySethi) where
import RegExpr
import Automaton
import Parser
import NumSym

import Data.Map(Map)
import Data.Set(Set)
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List as List

type Estado = Set (NumSym Char)
type Arco   = (Estado,Set (Char,Estado))
type Follows = Map (NumSym Char) (Set (NumSym Char))

-- Funciones Para Manejar Tripletas

-- |'fstt' obtiene el primer elemento de una tripleta
fstt :: (a,b,c) -> a
fstt (a,b,c) = a

-- |'sndt' obtiene el segundo elemento de una tripleta
sndt :: (a,b,c) -> b
sndt (a,b,c) = b

-- |'trdt' obtiene el tercer elemento de una tripleta
trdt :: (a,b,c) -> c
trdt (a,b,c) = c


-- Otras Utilidades

-- |'setSetToMapMap' convierte un Set con un Set a un Map con un Map
setSetToMapMap :: Set (Estado, Set (Char, Estado))
                    -> Map Estado (Map Char Estado)
setSetToMapMap ss = convertTuples (convertListTupleSet (Set.toList ss)) 

-- | 'convertTuples' converte una lista de tuplas a un mapa
convertTuples :: [(Estado, Map Char Estado)] -> Map Estado (Map Char Estado)
convertTuples lista = Map.fromList lista

-- | 'convertTupleSet' convierte el segundo elemento (que es una tupla)
--                     de una tupla a un map
convertTupleSet :: (Estado, Set (Char, Estado)) -> (Estado, Map Char Estado)
convertTupleSet (e,a) = (e,setToMap a)

-- | 'convertListTupleSet' convierte los segundos elementos de una lista
--                         de tuplas a mapas
convertListTupleSet :: [(Estado, Set (Char, Estado))]
                      -> [(Estado, Map Char Estado)]
convertListTupleSet listSet = map convertTupleSet listSet

-- | 'setToMap' convierto un conjunto a un mapa
setToMap :: (Ord a) => Set (a,b) -> Map a b
setToMap set = Map.fromList (Set.toList set)

-- | 'getAlphas' convierte una lista de NumSym a una lista de caracteres
getAlphas  :: [NumSym Char] -> [Char]
getAlphas x = noRepeat (getAlphas' x)

-- | 'getAlphas'' metodo auxiliar de getAlphas
getAlphas' :: [NumSym Char] -> [Char]
getAlphas' [] = []
getAlphas' (x:xs) = (getAlpha x):(getAlphas xs)

-- | 'getAlpha' convierte un NumSym a un caracter
getAlpha :: NumSym Char -> Char
getAlpha (NS i l) = l
getAlpha NEp      = '€'
getAlpha NTerm    = '˧'

-- | 'noRepeat' elimina los repetidos de una lista
noRepeat :: (Ord a) => [a] -> [a]
noRepeat x = Set.toList (Set.fromList x)

-- | 'isTerm' dado un estado dice si es terminal
isTerm :: Estado -> Bool
isTerm e = or $ map (\x -> x == NTerm) listaNS 
          where listaNS = Set.toList e

-- | 'groupBy'' agrupa de acuerdo al estado en los arcos
groupBy' :: [Arco] -> [[Arco]]
groupBy' arcos = List.groupBy (\a b -> fst a == fst b) arcos

-- | 'unionTableArcos' usado junto con groupBy' para re-agrupar los Arcos
unionTableArcos :: [[Arco]] -> [Arco]
unionTableArcos [xs] =  (fst (head xs), mergeRightArcos xs):[]
unionTableArcos (xs:xss) = (fst (head xs),
                                  mergeRightArcos xs):(unionTableArcos xss)

-- | 'mergeRightArcos' de una lista de arcos agrupa los segundos elementos
mergeRightArcos :: [Arco] -> Set (Char, Estado)
mergeRightArcos [x] = snd x
mergeRightArcos (x:xs) = Set.union (snd x) (mergeRightArcos xs)

-- | 'unionFb'' realiza la union de los siguientes de b'
unionFb' :: [NumSym Char] -> Follows -> Estado
unionFb' [] sigs = Set.empty
unionFb' (x:xs) sigs = if ((x == NTerm) || (x ==NEp))
                       then Set.empty
                       else Set.union (sigs Map.! x) (unionFb' xs sigs) 

-- | 'filterChar' dado un caracter dice cuales NumSym lo tienen
filterChar ::  Char -> [NumSym Char] -> [NumSym Char]
filterChar c q = filter (checkSym c) q 

-- | 'checkSym' dado un caracter y un NumSym dice si tiene ese el caracter
checkSym :: Char -> NumSym Char -> Bool
checkSym a (NS i b) = a==b
checkSym a NEp = True
checkSym a NTerm = True


-- Algoritmo de Berry-Sethi

-- | 'berrySethi' apartir de una expresion regular numerada obtiene el automata
berrySethi :: RegExpr (NumSym Char) -> Automaton Estado Char
berrySethi e' = Automaton {
                  state = losEstados,
                  sigma = elABCDario,
                  delta = elDelta,
                  initial = elInicial,
                  accepting = losDeTerm
                }
                where
                  mQ         = (Set.empty ,Set.singleton (inicial e'),
                                                                    Set.empty)
                  bs         = (qM lossig mQ)
                  q          = Set.elemAt 0 (sndt mQ)
                  lossig     = siguientes (dig e')
                  elDelta    = setSetToMapMap (trdt bs)
                  elInicial  = inicial e'
                  losEstados = fstt bs
                  elABCDario = Set.fromList
                    $ getAlphas (concat (map Set.toList
                                          (Set.toList (fstt (qM lossig mQ)))))
                  losDeTerm  = Set.fromList $ filter isTerm (Set.toList
                                                                  losEstados)

-- | 'qM' mientras existan estados q que no han sido visitados en Q
qM :: Follows -> (Set Estado, Set Estado, Set Arco)
                                          -> (Set Estado, Set Estado, Set Arco)
qM sig mQ = if Set.null (sndt mQ)
          then mQ
          else qM sig mQ'
            where mQ' = q' (getAlphas (Set.toList
                                              (Set.elemAt 0 (sndt mQ)))) sig mQ

-- | 'q'' por cada caracter b perteneciente a sigma
q' :: [Char] -> Follows -> (Set Estado, Set Estado, Set Arco)
                                          -> (Set Estado, Set Estado, Set Arco)
q' [] _ mQ = (Set.union (fstt mQ) (Set.singleton fstNV),
                                             Set.deleteAt 0 (sndt mQ), tercero)
              where fstNV = Set.elemAt 0 (sndt mQ)
                    tercero = Set.fromList (unionTableArcos
                                             (groupBy' (Set.toList (trdt mQ))))
q' (b:bs) fol mQ = if Set.member q'' (fstt mQ) || Set.member q'' (sndt mQ)
                      then q' bs fol (fstt mQ, sndt mQ, Set.union (trdt mQ)
                                (Set.singleton (fstNV, Set.singleton (b,q''))) )
                      else q' bs fol (fstt mQ,
                                        Set.union (sndt mQ) (Set.singleton q''),
                                        Set.union (trdt mQ)
                                            (Set.singleton
                                                (fstNV, Set.singleton (b,q''))))
                        where q'' = unionFb'
                                      (filterChar b (Set.toList fstNV)) fol
                              fstNV = Set.elemAt 0 (sndt mQ)


-- | 'inicial' calcula los estados iniciales de una expresion regular
inicial :: (Ord a) => RegExpr a -> (Set a)
inicial Empty        = Set.empty
inicial (Sym a)      = Set.fromList (a:[])
inicial (Union e e') = Set.union (inicial e) (inicial e')
inicial (Con e e')   = if nulo(e)
                        then Set.union (inicial e) (inicial e')
                        else (inicial e)
inicial (Kleene e)   = inicial e

-- | 'siguientes' calcula los estados siguientes de un set de tuplas de estados
siguientes :: (Ord a) => Set (a,a) -> Map.Map a (Set a)
siguientes s = Set.foldr f Map.empty s where
               f (k,v) = Map.insertWith (Set.union) k (Set.singleton v)

-- | 'dig' obtiene los digrafos de una expresion regular
dig :: (Ord a) => RegExpr a -> Set (a,a)
dig Empty        = Set.empty
dig (Sym a)      = Set.empty
dig (Union e e') = Set.union (dig e) (dig e')
dig (Con e e')   = Set.union (dig e) (Set.union (dig e')
                                        (cartesian (final(e)) (inicial(e'))))
dig (Kleene e)   = Set.union (dig e) (cartesian (final(e)) (inicial(e)))

-- | 'ab' obtener el abcedario                     
ab :: (Ord a) => RegExpr a -> Set a
ab (Con a b) = Set.union (ab a) (ab b)
ab (Union a b) = Set.union (ab a) (ab b)
ab (Kleene a) = ab a
ab (Sym a) = Set.fromList (a:[])

-- | 'nulo' dice si una expreison regular es nula
nulo :: RegExpr a -> Bool
nulo Empty        = True
nulo (Sym a)      = False
nulo (Con e e')   = (nulo e) && (nulo e')
nulo (Union e e') = (nulo e) || (nulo e')
nulo (Kleene e)   = True
-- | 'cartesian' hacer el producto cartesiano entre dos conjuntos
cartesian :: (Ord a, Ord b) => Set a -> Set b -> Set (a,b)
cartesian x y = Set.fromList [(i,j)| i <- (Set.toList x), j <- (Set.toList y) ] 

-- | 'final' da los estados finales de una expresion regular
final :: (Ord a) => RegExpr a -> (Set a)
final Empty = Set.empty
final (Sym a) = Set.fromList (a:[])
final (Union e e') = Set.union (final e) (final e')
final (Con e e')  = if nulo(e')
                      then Set.union (final e) (final e')
                      else (final e')
final (Kleene e) = final e

-- | 'fin' wrapper de la anterior
fin :: (Ord a) => RegExpr a -> Set (NumSym a)
fin reg = final (snd $ (toRegExprNum (1,reg)))

-- TEST

-- | 'match' dado un patron y una cadena verifica si cumple con el patron
match :: String -> String -> Bool
match pattern subject = runAutomaton (berrySethi
                            (parseRegExprNumWithTerm "unknown" pattern))
                            subject