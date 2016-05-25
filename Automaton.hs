module Automaton(Automaton(..),
                 runAutomaton) where

import Data.Set(Set)
import Data.Map(Map)
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
    
data Automaton q a = Automaton { state :: Set q,
                                 sigma :: Set a,
                                 delta :: Map q (Map a q),
                                 initial :: q,
                                 accepting :: Set q
                               }
                   deriving (Show,Read)

type Delta q a = Map q (Map a q)
iterar :: (Ord q, Ord a) => Delta q a -> q -> a -> Maybe q
iterar m q cc = do
  m' <- Map.lookup q m
  Map.lookup cc m'

runDelta:: (Ord q, Ord a) => Delta q a -> q -> [a] -> Maybe q
runDelta m q0 [] = Just q0
runDelta m q0 [x] = iterar m q0 x
runDelta m q0 (x:xs) = do q' <- iterar m q0 x
                          runDelta m q' xs

runAutomaton :: (Ord a, Ord q) => Automaton q a -> [a] -> Bool
runAutomaton auto xs = 
  if Maybe.isNothing $ result
    then False
    else Set.member (Maybe.fromJust result) (accepting auto) 
    where
      result = runDelta (delta auto) (initial auto) xs
