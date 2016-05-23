module BerrySethi(berrySethi) where
import RegExpr
import Automaton
import Parser
import NumSym

import Data.Map(Map)
import Data.Set(Set)
import qualified Data.Set as Set
import qualified Data.Map as Map

berrySethi :: (Ord a) => RegExpr a -> Automaton (Set (NumSym a)) a
berrySethi reg = undefined