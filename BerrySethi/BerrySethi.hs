module BerrySethi (berrySethi) where

import RegExpr
import Automaton
import Data.Map(Map)
import Data.Set(Set)
import qualified Data.Map as Map
import qualified Data.Set as Set

berrySethi :: (Ord a) => RegExpr a -> Automaton (Set (NumSym a)) a
berrySethi reg = undefined
