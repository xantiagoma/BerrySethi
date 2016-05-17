module DFA where

import Data.Map(Map)
import qualified Data.Map as M
import Data.Set(Set)
import qualified Data.Set as S

--delta :: (Ord q, Ord a) => Map q (Map a q)
--delta = undefined

type Delta q a = Map q (Map a q)
iterar :: (Ord q, Ord a) => Delta q a -> q -> a -> Maybe q
iterar m q cc = do
  m' <- M.lookup q m
  M.lookup cc m'
--iterar m q cc = M.lookup q m  >>= \m' -> M.lookup cc m'

--iterar m a cc = let m' = M.lookup m q
--                in case m' of
--                     Just m'' -> let nq = M.lookup m'' cc
--                                 in nq
--                     Nothing -> Nothing

--runDelta :: (Ord q, Ord a) => Map q (Map a q)
--         -> Set q -> q -> [a] -> Bool
--runDelta m accept q0 xs = undefined

runDelta:: (Ord q, Ord a) => Delta q a -> q -> [a] -> Maybe q
runDelta m q0 [x] = iterar m q0 x
runDelta m q0 (x:xs) = do q' <- iterar m q0 x
                          runDelta m q' xs
--runDelta m q0 (x:xs) = iterar m q0 x >>= \q' -> runDelta m q' xs
-- fold1
