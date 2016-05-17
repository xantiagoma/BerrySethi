module RegExpr(RegExpr(..)) where

import qualified Data.Foldable as F
import Data.Set(Set)
import qualified Data.Set as Set

data RegExpr a = Empty
                | Sym a
                | Con (RegExpr a) (RegExpr a)
                | Union (RegExpr a) (RegExpr a)
                | Kleene (RegExpr a) 
                  deriving (Show)

instance F.Foldable RegExpr where
    foldMap f Empty = mempty
    foldMap f (Sym a) = f a
    foldMap f (Con l r) = F.foldMap f l `mappend` F.foldMap f r
    foldMap f (Union l r) = F.foldMap f l `mappend` F.foldMap f r
    foldMap f (Kleene r) = F.foldMap f r


    --Con (Union (Sym 'a') (Sym 'b')) (Kleene (Sym 'a'))
--pSym = do s <- many alphaNum
--          return $ (s)




