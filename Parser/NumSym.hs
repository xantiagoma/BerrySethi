module NumSym(NumSym(..)) where
data NumSym a = NEp
              | NS Int a
              | NTerm

instance (Eq a) => Eq (NumSym a) where
  NEp      == (NS x y)   = False
  NEp      == NEp        = True
  NEp      == NTerm      = False
  (NS x y) == NEp        = False
  (NS x y) == (NS x' y') = x==x' && y==y'
  (NS x y) == NTerm      = False
  NTerm    == NEp        = False
  NTerm    == (NS x y)   = False
  NTerm    == NTerm      = True

instance (Ord a) => Ord (NumSym a) where
  compare NEp      NEp        = EQ
  compare NEp      (NS x y)   = LT
  compare NEp      NTerm      = LT
  compare (NS x y) NEp        = GT
  compare (NS x y) (NS x' y') = compare x x'
  compare (NS x y) NTerm      = LT
  compare NTerm    NEp        = GT
  compare NTerm    (NS x y)   = GT
  compare NTerm    NTerm      = EQ

instance (Show a) => Show (NumSym a) where
  show NEp = "ε"
  show (NS num d) = (show d)++"_"++(show num)
  show NTerm = "˧"