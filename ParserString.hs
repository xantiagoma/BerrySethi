module ParserString where

import RegExpr
import Text.ParserCombinators.Parsec



pA :: GenParser Char st String
pA = do l <- alphaNum
        ls <- many alphaNum
        return $ (l:ls)

parseRegExpr :: SourceName -> String -> Either ParseError (RegExpr String)
parseRegExpr source st = parse pRegExpr source st    

pSym :: GenParser Char st (RegExpr String)
pSym = Sym <$> pA

pKleene :: GenParser Char st (RegExpr String)
pKleene = try (Kleene <$> (pFactor <* char '*'))
  <|> pFactor

pOpUnion :: GenParser Char st
                        (RegExpr String -> RegExpr String -> RegExpr String)
pOpUnion = Union <$ char '|'
         <?> "Expected '|'"

pConcat :: GenParser Char st (RegExpr String)
pConcat = pKleene `chainl1` pOpConcat

pOpConcat :: GenParser Char st
                        (RegExpr String -> RegExpr String -> RegExpr String)
pOpConcat = Con <$ char '.'
        <?> "Expected '.'"

pRegExpr :: GenParser Char st (RegExpr String)
pRegExpr = pConcat `chainl1` pOpUnion

pFactor :: GenParser Char st (RegExpr String)
pFactor = Sym <$> pA
       <|> pParens pRegExpr

pParens = between(char '(') (char ')')