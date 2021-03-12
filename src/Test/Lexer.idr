module Test.Lexer

import Data.String
import Data.Vect
import Text.Lexer
import Text.WebIDL.Identifier
import Text.WebIDL.Lexer
import Hedgehog

lex : String -> Either String (List IdlToken)
lex s = map (map tok) $ lexIdl s

export
identifier : Gen Identifier
identifier = [| concIdent (maybe line) alpha rest |]
  where line : Gen Char
        line = element ['-','_']

        rest : Gen String
        rest = string (linear 0 15) (frequency [(20, alphaNum),(1,line)])

        concIdent : Maybe Char -> Char -> String -> Identifier
        concIdent mc c r =  MkIdent 
                         $  maybe "" singleton mc
                         ++ singleton c
                         ++ r

prop_identifier : Property
prop_identifier = property $ do
                    (MkIdent i) <- forAll identifier
                    lex i === Right [Ident $ MkIdent i]

export
props : Group
props = MkGroup "Lexer Properties" [
          ("prop_identifier", prop_identifier)
        ]
