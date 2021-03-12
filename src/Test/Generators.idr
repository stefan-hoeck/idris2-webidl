module Test.Generators

import Data.List
import Data.String
import Data.Vect
import public Hedgehog
import public Text.WebIDL.Identifier
import public Text.WebIDL.StringLit

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

export
space : Gen String
space = string (linear 1 5) (element [' ','\t','\n','\r'])

export
stringLit : Gen StringLit
stringLit = toStringLit <$> list (linear 0 15) unicode
  where toStringLit : List Char -> StringLit
        toStringLit cs = MkStringLit . (++ "\"") . fastPack 
                       $ '"' :: filter (not . (== '"')) cs
