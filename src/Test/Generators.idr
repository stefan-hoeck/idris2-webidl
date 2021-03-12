module Test.Generators

import Data.List
import Data.String
import Data.Vect
import public Hedgehog
import public Text.WebIDL.Identifier
import public Text.WebIDL.StringLit

--------------------------------------------------------------------------------
--          Tokens
--------------------------------------------------------------------------------

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

toOctDigit : Integer -> Char
toOctDigit 0 = '0'
toOctDigit 1 = '1'
toOctDigit 2 = '2'
toOctDigit 3 = '3'
toOctDigit 4 = '4'
toOctDigit 5 = '5'
toOctDigit 6 = '6'
toOctDigit 7 = '7'
toOctDigit _ = '_'

toHexDigit : Integer -> Char
toHexDigit 8 = '8'
toHexDigit 9 = '9'
toHexDigit 10 = 'A'
toHexDigit 11 = 'B'
toHexDigit 12 = 'C'
toHexDigit 13 = 'D'
toHexDigit 14 = 'E'
toHexDigit 15 = 'F'
toHexDigit n  = toOctDigit n

hexShow : Integer -> String
hexShow n = "0x" ++ fastPack(run n [])
  where run : Integer -> List Char -> List Char
        run n cs = let n'  = n `div` 16
                       cs' = (toHexDigit $ n `mod` 16) :: cs
                    in if n' == 0 then cs' else run n' cs'

octShow : Integer -> String
octShow n = "0" ++ fastPack(run n [])
  where run : Integer -> List Char -> List Char
        run n cs = let n'  = n `div` 8
                       cs' = (toOctDigit $ n `mod` 8) :: cs
                    in if n' == 0 then cs' else run n' cs'

maxInt : Integer
maxInt = 18446744073709551616

export
intLit : Gen (String,Integer)
intLit = choice [decimal,hex,oct]
  where decimal : Gen (String,Integer)
        decimal = map (\n => (show n, n)) . integer
                $ exponentialFrom 0 (-maxInt) maxInt

        hex : Gen (String,Integer)
        hex = map (\n => (hexShow n, n)) . integer $ exponential 0 maxInt

        oct : Gen (String,Integer)
        oct = map (\n => (octShow n, n)) . integer $ exponential 0 maxInt
