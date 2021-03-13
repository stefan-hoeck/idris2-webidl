module Test.Generators

import Data.List
import Data.String
import Data.Vect
import public Hedgehog
import public Text.WebIDL.Identifier
import public Text.WebIDL.Numbers
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

maxInt : Integer
maxInt = 18446744073709551616

posInt : Gen Integer
posInt = integer $ exponential 0 maxInt

nat : Gen Nat
nat = map fromInteger . integer $ exponential 0 maxInt

export
intLit : Gen (String,Integer)
intLit = choice [decimal,hex,oct]
  where decimal : Gen (String,Integer)
        decimal = map (\n => (show n, n)) . integer
                $ exponentialFrom 0 (-maxInt) maxInt

        hex : Gen (String,Integer)
        hex = map (\n => (toHex n, natToInteger n)) nat

        oct : Gen (String,Integer)
        oct = map (\n => (toOct n, natToInteger n)) nat

export
floatLit : Gen (String,FloatLit)
floatLit = map (\fl => (toFloatLit fl, fl)) float
  where exp : Gen Integer
        exp = integer $ linearFrom 0 (-30) (30)

        sig : Gen Signum
        sig = element [Plus,Minus]

        float : Gen FloatLit
        float = frequency [ (5, [| Exp sig nat (maybe nat) exp |])
                          , (5, [| NoExp sig nat nat |])
                          , (1, pure Infinity)
                          , (1, pure NegativeInfinity)
                          , (1, pure NaN)
                          ]
