module Test.Generators

import Data.List
import Data.List1
import Data.SOP
import Data.String
import Data.Vect
import public Hedgehog
import public Text.WebIDL.Identifier
import public Text.WebIDL.Numbers
import public Text.WebIDL.StringLit

--------------------------------------------------------------------------------
--          Utilities
--------------------------------------------------------------------------------

linList : Nat -> Gen a -> Gen (List a)
linList n = list (linear 0 n)

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
stringLit = toStringLit <$> linList 15 unicode
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

export
comment : Gen String
comment = choice [line, multiline]
  where noControl : Gen Char
        noControl = map (\c => if isControl c then ' ' else c) unicode

        noForward : Gen Char
        noForward = map (\c => if c == '/' then ' ' else c) unicode

        line : Gen String
        line = map ("//" ++) $ string (linear 0 20) noControl

        multiline : Gen String
        multiline = map (\s => "/*" ++ s ++ "*/")
                  $ string (linear 0 20) noForward

export
latinSymbol : Gen String
latinSymbol = map singleton $ choice [ charc '!' '/'
                                     , charc ':' '@'
                                     , charc '[' '`'
                                     , charc '{' '~'
                                     , charc (chr 161) (chr 255)
                                     ]

--------------------------------------------------------------------------------
--          Parser
--------------------------------------------------------------------------------

-- separator, possibly but not necessarily surrounded by spaces
sep : String -> Gen String
sep s = [| conc (maybe space) (maybe space) |]
  where conc : Maybe String -> Maybe String -> String
        conc a b = fromMaybe "" a ++ s ++ fromMaybe "" b

||| Comma-separated list of identifiers
export
identifiers : Gen (String,List1 Identifier)
identifiers = [| enc identifier (linList 10 $ np [sep ",", identifier]) |]
  where enc :  Identifier
            -> List (NP I [String,Identifier])
            -> (String, List1 Identifier)
        enc i ps =
          let is  = i ::: map (\[_,v] => v) ps
              idl = fastConcat $ i.value :: map (\[s,v] => s ++ v.value) ps
           in (idl,is)
