-- We refrain from doing anything fancy with type aliases and composed
-- applicatives here. This may make generators slightly more verbose,
-- but it should be clear from their definitions, how WebIDL strings and
-- the corresponding data types are being built up.
module Test.Generators

import Data.List
import Data.List.Elem
import Data.String
import Data.Vect
import public Hedgehog
import public Text.WebIDL.Types

--------------------------------------------------------------------------------
--          Utilities
--------------------------------------------------------------------------------

-- list whose length scales linearily and shrinks towards 0
linList : Nat -> Gen a -> Gen (List a)
linList n = list (linear 0 n)

-- string whose length scales linearily and shrinks towards 0
linString : Nat -> Gen Char -> Gen String
linString n = string (linear 0 n)

-- non-empty string whose length scales linearily and shrinks towards 1
linString1 : Nat -> Gen Char -> Gen String
linString1 n = string (linear 1 n)

maxInt : Integer
maxInt = 18446744073709551616

anyInt : Gen Integer
anyInt = integer $ exponentialFrom 0 (- maxInt) maxInt

posInt : Gen Integer
posInt = integer $ exponential 0 maxInt

nat : Gen Nat
nat = map fromInteger posInt

pairFst : Functor f => (a -> b) -> f a -> f (a,b)
pairFst g = map (\a => (a, g a))

pairSnd : Functor f => (a -> b) -> f a -> f (b,a)
pairSnd g = map (\a => (g a, a))

--------------------------------------------------------------------------------
--          Tokens
--------------------------------------------------------------------------------

export
identifier : Gen (String,Identifier)
identifier = pairFst MkIdent [| concIdent (maybe line) alpha rest |]
  where line : Gen Char
        line = element ['-','_']

        rest : Gen String
        rest = linString 15 (frequency [(20, alphaNum),(1,line)])

        concIdent : Maybe Char -> Char -> String -> String
        concIdent mc c r = maybe "" singleton mc ++ singleton c ++ r

export
space : Gen String
space = linString1 5 (element [' ','\t','\n','\r'])

export
maybeSpace : Gen String
maybeSpace = frequency [(1, pure ""), (4, space)]

export
stringLit : Gen (String,StringLit)
stringLit = pairFst MkStringLit $ toStringLit <$> linList 15 unicode
  where toStringLit : List Char -> String
        toStringLit cs = (++ "\"") . fastPack 
                       $ '"' :: filter (not . (== '"')) cs

export
intLit : Gen (String,Integer)
intLit = choice [ pairSnd show anyInt
                , pairSnd (toHex . fromInteger) posInt
                , pairSnd (toOct . fromInteger) posInt
                ]

export
floatLit : Gen (String,FloatLit)
floatLit = pairSnd toFloatLit float
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
symbol : Gen (String,Symbol)
symbol = frequency [ (10, map (\c => (singleton c, Symb c)) latinSymbol)
                   , (1, pure ("...", Ellipsis))
                   ]
  where latinSymbol : Gen Char
        latinSymbol = choice [ charc '!' '/'
                             , charc ':' '@'
                             , charc '[' '`'
                             , charc '{' '~'
                             , charc (chr 161) (chr 255)
                             ]

symbolUnless : (Char -> Bool) -> Gen (String,Symbol)
symbolUnless f = map replace symbol
  where replace : (String,Symbol) -> (String,Symbol)
        replace (s,Symb c) = if f c then ("@",Symb '@') else (s,Symb c)
        replace p          = p

--------------------------------------------------------------------------------
--          Parser
--------------------------------------------------------------------------------

-- separator, possibly but not necessarily surrounded by spaces
sep : String -> Gen String
sep s = [| (\a,b => a ++ s ++ b) maybeSpace maybeSpace |]

between : String -> String -> Gen (String,a) -> Gen (String,a)
between l r g = [| comb (sep l) (sep r) g |]
  where comb : String -> String -> (String,a) -> (String,a)
        comb sl sr (s,a) = (sl ++ s ++ sr, a)

inParens : Gen (String,a) -> Gen (String,a)
inParens = between "(" ")"

inBrackets : Gen (String,a) -> Gen (String,a)
inBrackets = between "[" "]"

inBraces : Gen (String,a) -> Gen (String,a)
inBraces = between "{" "}"

inAnyParens : Gen (String,a) -> Gen (String,a)
inAnyParens g = choice [ inParens g, inBrackets g, inBraces g ]

-- non-empty list of encoded values, separated by the given
-- separator. The separator can be pre- or postfixed by arbitrary
-- whitespace.
sepList1 : Nat -> String -> Gen (String,a) -> Gen (String,List1 a)
sepList1 n s g = [| enc g (linList n gsep) |]
  where enc : (String,a) -> List (String,a) -> (String, List1 a)
        enc (s1,a1) ps = let (ss,as) = unzip ps
                          in (fastConcat $ s1 :: ss, a1 ::: as)

        prependSep : String -> (String,a) -> (String,a)
        prependSep s = mapFst (s ++)

        gsep : Gen (String,a)
        gsep = [| prependSep (sep s) g |] 

sepListNonEmpty : Nat -> String -> Gen (String,a) -> Gen (String,List a)
sepListNonEmpty n s = map (mapSnd forget) . sepList1 n s

sepList : Nat -> String -> Gen (String,a) -> Gen (String,List a)
sepList n s g =   fromMaybe ("",[]) <$> maybe (sepListNonEmpty n s g)

||| Comma-separated list of identifiers
export
identifiers : Gen (String, IdentifierList)
identifiers = sepList1 10 "," identifier

otherUnless : (Char -> Bool) -> Gen (String,Other)
otherUnless f = choice [ map (mapSnd \v => inject v) identifier
                       , map (mapSnd \v => inject v) intLit
                       , map (mapSnd \v => inject v) stringLit
                       , map (mapSnd \v => inject v) floatLit
                       , map (mapSnd \v => inject v) (symbolUnless f)
                       ]

export
otherOrComma : Gen (String,Other)
otherOrComma = otherUnless isParenOrQuote

export
other : Gen (String,Other)
other = otherUnless isCommaOrParenOrQuote

eaInner : Nat -> Gen (String, EAInner)
eaInner 0 = pure ("", EAIEmpty)
eaInner (S k) =
   frequency [ (1, eaInner 0)
             , (2, [| combOther otherOrComma space (eaInner k) |])
             , (2, [| combParens (inAnyParens $ eaInner k) (eaInner k) |])
             ]
  where combOther : (String,Other) -> String -> (String,EAInner) -> (String,EAInner)
        combOther (so,o) p (sa,a) = (so ++ p ++ sa, EAIOther o a)

        combParens : (String,EAInner) -> (String,EAInner) -> (String,EAInner)
        combParens (sa,a) (sb,b) = (sa ++ sb, EAIParens a b)

extAttribute : Nat -> Gen (String, ExtAttribute)
extAttribute 0 =
  choice [ map (\(s,i) => (s, EAParens i Nothing)) $ inAnyParens (eaInner 0)
         , map (\(s,o) => (s, EAOther o Nothing)) other ]

extAttribute (S k) =
  choice [ [| combParens (inAnyParens $ eaInner k) rest |]
         , [| combOther other space rest |]
         ]

  where rest : Gen (String,Maybe ExtAttribute)
        rest = map (\case Just (s,a) => (s, Just a)
                          Nothing    => ("", Nothing)) (maybe $ extAttribute k)

        combOther : (String,Other) -> String -> (String,Maybe ExtAttribute) -> (String,ExtAttribute)
        combOther (so,o) p (sa,a) = (so ++ p ++ sa, EAOther o a)

        combParens : (String,EAInner) -> (String,Maybe ExtAttribute) -> (String,ExtAttribute)
        combParens (sa,a) (sb,b) = (sa ++ sb, EAParens a b)

export
extAttributes : Gen (String, ExtAttributeList)
extAttributes = inBrackets . sepList 6 "," $ extAttribute 3
