module Text.WebIDL.Types.Numbers

import Data.String
import Data.List1
import Data.Nat

import Generics.Derive

%language ElabReflection

||| The default Eq for Nat runs in O(n), which is too slow
||| if we want to support large natlits in our tests.
export
[FastNatEq] Eq Nat where
  (==) = (==) `on` natToInteger

--------------------------------------------------------------------------------
--          IntLit
--------------------------------------------------------------------------------

public export
data IntLit = Hex Nat | Oct Nat | I Integer

%runElab derive "IntLit" [Generic,Meta,Show]

export
Eq IntLit using FastNatEq where (==) = genEq

--------------------------------------------------------------------------------
--          Parsing Integers
--------------------------------------------------------------------------------

digitToInt : Char -> Integer
digitToInt c = cast $ if isDigit c
                         then ord c - ord '0'
                         else ord (toUpper c) - ord 'A' + 10

charsToPosInt : (base : Integer) -> List Char -> Maybe Integer
charsToPosInt base t = calc <$> traverse readDigit t
  where readDigit : Char -> Maybe Integer
        readDigit c = let d = digitToInt c
                       in if d >= 0 && d < base then Just d else Nothing

        calc : List Integer -> Integer
        calc = foldl (\a,e => a * base + e) 0

export
readInt : String -> Maybe Integer
readInt s = case fastUnpack s of
                 '0'::'x'::t => charsToPosInt 16 t
                 '0'::'X'::t => charsToPosInt 16 t
                 '0'::t      => charsToPosInt 8  t
                 '-'::t      => negate <$> charsToPosInt 10 t
                 t           => charsToPosInt 10 t

--------------------------------------------------------------------------------
--          Floating Point Literals
--------------------------------------------------------------------------------

public export
data Signum = Plus | Minus

%runElab derive "Signum" [Generic,Meta,Eq,Show]

public export
data FloatLit : Type where
  Exp :  (signum    : Signum)
      -> (beforeDot : Nat)
      -> (afterDot  : Maybe Nat)
      -> (exp       : Integer)
      -> FloatLit

  NoExp :  (signum    : Signum)
        -> (beforeDot : Nat)
        -> (afterDot  : Nat)
        -> FloatLit

  Infinity         : FloatLit

  NegativeInfinity : FloatLit

  NaN              : FloatLit

%runElab derive "FloatLit" [Generic,Meta,Show]

export
Eq FloatLit using FastNatEq where
  (==) = genEq

--------------------------------------------------------------------------------
--          Encoding Floats
--------------------------------------------------------------------------------

sig : Signum -> String
sig Plus  = ""
sig Minus = "-"

export
toFloatLit : FloatLit -> String
toFloatLit Infinity         = "Infinity"
toFloatLit NegativeInfinity = "-Infinity"
toFloatLit NaN              = "NaN"
toFloatLit (Exp s bd ad exp) =
  fastConcat [sig s,show bd,maybe "" (("." ++) . show) ad,"e",show exp]
toFloatLit (NoExp s bd ad) =
  fastConcat [sig s,show bd,".",show ad]

--------------------------------------------------------------------------------
--          Parsing Floats
--------------------------------------------------------------------------------

export
charsToNat : List Char -> Maybe Nat
charsToNat = map fromInteger . charsToPosInt 10

beforeDot : List Char -> (Signum,Maybe Nat)
beforeDot ('-'::cs) = (Minus,charsToNat cs)
beforeDot cs        = (Plus, charsToNat cs)

exp : String -> Maybe String -> String -> Maybe FloatLit
exp bds ads es =
  let (s,bdNat) = beforeDot (fastUnpack bds)
   in do bd <- bdNat
         ad <- maybe (Just Nothing) (map Just . charsToNat . fastUnpack) ads
         e  <- afterExp (fastUnpack es)
         pure $ Exp s bd ad e
        
  where afterExp : List Char -> Maybe Integer
        afterExp ('-'::cs) = negate <$> charsToPosInt 10 cs
        afterExp ('+'::cs) = charsToPosInt 10 cs
        afterExp cs        = charsToPosInt 10 cs

noExp : String -> String -> Maybe FloatLit
noExp bds ads =
  let (s,bdNat) = beforeDot $ fastUnpack bds
   in do bd <- bdNat
         ad <- charsToNat $ fastUnpack ads
         pure $ NoExp s bd ad


export
readFloat : String -> Maybe FloatLit
readFloat s =
  case split (('E' ==) . toUpper) s of
    -- with exponent, dot is optional
    h ::: [e] => case split ('.' ==) h of
      h2 ::: [t2] => exp h2 (Just t2) e
      h2 ::: []   => exp h2 Nothing e
      _  ::: _    => Nothing

    -- without exponent, dot is mandatory
    -- (otherwise it is an integer)
    h ::: [] => case split ('.' ==) h of
      h2 ::: [t2] => noExp h2 t2
      _  ::: _    => Nothing

    -- more than one E in string
    _ ::: _  => Nothing
