module Text.WebIDL.Types.Numbers

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

||| An integer literal in hexadecimal, octal, or decimal representation.
||| The code generator will use the same representation when
||| generating code for constants and default values.
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

||| Tries to read an integer literal from a `String`.
export
readInt : String -> Maybe IntLit
readInt s = case fastUnpack s of
                 '0'::'x'::t => map (Hex . fromInteger) $ charsToPosInt 16 t
                 '0'::'X'::t => map (Hex . fromInteger) $ charsToPosInt 16 t
                 '0'::t      => map (Oct . fromInteger) $ charsToPosInt 8  t
                 '-'::t      => I . negate <$> charsToPosInt 10 t
                 t           => map I $ charsToPosInt 10 t

--------------------------------------------------------------------------------
--          Floating Point Literals
--------------------------------------------------------------------------------

||| The sign of a floating point literal.
public export
data Signum = Plus | Minus

%runElab derive "Signum" [Generic,Meta,Eq,Show]

||| A parsed floating point literal.
|||
||| A floating point literal is either one of three
||| special values (`NaN`, `Infinity`, or `-Infinity`)
||| or a decimal floating point number (`NoExp`: dot is
||| mandatory), or a float in scientific notation (`Exp`: 
||| dot is optional).
|||
||| The main focus of this data type is one of
||| preserving information. Encoding a `FloatLit` should
||| yield (almost) exactly the same literal as the one
||| encountered during parsin with two minor exceptions:
||| a) The encoded literal will always use a lowercase 'e' as
||| the delimiter for the exponent and b) in case of a
||| positive exponent, there will not be a '+' in the
||| encoded literal.
public export
data FloatLit : Type where
  ||| Floating point number in scientific notation.
  |||
  ||| Example: `-12.10e10`
  Exp :  (signum    : Signum)
      -> (beforeDot : Nat)
      -> (afterDot  : Maybe Nat)
      -> (exp       : Integer)
      -> FloatLit

  ||| Floating point number without exponent.
  |||
  ||| Example: `-12.1002`
  NoExp :  (signum    : Signum)
        -> (beforeDot : Nat)
        -> (afterDot  : Nat)
        -> FloatLit

  ||| Corresponds to the WebIDL keyword `Infinity`
  Infinity         : FloatLit

  ||| Corresponds to the WebIDL keyword `-Infinity`
  NegativeInfinity : FloatLit

  ||| Corresponds to the WebIDL keyword `NaN`
  NaN              : FloatLit

%runElab derive "FloatLit" [Generic,Meta,Show]

export
Eq FloatLit using FastNatEq where (==) = genEq

--------------------------------------------------------------------------------
--          Parsing Floats
--------------------------------------------------------------------------------

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


||| Tries to read a floating point literal
||| from a `String`.
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
