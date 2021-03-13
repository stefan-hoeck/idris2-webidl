module Text.WebIDL.Numbers

import Data.String

import Generics.Derive

%language ElabReflection

--------------------------------------------------------------------------------
--          Encoding Integers
--------------------------------------------------------------------------------

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

export
toHex : Integer -> String
toHex n = "0x" ++ fastPack(run n [])
  where run : Integer -> List Char -> List Char
        run n cs = let n'  = n `div` 16
                       cs' = (toHexDigit $ n `mod` 16) :: cs
                    in if n' == 0 then cs' else run n' cs'

export
toOct : Integer -> String
toOct n = "0" ++ fastPack(run n [])
  where run : Integer -> List Char -> List Char
        run n cs = let n'  = n `div` 8
                       cs' = (toOctDigit $ n `mod` 8) :: cs
                    in if n' == 0 then cs' else run n' cs'


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

%runElab derive "Text.WebIDL.Numbers.Signum" [Generic,Meta,Eq,Show]

public export
data FloatLit : Type where
  MkFloat : Signum -> (significand : Integer) -> (exp : Integer) -> FloatLit
  Infinity         : FloatLit
  NegativeInfinity : FloatLit
  NaN              : FloatLit

%runElab derive "Text.WebIDL.Numbers.FloatLit" [Generic,Meta,Eq,Show]
