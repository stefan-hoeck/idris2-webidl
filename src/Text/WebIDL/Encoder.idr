module Text.WebIDL.Encoder

import Text.WebIDL.Types

--------------------------------------------------------------------------------
--          Utilities
--------------------------------------------------------------------------------

0 Encoder : Type -> Type
Encoder a = a -> String

export
ident : Encoder Identifier
ident = value

--------------------------------------------------------------------------------
--          Numbers
--------------------------------------------------------------------------------

toHexDigit : Integer -> Char
toHexDigit 0 = '0'
toHexDigit 1 = '1'
toHexDigit 2 = '2'
toHexDigit 3 = '3'
toHexDigit 4 = '4'
toHexDigit 5 = '5'
toHexDigit 6 = '6'
toHexDigit 7 = '7'
toHexDigit 8 = '8'
toHexDigit 9 = '9'
toHexDigit 10 = 'A'
toHexDigit 11 = 'B'
toHexDigit 12 = 'C'
toHexDigit 13 = 'D'
toHexDigit 14 = 'E'
toHexDigit 15 = 'F'
toHexDigit _ = '_'

toDigits : (pre : String) -> (base : Integer) -> Nat -> String
toDigits pre base n = pre ++ fastPack(run (natToInteger n) [])
  where run : Integer -> List Char -> List Char
        run n cs = let n'  = n `div` base
                       cs' = (toHexDigit $ n `mod` base) :: cs
                    in if n' == 0 then cs' else run n' cs'

export
intLit : Encoder IntLit
intLit (Hex k) = toDigits "0x" 16 k
intLit (Oct k) = toDigits "0" 8 k
intLit (I x)   = show x
