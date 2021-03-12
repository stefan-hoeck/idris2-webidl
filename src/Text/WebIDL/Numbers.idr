module Text.WebIDL.Numbers

--------------------------------------------------------------------------------
--          Parsing strings of characters
--------------------------------------------------------------------------------

digitToInt : Char -> Integer
digitToInt c = cast $ if isDigit c
                         then ord c - ord '0'
                         else ord (toUpper c) - ord 'A' + 10

public export
charsToPosInt : (base : Integer) -> List Char -> Maybe Integer
charsToPosInt base t = calc <$> traverse readDigit t
  where readDigit : Char -> Maybe Integer
        readDigit c = let d = digitToInt c
                       in if d >= 0 && d < base then Just d else Nothing

        calc : List Integer -> Integer
        calc = foldl (\a,e => a * base + e) 0

public export
charsToPosDec : List Char -> Maybe Integer
charsToPosDec = charsToPosInt 10
