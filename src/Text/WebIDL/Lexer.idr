module Text.WebIDL.Lexer

import Experimental.Prelude
import Data.List
import Data.String
import Experimental.Prelude
import Text.Lexer
import Text.WebIDL.Types

%default total

-- alias for `some`
plus : Lexer -> Lexer
plus = some

-- alias for `some`
star : Lexer -> Recognise False
star = many

-- /[1-9]/
nonZeroDigit : Lexer
nonZeroDigit = pred \c => '1' <= c && c <= '9'

--------------------------------------------------------------------------------
--          Numbers
--------------------------------------------------------------------------------

parseInt : String -> IdlToken
parseInt s = maybe (Invalid s) ILit $ readInt s

parseFloat : String -> IdlToken
parseFloat s = maybe (Invalid s) FLit $ readFloat s

-- /0[Xx][0-9A-Fa-f]+/
hex : Lexer
hex = (exact "0x" <|> exact "0X") <+> plus hexDigit

-- /0[0-7]*/
oct : Lexer
oct = is '0' <+> star octDigit

-- any integer literal
int : Lexer
int = hex <|> oct <|> (opt (is '-') <+> plus digit)

-- /[Ee][+-]?[0-9]+/
exp : Lexer
exp = oneOf "Ee" <+> opt (oneOf "+-") <+> plus digit

-- /([0-9]+\.[0-9]*|[0-9]*\.[0-9]+)([Ee][+-]?[0-9]+)?/
expOpt : Lexer
expOpt = let pre =   (plus digit <+> is '.' <+> star digit)
                 <|> (star digit <+> is '.' <+> plus digit)
          in pre <+> opt exp

-- [0-9]+[Ee][+-]?[0-9]+
expNonOpt : Lexer
expNonOpt = plus digit <+> exp

float : Lexer
float = (opt (is '-') <+> (expOpt <|> expNonOpt))

--------------------------------------------------------------------------------
--          Others
--------------------------------------------------------------------------------

-- [_-]?[A-Za-z][0-9A-Z_a-z-]*
identifier : Lexer
identifier =   opt (oneOf "_-")
           <+> alpha
           <+> star (pred \c => isAlphaNum c || c == '_' || c == '-')

-- Takes a valid identifier and converts it either
-- to a FloatLit, a Keyword, or an Identifier
ident : String -> IdlToken
ident "Infinity"        = FLit Infinity
ident "-Infinity"       = FLit NegativeInfinity
ident "NaN"             = FLit NaN
ident s                 = maybe (Ident $ MkIdent s) Key (refine s)

-- /\/\/.*/
comment : Lexer
comment =   lineComment (exact "//" )
        <|> surround (exact "/*" ) (exact "*/") any

-- /[^\t\n\r 0-9A-Za-z]/
other : Lexer
other = pred \c => not $
        isAlpha c || isDigit c || isSpace c || isControl c

symbol : String -> IdlToken
symbol "..." = Other Ellipsis
symbol s     = case fastUnpack s of
                    [c] => Other (Symb c)
                    _   => Invalid s

--------------------------------------------------------------------------------
--          Lexing
--------------------------------------------------------------------------------

tokenMap : TokenMap IdlToken
tokenMap = [ (spaces,                const Space)
           , (stringLit,             SLit . MkStrLit)
           , (comment,               Comment)
           , (identifier,            ident)
           , (float,                 parseFloat)
           , (int,                   parseInt)
           , (exact "..." <|> other, symbol)
           ]

isNoise : IdlToken -> Bool
isNoise Space       = True
isNoise (Comment _) = True
isNoise _           = False

||| Generates a list of IdlTokens (wrapped in TokenData, so
||| they come with line and position numbers) from an input
||| string.
export
lexIdl : String -> Either String $ List (TokenData IdlToken)
lexIdl s = case lex tokenMap s of
                (ts, (_,_,"")) => Right ts
                (_,  t)        => Left $ "Lexer aborted at " ++ show t

||| Generates a list of IdlTokens
||| from an input string, removing unnecessary tokens by
||| means of `isNoise`.
export
lexIdlNoNoise : String -> Either String $ List (TokenData IdlToken)
lexIdlNoNoise = map (filter (not . isNoise . tok)) . lexIdl
