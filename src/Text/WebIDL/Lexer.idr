module Text.WebIDL.Lexer

import Data.List
import Text.WebIDL.Identifier
import Text.WebIDL.Numbers
import Text.WebIDL.StringLit
import Text.Lexer

import Generics.Derive

%language ElabReflection

public export
data IdlToken : Type where
  Space   : IdlToken
  StrLit  : StringLit -> IdlToken
  IntLit  : Integer -> IdlToken
  Ident   : Identifier -> IdlToken
  Invalid : String -> IdlToken

%runElab derive "Text.WebIDL.Lexer.IdlToken" [Generic,Meta,Eq,Show]

--------------------------------------------------------------------------------
--          Utilities
--------------------------------------------------------------------------------

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
parseInt s = maybe (Invalid s) IntLit $ readInt s

-- /0[Xx][0-9A-Fa-f]+/
hex : Lexer
hex = (exact "0x" <|> exact "0X") <+> plus hexDigit

-- /0[0-7]*/
oct : Lexer
oct = is '0' <+> star octDigit

int : Lexer
int = hex <|> oct <|> (opt (is '-') <+> plus digit)

--------------------------------------------------------------------------------
--          Others
--------------------------------------------------------------------------------

-- [_-]?[A-Za-z][0-9A-Z_a-z-]*
identifier : Lexer
identifier =   opt (oneOf "_-")
           <+> alpha
           <+> star (pred \c => isAlphaNum c || c == '_' || c == '-')

ident : String -> IdlToken
ident = Ident . MkIdent

--------------------------------------------------------------------------------
--          Lexing
--------------------------------------------------------------------------------

export tokenMap : TokenMap IdlToken
tokenMap = [ (spaces,     const Space)
           , (stringLit,  StrLit . MkStringLit)
           , (identifier, ident)
           , (int,        parseInt)
           ]

public export
isNoise : IdlToken -> Bool
isNoise Space       = True
isNoise _           = False

||| Generates a list of IdlTokens (wrapped in TokenData, so
||| they come with line and position numbers) from an input
||| string.
export lexIdl : String -> Either String $ List (TokenData IdlToken)
lexIdl s = case lex tokenMap s of
                (ts, (_,_,"")) => Right ts
                (_,  t)        => Left $ "Lexer aborted at " ++ show t

||| Generates a list of IdlTokens
||| from an input string, removing unnecessary tokens by
||| means of `removeNoise`.
export lexIdlNoNoise : String -> Either String $ List (TokenData IdlToken)
lexIdlNoNoise = map (filter (not . isNoise . tok)) . lexIdl
