module Text.WebIDL.Lexer

import Data.List
import Text.WebIDL.Identifier
import Text.Lexer

import Generics.Derive

%language ElabReflection

public export
data IdlToken : Type where
  Space : IdlToken
  Ident : Identifier -> IdlToken

%runElab derive "Text.WebIDL.Lexer.IdlToken" [Generic,Meta,Eq,Show]

--------------------------------------------------------------------------------
--          Utilities
--------------------------------------------------------------------------------

-- alias for `some`
private plus : Lexer -> Lexer
plus = some

-- alias for `some`
private star : Lexer -> Recognise False
star = many

--------------------------------------------------------------------------------
--          Others
--------------------------------------------------------------------------------

-- [_-]?[A-Za-z][0-9A-Z_a-z-]*
private identifier : Lexer
identifier =   opt (oneOf "_-")
           <+> alpha
           <+> star (pred \c => isAlphaNum c || c == '_' || c == '-')

private ident : String -> IdlToken
ident = Ident . MkIdent

--------------------------------------------------------------------------------
--          Lexing
--------------------------------------------------------------------------------

export tokenMap : TokenMap IdlToken
tokenMap = [ (spaces,     const Space)
           , (identifier, ident)
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
