module Text.WebIDL.Parser

import Text.Lexer
import Text.Parser
import Text.WebIDL.Types
import Text.WebIDL.Lexer

import Generics.Derive

%language ElabReflection

public export
IdlGrammarAny : (b : Bool) -> Type -> Type
IdlGrammarAny b t = Grammar (TokenData IdlToken) b t

public export
IdlGrammar : Type -> Type
IdlGrammar = IdlGrammarAny True

public export
IdlGrammar' : Type -> Type
IdlGrammar' = IdlGrammarAny False

tok : String -> (IdlToken -> Maybe a) -> IdlGrammar a
tok s f = terminal s (f . tok)

--------------------------------------------------------------------------------
--          Symbols
--------------------------------------------------------------------------------

symbol : Char -> IdlGrammar ()
symbol c = tok ("Symbol " ++ show c) \case Other (Symb v) => guard (c == v)
                                           _              => Nothing

comma : IdlGrammar ()
comma = symbol ','

ellipsis : IdlGrammar ()
ellipsis = tok "Ellipsis" \case Other Ellipsis => Just ()
                                _              => Nothing

--------------------------------------------------------------------------------
--          Identifiers
--------------------------------------------------------------------------------

export
ident : IdlGrammar Identifier
ident = tok "identifier" \case Ident i => Just i
                               _       => Nothing

||| IdentifierList :: identifier Identifiers
||| Identifiers :: , identifier Identifiers ε
export
identifierList : IdlGrammar IdentifierList
identifierList = [| ident ::: many (comma *> ident) |]

--------------------------------------------------------------------------------
--          Parsing WebIDL
--------------------------------------------------------------------------------

toParseErr : ParseError (TokenData IdlToken) -> Err
toParseErr (Error x []) = ParseErr x
toParseErr (Error x (MkToken l c t :: _)) = ParseErrAt x l c t

export
parseIdl : IdlGrammar a -> String -> Either Err a
parseIdl g s = do ts <- mapFst LexErr (lexIdlNoNoise s)
                  (res,Nil) <- mapFst toParseErr (parse g ts)
                    | (_,MkToken l c t :: _) => Left (NoEOI l c t)
                  pure res
