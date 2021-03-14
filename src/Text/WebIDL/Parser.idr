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

intLit : IdlGrammar Integer
intLit = tok "Int Lit" \case IntLit n => Just n
                             _        => Nothing

stringLit : IdlGrammar StringLit
stringLit = tok "String Lit" \case StrLit s => Just s
                                   _        => Nothing

floatLit : IdlGrammar FloatLit
floatLit = tok "Float Lit" \case FltLit v => Just v
                                 _        => Nothing

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
--          Extended Attributes
--------------------------------------------------------------------------------

symbolUnless : String -> (Char -> Bool) -> IdlGrammar Symbol
symbolUnless s f = tok s \case Other s => fromSym s
                               _       => Nothing
  where fromSym : Symbol -> Maybe Symbol
        fromSym Ellipsis = Just Ellipsis
        fromSym (Symb c) = if f c then Nothing else Just (Symb c)
        

otherSym : IdlGrammar Symbol -> IdlGrammar Other
otherSym sym = choice {t = List} [ map (\v => inject v) intLit
                                 , map (\v => inject v) stringLit
                                 , map (\v => inject v) floatLit
                                 , map (\v => inject v) ident
                                 , map (\v => inject v) sym
                                 ]

export
other : IdlGrammar Other
other = otherSym $ symbolUnless "other" isCommaOrParen

export
otherOrComma : IdlGrammar Other
otherOrComma = otherSym $ symbolUnless "otherOrComma" isParen

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
