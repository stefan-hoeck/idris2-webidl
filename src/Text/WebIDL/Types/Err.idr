module Text.WebIDL.Types.Err

import Derive.Prelude
import Text.WebIDL.Types.Token
import Text.Lexer

%default total

%language ElabReflection

||| Errors that can happen during lexing and parsing.
public export
data Err : Type where
  ||| An error that happened during lexing
  LexErr     : (msg : String) -> Err

  ||| The parser will fail with this error, if not all tokens
  ||| have been consumed at the end of parsing.
  ||| The error constains position and content of the next token.
  NoEOI      : (tok : WithBounds IdlToken) -> Err

  ||| There was an error in the parser but all tokens were already
  ||| consumed.
  ParseErr   :  (msg : String) -> Err

  ||| There was an error in the parser and the position
  ||| is given.
  ParseErrAt :  (msg : String)
             -> (line : Int)
             -> (col : Int)
             -> Err

%runElab derive "Err" [Eq,Show]
