module Text.WebIDL.Types.Err

import Text.WebIDL.Types.Token
import Generics.Derive

%language ElabReflection

public export
data Err : Type where 
  LexErr     : (msg : String) -> Err
  NoEOI      : (line : Int) -> (col : Int) -> (tok : IdlToken) -> Err
  ParseErr   :  (msg : String) -> Err
  ParseErrAt :  (msg : String)
             -> (line : Int)
             -> (col : Int)
             -> (tok : IdlToken)
             -> Err

%runElab derive "Err" [Generic,Meta,Eq,Show]
