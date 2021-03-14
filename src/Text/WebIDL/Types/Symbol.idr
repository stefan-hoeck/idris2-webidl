module Text.WebIDL.Types.Symbol

import Generics.Derive

%language ElabReflection

public export
data Symbol : Type where
  Ellipsis : Symbol
  Symb     : (symb : Char) -> Symbol

%runElab derive "Symbol" [Generic,Meta,Eq,Show]
