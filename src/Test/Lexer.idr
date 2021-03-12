module Test.Lexer

import Test.Generators
import Text.Lexer
import Text.WebIDL.Lexer

lex : String -> Either String (List IdlToken)
lex s = map (map tok) $ lexIdl s

prop_identifier : Property
prop_identifier = property $ do
                    (MkIdent i) <- forAll identifier
                    lex i === Right [Ident $ MkIdent i]

prop_space : Property
prop_space = property $ do
               s <- forAll space
               lex s === Right [Space]

prop_stringLit : Property
prop_stringLit = property $ do
                 (MkStringLit s) <- forAll stringLit
                 lex s === Right [StrLit $ MkStringLit s]

export
props : Group
props = MkGroup "Lexer Properties" [
          ("prop_identifier", prop_identifier)
        , ("prop_space", prop_space)
        , ("prop_stringLit", prop_stringLit)
        ]
