module Test.Lexer

import Data.String
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

prop_intLit : Property
prop_intLit = property $ do
                (s,n) <- forAll intLit
                lex s === Right [IntLit n]

prop_floatLit : Property
prop_floatLit = withTests 1000 . property $ do
                  (s,n) <- forAll floatLit
                  lex s === Right [FltLit n]

prop_comment : Property
prop_comment = property $ do
                 s <- forAll comment
                 lex s === Right [Comment s]

prop_other : Property
prop_other = withTests 1000 . property $ do
               c <- forAll latinSymbol
               footnote $ show (ord c)
               lex (singleton c) === Right [Other $ Symb c]

export
props : Group
props = MkGroup "Lexer Properties" [
          ("prop_identifier", prop_identifier)
        , ("prop_space", prop_space)
        , ("prop_stringLit", prop_stringLit)
        , ("prop_intLit", prop_intLit)
        , ("prop_floatLit", prop_floatLit)
        , ("prop_comment", prop_comment)
        , ("prop_other", prop_other)
        ]
