module Test.Lexer

import Test.Generators
import Text.Lexer
import Text.WebIDL.Lexer
import Text.WebIDL.Encoder

lex : String -> Either String (List IdlToken)
lex s = map (map tok) $ lexIdl s

prop_identifier : Property
prop_identifier = property $ do
                    i <- forAll identifier
                    lex i.value === Right [Ident i]

prop_space : Property
prop_space = property $ do
               s <- forAll space
               lex s === Right [Space]

prop_stringLit : Property
prop_stringLit = property $ do
                   v <- forAll stringLit
                   lex v.value === Right [SLit v]

prop_intLit : Property
prop_intLit = property $ do
                n <- forAll intLit
                footnote ("Encoded: " ++ intLit n)
                lex (intLit n) === Right [ILit n]

prop_floatLit : Property
prop_floatLit = property $ do
                  n <- forAll floatLit
                  lex (floatLit n) === Right [FLit n]

prop_comment : Property
prop_comment = property $ do
                 s <- forAll comment
                 lex s === Right [Comment s]

prop_other : Property
prop_other = property $ do
               v <- forAll symbol
               lex (symbol v) === Right [Other v]

export
props : Group
props = MkGroup "Lexer Properties"
          [ ("prop_identifier", prop_identifier)
          , ("prop_space", prop_space)
          , ("prop_stringLit", prop_stringLit)
          , ("prop_intLit", prop_intLit)
          , ("prop_floatLit", prop_floatLit)
          , ("prop_comment", prop_comment)
          , ("prop_other", prop_other)
          ]
