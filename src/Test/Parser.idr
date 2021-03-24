module Test.Parser

import Text.WebIDL.Encoder
import Text.WebIDL.Parser
import Test.Generators

prp : Eq a => Show a => Encoder a -> Gen a -> IdlGrammar a -> Property
prp enc gen p = property $ do
                  v <- forAll gen
                  let str : String
                      str = enc v

                      len : Integer
                      len = natToInteger $ length str

                  classify "Length in (0000,10]"       (len <= 10)
                  classify "Length in (0010,50]"    (len > 10 && len <= 50)
                  classify "Length in (0050,100]"   (len > 50 && len <= 100)
                  classify "Length in (0100,500]"   (len > 100 && len <= 500)
                  classify "Length in (0500,1000]"  (len > 500 && len <= 1000)
                  classify "Length in (1000,10000]" (len > 1000 && len <= 10000)

                  footnote ("Encoded: " ++ str)

                  parseIdl p str === Right v

prop_other : Property
prop_other = prp other other other

prop_extAttributes : Property
prop_extAttributes = prp extAttribute (extAttribute 4) extAttribute

prop_primitiveType : Property
prop_primitiveType = prp primitive primitive primitive

prop_idlType : Property
prop_idlType = prp idlType (idlType 5) idlType

prop_argumentRest : Property
prop_argumentRest = prp argumentRest argumentRest argumentRest

prop_const : Property
prop_const = prp const const const

prop_operation : Property
prop_operation = prp operation operation operation

prop_interfaceMember : Property
prop_interfaceMember = prp interfaceMember interfaceMember interfaceMember

prop_definition : Property
prop_definition = prp definition definition definition

prop_part : Property
prop_part = prp part part part

export
props : Group
props = MkGroup "Parser Properties"
          [ ("prop_other", prop_other)
          , ("prop_extAttributes", prop_extAttributes)
          , ("prop_primitiveType", prop_primitiveType)
          , ("prop_idlType", prop_idlType)
          , ("prop_argumentRest", prop_argumentRest)
          , ("prop_const", prop_const)
          , ("prop_operation", prop_operation)
          , ("prop_interfaceMember", prop_interfaceMember)
          , ("prop_definition", prop_definition)
          , ("prop_part", prop_part)
          ]
