module Test.Parser

import Text.WebIDL.Parser
import Test.Generators

prop_identifierList : Property
prop_identifierList = property $ do
                        (s,is) <- forAll identifiers
                        parseIdl identifierList s === Right is

prop_other : Property
prop_other = withTests 1000 . property $ do
               (s,v) <- forAll other
               parseIdl other s === Right v

export
props : Group
props = MkGroup "Parser Properties" [
          ("prop_identifierList", prop_identifierList)
        , ("prop_other", prop_other)
        ]
