module Test.Parser

import Data.List1
import Text.WebIDL.Parser
import Test.Generators

prop_identifierList : Property
prop_identifierList = property $ do
                        (s,is) <- forAll identifiers
                        parseIdl identifierList s === Right is

export
props : Group
props = MkGroup "Parser Properties" [
          ("prop_identifierList", prop_identifierList)
        ]
