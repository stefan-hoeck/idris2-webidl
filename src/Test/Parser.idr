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

prop_extAttributes : Property
prop_extAttributes = withTests 1000 . property $ do
                     (s,v) <- forAll extAttributes
                     let dv : Nat
                         dv = foldl (\n,a => max n (depth a)) 0 v

                         lv : Nat
                         lv = foldl (\n,a => n + (leaves a)) 0 v

                         sv : Nat
                         sv = foldl (\n,a => n + (size a)) 0 v
                     classify "depth 0" (dv == 0)
                     classify "depth 1" (dv == 1)
                     classify "depth 2" (dv == 2)
                     classify "depth 3" (dv == 3)
                     classify "depth 4" (dv == 4)
                     classify "depth > 4" (dv > 4)

                     classify "leaves 0" (lv == 0)
                     classify "leaves in [01,10)" (lv >= 1 && lv < 10)
                     classify "leaves in [10,20)" (lv >= 10 && lv < 20)
                     classify "leaves in [20,30)" (lv >= 20 && lv < 30)
                     classify "leaves in [30,40)" (lv >= 30 && lv < 40)

                     classify "size 0" (sv == 0)
                     classify "size in [01,05)"   (sv >= 1 && sv < 5)
                     classify "size in [05,10)"  (sv >= 5 && sv < 10)
                     classify "size in [10,15)" (sv >= 10 && sv < 15)
                     classify "size in [15,20)" (sv >= 15 && sv < 20)
                     parseIdl extAttributes s === Right v

export
props : Group
props = MkGroup "Parser Properties" [
          ("prop_identifierList", prop_identifierList)
        , ("prop_other", prop_other)
        , ("prop_extAttributes", prop_extAttributes)
        ]
