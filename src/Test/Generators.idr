-- We refrain from doing anything fancy with type aliases and composed
-- applicatives here. This may make generators slightly more verbose,
-- but it should be clear from their definitions, how WebIDL strings and
-- the corresponding data types are being built up.
module Test.Generators

import Data.List
import Data.List.Elem
import Data.String
import Data.Vect
import public Hedgehog
import public Text.WebIDL.Types

--------------------------------------------------------------------------------
--          Utilities
--------------------------------------------------------------------------------

-- list whose length scales linearily and shrinks towards 0
linList : Nat -> Gen a -> Gen (List a)
linList n = list (linear 0 n)

-- string whose length scales linearily and shrinks towards 0
linString : Nat -> Gen Char -> Gen String
linString n = string (linear 0 n)

-- non-empty string whose length scales linearily and shrinks towards 1
linString1 : Nat -> Gen Char -> Gen String
linString1 n = string (linear 1 n)

maxInt : Integer
maxInt = 18446744073709551616

anyInt : Gen Integer
anyInt = integer $ exponentialFrom 0 (- maxInt) maxInt

posInt : Gen Integer
posInt = integer $ exponential 0 maxInt

nat : Gen Nat
nat = map fromInteger posInt

pairFst : Functor f => (a -> b) -> f a -> f (a,b)
pairFst g = map (\a => (a, g a))

pairSnd : Functor f => (a -> b) -> f a -> f (b,a)
pairSnd g = map (\a => (g a, a))

--------------------------------------------------------------------------------
--          Tokens
--------------------------------------------------------------------------------

export
identifier : Gen (String,Identifier)
identifier = pairFst MkIdent [| concIdent (maybe line) alpha rest |]
  where line : Gen Char
        line = element ['-','_']

        rest : Gen String
        rest = linString 15 (frequency [(20, alphaNum),(1,line)])

        concIdent : Maybe Char -> Char -> String -> String
        concIdent mc c r = maybe "" singleton mc ++ singleton c ++ r

export
space : Gen String
space = linString1 5 (element [' ','\t','\n','\r'])

export
maybeSpace : Gen String
maybeSpace = frequency [(1, pure ""), (4, space)]

export
stringLit : Gen (String,StringLit)
stringLit = pairFst MkStringLit $ toStringLit <$> linList 15 unicode
  where escape : Char -> List Char
        escape '"'  = ['\\','"']
        escape '\\' = ['\\','\\']
        escape c    = [c]

        toStringLit : List Char -> String
        toStringLit cs = fastPack $ '"' :: (cs >>= escape) ++ ['"']

export
intLit : Gen (String,Integer)
intLit = choice [ pairSnd show anyInt
                , pairSnd (toHex . fromInteger) posInt
                , pairSnd (toOct . fromInteger) posInt
                ]

export
floatLit : Gen (String,FloatLit)
floatLit = pairSnd toFloatLit float
  where exp : Gen Integer
        exp = integer $ linearFrom 0 (-30) (30)

        sig : Gen Signum
        sig = element [Plus,Minus]

        float : Gen FloatLit
        float = frequency [ (5, [| Exp sig nat (maybe nat) exp |])
                          , (5, [| NoExp sig nat nat |])
                          , (1, pure Infinity)
                          , (1, pure NegativeInfinity)
                          , (1, pure NaN)
                          ]

export
comment : Gen String
comment = choice [line, multiline]
  where noControl : Gen Char
        noControl = map (\c => if isControl c then ' ' else c) unicode

        noForward : Gen Char
        noForward = map (\c => if c == '/' then ' ' else c) unicode

        line : Gen String
        line = map ("//" ++) $ string (linear 0 20) noControl

        multiline : Gen String
        multiline = map (\s => "/*" ++ s ++ "*/")
                  $ string (linear 0 20) noForward

export
symbol : Gen (String,Symbol)
symbol = frequency [ (10, map (\c => (singleton c, Symb c)) latinSymbol)
                   , (1, pure ("...", Ellipsis))
                   ]
  where latinSymbol : Gen Char
        latinSymbol = choice [ charc '!' '/'
                             , charc ':' '@'
                             , charc '[' '`'
                             , charc '{' '~'
                             , charc (chr 161) (chr 255)
                             ]

symbolUnless : (Char -> Bool) -> Gen (String,Symbol)
symbolUnless f = map replace symbol
  where replace : (String,Symbol) -> (String,Symbol)
        replace (s,Symb c) = if f c then ("@",Symb '@') else (s,Symb c)
        replace p          = p

--------------------------------------------------------------------------------
--          Parser
--------------------------------------------------------------------------------

-- separator, possibly but not necessarily surrounded by spaces
sep : String -> Gen String
sep s = [| (\a,b => a ++ s ++ b) maybeSpace maybeSpace |]

between : String -> String -> Gen (String,a) -> Gen (String,a)
between l r g = [| comb (sep l) (sep r) g |]
  where comb : String -> String -> (String,a) -> (String,a)
        comb sl sr (s,a) = (sl ++ s ++ sr, a)

inParens : Gen (String,a) -> Gen (String,a)
inParens = between "(" ")"

inBrackets : Gen (String,a) -> Gen (String,a)
inBrackets = between "[" "]"

inBraces : Gen (String,a) -> Gen (String,a)
inBraces = between "{" "}"

inAnyParens : Gen (String,a) -> Gen (String,a)
inAnyParens g = choice [ inParens g, inBrackets g, inBraces g ]

-- non-empty list of encoded values, separated by the given
-- separator. The separator can be pre- or postfixed by arbitrary
-- whitespace.
sepList1 : Nat -> String -> Gen (String,a) -> Gen (String,List1 a)
sepList1 n s g = [| enc g (linList n gsep) |]
  where enc : (String,a) -> List (String,a) -> (String, List1 a)
        enc (s1,a1) ps = let (ss,as) = unzip ps
                          in (fastConcat $ s1 :: ss, a1 ::: as)

        prependSep : String -> (String,a) -> (String,a)
        prependSep s = mapFst (s ++)

        gsep : Gen (String,a)
        gsep = [| prependSep (sep s) g |] 

sepListNonEmpty : Nat -> String -> Gen (String,a) -> Gen (String,List a)
sepListNonEmpty n s = map (mapSnd forget) . sepList1 n s

sepList : Nat -> String -> Gen (String,a) -> Gen (String,List a)
sepList n s g =   fromMaybe ("",[]) <$> maybe (sepListNonEmpty n s g)

||| Comma-separated list of identifiers
export
identifiers : Gen (String, IdentifierList)
identifiers = sepList1 10 "," identifier

otherUnless : (Char -> Bool) -> Gen (String,Other)
otherUnless f = choice [ map (mapSnd \v => inject v) identifier
                       , map (mapSnd \v => inject v) intLit
                       , map (mapSnd \v => inject v) stringLit
                       , map (mapSnd \v => inject v) floatLit
                       , map (mapSnd \v => inject v) (symbolUnless f)
                       ]

export
otherOrComma : Gen (String,Other)
otherOrComma = otherUnless isParenOrQuote

export
other : Gen (String,Other)
other = otherUnless isCommaOrParenOrQuote

eaInner : Nat -> Gen (String, EAInner)
eaInner 0 = pure ("", EAIEmpty)
eaInner (S k) =
   frequency [ (4, eaInner 0)
             , (1, [| combOther otherOrComma space (eaInner k) |])
             , (1, [| combParens (inAnyParens $ eaInner k) (eaInner k) |])
             ]
  where combOther : (String,Other) -> String -> (String,EAInner) -> (String,EAInner)
        combOther (so,o) p (sa,a) = (so ++ p ++ sa, EAIOther o a)

        combParens : (String,EAInner) -> (String,EAInner) -> (String,EAInner)
        combParens (sa,a) (sb,b) = (sa ++ sb, EAIParens a b)

extAttribute : Nat -> Gen (String, ExtAttribute)
extAttribute 0 =
  choice [ map (\(s,i) => (s, EAParens i Nothing)) $ inAnyParens (eaInner 0)
         , map (\(s,o) => (s, EAOther o Nothing)) other ]

extAttribute (S k) =
  choice [ extAttribute 0
         , [| combParens (inAnyParens $ eaInner k) rest |]
         , [| combOther other space rest |]
         ]

  where rest : Gen (String,Maybe ExtAttribute)
        rest = map (\case Just (s,a) => (s, Just a)
                          Nothing    => ("", Nothing)) (maybe $ extAttribute k)

        combOther : (String,Other) -> String -> (String,Maybe ExtAttribute) -> (String,ExtAttribute)
        combOther (so,o) p (sa,a) = (so ++ p ++ sa, EAOther o a)

        combParens : (String,EAInner) -> (String,Maybe ExtAttribute) -> (String,ExtAttribute)
        combParens (sa,a) (sb,b) = (sa ++ sb, EAParens a b)

export
extAttributes : (depth : Nat) -> Gen (String, ExtAttributeList)
extAttributes = inBrackets . sepListNonEmpty 5 "," . extAttribute

export
attributed : Gen (String, a) -> Gen (String, Attributed a)
attributed g = frequency [ (3, map (mapSnd ([],)) g)
                         , (1, [| comb (extAttributes 3) maybeSpace g|])
                         ]
  where comb :  (String, ExtAttributeList)
             -> String
             -> (String,a)
             -> (String,Attributed a)
        comb (s1,as) s2 (s3,v) = (s1 ++ s2 ++ s3, (as,v))

--------------------------------------------------------------------------------
--          Types
--------------------------------------------------------------------------------

bufferRelated : Gen (String, BufferRelatedType)
bufferRelated = pairSnd show $
                  element [ ArrayBuffer
                          , DataView
                          , Int8Array
                          , Int16Array
                          , Int32Array
                          , Uint8Array
                          , Uint16Array
                          , Uint32Array
                          , Uint8ClampedArray
                          , Float32Array
                          , Float64Array
                          ]

stringType : Gen (String, StringType)
stringType = pairSnd show $ element [ByteString, DOMString, USVString]

export
primitive : Gen (String, PrimitiveType)
primitive = element [ ("undefined", Undefined)
                    , ("boolean", Boolean)
                    , ("octet", Octet)
                    , ("byte", Byte)
                    , ("bigint", BigInt)
                    , ("double", Restricted Dbl)
                    , ("float", Restricted Float)
                    , ("short", Signed Short)
                    , ("long", Signed Long)
                    , ("long long", Signed LongLong)
                    , ("unsigned short", Unsigned Short)
                    , ("unsigned long", Unsigned Long)
                    , ("unsigned long long", Unsigned LongLong)
                    , ("unrestricted double", Unrestricted Dbl)
                    , ("unrestricted float", Unrestricted Float)
                    ]


nullable : Gen (String,a) -> Gen (String, Nullable a)
nullable g = choice [ map (mapSnd NotNull) g
                    , map (\(s,a) => (s ++ "?", MaybeNull a)) g
                    ]

-- PrimitiveType
-- StringType
-- identifier
-- object
-- symbol
-- BufferRelatedType
mutual
  export
  idlType : Nat -> Gen (String, IdlType)
  idlType 0 = frequency [ (1, pure ("any", Any))
                        , (10, map (mapSnd D) (distinguishable 0))
                        ]
  idlType (S k) = frequency [ (2, idlType 0)
                            , (1, promise k)
                            , (2, map (mapSnd D) (distinguishable k))
                            ]

  dist : Nat -> Gen (String, Distinguishable)
  dist 0     = choice [ (map (mapSnd P) primitive)
                      , (map (mapSnd S) stringType)
                      , (map (mapSnd I) identifier)
                      , (map (mapSnd B) bufferRelated)
                      , element [ ("object", Object), ("symbol", Symbol) ]
                      ]
  dist (S k) = choice [sequence k, frozen k, observable k, recrd k]

  distinguishable : Nat -> Gen (String, Nullable Distinguishable)
  distinguishable n = nullable (dist n)

  promise : Nat -> Gen (String, IdlType)
  promise k = map (\(s,t) => ("Promise<" ++ s ++ ">",Promise t))
                  (idlType k)

  typeWithAttr : Nat -> Gen (String, Attributed IdlType)
  typeWithAttr k = attributed (idlType k)

  sequence : Nat -> Gen (String,Distinguishable)
  sequence k = map (\(s,t) => ("sequence<" ++ s ++ ">", Sequence t))
                   (typeWithAttr k)

  frozen : Nat -> Gen (String,Distinguishable)
  frozen k = map (\(s,t) => ("FrozenArray<" ++ s ++ ">", FrozenArray t))
                 (typeWithAttr k)

  observable : Nat -> Gen (String,Distinguishable)
  observable k = map (\(s,t) => ("ObservableArray<" ++ s ++ ">", ObservableArray t))
                     (typeWithAttr k)

  recrd : Nat -> Gen (String,Distinguishable)
  recrd n = [| comb stringType (typeWithAttr n) |]
    where comb :  (String,StringType)
               -> (String,Attributed IdlType)
               -> (String,Distinguishable)
          comb (s1,st) (s2,at) = ( "record<" ++ s1 ++ "," ++ s2 ++ ">"
                                 , Record st at)

  union : Nat -> Gen (String,UnionType)
  union n = inParens [| comb (unionMember n)
                             (unionMember n)
                             (sepList 2 " or " (unionMember n)) |]
    where comb :  (String,UnionMemberType)
               -> (String,UnionMemberType)
               -> (String,List UnionMemberType)
               -> (String,UnionType)
          comb (s1,u1) (s2,u2) (_,Nil) = (s1 ++ " or " ++ s2, UT u1 u2 Nil)
          comb (s1,u1) (s2,u2) (s3,ts) =
            (s1 ++ " or " ++ s2 ++ " or " ++ s3, UT u1 u2 ts)

  unionMember : Nat -> Gen (String,UnionMemberType)
  unionMember 0     = map (mapSnd UD) (attributed $ distinguishable 0)
  unionMember (S k) = choice [ map (mapSnd UD) (attributed $ distinguishable k)
                             , map (mapSnd UU) (nullable $ union k)
                             ]

--------------------------------------------------------------------------------
--          Arguments
--------------------------------------------------------------------------------

||| ConstValue ::
|||     BooleanLiteral
|||     FloatLiteral
|||     integer
constValue : Gen (String, ConstValue)
constValue = choice [ map (mapSnd F) floatLit
                    , map (mapSnd I) intLit
                    , element [("false", B False), ("true", B True)]
                    ]

defaultVal : Gen (String, Default)
defaultVal = choice [ map (mapSnd C) constValue
                    , map (mapSnd S) stringLit
                    , element [ ("",None)
                              , ("[]",EmptyList)
                              , ("{}",EmptySet)
                              , ("null",Null)
                              ]
                    ]

argName : Gen (String, ArgumentName)
argName = choice [ map (\(s,_) => (s, MkArgName s)) identifier
                 , pairFst MkArgName $ element [ "async"
                                               , "attribute"
                                               , "callback"
                                               , "const"
                                               , "constructor"
                                               , "deleter"
                                               , "dictionary"
                                               , "enum"
                                               , "getter"
                                               , "includes"
                                               , "inherit"
                                               , "interface"
                                               , "iterable"
                                               , "maplike"
                                               , "mixin"
                                               , "namespace"
                                               , "partial"
                                               , "readonly"
                                               , "required"
                                               , "setlike"
                                               , "setter"
                                               , "static"
                                               , "stringifier"
                                               , "typedef"
                                               , "unrestricted" ]
                 ]

export
argumentRest : Gen (String,ArgumentRest)
argumentRest = choice [ [| optional (typeWithAttr 3) argName defaultVal |]
                      , [| mandatory (idlType 3) argName |]
                      , [| vararg (idlType 3) argName |]
                      ]
  where optional :  (String,Attributed IdlType)
                 -> (String,ArgumentName)
                 -> (String,Default)
                 -> (String,ArgumentRest)
        optional (s1,t) (s2,n) (s3,d) = ("optional " ++ s1 ++ " " ++ s2 ++ " " ++ s3
                                        , Optional t n d )

        mandatory : (String,IdlType)
                 -> (String,ArgumentName)
                 -> (String,ArgumentRest)
        mandatory (s1,t) (s2,n) = (s1 ++ " " ++ s2, Mandatory t n)

        vararg :  (String,IdlType)
               -> (String,ArgumentName)
               -> (String,ArgumentRest)
        vararg (s1,t) (s2,n) = (s1 ++ "... " ++ s2, VarArg t n)

argumentList : Gen (String,ArgumentList)
argumentList = sepList 10 "," (attributed argumentRest)

constType : Gen (String,ConstType)
constType = choice [map (mapSnd CP) primitive, map (mapSnd CI) identifier]

--------------------------------------------------------------------------------
--          Member
--------------------------------------------------------------------------------

export
const : Gen (String,Const)
const = [| comb constType identifier constValue |]
  where comb :  (String,ConstType)
             -> (String,Identifier)
             -> (String,ConstValue)
             -> (String,Const)
        comb (s1,t) (s2,i) (s3,v) =
          ("const " ++ s1 ++ " " ++ s2 ++ " = " ++ s3 ++ ";", MkConst t i v)

--------------------------------------------------------------------------------
--          Definition
--------------------------------------------------------------------------------

export
definition : Gen (String, Definition)
definition =
  choice
    [ [| typeDef (typeWithAttr 3) identifier |]
    , [| enum identifier (inBraces $ sepList1 5 "," stringLit) |]
    ]

  where typeDef :  (String,Attributed IdlType)
                -> (String,Identifier)
                -> (String,Definition)
        typeDef (s1,(a,t)) (s2,i) = ("typedef " ++ s1 ++ " " ++ s2 ++ ";",
                                    Typedef a t i)

        enum :  (String,Identifier)
             -> (String,List1 StringLit)
             -> (String,Definition)
        enum (s1,i) (s2,vs) = ("enum " ++ s1 ++ s2 ++ ";", Enum i vs)
