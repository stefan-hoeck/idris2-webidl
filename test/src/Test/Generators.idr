module Test.Generators

import Data.Nat
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

-- list whose length scales linearily and shrinks towards 1
linList1 : Nat -> Gen a -> Gen (List1 a)
linList1 n g = [| g ::: linList (pred n) g |]

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

--------------------------------------------------------------------------------
--          Tokens
--------------------------------------------------------------------------------

export
identifier : Gen Identifier
identifier = map ident [| concIdent (maybe line) alpha rest |]
  where line : Gen Char
        line = element ['-','_']

        rest : Gen String
        rest = linString 15 (frequency [(20, alphaNum),(1,line)])

        concIdent : Maybe Char -> Char -> String -> String
        concIdent mc c r = maybe "" singleton mc ++ singleton c ++ r

        ident : String -> Identifier
        ident s = if isKeyword s then MkIdent "anIdentifier1_2_3" else MkIdent s

export
space : Gen String
space = linString1 5 (element [' ','\t','\n','\r'])

export
maybeSpace : Gen String
maybeSpace = frequency [(1, pure ""), (4, space)]

export
stringLit : Gen StringLit
stringLit = strLit <$> linList 15 unicode
  where escape : Char -> List Char
        escape '"'  = ['\\','"']
        escape '\\' = ['\\','\\']
        escape c    = [c]

        strLit : List Char -> StringLit
        strLit cs = MkStrLit $ fastPack $ '"' :: (cs >>= escape) ++ ['"']

export
intLit : Gen IntLit
intLit = choice [map int anyInt, map Hex nat, map Oct nat]
  where int : Integer -> IntLit
        int 0 = Oct 0
        int n = I n

export
floatLit : Gen FloatLit
floatLit = frequency [ (5, [| Exp sig nat (maybe nat) exp |])
                     , (5, [| NoExp sig nat nat |])
                     , (1, pure Infinity)
                     , (1, pure NegativeInfinity)
                     , (1, pure NaN)
                     ]

  where exp : Gen Integer
        exp = integer $ linearFrom 0 (-30) (30)

        sig : Gen Signum
        sig = element [Plus,Minus]


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
symbol : Gen Symbol
symbol = frequency [ (10, map Symb latinSymbol)
                   , (1, pure Ellipsis)
                   ]
  where latinSymbol : Gen Char
        latinSymbol = choice [ charc '!' '/'
                             , charc ':' '@'
                             , charc '[' '`'
                             , charc '{' '~'
                             , charc (chr 161) (chr 255)
                             ]

symbolUnless : Char -> (Char -> Bool) -> Gen Symbol
symbolUnless x f = map replace symbol
  where replace : Symbol -> Symbol
        replace (Symb c) = if f c then Symb x else Symb c
        replace p        = p

--------------------------------------------------------------------------------
--          Parser
--------------------------------------------------------------------------------

otherUnless : (Char -> Bool) -> Gen Other
otherUnless f = choice [ map (\v => inject v) identifier
                       , map (\v => inject v) intLit
                       , map (\v => inject v) stringLit
                       , map (\v => inject v) floatLit
                       , map (\v => inject v) (symbolUnless '@' f)
                       ]

export
otherOrComma : Gen Other
otherOrComma = otherUnless isParenOrQuote

export
other : Gen Other
other = otherUnless isCommaOrParenOrQuote

eaInner : Nat -> Gen EAInner
eaInner 0     = pure EAIEmpty
eaInner (S k) = frequency [ (4, eaInner 0)
                          , (1, [| EAIOther otherOrComma (eaInner k) |])
                          , (1, [| EAIParens (eaInner k) (eaInner k) |])
                          ]

export
extAttribute : Nat -> Gen ExtAttribute
extAttribute 0     = choice [ map (`EAParens` Nothing) (eaInner 0)
                            , map (`EAOther` Nothing) other ]

extAttribute (S k) = let rest = maybe $ extAttribute k
                      in choice [ extAttribute 0
                                , [| EAParens (eaInner k) rest |]
                                , [| EAOther other rest |]
                                ]

export
attributes : Gen ExtAttributeList
attributes = linList 2 $ extAttribute 3

export
attributed : Gen a -> Gen (Attributed a)
attributed ga = [| (,) attributes ga |]

--------------------------------------------------------------------------------
--          Types
--------------------------------------------------------------------------------

bufferRelated : Gen BufferRelatedType
bufferRelated = element [ ArrayBuffer
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

stringType : Gen StringType
stringType = element [ByteString, DOMString, USVString]

export
primitive : Gen PrimitiveType
primitive = element [ Undefined
                    , Boolean
                    , Octet
                    , Byte
                    , BigInt
                    , Restricted Dbl
                    , Restricted Float
                    , Signed Short
                    , Signed Long
                    , Signed LongLong
                    , Unsigned Short
                    , Unsigned Long
                    , Unsigned LongLong
                    , Unrestricted Dbl
                    , Unrestricted Float
                    ]


nullable : Gen a -> Gen (Nullable a)
nullable g = choice [ map NotNull g, map MaybeNull g ]

mutual
  export
  idlType : Nat -> Gen IdlType
  idlType 0     = frequency [ (1, pure Any), (10, map D (distinguishable 0)) ]
  idlType (S k) = frequency [ (2, idlType 0)
                            , (1, Promise <$> idlType k)
                            , (2, map D (distinguishable k))
                            ]

  dist : Nat -> Gen Distinguishable
  dist 0     = choice [ (map P primitive)
                      , (map S stringType)
                      , (map I identifier)
                      , (map B bufferRelated)
                      , element [Object, Symbol]
                      ]

  dist (S k) = choice [ [| Sequence attributes (idlType k) |]
                      , [| FrozenArray attributes (idlType k) |]
                      , [| ObservableArray attributes (idlType k) |]
                      , [| Record stringType attributes (idlType k) |]
                      ]

  distinguishable : Nat -> Gen (Nullable Distinguishable)
  distinguishable n = nullable (dist n)

  union : Nat -> Gen UnionType
  union n = let um = unionMember n
             in [| UT um um (linList 2 um) |]

  unionMember : Nat -> Gen UnionMemberType
  unionMember 0     = [| MkUnionMember attributes (dist 0) |]
  unionMember (S k) = [| MkUnionMember attributes (dist k) |]

idlType' : Gen IdlType
idlType' = idlType 4

attributedType : Gen (Attributed IdlType)
attributedType = attributed idlType'

optionalType : Gen OptionalType
optionalType = maybe attributedType

--------------------------------------------------------------------------------
--          Arguments
--------------------------------------------------------------------------------

constValue : Gen ConstValue
constValue = choice [ map B bool, map I intLit, map F floatLit ]

defaultVal : Gen Default
defaultVal = choice [ map C constValue
                    , map S stringLit
                    , element [None, EmptyList, EmptySet, Null]
                    ]

argName : Gen ArgumentName
argName = choice [ map (MkArgName . value) identifier
                 , map MkArgName $ element [ "async"
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

arg : Gen Arg
arg = [| MkArg attributes idlType' argName |]

optArg : Gen OptArg
optArg = [| MkOptArg attributes attributes idlType' argName defaultVal |]

argumentList : Gen ArgumentList
argumentList =
  choice [ [| VarArg (linList 5 arg) arg |]
         , [| NoVarArg (linList 5 arg) (linList 5 optArg) |]
         ]

constType : Gen ConstType
constType = choice [map CP primitive, map CI identifier]

--------------------------------------------------------------------------------
--          Member
--------------------------------------------------------------------------------

memberSize : Nat
memberSize = 20

export
const : Gen Const
const = [| MkConst constType identifier constValue |]

special : Gen Special
special = element [Getter,Setter,Deleter]

opName : Gen OperationName
opName = frequency [ (1, pure (MkOpName "includes"))
                   , (10, map (MkOpName . value) identifier)
                   ]

op : Gen a -> Gen (Op a)
op g = [| MkOp g idlType' (maybe opName) (argumentList) |]

regularOperation : Gen RegularOperation
regularOperation = op $ pure ()

specialOperation : Gen SpecialOperation
specialOperation = op special

export
operation : Gen Operation
operation = choice [ map regToOp  regularOperation
                   , map specToOp specialOperation
                   ]

callbackInterfaceMember : Gen CallbackInterfaceMember
callbackInterfaceMember = choice [ map (\v => inject v) const
                                 , map (\v => inject v) regularOperation
                                 ]

callbackInterfaceMembers : Gen CallbackInterfaceMembers
callbackInterfaceMembers = linList memberSize (attributed callbackInterfaceMember)

inheritance : Gen Inheritance
inheritance = maybe identifier

dictMemberRest : Gen DictionaryMemberRest
dictMemberRest =
  choice [ [| Required attributes idlType' identifier |]
         , [| Optional idlType' identifier defaultVal |]
         ]

dictMembers : Gen DictionaryMembers
dictMembers = linList memberSize (attributed dictMemberRest)

attributeName : Gen AttributeName
attributeName =
  frequency [ (5, map (MkAttributeName . value) identifier)
            , (1, map MkAttributeName $ element ["async","required"])
            ]

readonly : Gen a -> Gen (Readonly a)
readonly = map MkRO

inherit : Gen a -> Gen (Inherit a)
inherit = map MkI

attribute : Gen Attribute
attribute = [| MkAttribute attributes idlType' attributeName |]

stringifier : Gen Stringifier
stringifier = choice [ map (\v => inject v) regularOperation
                     , map (\v => inject v) $ readonly attribute
                     , map (\v => inject v) $ attribute
                     , map (\v => inject v) (pure ())
                     ]

static : Gen StaticMember
static = choice [ map (\v => inject v) regularOperation
                , map (\v => inject v) $ readonly attribute
                , map (\v => inject v) $ attribute
                ]

maplike : Gen Maplike
maplike = [| MkMaplike attributedType attributedType |]

setlike : Gen Setlike
setlike = [| MkSetlike attributedType |]

namespaceMember : Gen NamespaceMember
namespaceMember = choice [ map (\v => inject v) regularOperation
                         , map (\v => inject v) $ readonly attribute
                         ]

namespaceMembers : Gen NamespaceMembers
namespaceMembers = linList memberSize (attributed namespaceMember)

constructor_ : Gen Constructor
constructor_ = map MkConstructor argumentList

partialInterfaceMember : Gen PartialInterfaceMember
partialInterfaceMember =
  choice [ map IConst const
         , map IOp operation
         , map IAttr attribute
         , map IAttrRO (readonly attribute)
         , map IAttrInh (inherit attribute)
         , map IMap maplike
         , map IMapRO (readonly maplike)
         , map ISet setlike
         , map ISetRO (readonly setlike)
         , map IStr stringifier
         , map IStatic static
         , [| IIterable attributedType optionalType |]
         , [| IAsync attributedType optionalType argumentList |]
         ]

mixinMember : Gen MixinMember
mixinMember = choice [ map MConst const
                     , map MOp regularOperation
                     , map MAttr attribute
                     , map MAttrRO (readonly attribute)
                     , map MStr stringifier
                     ]

mixinMembers : Gen MixinMembers
mixinMembers = linList memberSize (attributed mixinMember)

partialInterfaceMembers : Gen PartialInterfaceMembers
partialInterfaceMembers = linList memberSize (attributed partialInterfaceMember)

export
interfaceMember : Gen InterfaceMember
interfaceMember = frequency [ (1, map (\v => inject v) constructor_)
                            , (10, map (\v => inject v) partialInterfaceMember)
                            ]

interfaceMembers : Gen InterfaceMembers
interfaceMembers = linList memberSize (attributed interfaceMember)

--------------------------------------------------------------------------------
--          Definition
--------------------------------------------------------------------------------

export
definition : Gen Definition
definition = ns
  [ [| MkCallback attributes identifier idlType' argumentList |]
  , [| MkCallbackInterface attributes identifier callbackInterfaceMembers |]
  , [| MkDictionary attributes identifier inheritance dictMembers |]
  , [| MkEnum attributes identifier (linList1 5 stringLit) |]
  , [| MkIncludes attributes identifier identifier |]
  , [| MkInterface attributes identifier inheritance interfaceMembers |]
  , [| MkMixin  attributes identifier mixinMembers |]
  , [| MkNamespace attributes identifier namespaceMembers |]
  , [| MkTypedef attributes attributes idlType' identifier |]
  ]

export
part : Gen Part
part = ns
  [ [| MkPDictionary attributes identifier dictMembers |]
  , [| MkPInterface attributes identifier partialInterfaceMembers |]
  , [| MkPMixin attributes identifier mixinMembers |]
  , [| MkPNamespace attributes identifier namespaceMembers |]
  ]
