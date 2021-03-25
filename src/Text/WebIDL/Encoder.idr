module Text.WebIDL.Encoder

import Data.SOP
import Data.List
import Data.String
import Text.WebIDL.Types

--------------------------------------------------------------------------------
--          Utilities
--------------------------------------------------------------------------------

public export
0 Encoder : Type -> Type
Encoder a = a -> String

runEnc : forall a . Encoder a -> a -> String
runEnc = apply

export
ident : Encoder Identifier
ident = value

inParens : Encoder a -> Encoder a
inParens f a = "(" ++ f a ++ ")"

inBrackets : Encoder a -> Encoder a
inBrackets f a = "[" ++ f a ++ "]"

inBraces : Encoder a -> Encoder a
inBraces f a = "{" ++ f a ++ "}"

inAngles : Encoder a -> Encoder a
inAngles f a = "<" ++ f a ++ ">"

emaybe : Encoder a -> Encoder (Maybe a)
emaybe f = maybe "" f

sepList : (sep : String) -> Encoder a -> Encoder (List a)
sepList sep f = fastConcat . intersperse sep . map f

emptyIfNull : Foldable f =>  Encoder (f a) -> Encoder (f a)
emptyIfNull f as = if null as then "" else f as

spaced : List String -> String
spaced = fastConcat . intersperse " "

--------------------------------------------------------------------------------
--          Literals
--------------------------------------------------------------------------------

toHexDigit : Integer -> Char
toHexDigit 0 = '0'
toHexDigit 1 = '1'
toHexDigit 2 = '2'
toHexDigit 3 = '3'
toHexDigit 4 = '4'
toHexDigit 5 = '5'
toHexDigit 6 = '6'
toHexDigit 7 = '7'
toHexDigit 8 = '8'
toHexDigit 9 = '9'
toHexDigit 10 = 'A'
toHexDigit 11 = 'B'
toHexDigit 12 = 'C'
toHexDigit 13 = 'D'
toHexDigit 14 = 'E'
toHexDigit 15 = 'F'
toHexDigit _ = '_'

export
toDigits : (pre : String) -> (base : Integer) -> Nat -> String
toDigits pre base n = pre ++ fastPack(run (natToInteger n) [])
  where run : Integer -> List Char -> List Char
        run n cs = let n'  = n `div` base
                       cs' = (toHexDigit $ n `mod` base) :: cs
                    in if n' == 0 then cs' else run n' cs'

export
intLit : Encoder IntLit
intLit (Hex k) = toDigits "0x" 16 k
intLit (Oct k) = toDigits "0" 8 k
intLit (I x)   = show x

sig : Encoder Signum
sig Plus  = ""
sig Minus = "-"

export
floatLit : Encoder FloatLit
floatLit Infinity          = "Infinity"
floatLit NegativeInfinity  = "-Infinity"
floatLit NaN               = "NaN"
floatLit (NoExp s bd ad)   = fastConcat [sig s,show bd,".",show ad]
floatLit (Exp s bd ad exp) =
  fastConcat [sig s,show bd,maybe "" (("." ++) . show) ad,"e",show exp]

export
stringLit : Encoder StringLit
stringLit = value

export
keyword : Encoder Keyword
keyword = value

export
symbol : Encoder Symbol
symbol Ellipsis = "..."
symbol (Symb c) = singleton c

--------------------------------------------------------------------------------
--          Attribute
--------------------------------------------------------------------------------

export
other : Encoder Other
other = collapseNS 
      . hliftA2 runEnc [intLit,floatLit,stringLit,ident,keyword,symbol]

export
eaInner : Encoder EAInner
eaInner (EAIParens ip eai) = inParens eaInner ip ++ " " ++ eaInner eai
eaInner (EAIOther o eai)   = other o ++ " " ++ eaInner eai
eaInner EAIEmpty           = ""

export
extAttribute : Encoder ExtAttribute
extAttribute (EAParens i r) = inParens eaInner i ++ emaybe extAttribute r
extAttribute (EAOther o r)  = other o ++ " " ++ emaybe extAttribute r

export
extAttributes : Encoder ExtAttributeList
extAttributes = emptyIfNull . inBrackets $ sepList "," extAttribute

export
attributed : Encoder a -> Encoder (Attributed a)
attributed f (as,a) = extAttributes as ++ " " ++ f a

--------------------------------------------------------------------------------
--          Type
--------------------------------------------------------------------------------

export
bufferRelated : Encoder BufferRelatedType
bufferRelated = show

export
stringType : Encoder StringType
stringType = show

export
intType : Encoder IntType
intType Short    = "short"
intType Long     = "long"
intType LongLong = "long long"

export
floatType : Encoder FloatType
floatType Float = "float"
floatType Dbl   = "double"

export
primitive : Encoder PrimitiveType
primitive (Unsigned x)     = "unsigned " ++ intType x
primitive (Signed x)       = intType x
primitive (Unrestricted x) = "unrestricted " ++ floatType x
primitive (Restricted x)   = floatType x
primitive Undefined        = "undefined"
primitive Boolean          = "boolean"
primitive Byte             = "byte"
primitive Octet            = "octet"
primitive BigInt           = "bigint"

export
constType : Encoder ConstType
constType (CP x) = primitive x
constType (CI x) = ident x

export
nullable : Encoder a -> Encoder (Nullable a)
nullable f (MaybeNull x) = f x ++ "?"
nullable f (NotNull x)   = f x

mutual
  export
  idlType : Encoder IdlType
  idlType Any         = "any"
  idlType (D x)       = nullable distinguishable x
  idlType (U x)       = nullable union x
  idlType (Promise x) = "Promise" ++ inAngles idlType x

  export
  unionMember : Encoder UnionMemberType
  unionMember (UD x) = attributed (nullable distinguishable) x
  unionMember (UU x) = nullable union x

  export
  union : Encoder UnionType
  union (UT fst snd rest) =
    inParens (sepList " or " unionMember) (fst :: snd :: rest)

  export
  distinguishable : Encoder Distinguishable
  distinguishable (P x) = primitive x
  distinguishable (S x) = stringType x
  distinguishable (I x) = ident x
  distinguishable (B x) = bufferRelated x
  distinguishable (Sequence x) =
    "sequence" ++ inAngles (attributed idlType) x
  distinguishable (FrozenArray x) =
    "FrozenArray" ++ inAngles (attributed idlType) x
  distinguishable (ObservableArray x) =
    "ObservableArray" ++ inAngles (attributed idlType) x
  distinguishable (Record x y) =
    "record<" ++ stringType x ++ "," ++ attributed idlType y ++ ">"
  distinguishable Object = "object"
  distinguishable Symbol = "symbol"

optionalType : Encoder OptionalType
optionalType = maybe "" (\p => "," ++ attributed idlType p)

--------------------------------------------------------------------------------
--          Arguments
--------------------------------------------------------------------------------

export
constValue : Encoder ConstValue
constValue (B True)  = "true"
constValue (B False) = "false"
constValue (F x)     = floatLit x
constValue (I x)     = intLit x

export
defaultV : Encoder Default
defaultV None      = ""
defaultV EmptyList = "= []"
defaultV EmptySet  = "= {}"
defaultV Null      = "= null"
defaultV (S x)     = "= " ++ stringLit x
defaultV (C x)     = "= " ++ constValue x

export
argumentRest : Encoder ArgumentRest
argumentRest (Optional t n d) =
  spaced ["optional",attributed idlType t,n.value,defaultV d]
argumentRest (Mandatory t n) = spaced [idlType t,n.value]
argumentRest (VarArg t n)    = spaced [idlType t ++ "...",n.value]

export
argumentList : Encoder ArgumentList
argumentList = sepList "," (attributed argumentRest)

optArgList : Encoder ArgumentList
optArgList Nil = ""
optArgList as  = inParens argumentList as

--------------------------------------------------------------------------------
--          Members
--------------------------------------------------------------------------------

member : (key : String) -> List String -> String
member "" vs = spaced vs ++ ";"
member k vs  = spaced (k :: vs) ++ ";"

export
const : Encoder Const
const (MkConst t n v) = member "const" [constType t,n.value,"=",constValue v]

export
special : Encoder Special
special Getter  = "getter"
special Setter  = "setter"
special Deleter = "deleter"

export
op : Encoder a -> Encoder (Op a)
op f (MkOp s t n a) =
  member "" [f s, idlType t, maybe "" value n, inParens argumentList a]

export
regularOperation : Encoder RegularOperation
regularOperation = op (const "")

export
specialOperation : Encoder SpecialOperation
specialOperation = op special

export
operation : Encoder Operation
operation = op (maybe "" special)

callbackInterfaceMember : Encoder CallbackInterfaceMember
callbackInterfaceMember = collapseNS . hliftA2 runEnc [const,regularOperation]

callbackInterfaceMembers : Encoder CallbackInterfaceMembers
callbackInterfaceMembers = sepList " " $ attributed callbackInterfaceMember

inheritance : Encoder Inheritance
inheritance = maybe "" \i => " : " ++ i.value

dictMemberRest : Encoder DictionaryMemberRest
dictMemberRest (Required as t n) =
  member "required" [extAttributes as,idlType t,n.value]
dictMemberRest (Optional t n d) =
  member "" [idlType t, n.value, defaultV d]

dictMembers : Encoder DictionaryMembers
dictMembers = sepList " " $ attributed dictMemberRest

readonly : Encoder a -> Encoder (Readonly a)
readonly f = ("readonly " ++) . f . value

inherit : Encoder a -> Encoder (Inherit a)
inherit f = ("inherit " ++) . f . value

attribute : Encoder Attribute
attribute (MkAttribute as t n) =
  member "attribute" [extAttributes as, idlType t, n.value]

stringifier : Encoder Stringifier
stringifier = ("stringifier " ++)
            . collapseNS
            . hliftA2 runEnc [attribute,readonly attribute,regularOperation,const ";"]

static : Encoder StaticMember
static = ("static " ++)
       . collapseNS
       . hliftA2 runEnc [attribute,readonly attribute,regularOperation]

maplike : Encoder Maplike
maplike (MkMaplike l r) =
  member "maplike" ["<",attributed idlType l,",",attributed idlType r,">"]

setlike : Encoder Setlike
setlike (MkSetlike p) = member "setlike" ["<",attributed idlType p,">"]

namespaceMember : Encoder NamespaceMember
namespaceMember = collapseNS 
                . hliftA2 runEnc [regularOperation,readonly attribute]

namespaceMembers : Encoder NamespaceMembers
namespaceMembers = sepList " " $ attributed namespaceMember

constructor_ : Encoder Constructor
constructor_ (MkConstructor args) =
  member "constructor" [inParens argumentList args] 

partialInterfaceMember : Encoder PartialInterfaceMember
partialInterfaceMember (IConst x)       = const x
partialInterfaceMember (IOp x)          = operation x
partialInterfaceMember (IAttr x)        = attribute x
partialInterfaceMember (IAttrRO x)      = readonly attribute x
partialInterfaceMember (IAttrInh x)     = inherit attribute x
partialInterfaceMember (IMap x)         = maplike x
partialInterfaceMember (IMapRO x)       = readonly maplike x
partialInterfaceMember (ISet x)         = setlike x
partialInterfaceMember (ISetRO x)       = readonly setlike x
partialInterfaceMember (IStr x)         = stringifier x
partialInterfaceMember (IStatic x)      = static x
partialInterfaceMember (IIterable p o)  =
  member "iterable" ["<",attributed idlType p,optionalType o,">"]
partialInterfaceMember (IAsync p o a)   =
  member "async iterable"
    ["<",attributed idlType p,optionalType o,">",optArgList a]

mixinMember : Encoder MixinMember
mixinMember (MConst x)       = const x
mixinMember (MOp x)          = regularOperation x
mixinMember (MAttr x)        = attribute x
mixinMember (MAttrRO x)      = readonly attribute x
mixinMember (MStr x)         = stringifier x

partialInterfaceMembers : Encoder PartialInterfaceMembers
partialInterfaceMembers = sepList " " $ attributed partialInterfaceMember

mixinMembers : Encoder MixinMembers
mixinMembers = sepList " " $ attributed mixinMember

export
interfaceMember : Encoder InterfaceMember
interfaceMember = collapseNS 
                . hliftA2 runEnc [constructor_,partialInterfaceMember]

interfaceMembers : Encoder InterfaceMembers
interfaceMembers = sepList " " $ attributed interfaceMember

--------------------------------------------------------------------------------
--          Definition
--------------------------------------------------------------------------------

def : ExtAttributeList -> (key : String) -> List String -> String
def as ""  ss = extAttributes as ++ spaced ss ++ ";"
def as key ss = extAttributes as ++ spaced (key :: ss) ++ ";"

callback : Encoder Callback
callback (MkCallback as n t args) =
  def as "callback" [n.value, "=", idlType t, inParens argumentList args]

callbackInterface : Encoder CallbackInterface
callbackInterface (MkCallbackInterface as n ms) =
  def as "callback interface"
  [n.value, inBraces callbackInterfaceMembers ms]

dictionary : Encoder Dictionary
dictionary (MkDictionary as n i ms) =
  def as "dictionary"
  [n.value, inheritance i, inBraces dictMembers ms]

enum : Encoder Enum
enum (MkEnum as n vs) =
  def as "enum" [n.value, inBraces (sepList "," stringLit) (forget vs)]

iface : Encoder Interface
iface (MkInterface as n i ms) =
  def as "interface" [n.value, inheritance i, inBraces interfaceMembers ms]

includes : Encoder Includes
includes (MkIncludes as a b) = def as "" [a.value,"includes",b.value]

mixin : Encoder Mixin
mixin (MkMixin as n ms) =
  def as "interface mixin" [n.value, inBraces mixinMembers ms]

nspace : Encoder Namespace
nspace (MkNamespace as n ms) =
  def as "namespace" [n.value, inBraces namespaceMembers ms]

pdictionary : Encoder PDictionary
pdictionary (MkPDictionary as n ms) =
  def as "partial dictionary" [n.value, inBraces dictMembers ms]

pinterface : Encoder PInterface
pinterface (MkPInterface as n ms) =
  def as "partial interface" [n.value, inBraces partialInterfaceMembers ms]

pmixin : Encoder PMixin
pmixin (MkPMixin as n ms) =
  def as "partial interface mixin" [n.value, inBraces mixinMembers ms]

pnamespace : Encoder PNamespace
pnamespace (MkPNamespace as n ms) =
  def as "partial namespace" [n.value, inBraces namespaceMembers ms]

typedef : Encoder Typedef
typedef (MkTypedef as tas t n) =
  def as "typedef" [extAttributes tas, idlType t, n.value]

export
definition : Encoder Definition
definition = collapseNS 
           . hliftA2 runEnc [ callback
                            , callbackInterface
                            , dictionary
                            , enum
                            , includes
                            , iface
                            , mixin
                            , nspace
                            , typedef
                            ]

export
part : Encoder Part
part = collapseNS . hliftA2 runEnc [pdictionary,pinterface,pmixin,pnamespace]
