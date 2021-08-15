module Text.WebIDL.Parser

import Data.SOP
import Data.List
import Data.List.Elem
import Data.List1
import Text.Lexer
import Text.Parser
import Text.WebIDL.Types
import Text.WebIDL.Lexer

--------------------------------------------------------------------------------
--          Utilities
--------------------------------------------------------------------------------

public export
IdlGrammarAny : (b : Bool) -> Type -> Type
IdlGrammarAny b t = Grammar () IdlToken b t

public export
IdlGrammar : Type -> Type
IdlGrammar = IdlGrammarAny True

public export
IdlGrammar' : Type -> Type
IdlGrammar' = IdlGrammarAny False

tok : String -> (IdlToken -> Maybe a) -> IdlGrammar a
tok s f = terminal s f

withKey : String -> (String -> Maybe a) -> IdlGrammar a
withKey s f = tok s $ \case (Key $ MkKeyword s _) => f s
                            _                     => Nothing

intLit : IdlGrammar IntLit
intLit = tok "Int Lit" $ \case ILit n => Just n
                               _      => Nothing

stringLit : IdlGrammar StringLit
stringLit = tok "String Lit" $ \case SLit s => Just s
                                     _      => Nothing

floatLit : IdlGrammar FloatLit
floatLit = tok "Float Lit" $ \case FLit v => Just v
                                   _      => Nothing

--------------------------------------------------------------------------------
--          Symbols
--------------------------------------------------------------------------------

symbol : Char -> IdlGrammar ()
symbol c = tok ("Symbol " ++ show c) $ \case Other (Symb v) => guard (c == v)
                                             _              => Nothing

comma : IdlGrammar ()
comma = symbol ','

ellipsis : IdlGrammar ()
ellipsis = tok "Ellipsis" $ \case Other Ellipsis => Just ()
                                  _              => Nothing

inParens : {b : _} -> Inf (IdlGrammarAny b a) -> IdlGrammar a
inParens g = symbol '(' *> g <* symbol ')'

inBrackets : {b : _} -> Inf (IdlGrammarAny b a) -> IdlGrammar a
inBrackets g = symbol '[' *> g <* symbol ']'

inBraces : {b : _} -> Inf (IdlGrammarAny b a) -> IdlGrammar a
inBraces g = symbol '{' *> g <* symbol '}'

inAngles : {b : _} -> Inf (IdlGrammarAny b a) -> IdlGrammar a
inAngles g = symbol '<' *> g <* symbol '>'

inAnyParens : {b : _} -> Inf (IdlGrammarAny b a) -> IdlGrammar a
inAnyParens g = inParens g <|> inBrackets g <|> inBraces g

sepList1 : Char -> IdlGrammar a -> IdlGrammar (List1 a)
sepList1 c g =   [| (g <* symbol c) ::: sepBy (symbol c) g |]
             <|> map (\x => x ::: Nil) g

--------------------------------------------------------------------------------
--          Identifiers
--------------------------------------------------------------------------------

export
key : String -> IdlGrammar ()
key s = tok s $ \case Key (MkKeyword i _) => guard (i == s)
                      _                   => Nothing

export
ident : IdlGrammar Identifier
ident = tok "identifier" $ \case Ident i => Just i
                                 _       => Nothing

export
keyword : IdlGrammar Keyword
keyword = tok "keyword" $ \case Key i => Just i
                                _     => Nothing

||| IdentifierList :: identifier Identifiers
||| Identifiers :: , identifier Identifiers ε
export
identifierList : IdlGrammar IdentifierList
identifierList = [| ident ::: many (comma *> ident) |]

--------------------------------------------------------------------------------
--          Extended Attributes
--------------------------------------------------------------------------------

symbolUnless : String -> (Char -> Bool) -> IdlGrammar Symbol
symbolUnless s f = tok s $ \case Other s => fromSym s
                                 _       => Nothing
  where fromSym : Symbol -> Maybe Symbol
        fromSym Ellipsis = Just Ellipsis
        fromSym (Symb c) = if f c then Nothing else Just (Symb c)
        

otherSym : IdlGrammar Symbol -> IdlGrammar Other
otherSym sym = choice {t = List} [ map (\v => inject v) intLit
                                 , map (\v => inject v) stringLit
                                 , map (\v => inject v) floatLit
                                 , map (\v => inject v) ident
                                 , map (\v => inject v) sym
                                 , map (\v => inject v) keyword
                                 ]

export
other : IdlGrammar Other
other = otherSym $ symbolUnless "other" isCommaOrParenOrQuote

export
otherOrComma : IdlGrammar Other
otherOrComma = otherSym $ symbolUnless "otherOrComma" isParenOrQuote

export
eaInner : IdlGrammar' EAInner
eaInner =   [| EAIParens (inAnyParens eaInner) eaInner |]
        <|> [| EAIOther otherOrComma eaInner |]
        <|> pure EAIEmpty

export
extAttribute : IdlGrammar ExtAttribute
extAttribute =   [| EAParens (inAnyParens eaInner) (optional extAttribute) |]
             <|> [| EAOther other (optional extAttribute) |]

export
extAttrs1 : IdlGrammar ExtAttributeList
extAttrs1 = forget <$> inBrackets (sepBy1 comma extAttribute)

export
attributes : IdlGrammar' ExtAttributeList
attributes = extAttrs1 <|> pure Nil

export
attributed : IdlGrammar a -> IdlGrammar (Attributed a)
attributed g = [| (,) extAttrs1 g |] <|> map (Nil,) g

--------------------------------------------------------------------------------
--          Types
--------------------------------------------------------------------------------

bufferRelated : IdlGrammar BufferRelatedType
bufferRelated = withKey "BufferRelated" $
                  \case "ArrayBuffer"       => Just ArrayBuffer
                        "DataView"          => Just DataView
                        "Int8Array"         => Just Int8Array
                        "Int16Array"        => Just Int16Array
                        "Int32Array"        => Just Int32Array
                        "Uint8Array"        => Just Uint8Array
                        "Uint16Array"       => Just Uint16Array
                        "Uint32Array"       => Just Uint32Array
                        "Uint8ClampedArray" => Just Uint8ClampedArray
                        "Float32Array"      => Just Float32Array
                        "Float64Array"      => Just Float64Array
                        _                   => Nothing

stringType : IdlGrammar StringType
stringType = withKey "stringType" $
               \case "ByteString" => Just ByteString
                     "DOMString"  => Just DOMString
                     "USVString"  => Just USVString
                     _            => Nothing

export
primitive : IdlGrammar PrimitiveType
primitive =   key "unsigned"     *> map Unsigned int
          <|> key "unrestricted" *> map Unrestricted float
          <|> map Signed int
          <|> map Restricted float
          <|> withKey "Primitive" (\case "boolean"   => Just Boolean 
                                         "byte"      => Just Byte
                                         "octet"     => Just Octet
                                         "bigint"    => Just BigInt
                                         "undefined" => Just Undefined
                                         _           => Nothing)

  where int : IdlGrammar IntType
        int =   (key "long"  *> key "long" $> LongLong)
            <|> (key "long"  $> Long)
            <|> (key "short" $> Short)

        float : IdlGrammar FloatType
        float = withKey "FloatType" $ \case "double" => Just Dbl
                                            "float"  => Just Float
                                            _        => Nothing

constType : IdlGrammar ConstType
constType = map CP primitive <|> map CI ident

nullable : IdlGrammar a -> IdlGrammar (Nullable a)
nullable g = map MaybeNull (g <* symbol '?') <|> map NotNull g


mutual
  -- Type ::
  --     SingleType
  --     UnionType Null
  -- 
  -- SingleType ::
  --     DistinguishableType
  --     any
  --     PromiseType
  -- PromiseType ::
  --     Promise < Type >
  export
  idlType : IdlGrammar IdlType
  idlType =   (key "any" $> Any)
          <|> map Promise (key "Promise" *> inAngles idlType)
          <|> map D distinguishableType
          <|> (nullable flatUnion >>= map U . fromFlatUnion)

    where um : Attributed (Nullable Distinguishable) -> UnionMemberType
          um (a, MaybeNull x) = MkUnionMember a x
          um (a, NotNull x)   = MkUnionMember a x

          fromFlatUnion :  Nullable (List1 $ Attributed $ Nullable Distinguishable)
                        -> IdlGrammar' (Nullable UnionType)
          fromFlatUnion (MaybeNull $ a ::: b :: t) =
            pure . MaybeNull $ UT (um a) (um b) (map um t)

          fromFlatUnion (NotNull   $ a ::: b :: t) =
            if any (isNullable . snd) (a::b::t)
               then pure . MaybeNull $ UT (um a) (um b) (map um t)
               else pure . NotNull   $ UT (um a) (um b) (map um t)

          fromFlatUnion _                          = fail "no enough union members"

  -- TypeWithExtendedAttributes ::
  --     ExtendedAttributeList Type
  attrTpe : IdlGrammar (Attributed IdlType)
  attrTpe = attributed idlType

  -- RecordType ::
  --     record < StringType , TypeWithExtendedAttributes >
  recrd : IdlGrammar Distinguishable
  recrd = Record <$> (key "record" *> symbol '<' *> stringType)
                 <*> (comma *> attributes)
                 <*> (idlType <* symbol '>')

  -- DistinguishableType ::
  --     PrimitiveType Null
  --     StringType Null
  --     identifier Null
  --     sequence < TypeWithExtendedAttributes > Null
  --     object Null
  --     symbol Null
  --     BufferRelatedType Null
  --     FrozenArray < TypeWithExtendedAttributes > Null
  --     ObservableArray < TypeWithExtendedAttributes > Null
  --     RecordType Null
  distinguishable : IdlGrammar Distinguishable
  distinguishable =
        map P primitive
    <|> map S stringType
    <|> map B bufferRelated
    <|> (key "object" $> Object)
    <|> (key "symbol" $> Symbol)
    <|> (key "sequence" *> inAngles [| Sequence attributes idlType |])
    <|> (key "FrozenArray" *> inAngles [| FrozenArray attributes idlType |])
    <|> (key "ObservableArray" *> inAngles [| ObservableArray attributes idlType |])
    <|> recrd
    <|> map I ident

  distinguishableType : IdlGrammar (Nullable Distinguishable)
  distinguishableType = nullable distinguishable

  -- UnionType ::
  --     ( UnionMemberType or UnionMemberType UnionMemberTypes )
  --
  -- UnionMemberTypes ::
  --     or UnionMemberType UnionMemberTypes
  --     ε
  flatUnion : IdlGrammar (List1 $ Attributed $ Nullable Distinguishable)
  flatUnion = inParens $ do (a :: b :: t) <- sepBy (key "or") flatMember
                              | _ => fail "Non enough Union members"
                            pure (join $ a ::: b :: t)

  -- UnionMemberType ::
  --     ExtendedAttributeList DistinguishableType
  --     UnionType Null
  flatMember : IdlGrammar (List1 $ Attributed $ Nullable Distinguishable)
  flatMember = map singleton (attributed distinguishableType) <|> flatUnion

optionalType : IdlGrammar' OptionalType
optionalType = optional (symbol ',' *> attributed idlType)

--------------------------------------------------------------------------------
--          Arguments
--------------------------------------------------------------------------------

boolLit : IdlGrammar Bool
boolLit = (key "false" $> False) <|> (key "true" $> True)

constValue : IdlGrammar ConstValue
constValue = map B boolLit <|> map F floatLit <|> map I intLit

defaultV : IdlGrammar' Default
defaultV =   (symbol '=' *> (
                   (symbol '[' *> symbol ']' $> EmptyList)
               <|> (symbol '{' *> symbol '}' $> EmptySet)
               <|> (key "null" $> Null)
               <|> map S stringLit
               <|> map C constValue
             )) <|> pure None

argName : IdlGrammar ArgumentName
argName =   withKey "ArgumentNameKeyword"
              (map (MkArgName . value) . ArgumentNameKeyword.refine)
        <|> map (MkArgName . value) ident

arg : IdlGrammar Arg
arg = [| MkArg attributes idlType argName |]

vararg : IdlGrammar Arg
vararg = [| MkArg attributes (idlType <* ellipsis) argName |]

optArg : IdlGrammar OptArg
optArg = [| MkOptArg attributes
                     (key "optional" *> attributes)
                     idlType
                     argName
                     defaultV |]

argumentList : IdlGrammar' ArgumentList
argumentList =   [| VarArg args vararg |]
             <|> [| NoVarArg (args1 <* comma) (sepBy comma optArg) |]
             <|> [| NoVarArg args1 (pure Nil) |]
             <|> [| NoVarArg (pure Nil) (sepBy comma optArg) |]

  where args1 : IdlGrammar (List Arg)
        args1 = forget <$> sepBy1 comma arg

        args : IdlGrammar' (List Arg)
        args = (args1 <* comma) <|> pure Nil

optArgList : IdlGrammar' ArgumentList
optArgList = inParens argumentList <|> pure (NoVarArg Nil Nil)

--------------------------------------------------------------------------------
--          Member
--------------------------------------------------------------------------------

member : List String -> IdlGrammar a -> IdlGrammar a
member [] g = g <* symbol ';'
member (h :: t) g = key h *> member t g

export
const : IdlGrammar Const
const = member ["const"]
        [| MkConst constType ident (symbol '=' *> constValue) |]

special : IdlGrammar Special
special =   (key "getter"  $> Getter)
        <|> (key "setter"  $> Setter)
        <|> (key "deleter" $> Deleter)

opName : IdlGrammar OperationName
opName =   (key "includes" $> MkOpName "includes")
       <|> map (\(MkIdent s) => MkOpName s) ident

regularOperation : IdlGrammar RegularOperation
regularOperation =
  member []
  [| MkOp (pure ()) idlType (optional opName) (inParens argumentList) |]

specialOperation : IdlGrammar SpecialOperation
specialOperation =
  member []
  [| MkOp special idlType (optional opName) (inParens argumentList) |]

export
operation : IdlGrammar Operation
operation = map specToOp specialOperation <|> map regToOp regularOperation

callbackInterfaceMember : IdlGrammar CallbackInterfaceMember
callbackInterfaceMember =   map (\v => inject v) const
                        <|> map (\v => inject v) regularOperation

dictMember : IdlGrammar DictionaryMemberRest
dictMember = member ["required"] [| Required attributes idlType ident |]
           <|> member [] [| Optional idlType ident defaultV |]

inheritance : IdlGrammar' Inheritance
inheritance = optional (symbol ':' *> ident)

attributeName : IdlGrammar AttributeName
attributeName =  withKey "AttributeNameKeyword"
                   (map (MkAttributeName . value) . AttributeNameKeyword.refine)
             <|> map (MkAttributeName . value) ident

readonly : IdlGrammar a -> IdlGrammar (Readonly a)
readonly g = key "readonly" *> map MkRO g

inherit : IdlGrammar a -> IdlGrammar (Inherit a)
inherit g = key "inherit" *> map MkI g

attribute : IdlGrammar Attribute
attribute = member ["attribute"]
            [| MkAttribute attributes idlType attributeName |]

stringifier : IdlGrammar Stringifier
stringifier =   key "stringifier" *> (
                map (\v => inject v) attribute
            <|> map (\v => inject v) (readonly attribute)
            <|> map (\v => inject v) regularOperation
            <|> map (\v => inject v) (symbol ';')
            )

static : IdlGrammar StaticMember
static =   key "static" *> (
           map (\v => inject v) attribute
       <|> map (\v => inject v) (readonly attribute)
       <|> map (\v => inject v) regularOperation
       )

maplike : IdlGrammar Maplike
maplike = member ["maplike"] $ inAngles [| MkMaplike (attributed idlType)
                                        (symbol ',' *> attributed idlType) |]

setlike : IdlGrammar Setlike
setlike = member ["setlike"] $ inAngles [| MkSetlike (attributed idlType) |]

namespaceMember : IdlGrammar NamespaceMember
namespaceMember =   map (\v => inject v) regularOperation
                <|> map (\v => inject v) (readonly attribute)

constructor_ : IdlGrammar Constructor
constructor_ =
  member ["constructor"] (map MkConstructor $ inParens argumentList)

partialInterfaceMember : IdlGrammar PartialInterfaceMember
partialInterfaceMember =
      map IConst const
  <|> map IOp operation
  <|> map IAttr attribute
  <|> map IAttrRO (readonly attribute)
  <|> map IAttrInh (inherit attribute)
  <|> map IMap maplike
  <|> map IMapRO (readonly maplike)
  <|> map ISet setlike
  <|> map ISetRO (readonly setlike)
  <|> map IStr stringifier
  <|> map IStatic static
  <|> member ["iterable"] (inAngles [| IIterable (attributed idlType)
                                            optionalType |])
  <|> member ["async","iterable"] (
        do p  <- inAngles [| (,) (attributed idlType) optionalType |]
           as <- optArgList
           pure (IAsync (fst p) (snd p) as))

mixinMember : IdlGrammar MixinMember
mixinMember =   map MConst const
            <|> map MOp regularOperation
            <|> map MAttr attribute
            <|> map MAttrRO (readonly attribute)
            <|> map MStr stringifier

export
interfaceMember : IdlGrammar InterfaceMember
interfaceMember =   map (\v => inject v) constructor_
                <|> map (\v => inject v) partialInterfaceMember

members : IdlGrammar a -> IdlGrammar (List $ Attributed a)
members g = inBraces (many $ attributed g)

--------------------------------------------------------------------------------
--          Definition
--------------------------------------------------------------------------------

def :  (ss : List String)
    -> {auto 0 prf : NonEmpty ss}
    -> (IdlGrammar ExtAttributeList -> IdlGrammar a)
    -> IdlGrammar a
def (s :: ss) g = g (run ss (attributes <* key s)) <* symbol ';'
  where run : List String -> IdlGrammar x -> IdlGrammar x
        run []        y = y
        run (x :: xs) y = run xs (y <* key x)

def0 : (IdlGrammar' ExtAttributeList -> IdlGrammar a) -> IdlGrammar a
def0 g = g attributes <* symbol ';'

-- optional trailing comma
enumLits : IdlGrammar (List1 StringLit)
enumLits = sepList1 ',' stringLit <* (symbol ',' <|> pure ())

callback : IdlGrammar Callback
callback =
  def ["callback"] $ \as =>
  [| MkCallback as ident (symbol '=' *> idlType) (inParens argumentList) |]

callbackInterface : IdlGrammar CallbackInterface
callbackInterface =
  def ["callback","interface"] $ \as =>
  [| MkCallbackInterface as ident (members callbackInterfaceMember) |]

dictionary : IdlGrammar Dictionary
dictionary =
  def ["dictionary"] $ \as =>
  [| MkDictionary as ident inheritance (members dictMember) |]

enum : IdlGrammar Enum
enum = def ["enum"] $ \as => [| MkEnum as ident (inBraces enumLits) |]

iface : IdlGrammar Interface
iface =
  def ["interface"] $ \as =>
  [| MkInterface as ident inheritance (members interfaceMember) |]

includes : IdlGrammar Includes
includes =
  def0 $ \as => [| MkIncludes as ident (key "includes" *> ident) |]

mixin : IdlGrammar Mixin
mixin = def ["interface","mixin"] $ \as =>
        [| MkMixin as ident (members mixinMember) |]

nspace : IdlGrammar Namespace
nspace = def ["namespace"] $ \as =>
         [| MkNamespace as ident (members namespaceMember) |]

pdictionary : IdlGrammar PDictionary
pdictionary = def ["partial","dictionary"] $ \as =>
              [| MkPDictionary as ident (members dictMember) |]

pnamespace : IdlGrammar PNamespace
pnamespace = def ["partial","namespace"] $ \as =>
             [| MkPNamespace as ident (members namespaceMember) |]

pmixin : IdlGrammar PMixin
pmixin = def ["partial","interface","mixin"] $ \as =>
         [| MkPMixin as ident (members mixinMember) |]

pinterface : IdlGrammar PInterface
pinterface =
  def ["partial","interface"] $ \as =>
  [| MkPInterface as ident (members partialInterfaceMember) |]

typedef : IdlGrammar Typedef
typedef = def ["typedef"] $ \as =>
          [| MkTypedef as attributes idlType ident |]

export
definition : IdlGrammar Definition
definition =
      map (\v => inject v) callbackInterface
  <|> map (\v => inject v) callback
  <|> map (\v => inject v) dictionary
  <|> map (\v => inject v) enum
  <|> map (\v => inject v) iface
  <|> map (\v => inject v) includes
  <|> map (\v => inject v) mixin
  <|> map (\v => inject v) nspace
  <|> map (\v => inject v) typedef

export
part : IdlGrammar Part
part =   map (\v => inject v) pdictionary
     <|> map (\v => inject v) pinterface
     <|> map (\v => inject v) pmixin
     <|> map (\v => inject v) pnamespace


export
partsAndDefs : IdlGrammar PartsAndDefs
partsAndDefs = accumNs . forget <$> some partOrDef
  where partOrDef : IdlGrammar PartOrDef
        partOrDef =   map Z part
                  <|> map (S . Z) definition

--------------------------------------------------------------------------------
--          Parsing WebIDL
--------------------------------------------------------------------------------

toParseErr : ParsingError IdlToken -> Err
toParseErr (Error x Nothing)  = ParseErr x
toParseErr (Error x $ Just $ MkBounds startLine startCol _ _) =
  ParseErrAt x startLine startCol

export
parseIdl : IdlGrammar a -> String -> Either Err a
parseIdl g s = do ts <- mapFst LexErr (lexIdlNoNoise s)
                  (res,Nil) <- mapFst (toParseErr . head) (parse g ts)
                    | (_,b :: _) => Left (NoEOI b)
                  pure res
