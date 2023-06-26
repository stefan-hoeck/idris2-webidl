module Main

import Control.RIO.App
import Control.RIO.File
import Data.List.Elem
import Data.SOP
import Data.String
import System
import System.GetOpts
import Text.WebIDL.Codegen as Codegen
import Text.WebIDL.Encoder
import Text.WebIDL.Types
import Text.WebIDL.Parser
import Text.PrettyPrint.Bernardy

%default total

--------------------------------------------------------------------------------
--          Command line options
--------------------------------------------------------------------------------

record Config where
  [noHints]
  constructor MkConfig
  outDir         : Path Abs
  maxInheritance : Nat
  files          : List (File Abs)

init : Path Abs -> List (File Abs) -> Config
init cd = MkConfig (cd </> "../dom/modular/src/Web") 100

setOutDir : (cd : Path Abs) -> String -> Config -> Either (List String) Config
setOutDir cd s = case fromString {ty = FilePath} s of
  FP (PAbs sx) => Right . {outDir := PAbs sx}
  FP (PRel sx) => Right . {outDir := cd </> PRel sx}

descs : (cd : Path Abs) -> List $ OptDescr (Config -> Either (List String) Config)
descs cd = [ MkOpt ['o'] ["outDir"] (ReqArg (setOutDir cd) "<dir>")
            "output directory"
        ]

applyArgs : (cd : Path Abs) -> List String -> Either (List String) Config
applyArgs cd args =
  case getOpt RequireOrder (descs cd) args of
       MkResult opts n  [] [] => foldl (>>=) (Right $ init cd (mapMaybe fil n)) opts
       MkResult _ _ u e       => Left $ map unknown u ++ e

  where unknown : String -> String
        unknown = ("Unknown option: " ++)

        fil : String -> Maybe (File Abs)
        fil s = AbsFile.parse s <|> map (cd </>) (RelFile.parse s)

--------------------------------------------------------------------------------
--          Codegen
--------------------------------------------------------------------------------

0 Prog : Type -> Type
Prog =
  App
    [ List CodegenErr
    , FileErr
    , List String
    , String
    ]

printErr : CodegenErr -> String
printErr (CBInterfaceInvalidOps x y k) =
  "Invalid number of callback operations in \{x.domain}: \{y.value} (\{show k} operations)"
printErr (RegularOpWithoutName x y) =
  "Unnamed regular operation in \{x.domain}: \{y.value}"
printErr (InvalidGetter x y) =
  "Invalid getter in \{x.domain}: \{y.value}"
printErr (InvalidSetter x y) =
  "Invalid setter in \{x.domain}: \{y.value}"
printErr (UnresolvedAlias x y) =
  "Unresolved alias in \{x.domain}: \{y.value}"
printErr (AnyInUnion x) = "\"Any\" type in a union type in \{x.domain}"
printErr (PromiseInUnion x) = "\"Promise\" type in a union type in \{x.domain}"
printErr (NullableAny x) = "Nullable \"Any\" type in \{x.domain}"
printErr (NullablePromise x) = "Nullable \"Promise\" type in \{x.domain}"
printErr (InvalidConstType x) = "Invalid constant type in \{x.domain}"

parseErr : Interpolation a => File Abs -> String -> Bounded a -> String
parseErr x str (B v bs) = printParseError str (FC (FileSrc "\{x}") bs) v

parameters {auto conf : Config}
           {auto fs   : FS}

  loadDef : File Abs -> Prog (String,PartsAndDefs)
  loadDef f = do
    s <- read f 1_000_000_000
    p <- injectEither (mapFst (parseErr f s) $ parseIdl partsAndDefs s)
    pure $ (maybe "" interpolate $ fileStem f, p)

  writeTo : Body -> (a -> Identifier) -> (a -> String) -> a -> Prog ()
  writeTo b f g v = case Body.parse (value $ f v) of
    Nothing => throw "Can't create file body for \{f v}"
    Just x  =>
      let parent := conf.outDir /> b
       in Prelude.do
         b <- exists parent
         when (not b) (mkDirP parent)
         write (parent /> x <.> "idr") (g v)

  types : CGDomain -> Prog ()
  types (MkDomain name callbacks dicts enums ifaces mixins) = Prelude.do
    traverse_ (writeTo "Types" Enum.name enum) enums
    traverse_ (writeTo "Types" CGIface.name ifaceType) ifaces
    traverse_ (writeTo "Types" CGDict.name dictType) dicts
    traverse_ (writeTo "Types" CGCallback.name callbackType) callbacks
    traverse_ (writeTo "Types" CGMixin.name mixinType) mixins

  raw : CGDomain -> Prog ()
  raw (MkDomain name callbacks dicts _ ifaces mixins) = Prelude.do
    traverse_ (writeTo "Raw" CGIface.name iface) ifaces
    traverse_ (writeTo "Raw" CGDict.name dict) dicts
    traverse_ (writeTo "Raw" CGCallback.name Definitions.callback) callbacks
    traverse_ (writeTo "Raw" CGMixin.name mixin) mixins


--------------------------------------------------------------------------------
--          Main Function
--------------------------------------------------------------------------------

covering
run : FS => List String -> Prog ()
run args = Prelude.do
  cd     <- curDir
  config <- injectEither $ applyArgs cd args
  ds     <- toDomains <$> traverse loadDef config.files

  let e = env config.maxInheritance ds

  doms   <- injectEither (traverse (domain e) ds)
  traverse_ types doms
  traverse_ raw doms
  pure ()

covering
main : IO ()
main = do
  (pn :: args) <- getArgs | Nil => die "Missing executable name. Aborting..."
  runApp
    [ traverse_ (putStrLn . printErr)
    , putStrLn . printErr
    , traverse_ putStrLn
    , putStrLn
    ]
    (run @{local} args)
