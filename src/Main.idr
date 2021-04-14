module Main

import Control.Monad.Either
import Data.List.Elem
import Data.SOP
import Data.String
import Data.Validated
import System
import System.Console.GetOpt
import System.File
import Text.WebIDL.Codegen as Codegen
import Text.WebIDL.Encoder
import Text.WebIDL.Types
import Text.WebIDL.Parser
import Text.PrettyPrint.Prettyprinter

--------------------------------------------------------------------------------
--          Command line options
--------------------------------------------------------------------------------

record Config where
  constructor MkConfig
  outDir         : String
  maxInheritance : Nat
  files          : List String

init : List String -> Config
init = MkConfig "../dom/src" 100

setOutDir : String -> Config -> Either (List String) Config
setOutDir s = Right . record { outDir = s }

descs : List $ OptDescr (Config -> Either (List String) Config)
descs = [ MkOpt ['o'] ["outDir"] (ReqArg setOutDir "<dir>")
            "output directory"
        ]

applyArgs : List String -> Either (List String) Config
applyArgs args =
  case getOpt RequireOrder descs args of
       MkResult opts n  [] [] => foldl (>>=) (Right $ init n) opts
       MkResult _ _ u e       => Left $ map unknown u ++ e

  where unknown : String -> String
        unknown = ("Unknown option: " ++)

--------------------------------------------------------------------------------
--          Codegen
--------------------------------------------------------------------------------

0 Prog : Type -> Type
Prog = EitherT String IO

toProgWith : (a -> String) ->  IO (Either a b) -> Prog b
toProgWith f io = MkEitherT $ map (mapFst f) io

toProg : Show a => IO (Either a b) -> Prog b
toProg = toProgWith show

runProg : Prog () -> IO ()
runProg (MkEitherT p) = do Right _ <- p
                             | Left e => putStrLn ("Error: " ++ e)
                           pure ()

fromCodegen : CodegenV a -> Prog a
fromCodegen = toProgWith (fastUnlines . map err) . pure . toEither
  where err : CodegenErr -> String
        err (CBInterfaceInvalidOps x y k) =
          #"Invalid number of callback operations in \#{x.domain}: \#{y.value} (\#{show k} operations)"#
        err (RegularOpWithoutName x y) =
          #"Unnamed regular operation in \#{x.domain}: \#{y.value}"#
        err (InvalidGetter x y) =
          #"Invalid getter in \#{x.domain}: \#{y.value}"#
        err (InvalidSetter x y) =
          #"Invalid setter in \#{x.domain}: \#{y.value}"#
        err (UnresolvedAlias x y) =
          #"Unresolved alias in \#{x.domain}: \#{y.value}"#
        err (AnyInUnion x) = #"\"Any\" type in a union type in \#{x.domain}"#
        err (PromiseInUnion x) = #"\"Promise\" type in a union type in \#{x.domain}"#
        err (NullableAny x) = #"Nullable \"Any\" type in \#{x.domain}"#
        err (NullablePromise x) = #"Nullable \"Promise\" type in \#{x.domain}"#
        err (InvalidConstType x) = #"Invalid constant type in \#{x.domain}"#


writeDoc : String -> String -> Prog ()
writeDoc f doc = toProg $ writeFile f doc

loadDef : String -> Prog (String,PartsAndDefs)
loadDef f = let mn = moduleName
                   . head
                   . split ('.' ==)
                   . last
                   $ split ('/' ==) f

             in do s <- toProg (readFile f)
                   d <- toProg (pure $ parseIdl partsAndDefs s)
                   pure (mn,d)

typesGen : Config -> List CGDomain -> Prog ()
typesGen c ds =
  let typesFile = c.outDir ++ "/Web/Internal/Types.idr"
   in writeDoc typesFile (typedefs ds)

codegen : Config -> CGDomain -> Prog ()
codegen c d =
  let typesFile = c.outDir ++ "/Web/Internal/" ++ d.name ++ "Types.idr"
      primFile  = c.outDir ++ "/Web/Internal/" ++ d.name ++ "Prim.idr"
      apiFile   = c.outDir ++ "/Web/Raw/" ++ d.name ++ ".idr"

   in do writeDoc typesFile (types d)
         writeDoc primFile (primitives d)
         writeDoc apiFile  (definitions d)

logAttributes : HasAttributes a => a -> Prog ()
logAttributes = traverse_ (putStrLn . extAttribute) . attributes

--------------------------------------------------------------------------------
--          Main Function
--------------------------------------------------------------------------------

run : List String -> Prog ()
run args = do config <- toProg (pure $ applyArgs args)
              ds     <- toDomains <$> traverse loadDef config.files

              let e = env config.maxInheritance ds

              doms   <- fromCodegen (traverse (domain e) ds)

--              logAttributes ds
              traverse_ (codegen config) doms
              typesGen config doms

main : IO ()
main = do (pn :: args) <- getArgs
                       |  Nil => putStrLn "Missing executable name. Aborting..."

          runProg (run args)
