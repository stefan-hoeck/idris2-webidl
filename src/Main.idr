module Main

import Control.Monad.Either
import Data.String
import System
import System.Console.GetOpt
import System.File
import Text.WebIDL.Codegen as Codegen
import Text.WebIDL.Types
import Text.WebIDL.Parser
import Text.PrettyPrint.Prettyprinter

--------------------------------------------------------------------------------
--          Command line options
--------------------------------------------------------------------------------

record Config where
  constructor MkConfig
  outDir : String
  files  : List String

init : List String -> Config
init = MkConfig "../dom/src"

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

toProg : Show a => IO (Either a b) -> Prog b
toProg io = MkEitherT $ map (mapFst show) io

runProg : Prog () -> IO ()
runProg (MkEitherT p) = do Right _ <- p
                             | Left e => putStrLn ("Error: " ++ e)
                           pure ()

writeDoc : String -> Doc () -> Prog ()
writeDoc f doc = toProg $ writeFile f (show doc)

moduleName : String -> String
moduleName s = let (h ::: _) = split ('.' ==) . last $ split ('/' ==) s
                in firstToUpper h
  where firstToUpper : String -> String
        firstToUpper s = case fastUnpack s of
                              [] => ""
                              (h :: t) => fastPack (toUpper h :: t)
        
loadDef : String -> Prog (String,Definitions)
loadDef f = let mn = moduleName f
             in do s <- toProg (readFile f)
                   d <- toProg (pure $ parseIdl definitions s)
                   pure (mn,d)

typesGen : Config -> Definitions -> Prog ()
typesGen c ds =
  let typesFile = c.outDir ++ "/JS/DOM/Raw/Types.idr"
   in writeDoc typesFile (typedefs ds)

codegen : Config -> (String,Definitions) -> Prog ()
codegen c (mod,ds) =
  let typesFile = c.outDir ++ "/JS/DOM/Raw/" ++ mod ++ "Types.idr"
      modFile = c.outDir ++ "/JS/DOM/Raw/" ++ mod ++ ".idr"
      typesTestFile = c.outDir ++ "/Test/" ++ mod ++ "Types.idr"

   in do writeDoc typesFile (types mod ds)
         writeDoc modFile (definitions mod ds)
         writeDoc typesTestFile (typeTests mod ds)

--------------------------------------------------------------------------------
--          Main Function
--------------------------------------------------------------------------------

run : List String -> Prog ()
run args = do config <- toProg (pure $ applyArgs args)
              ps     <- traverse loadDef config.files
              traverse_ (codegen config) ps
              typesGen config (concatMap snd ps)
              pure ()

main : IO ()
main = do (pn :: args) <- getArgs
                       |  Nil => putStrLn "Missing executable name. Aborting..."

          runProg (run args)
