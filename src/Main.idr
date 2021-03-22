module Main

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
init = MkConfig "../dom/src/JS/DOM/Raw"

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

moduleName : String -> String
moduleName s = let (h ::: _) = split ('.' ==) . last $ split ('/' ==) s
                in firstToUpper h
  where firstToUpper : String -> String
        firstToUpper s = case fastUnpack s of
                              [] => ""
                              (h :: t) => fastPack (toUpper h :: t)
        

codegen : Config -> String -> IO ()
codegen c f = do Right s <- readFile f
                   | Left err => putStrLn $ "File error " ++ f ++ ": " ++ show err
                 Right ds <- pure (parseIdl definitions s)
                   | Left err => printLn err

                 let mod = moduleName f
                     typesFile = c.outDir ++ "/" ++ mod ++ "Types.idr"
                     modFile = c.outDir ++ "/" ++ mod ++ ".idr"

                 Right () <- writeFile typesFile
                                       (show $ Codegen.types mod ds)
                   | Left err => putStrLn $ "File error " ++ typesFile  ++ ": " ++ show err

                 Right () <- writeFile modFile
                                       (show $ Codegen.definitions mod ds)
                   | Left err => putStrLn $ "File error " ++ modFile  ++ ": " ++ show err

                 pure ()

--------------------------------------------------------------------------------
--          Main Function
--------------------------------------------------------------------------------

main : IO ()
main = do (pn :: args) <- getArgs
                       |  Nil => putStrLn "Missing executable name. Aborting..."
          Right config <- pure $ applyArgs args
                       | Left es => traverse_ putStrLn es

          traverse_ (codegen config) config.files
