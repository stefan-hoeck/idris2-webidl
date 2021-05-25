module Test.Main


import Data.List
import Data.String
import Experimental.Prelude
import Hedgehog
import System
import System.Console.GetOpt
import System.File
import Test.Lexer
import Test.Parser
import Text.WebIDL.Codegen as Codegen
import Text.WebIDL.Types
import Text.WebIDL.Parser
import Text.PrettyPrint.Prettyprinter

--------------------------------------------------------------------------------
--          Command line options
--------------------------------------------------------------------------------

record Config where
  constructor MkConfig
  numTests : TestLimit
  files    : List String

init : List String -> Config
init = MkConfig 100

setTests : String -> Config -> Either (List String) Config
setTests s c = maybe (Left ["Not a natural number: " ++ s])
                     (\n => Right $ record { numTests = MkTagged n} c)
                     (parsePositive {a = Nat} s)

descs : List $ OptDescr (Config -> Either (List String) Config)
descs = [ MkOpt ['n'] ["testlimit"] (ReqArg setTests "<tests>")
            "number of tests to be passed by each property"
        ]

applyArgs : List String -> Either (List String) Config
applyArgs args =
  case getOpt RequireOrder descs args of
       MkResult opts n  [] [] => foldl (>>=) (Right $ init n) opts
       MkResult _ _ u e       => Left $ map unknown u ++ e

  where unknown : String -> String
        unknown = ("Unknown option: " ++)

adjust : Config -> Group -> Group
adjust (MkConfig t _) = withTests t

--------------------------------------------------------------------------------
--          Main Function
--------------------------------------------------------------------------------

run : Config -> IO ()
run cfg = do ignore . checkGroup $ adjust cfg Test.Lexer.props
             ignore . checkGroup $ adjust cfg Test.Parser.props

main : IO ()
main = do (pn :: args) <- getArgs
                       |  Nil => putStrLn "Missing executable name. Aborting..."
          Right config <- pure $ applyArgs args
                       | Left es => traverse_ putStrLn es

          run config
