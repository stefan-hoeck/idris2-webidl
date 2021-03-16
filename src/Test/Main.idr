module Test.Main


import Hedgehog
import Test.Lexer
import Test.Parser

main : IO ()
main = do ignore $ checkGroup Test.Lexer.props
          ignore $ checkGroup Test.Parser.props
