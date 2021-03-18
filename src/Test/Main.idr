module Test.Main


import Hedgehog
import Test.Lexer
import Test.Parser

main : IO ()
main = do ignore . checkGroup $ withTests 10000 Test.Lexer.props
          ignore . checkGroup $ withTests 10000 Test.Parser.props
