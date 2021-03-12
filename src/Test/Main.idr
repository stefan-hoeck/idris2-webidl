module Test.Main

import Hedgehog
import Test.Lexer

main : IO ()
main = do ignore $ checkGroup Test.Lexer.props
