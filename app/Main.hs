module Main where
import System.Exit
import System.Environment

import Lex (lexer)
import Types (ProgType(..))

main :: IO ()
main = do
    args <- getArgs
    if null args
        then print "Going to need an argument there, bud" >> exitFailure
    else
        if null (tail args) then lexer (head args) Normal
        else
            lexer (head args) $ case head (tail args) of
                "-l" -> Lex
                "-p" -> Parse
                "-c" -> Codegen
                _    -> error "Bad option!"
