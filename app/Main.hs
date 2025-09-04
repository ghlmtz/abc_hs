module Main where
import System.Exit
import System.Environment

import Lex (lexer)
import Parse (parser)
import Semantic (resolve)
import Tacky (tack)
import Codegen (genCode)
import Control.Exception (try, evaluate, SomeException)

data ProgType = Normal | Lex | Parse | Semantic | Tacky | Codegen
    deriving (Eq)

getProgType :: [String] -> ProgType
getProgType args = 
    if null args 
        then Normal
        else case head args of 
                "-l" -> Lex
                "-p" -> Parse
                "-s" -> Semantic
                "-t" -> Tacky
                "-c" -> Codegen
                _    -> error "Bad option!"

main :: IO ()
main = do
    args <- getArgs
    if null args
        then print "Going to need an argument there, bud" >> exitFailure
    else do
        input <- readFile (head args)
        let optIs = (== getProgType (tail args))
            result = contUnless (optIs Lex) (lexer input) >>= 
                     contUnless (optIs Parse) . parser >>= 
                     contUnless (optIs Semantic) . resolve >>=
                     contUnless (optIs Tacky) . tack >>=
                     contUnless (optIs Codegen) . genCode
        case result of 
            Left (Left e) -> print e >> exitFailure -- error from the parser
            Left (Right e) -> putStrLn e -- premature stop from option
            Right asm -> do
                res <- try (evaluate (show asm)) :: IO (Either SomeException String)
                case res of 
                    Left e -> print e >> exitFailure
                    Right e -> writeAsmFile (head args) e

writeAsmFile :: FilePath -> String -> IO ()
writeAsmFile = writeFile . (++ "s") . init

contUnless :: Show b => Bool -> Either a b -> Either (Either a String) b
contUnless stop input = case input of 
    Left e -> Left (Left e)
    Right e -> if stop then Left (Right (show e)) else Right e