module Main where

import Codegen (genCode)
import Emit (showProgram)
import Lex (lexer)
import Parse (parser)
import Semantic (resolve)
import System.Environment
import System.Exit
import Tacky (tack)
import TypeCheck (resolveType)

data ProgType = Normal | Lex | Parse | Semantic | Tacky | Codegen | Emit
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
      "-S" -> Emit
      _ -> error "Bad option!"

main :: IO ()
main = do
  args <- getArgs
  if null args
    then print "Going to need an argument there, bud" >> exitFailure
    else do
      input <- readFile (head args)
      let optIs = (== getProgType (tail args))
          result =
            contUnless (optIs Lex) (lexer input)
              >>= contUnless (optIs Parse) . parser
              >>= contUnless (optIs Parse) . resolve
              >>= contUnless (optIs Semantic) . resolveType
              >>= contUnless (optIs Tacky) . tack
              >>= contUnless (optIs Codegen) . genCode
              >>= cont2Unless (optIs Emit) . showProgram
      case result of
        Left (Left e) -> print e >> exitFailure -- error from the parser
        Left (Right e) -> putStrLn e -- premature stop from option
        Right asm -> writeAsmFile (head args) asm

writeAsmFile :: FilePath -> String -> IO ()
writeAsmFile = writeFile . (++ "s") . init

contUnless :: (Show b) => Bool -> Either a b -> Either (Either a String) b
contUnless stop input = case input of
  Left e -> Left (Left e)
  Right e -> if stop then Left (Right (show e)) else Right e

cont2Unless :: Bool -> Either a String -> Either (Either a String) String
cont2Unless stop input = case input of
  Left e -> Left (Left e)
  Right e -> if stop then Left (Right e) else Right e