import Data.Char (digitToInt, ord)
import Data.List (foldl', intercalate, intersperse)
import System.IO
import Text.Parsec
import Text.Parsec.Language (haskellDef)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as T
import qualified Text.Printf as Text

data LiteralType
  = Str String
  | Integer Int
  | Float Float
  deriving (Show)

data SExpr
  = Literal LiteralType
  | Identifier String
  | Exprs [SExpr]
  deriving (Show)

lexer = T.makeTokenParser haskellDef

lexeme = T.lexeme lexer

-- TODO: parse floats

number :: Parser SExpr
number = do
  x <- positiveNatural
  return (Literal . Integer $ x)

-- TODO: parse numbers correctly
positiveNatural =
  foldl' (\a i -> a * 10 + digitToInt i) 0 <$> many1 digit

mystring :: Parser SExpr
mystring = do
  char '"'
  content <- many (satisfy validAscii) -- TODO: support all characters
  char '"'
  return (Literal . Str $ content)

identifier :: Parser SExpr
identifier = do
  c <- satisfy validAscii
  rest <- many (satisfy validAscii)
  return (Identifier $ c : rest)

-- TODO: support also non-ascii characters
validAscii c = validAscii
  where
    n = ord c
    validAscii = n >= 65 && n <= 90 || n >= 97 && n <= 122

comment :: Parser ()
comment = do
  char '{'
  many (noneOf "}")
  char '}'
  return ()

expr :: Parser SExpr
expr = do
  optional $ lexeme comment
  lexeme (char '(')
  res <- many . lexeme $ number <|> mystring <|> identifier <|> expr
  lexeme (char ')')
  return (Exprs res)

mainParser :: Parser [SExpr]
mainParser = do
  r <- many expr
  eof
  return r

main = do
  handle <- openFile "test.scm" ReadMode
  contents <- hGetContents handle
  case parse mainParser "" contents of
    Left err -> print err
    Right exprs ->
      putStrLn "This is the parsed and formatted code:\n"
        >> putStrLn (intercalate "\n\n" $ formatSExpr <$> exprs)

formatSExpr :: SExpr -> String
formatSExpr exprs = doFormatSExpr exprs 0

doFormatSExpr :: SExpr -> Int -> String
doFormatSExpr (Literal l) _ = case l of
  Integer i -> Text.printf "%d" i
  Float f -> Text.printf "\"%2.f\"" f
  Str s -> Text.printf "\"%s\"" s
doFormatSExpr (Identifier l) _ = l
doFormatSExpr (Exprs exprs) n =
  if n == 0
    then Text.printf "(%s)" $ unwords (map mapper exprs)
    else Text.printf "\n%s(%s)" nTabs $ unwords (map mapper exprs)
  where
    nTabs = replicate (n * 2) ' '
    mapper expr = doFormatSExpr expr (n + 1)
