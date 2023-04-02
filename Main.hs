import Data.Char (digitToInt, isAlpha, isAlphaNum, ord)
import Data.List (foldl', intercalate)
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

-- TODO: create simple interpreter based on LC

lexer = T.makeTokenParser haskellDef

lexeme = T.lexeme lexer

number = many1 digit

plus = char '+' *> number

minus = (:) <$> char '-' <*> number

int = plus <|> minus <|> number

-- TODO: Float support

integer :: Parser SExpr
integer = Literal . Integer . rd <$> int
  where
    rd = read :: String -> Int

str :: Parser SExpr
str = do
  char '"'
  content <- many validStringChar
  char '"'
  return (Literal . Str $ content)

identifier :: Parser SExpr
identifier = do
  c <- satisfy isAlpha
  rest <- many $ satisfy isAlphaNum
  return (Identifier $ c : rest)

validStringChar :: Parser Char
validStringChar = satisfy (/= '"')

comment :: Parser ()
comment = do
  char '{'
  many (noneOf "}")
  char '}'
  return ()

expr :: Parser SExpr
expr = do
  optional $ many (lexeme comment)
  lexeme (char '(')
  res <- many . lexeme $ integer <|> str <|> identifier <|> expr
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
  Float f -> Text.printf "%.2f" f
  Str s -> Text.printf "\"%s\"" s
doFormatSExpr (Identifier l) _ = l
doFormatSExpr (Exprs exprs) n =
  if n == 0
    then Text.printf "(%s)" $ unwords (map mapper exprs)
    else Text.printf "\n%s(%s)" nTabs $ unwords (map mapper exprs)
  where
    nTabs = replicate (n * 2) ' '
    mapper expr = doFormatSExpr expr (n + 1)
