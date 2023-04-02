-- import Control.Applicative
import Data.Char (ord)

-- type alias
-- newtype Parser t = Parser
--   { runParser :: String -> Maybe (t, String)
--   }

-- instance Functor Parser where
--   fmap f a = undefined

-- instance Applicative Parser where
--   pure = Parser {runParser= const Nothing}
--   a <*> b = Parser {runParser= const Nothing}

type Parser t = String -> Maybe (t, String)

data LiteralType
  = Str String
  | Integer Int
  | Float Float
  deriving (Show)

data SExpr
  = Literal LiteralType
  | Identifier String
  | Expr [SExpr]
  deriving (Show)

{--

(define foo x
  ((+ x 2)))

--}

sexpr =
  Expr
    [ Identifier "define",
      Identifier "foo",
      Identifier "x",
      Expr [Identifier "+", Identifier "x", Literal (Integer 2)]
    ]

pOr :: Parser a -> Parser a -> Parser a
pOr p1 p2 text = do
  case p1 text of
    Just res -> Just res
    Nothing -> p2 text

digit "" = Nothing
digit (c : rest) =
  if n >= 48 && n <= 57
    then Just (n - 48, rest)
    else Nothing
  where
    n = ord c

digitsToNumber :: [Int] -> Int
digitsToNumber digits =
  fst $
    foldl
      (\(total, pow) n -> (total + n * pow, pow * 10))
      (0, 1)
      (reverse digits)

-- TODO: implement Functor fmap
pmap :: Parser a -> (a -> b) -> Parser b
pmap p1 f text = p1 text >>= \(a, rest) -> Just (f a, rest)

integer :: Parser LiteralType
integer text =
  pmany digit text
    >>= (\(digits, rest) -> Just (Integer (digitsToNumber digits), rest))

char :: Char -> Parser Char
char tc (c : rest) = if tc == c then Just (c, rest) else Nothing
char tc "" = Nothing

asciiChar :: Parser Char
asciiChar "" = Nothing
asciiChar (c : rest) = if validAscii then Just (c, rest) else Nothing
  where
    n = ord c
    validAscii = n >= 65 && n <= 90 || n >= 97 && n <= 122

-- TODO: test if this is working
string :: Parser LiteralType
-- TODO: add support for all characters (dont use asciiChar)
string = pmap (pmany (between '"' '"' asciiChar)) Str

exprLiteral :: Parser SExpr
exprLiteral =
  identifier
    `pOr` pmap string Literal
    `pOr` pmap integer Literal
    `pOr` between '(' ')' (pmap (pmany exprLiteral) Expr)

identifier :: Parser SExpr
identifier msg =
  pmany asciiChar msg
    >>= \(ident, rest) -> Just (Identifier ident, rest)

pmany :: Parser t -> Parser [t]
pmany p msg = do
  (res, rest) <- p msg
  parseMany2 p ([res], rest)

parseMany2 :: Parser t -> ([t], String) -> Maybe ([t], String)
parseMany2 p (acc, msg) =
  case p msg of
    Just (res, rest) -> parseMany2 p (res : acc, rest)
    Nothing -> Just (reverse acc, msg)

between :: Char -> Char -> Parser t -> Parser t
between fdigit ldigit p text = do
  (_, rest) <- char fdigit text
  (result, rest) <- p rest
  (_, _) <- char ldigit rest
  return (result, take (length rest - 1) rest)

parseExpr :: Parser SExpr
parseExpr = between '(' ')' identifier

main :: IO ()
main = putStrLn "Hello!"
