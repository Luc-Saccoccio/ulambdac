module Parser (parseExpr, Identifier, LambdaTree(..)) where

import           Data.Void                  (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char       hiding (space)
import qualified Text.Megaparsec.Char.Lexer as L

type Identifier = Char

data LambdaTree
  = App LambdaTree LambdaTree
  | Lambda Identifier LambdaTree
  | Variable Identifier
  | Alias Identifier LambdaTree
  -- deriving Show


type Parser = Parsec Void String

lexeme :: Parser a -> Parser a
lexeme = L.lexeme $ L.space space1 (L.skipLineComment "--") empty

symbol :: Char -> Parser Char
symbol = lexeme . char

keyword :: String -> Parser String
keyword = lexeme . string

varIdentifier :: Parser Identifier
varIdentifier = lexeme lowerChar

aliasIdentifier :: Parser Identifier
aliasIdentifier = lexeme upperChar

lambdaAbs :: Parser LambdaTree
lambdaAbs = flip (foldr Lambda) <$> (lexeme (oneOf ['\\', 'λ']) *> some varIdentifier <* symbol '.') <*> application

var :: Parser LambdaTree
var = Variable <$> (aliasIdentifier <|> varIdentifier)

expression :: Parser LambdaTree
expression = try application <|> term

alias :: Parser LambdaTree
alias = Alias <$> (aliasIdentifier <* keyword ":=") <*> expression

application :: Parser LambdaTree
application = term >>= rest
  where
    rest x = do { y <- term
                ; rest (App x y)
                } <|> return x

term :: Parser LambdaTree
term = alias <|> lambdaAbs <|> var <|> between (char '(') (char ')') application

parseExpr :: String -> String -> Either (ParseErrorBundle String Void) LambdaTree
parseExpr = parse (skipMany spaceChar *> expression <* eof)
