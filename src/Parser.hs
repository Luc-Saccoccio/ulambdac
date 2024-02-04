module Parser (parseExpr, parseCommand) where

import Def
import Data.Functor (($>), (<&>))
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L


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
aliasIdentifier = lexeme (upperChar <|> digitChar)

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
    rest x =
      do
        y <- term
        rest (App x y)
        <|> return x

term :: Parser LambdaTree
term = lambdaAbs <|> try alias <|> var <|> between (char '(') (char ')') application

metaCommand :: String -> String -> Parser ()
metaCommand start end = string start *> optional (string end) *> space

-- The parser for command feels very clumsy
command :: Parser Command
command =
  (eof $> Quit) <|> ((delete <|> bindings <|> quit <|> help) <|> (lambda <*> (skipMany spaceChar *> expression))) <* eof
  where
    delete = metaCommand ":d" "elete" *> aliasIdentifier <&> Delete
    help = metaCommand ":h" "elp" $> Help
    quit = metaCommand ":q" "uit" $> Quit
    bindings = metaCommand ":b" "indings" $> Bindings
    subterms = metaCommand "#s" "ubterms" $> Subterms
    redexes = metaCommand "#r" "edexes" $> Redexes
    fv = keyword "#fv" $> FV
    autoreduc = keyword "#ar" $> AutoReduc
    manualreduc = keyword "#mr" $> ManReduc
    lambda =
      subterms
        <|> redexes
        <|> fv
        <|> autoreduc
        <|> manualreduc
        <|> return None

parseExpr :: String -> String -> Either (ParseErrorBundle String Void) LambdaTree
parseExpr = parse (skipMany spaceChar *> expression <* eof)

parseCommand :: String -> String -> Either (ParseErrorBundle String Void) Command
parseCommand = parse (skipMany spaceChar *> lexeme command)
