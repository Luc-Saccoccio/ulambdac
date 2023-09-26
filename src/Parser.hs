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
    rest x =
      do
        y <- term
        rest (App x y)
        <|> return x

term :: Parser LambdaTree
term = alias <|> lambdaAbs <|> var <|> between (char '(') (char ')') application

filepath :: Parser FilePath
filepath = undefined

metaCommand :: String -> String -> Parser ()
metaCommand start end = string start *> optional (string end) *> space

-- The parser for command feels very clumsy
command :: Parser Command
command =
  (eof $> Quit) <|> ((load <|> edit <|> reload <|> quit) <|> (lambda <*> (skipMany spaceChar *> expression))) <* eof
  where
    load = metaCommand ":l" "oad" *> some filepath <&> Load
    edit = metaCommand ":e" "dit" *> filepath <&> Edit
    reload = metaCommand ":r" "eload" $> Reload
    quit = metaCommand ":q" "uit" $> Quit
    subterms = metaCommand ":s" "ubterms" $> Subterms
    redexes = keyword ":x" $> Redexes
    fv = keyword ":fv" $> FV
    autoreduc = keyword ":ar" $> AutoReduc
    manualreduc = keyword ":mr" $> ManReduc
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
