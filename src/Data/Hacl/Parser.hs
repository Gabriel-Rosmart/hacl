{-# LANGUAGE OverloadedStrings #-}

module Data.Hacl.Parser where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Map as M
import Data.List.NonEmpty
import Control.Monad
import Data.Hacl.Types

type Parser = Parsec Void T.Text

-- Space consumer
-- It allows for line comments but not block comments

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") empty 

-- Lexeme parser
-- Parses a lexeme and cosumes the whitespace after

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- Symbol parser
-- Parses a given symbol (string) and cosumes whitespace after

symbol :: T.Text -> Parser T.Text
symbol = L.symbol sc

-- Parser consuming trailing whitespaces

ws :: Parser a -> Parser a 
ws p = p <* space

-- Exposed parser -- 
haclParser :: Parser Hacl
haclParser = hobject

-- Object parser

hobject :: Parser Hacl
hobject = between (symbol "{") (symbol "}") $ do 
  pairs <- concat <$> some (pair `sepEndBy1` comma)
  return (HaclObject (M.fromList pairs))

-- Array Parser

harray :: Parser Hacl
harray = between (symbol "[") (symbol "]") $ do 
  elements <- concat <$> some ((try $ ws hval) `sepEndBy1` comma)
  return (HaclArray (fromList elements))

-- Pair parser

pair :: Parser (T.Text, Hacl)
pair = do 
  (HaclText key) <- (ws hstring)
  void $ (char ':') >> space
  val <- try (ws hvalI)
  return (key, val)

-- Any possible Hacl value parser

hval :: Parser Hacl
hval = choice [hobject, harray, hstring, hnumber, hbool, hnothing]

-- Same as hval but allowing import statements

hvalI :: Parser Hacl
hvalI = choice [hobject, harray, hstring, hnumber, hbool, hnothing, himport]

himport :: Parser Hacl
himport = (HaclImport . T.pack ) <$> ((string "import") >> space >> (char '\"' *> manyTill L.charLiteral (char '\"')))

-- Quoted string parser

hstring :: Parser Hacl
hstring = (HaclText . T.pack) <$> (char '\"' *> manyTill L.charLiteral (char '\"'))

-- Number parser

hnumber :: Parser Hacl
hnumber = HaclNumber <$> castHaclNumber <$> L.signed (return ()) L.scientific

-- Boolean parser

hbool :: Parser Hacl
hbool = ((HaclBool True <$ string "true") <|> (HaclBool False <$ string "false"))

hnothing :: Parser Hacl
hnothing = HaclNothing <$ string "nothing"

-- Comma character

comma :: Parser T.Text
comma = symbol ","
