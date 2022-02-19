module Parse where

import Text.ParserCombinators.ReadP hiding (many)
import Control.Applicative
import Data.Char
import Data.Foldable
import Data.Functor
import Flow
import Builtins

type Ast = [DecT]

data DecT
  = DecT
    { tname :: String
    , ttype :: TypeT
    , body :: ExpT
    }
    deriving Show

data TypeT
  = TypeT
    { inputs :: [String]
    , outputs :: [String]
    }
    deriving Show

type ExpT = [TermT]

data TermT
  = Lit Lit
  | Function Func
  | If ExpT ExpT
  | While ExpT ExpT
  deriving Show

data Lit
  = Integral Integer
  | Fractional Rational
  | String String
  deriving Show

data Func
  = Builtin String
  | ByName String
  deriving Show

runParser :: String -> [DecT]
runParser = cleanComments .> readP_to_S parseDecs .> filter (snd .> null) .> ( \case
  [] -> error "no parse"
  [(e,"")] -> e
  _ -> error "ambiguous parse"
                                                            )
cleanComments :: String -> String
cleanComments = lines .> map (takeWhile (/= '#')) .> unlines

parseDecs :: ReadP [DecT]
parseDecs = many (skipSpaces *> parseDec <* skipSpaces)

parseDec :: ReadP DecT
parseDec = DecT
  <$> (string "def" *> skipSpaces *> munch1 isAlphaNum)
  <*> parseType
  <*> parseCur

parseType :: ReadP TypeT
parseType = TypeT
  <$> parseTypeList
  <*> (string "->" *> parseTypeList)

parseTypeList :: ReadP [String]
parseTypeList =
  (skipSpaces *> string "void" *> skipSpaces $> []) <++
  many (skipSpaces *> munch1 isAlphaNum <* skipSpaces)

parseExpr :: ReadP ExpT
parseExpr = skipSpaces *> (parseTerm <&> (:)) <*> many (spaces *> parseTerm) <* skipSpaces

parseTerm :: ReadP TermT
parseTerm = (Lit <$> parseLit) <++  (parseIf <|> parseWhile) <++ (Function <$> parseFunc)

spaces :: ReadP ()
spaces = munch1 isSpace $> ()

parseLit :: ReadP Lit
parseLit =
  (Integral . read <$> munch1 isDigit) <|>
  (String <$> (char '"' *> munch (/= '"') <* char '"')) -- TODO support escaped "s
  -- TODO support fractional literals

-- TODO builtins can't be prefixes
parseFunc :: ReadP Func
parseFunc = parseBuiltin <++ parseByName

parseByName :: ReadP Func
parseByName = ByName <$> munch1 isAlphaNum

parseBuiltin :: ReadP Func
parseBuiltin = choice
  [ Builtin <$> string w | w <- toList builtinNames ]
  -- TODO this is probably slow
  -- ideally parse tokens and lookup rather they are builtins
  -- or maybe compile them all into a regex?

parseIf :: ReadP TermT
parseIf = If <$> (string "if" *> parseCur) <*> (string "else" *> parseCur) <++ pure []

parseWhile :: ReadP TermT
parseWhile = While <$> (string "while" *> parseParen) <*> parseCur

parseCur :: ReadP ExpT
parseCur = skipSpaces *> char '{' *> parseExpr <* char '}'

parseParen :: ReadP ExpT
parseParen = skipSpaces *> char '(' *> parseExpr <* char ')'
