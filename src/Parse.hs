module Parse where

import Text.ParserCombinators.ReadP hiding (many)
import Control.Applicative
import Data.Char
import Data.Foldable
import Data.Functor
import Flow
import Builtins

type Expr = [Term]

data Term
  = Lit Lit
  | Function Func
  | If Expr Expr
  | While Expr Expr
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

runParser :: String -> Expr
runParser = readP_to_S parseExpr .> filter (snd .> null) .> ( \case
  [] -> error "no parse"
  [(e,"")] -> e
  _ -> error "ambiguous parse"
                                                            )

cleanComments :: String -> String
cleanComments = lines .> map (takeWhile (/= '#')) .> unlines

parseExpr :: ReadP Expr
parseExpr = skipSpaces *> (parseTerm <&> (:)) <*> many (spaces *> parseTerm) <* skipSpaces

parseTerm :: ReadP Term
parseTerm = (Lit <$> parseLit) <++  (parseIf <|> parseWhile) <++ (Function <$> parseFunc)

spaces :: ReadP ()
spaces = munch1 isSpace $> ()

parseLit :: ReadP Lit
parseLit =
  (Integral . read <$> munch1 isDigit) <|>
  (String <$> (char '"' *> munch (/= '"') <* char '"')) -- TODO support escaped "s
  -- TODO support fractional literals

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

parseIf :: ReadP Term
parseIf = If <$> (string "if" *> parseCur) <*> (string "else" *> parseCur) <++ pure []

parseWhile :: ReadP Term
parseWhile = While <$> (string "while" *> parseParen) <*> parseCur

parseCur :: ReadP Expr
parseCur = skipSpaces *> char '{' *> parseExpr <* char '}'

parseParen :: ReadP Expr
parseParen = skipSpaces *> char '(' *> parseExpr <* char ')'
