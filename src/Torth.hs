module Torth where

import Core

import Control.Monad ((>=>))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Functor ((<&>))
import qualified Data.Map as M
import Flow ((.>))

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote

import Parse
import Builtins

parseProg :: String -> Expr
parseProg = runParser

compTerm :: Term -> Q Exp
compTerm = \case
  Lit (Integral n) -> [| push (fromInteger n) |]
  Lit (String w) -> [| push w |] -- TODO should this use fromString
  Lit (Fractional r)  -> [| push (fromRational r) |]

  If whenTrue whenFalse -> AppE <$> (AppE <$> [| tif |] <*>  compExpr whenTrue) <*> compExpr whenFalse
  While check body -> AppE <$> (AppE <$> [| tWhile |] <*>  compExpr check) <*> compExpr body

  Function (ByName _) -> error "custom functions not supported"
  Function (Builtin b) -> case M.lookup b builtins of
                            Just expr -> expr
                            Nothing -> error $ "internal error bad builtin: " ++ b

compExpr :: Expr -> Q Exp
compExpr = mapM compTerm >=> compose

compose :: [Exp] -> Q Exp
compose [] = [| nop |]
compose [e] = return e
compose (x:xs) = AppE <$> (AppE <$> [| (>=>) |] <*> pure x) <*> compose xs

andRun :: Exp -> Q Exp
andRun prog = AppE <$> [| run |] <&> ($ prog)

compile :: String -> Q Exp
compile = parseProg .> compExpr >=> andRun

compileDec :: String -> Q [Dec]
compileDec w = do
  e <- compile w
  ioType <- [t| IO () |]
  return [ SigD (mkName "main") ioType
         , FunD (mkName "main") [Clause [] (NormalB e) []] ]

compileFile :: String -> Q Exp
compileFile path = do
  txt <- liftIO $ readFile path
  compile txt

torthLoad :: QuasiQuoter
torthLoad = quoteFile torth

torth :: QuasiQuoter
torth = QuasiQuoter
  { quoteExp = compile
  , quotePat = undefined
  , quoteType=undefined
  , quoteDec = compileDec
                     }
