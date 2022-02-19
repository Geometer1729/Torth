module Torth where

import Core

import Control.Monad ((>=>))
import Control.Monad.IO.Class (MonadIO(liftIO))
import qualified Data.Map as M
import Data.Char
import Flow ((.>))

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote

import Parse
import Builtins

compTerm :: TermT -> Q Exp
compTerm = \case
  Lit (Integral n) -> [| push (fromInteger n) |]
  Lit (String w) -> [| push w |] -- TODO should this use fromString
  Lit (Fractional r)  -> [| push (fromRational r) |]

  If whenTrue whenFalse -> AppE <$> (AppE <$> [| tif |] <*>  compExp whenTrue) <*> compExp whenFalse
  While check body -> AppE <$> (AppE <$> [| tWhile |] <*>  compExp check) <*> compExp body

  Function (ByName n) -> return $ VarE $ mkName $ "ti_" ++ n
  Function (Builtin b) -> case M.lookup b builtins of
                            Just expr -> expr
                            Nothing -> error $ "internal error bad builtin: " ++ b

compExp :: ExpT -> Q Exp
compExp = mapM compTerm >=> compose

compose :: [Exp] -> Q Exp
compose [] = [| nop |]
compose [e] = return e
compose (x:xs) = AppE <$> (AppE <$> [| (>=>) |] <*> pure x) <*> compose xs

andRun :: [Dec] -> Q Exp
andRun decs =
  (LetE decs <$>)$ AppE <$> [|run|] <*> pure (VarE $ mkName "ti_main")

compDecs :: [DecT] -> Q [Dec]
compDecs = (concat <$>) . mapM compDec

compDec :: DecT -> Q [Dec]
compDec DecT{..} = do
  fname <- newName $ "ti_" ++ tname
  t <- makeType ttype
  impl <- compExp body
  return
      [ SigD fname t
      , FunD fname [Clause [] (NormalB impl) []]
      ]

makeType :: TypeT -> Q Type
makeType TypeT{..} = do
  sn <- newName "s"
  ins <- mapM mkType inputs
  ons <- mapM mkType outputs
  AppT <$> (AppT <$> [t| Action |]
    <*> makeStackType ins  (VarT sn))
    <*> makeStackType ons (VarT sn)

mkType :: String -> Q Type
mkType = \case
  "Int"    -> [t|Integer|]
  "String" -> [t|String|]
  "" -> error "empty type name"
  w
    | isLower (head w) -> pure $ VarT (mkName w)
    | otherwise -> error "unsupported constructor"

makeStackType :: [Type] -> Type -> Q Type
makeStackType ns st =
  foldr
    (\n -> (AppT <$> (AppT <$> [t|(:)|] <*> pure n) <*>))
      (pure st) ns

compile :: String -> Q Exp
compile = runParser .> compDecs >=> andRun

compileAsMain :: String -> Q [Dec]
compileAsMain w = do
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
  , quoteDec = compileAsMain
                     }
