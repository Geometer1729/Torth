module Loader where

import Torth

import Control.Monad.IO.Class (liftIO)
import Data.Functor ((<&>))
import Language.Haskell.TH.Syntax
import System.Directory
import System.FilePath

import Data.Map (Map)
import qualified Data.Map as M

genFileMap :: Q [Dec]
genFileMap = do
  paths <- liftIO getPaths
  list <- mkList paths
  m <- AppE <$> [| M.fromList |] <&> ($ list)
  mt <- [t| Map String (String,IO ()) |]
  return
    [ SigD (mkName "fileMap") mt
    , FunD (mkName "fileMap") [Clause [] (NormalB m) []]
    ]

getPaths :: IO [(String,FilePath)]
getPaths = do
  let dir = "./torth"
  ps <-  listDirectory dir
  ps' <- mapM canonicalizePath $ (dir </>) <$> ps
  return $ zip ps ps'


mkList :: [(String,FilePath)] -> Q Exp
mkList = (ListE <$>) . mapM mkEntry

mkEntry :: (String,FilePath) -> Q Exp
mkEntry (name,path) =
  AppE <$> [| (name,) |] <*> (AppE <$> [| (path,) |] <*> compileFile path)
