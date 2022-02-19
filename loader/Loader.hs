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
  mt <- [t| Map String (IO ()) |]
  return
    [ SigD (mkName "fileMap") mt
    , FunD (mkName "fileMap") [Clause [] (NormalB m) []]
    ]

getPaths :: IO [FilePath]
getPaths = do
  let dir = "./torth"
  ps <-  listDirectory dir
  mapM canonicalizePath $ (dir </>) <$> ps

mkList :: [FilePath] -> Q Exp
mkList = (ListE <$>) . mapM mkEntry

mkEntry :: FilePath -> Q Exp
mkEntry path =
  AppE <$> [| (path,) |] <*> compileFile path

