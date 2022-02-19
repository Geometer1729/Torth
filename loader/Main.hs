import Data.Map as M
import System.Exit
import System.Environment
import Loader
import System.Directory

$( genFileMap )

main :: IO ()
main = do
  getArgs >>= \case
    [path] -> do
      path' <- canonicalizePath path
      case M.lookup path' fileMap of
        Just io -> io
        Nothing -> die "file not found"
    args -> do
      putStrLn $ "got" ++ show args
      die "useage: torth <file>"
