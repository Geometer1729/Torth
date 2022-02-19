import Data.Map as M
import System.Exit
import System.Environment
import Loader
import Flow

$( genFileMap )

main :: IO ()
main = do
  getArgs >>= \case
    ["torth",path] -> case M.lookup (name path) fileMap of
                Just (_fullPath,io) -> io
                  -- TODO ideally verify full path
                  -- but I probably need Maps of sets or similar
                  -- to be able to have multiple files with the same name anyway
                Nothing -> die "file not found"
    args -> do
      putStrLn $ "got" ++ show args
      die "useage: torth <file>"

name :: FilePath -> String
name = reverse .> takeWhile (/= '/') .> reverse
