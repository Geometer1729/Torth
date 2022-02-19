import Torth
import Data.Map as M
import System.Exit
import System.Environment
import Loader
import System.Directory

$( genFileMap )

main :: IO ()
main = do
  getArgs >>= \case
    ["local"] -> local
    [path] -> do
      path' <- canonicalizePath path
      case M.lookup path' fileMap of
        Just io -> io
        Nothing -> die "file not found"
    args -> do
      putStrLn $ "got" ++ show args
      die "useage: torth <file>"

local :: IO ()
local = [torth|
def stepFib Int Int -> Int Int {
  dup over +
}

def runFib Int -> Int {
  0 1 over # n 1 0
  while (1 swap - dup 0 <){
    over over # a b n
    stepFib
    over
  }
  drop swap drop
}

def main void -> void {
  getLn read
  runFib
  print
}
|]
