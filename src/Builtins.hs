module Builtins where

import Data.Map as M
import Data.Set as S
import Data.Bits (xor)
import Language.Haskell.TH.Syntax
import Core

builtinNames :: Set String
builtinNames = keysSet builtins

builtins :: Map String (Q Exp)
builtins = M.fromList
  [ ("+"     ,[| lift2 (+)    |])
  , ("*"     ,[| lift2 (*)    |])
  , ("-"     ,[| lift2 (-)    |])
  , ("/"     ,[| lift2 (/)    |])
  , ("=="    ,[| lift2 (==)   |])
  , ("<"     ,[| lift2 (<)    |])
  , (">"     ,[| lift2 (>)    |])
  , (">="    ,[| lift2 (>=)   |])
  , ("<="    ,[| lift2 (<=)   |])
  , ("/="    ,[| lift2 (/=)   |])
  , ("&&"    ,[| lift2 (&&)   |])
  , ("||"    ,[| lift2 (||)   |])
  , ("^^"    ,[| lift2 xor    |])
  , ("div"   ,[| lift2 div    |])
  , ("print" ,[| tprint       |])
  , ("getLn" ,[| tgetLn       |])
  , ("read"  ,[| tread        |])
  , ("drop"  ,[| tdrop        |])
  , ("dup"   ,[| tdup         |])
  , ("swap"  ,[| tswap        |])
  ]

tprint :: Show a => Action (a ': s) s
tprint (Cons a s) = print a >> return s

tgetLn :: Action s (String ': s)
tgetLn s = do
  txt <- getLine
  return $ Cons txt s

tread :: Read a => Action (String ': s) (a ': s)
tread (Cons w s) = return $ Cons (read w) s

