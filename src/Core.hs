{-# LANGUAGE UndecidableInstances #-}
module Core where

import Control.Monad (void)
import Data.Kind (Type)

type StackType = [Type]
data Stack (s :: StackType )
  where
  Null :: Stack '[]
  Cons :: a -> Stack s -> Stack (a ': s)

type Action a b = Stack a -> IO (Stack b)
type Prog = Action '[] '[]

run :: Prog -> IO ()
run prog = void $ prog Null

push :: a -> Action s (a ': s)
push x = return . Cons x

tdrop :: Action (a ': s) s
tdrop (Cons _ s) = return s

tdup :: Action (a ': s) (a ': a ': s)
tdup (Cons x s) = return $ Cons x (Cons x s)

tswap :: Action (a ': b ': s) (b ': a ': s)
tswap (Cons a (Cons b s)) = return $ Cons b (Cons a s)

nop :: Action s s
nop = return

-- TODO can the lift functions be a class/type family?
lift0 :: a -> Action s (a ': s)
lift0 = push

lift1 :: (a -> b)  -> Action (a ': s) (b ': s)
lift1 f (Cons a s) = push (f a) s

lift2 :: (a -> b -> c) -> Action (a ': b ': s) (c ': s)
lift2 f (Cons a (Cons b s)) = push (f a b) s

tif :: Action a b -> Action a b -> Action (Bool ': a) b
tif onTrue onFalse (Cons bool s) =
  if bool
     then onTrue s
     else onFalse s

tWhile :: forall s. Action s (Bool ': s) -> Action s s -> Action s s
tWhile check body s = check s >>= \case
  Cons False s' -> return s'
  Cons True s' -> body s' >>= tWhile check body
