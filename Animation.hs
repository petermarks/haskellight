{-# language MultiParamTypeClasses, TupleSections #-} 

module Animation (
  Animation, -- abstract
  Animator, -- abstract
  Runner,
  animation,
  mkAnimation,
  while,
  when,
  start,
  stop,
  set,
  modify
 ) where

import Prelude hiding (id, (.))
import Control.Arrow
import Control.Category
import Control.Concurrent
import Control.Monad hiding (when)
import Data.IORef

data Animation a b 
  = Animation (a -> (Animation a b, b))
  | Stateless (a -> b)

data Animator a b = Animator (IORef (Animation a b)) QSem 

instance Functor (Animation a) where
  fmap f (Stateless a) = Stateless $ a >>> f
  fmap f (Animation a) = Animation $ a >>> (fmap f *** f)

instance Applicative (Animation a) where
  pure = Stateless . const
  f <*> v = (f &&& v) >>> arr (uncurry ($))

instance Category Animation where
  id = Stateless id
  (Stateless b)    . (Stateless a)    = Stateless $ a >>> b
  (Animation b)    . aa@(Stateless a) = Animation $ a >>> b >>> first (aa >>>) 
  bb@(Stateless b) . (Animation a)    = Animation $ a >>> ((>>> bb) *** b) 
  (Animation b)    . (Animation a)    = Animation $ a >>> second b >>> (\(aa, (bb, o)) -> (aa >>> bb, o)) 

instance Arrow Animation where
  arr = Stateless
  first  (Stateless a) = arr (first a)
  first  (Animation a) = Animation $ \(x,y) -> (first *** (,y)) (a x)
  second (Stateless a) = arr (second a)
  second (Animation a) = Animation $ \(x,y) -> (second *** (x,)) (a y)

animation :: (a -> (Animation a b, b)) -> Animation a b
animation = Animation

mkAnimation :: (a -> Animation a b) -> Animation a b
mkAnimation f = Animation $ step <*> f

while :: Animation a Bool -> Animation a b -> Animation a b -> Animation a b
while p t f = Animation $ \i -> let (p', c) = step i p in if c
  then let (t', o) = step i t in (while p' t' f, o)
  else let (f', o) = step i f in (f', o)

-- TODO: This pauses each animation then resumes. Is this the right behaviour?
when :: Animation a Bool -> Animation a b -> Animation a b -> Animation a b
when p t f = Animation $ \i -> let (p', c) = step i p in if c
  then let (t', o) = step i t in (when p' t' f, o)
  else let (f', o) = step i f in (when p' t f', o)

step :: a -> Animation a b -> (Animation a b, b)
step i aa@(Stateless a) = (aa, a i)
step i (Animation a)    = a i 

type Runner a b = (a -> IO b) -> IO () -> IO ()

start :: Animation a b -> Runner a b -> IO (Animator a b)
start a r = do
  stop <- newQSem 0
  animation <- newIORef a
  void $ forkIO $ r (atomicModifyIORef animation . step) (waitQSem stop) 
  return $ Animator animation stop

stop :: Animator a b -> IO ()
stop (Animator _ qsem) = signalQSem qsem

set :: Animator a b -> Animation a b -> IO ()
set (Animator r _) = writeIORef r

modify :: Animator a b -> (Animation a b -> Animation a b) -> IO ()
modify (Animator r _) = modifyIORef r
