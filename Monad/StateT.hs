module StateT 
  ( StateT
  , run, eval, exec
  , peek, poke, update
  ) where

import Control.Monad.Fix

newtype StateT s m a  = S (s -> m (a,s))


instance Functor m => Functor (StateT s m) where
  fmap f (S m)      = S (\s -> fmap (\ ~(a,s1) -> (f a, s1)) (m s))

instance Monad m => Monad (StateT s m) where
  return a          = S (\s -> return (a,s))
  S m1 >>= k        = S (\s -> do ~(a,s1)  <- m1 s
                                  let S m2  = k a
                                  m2 s1)

instance MonadFix m => MonadFix (StateT s m) where
  mfix f            = S (\s -> mfix (\it -> let S m = f (fst it)
                                            in m s))


run                :: s -> StateT s m a -> m (a,s)
run s (S m)         = m s

eval               :: Functor m => s -> StateT s m a -> m a
eval s m            = fmap fst (run s m)

exec               :: Functor m => s -> StateT s m a -> m s
exec s m            = fmap snd (run s m)

peek               :: Monad m => StateT s m s
peek                = S (\s -> return (s,s))

poke               :: Monad m => s -> StateT s m s
poke s              = S (\s1 -> return (s1,s))

update             :: Monad m => (s -> s) -> StateT s m s
update f            = S (\s -> return (s, f s))
                                  
                                        

