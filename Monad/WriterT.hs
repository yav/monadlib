module Monad.WriterT 
  ( WriterT
  , run, eval, exec
  , put, takeFrom
  ) where

import Control.Monad.Fix

newtype WriterT w m a = W (w -> (w -> w -> w) -> m (a,w))

instance Functor m => Functor (WriterT w m) where
  fmap f (W m)      = W (\n j -> fmap (\ ~(a,w) -> (f a,w)) (m n j))

instance Monad m => Monad (WriterT w m) where
  return a          = W (\n _ -> return (a,n))
  W m1 >>= k        = W (\n j -> do ~(a,w1) <- m1 n j
                                    let W m2 = k a
                                    ~(b,w2) <- m2 n j
                                    return (b, j w1 w2))

instance MonadFix m => MonadFix (WriterT w m) where
  mfix f            = W (\n j -> mfix (\it -> let W m = f (fst it)
                                              in m n j))
                                  
run                :: w -> (w -> w -> w) -> WriterT w m a -> m (a,w)
run n j (W m)       = m n j

eval               :: Functor m => w -> (w -> w -> w) -> WriterT w m a -> m a
eval n j m          = fmap fst (run n j m)

exec               :: Functor m => w -> (w -> w -> w) -> WriterT w m a -> m w
exec n j m          = fmap snd (run n j m)

put                :: Monad m => w -> WriterT w m ()
put w               = W (\_ _ -> return ((),w))

takeFrom           :: Functor m => WriterT w m a -> WriterT w m (a,w)
takeFrom (W m)      = W (\n j -> fmap (\ ~(a,w) -> ((a,w),n)) (m n j))


