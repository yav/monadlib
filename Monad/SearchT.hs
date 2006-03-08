module Monad.SearchT where

import Monad.Prelude
import Control.Monad.Fix

newtype SearchT m a = S { unS :: forall r. (a -> m r -> m r) -> m r -> m r }

runSearchOne       :: Monad m => SearchT m a -> m (Maybe a)
runSearchOne m      = unS m (\x _ -> return (Just x)) (return Nothing)

runSearchAll       :: Monad m => SearchT m a -> m [a]
runSearchAll m      = unS m (\x xs -> (x:) # xs) (return [])
                     
                    
instance Monad m => Functor (SearchT m) where
  fmap f m          = do x <- m
                         return (f x)

instance Monad m => Monad (SearchT m) where
  return x          = lift (return x)
  S m >>= f         = S (\cons nil -> m (\x xs -> unS (f x) cons xs) nil)

instance Trans SearchT where
  lift m            = S (\cons nil -> do x <- m
                                         cons x nil)

instance BaseM m b => BaseM (SearchT m) b where
  inBase m          = lift (inBase m)

instance ReaderM m r => ReaderM (SearchT m) r where
  getR              = lift getR

instance WriterM m w => WriterM (SearchT m) w where
  put x             = lift (put x)

instance StateM m s => StateM (SearchT m) s where
  get               = lift get
  set x             = lift (set x)

instance ExceptM m x => ExceptM (SearchT m) x where
  raise x           = lift (raise x)

instance Monad m => MonadPlus (SearchT m) where
  mzero             = S (\cons nil -> nil)
  mplus (S m) (S n) = S (\cons nil -> m cons (n cons nil))
                     
                    
