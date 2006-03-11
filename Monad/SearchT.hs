-- | An implementation of a searching transformer.
-- In 'mplus' alternatives are processed left to right, 
-- so avoid left recursion.  This implementation uses an explicit
-- data structure (i.e., it is not in continuation passing style).

module Monad.SearchT 
  ( SearchT, Answer(..)
  , runSearch, runSearchOne, runSearchAll
  ) where

import Monad.Prelude
import Control.Monad.Fix

newtype SearchT m a = S { runSearch :: m (Answer m a) }

-- | Answers returned by a search.
data Answer m a     = No                    -- ^ There was no result.
                    | Yes a (SearchT m a)   -- ^ We founds a result, 
                                            --      and there may be more.

-- | Find the first answer of a computation. 
runSearchOne       :: Monad m => SearchT m a -> m (Maybe a)
runSearchOne (S m)  = do x <- m
                         return (case x of
                                   No -> Nothing
                                   Yes x _ -> Just x)

-- | Find all possible answers for a computation.  
runSearchAll       :: Monad m => SearchT m a -> m [a]
runSearchAll (S m)  = do x <- m
                         case x of
                           No -> return []
                           Yes x xs -> (x:) # runSearchAll xs

instance Monad m => Functor (SearchT m) where
  fmap f m          = liftM f m

instance Monad m => Monad (SearchT m) where
  return x          = lift (return x)
  S m >>= f         = S (do x <- m
                            case x of
                              No -> return No
                              Yes x xs -> runSearch (f x `mplus` (xs >>= f)))
  S m >> S n = S (do x <- m
                     case x of
                       No -> return No
                       Yes _ xs -> 
                         do x <- n
                            case x of
                              No -> runSearch (xs >> S n)
                              Yes x ys -> 
                                 return (Yes x (ys `mplus` (xs >> S n))))
  

instance Trans SearchT where
  lift m            = S (do x <- m
                            return (Yes x mzero))

instance MonadFix m => MonadFix (SearchT m) where
  mfix f            = S (do z <- mfix (runSearch . f . head)
                            return $ case z of
                                       No -> No
                                       Yes x _ -> Yes x (mfix (tail . f)))
    where 
    loop            = error "<<SearchT: 'mfix' looped>>"
    head (Yes x _)  = x
    head _          = loop
    tail (S m)      = S (do x <- m
                            case x of
                              Yes _ xs -> runSearch xs
                              _        -> loop)

instance BaseM m b => BaseM (SearchT m) b where
  inBase m          = lift (inBase m)

instance ReaderM m r => ReaderM (SearchT m) r where
  getR              = lift getR

instance WriterM m w => WriterM (SearchT m) w where
  put x             = lift (put x)

instance StateM m s => StateM (SearchT m) s where
  get               = lift get
  set x             = lift (set x)
  update f          = lift (update f)

instance ExceptM m x => ExceptM (SearchT m) x where
  raise x           = lift (raise x)

instance Monad m => MonadPlus (SearchT m) where
  mzero             = S (return No)
  mplus (S m) (S n) = S (do x <- m
                            case x of
                               No -> n
                               Yes x xs -> return (Yes x (xs `mplus` S n)))

instance Monad m => SearchM (SearchT m) where
  checkSearch (S m) = S (do x <- m
                            return (case x of
                                      No -> Yes Nothing mzero
                                      Yes x xs -> Yes (Just (x,xs)) mzero))

instance ContM m => ContM (SearchT m) where
  callcc m          = S (callcc $ \k -> 
                         runSearch $ m $ \a -> lift $ k (Yes a mzero))

                               
  
