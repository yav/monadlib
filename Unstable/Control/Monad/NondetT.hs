-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) OGI at OHSU, 2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (multi-param classes, functional dependencies)
--
-- This module contains the implementation of the nondetermism and backtracking 
-- monad transformer.  For descriptions of the methods for dealing with
-- nondeterminism and backtracking see the 'MonadNondet' and 'MonadPlus' classes.
--
-- This implementation is similar to the one described in:
-- "Deriving Monad Transformers"
-- by Ralf Hinze
-----------------------------------------------------------------------------


module Unstable.Control.Monad.NondetT (
  -- * Type and instances
  NondetT, 

  -- * Functions
  -- ** Removing the transformer
  runNondet, runNondets,

  MonadPlus(..),
  module T
  ) where

import Monad(liftM,MonadPlus(..))
import Control.Monad.Fix
import Data.Maybe(listToMaybe)

import Unstable.Control.Monad.Trans as T
import Unstable.Control.Monad.Private.Utils

newtype NondetT m a = N { unN :: m (T m a) }
data T m a          = Empty | Cons a (NondetT m a)


instance MonadTrans NondetT where
  lift m            = N (liftM single m)

instance Monad m => Functor (NondetT m) where
  fmap              = liftM

instance Monad m => Monad (NondetT m) where
  return            = return'
  m >>= f           = N (do x <- unN m
                            case x of
                              Empty -> return Empty 
                              Cons a xs -> unN (mplus (f a) (xs >>= f)))

instance HasBaseMonad m n => HasBaseMonad (NondetT m) n where
  inBase            = inBase'
  mapBase f         = mapBase' f

instance MapTrans NondetT where
  mapTrans f (N m)      = N (f (liftM mapMT_T m))
    where 
    mapMT_T Empty       = Empty
    mapMT_T (Cons a m)  = Cons a (mapTrans f m)




-- CHECK: is this definition correct
instance MonadFix m => MonadFix (NondetT m) where
  mfix f  = N (do x <- mfix (unN . f . hd)
                  case x of
                    Empty    -> return Empty
                    Cons a _ -> return (Cons a (mfix (tl . f))))
    where hd (Cons a _) = a
          hd _          = error "NondetT: mfix looped (hd)"
          tl m          = N (do x <- unN m
                                case x of
                                  Cons _ m' -> unN m'
                                  _ -> error "NondetT: mfix looped (tl)")

--------------------------------------------------------------------------------

-- | Remove the nondeterminism transformer.  The resulting computation
-- returns the first successfull result if any.
runNondet           :: Monad m => NondetT m a -> m (Maybe a)
runNondet           = liftM listToMaybe . runNondets


-- | Remove the nondeterminism transformer.  The resulting computation
-- produces a list with all possible results.
runNondets          :: Monad m => NondetT m a -> m [a]
runNondets m        = flatten =<< unN m 

mapN                :: Monad m => (m (T m a) -> n (T n b)) -> NondetT m a -> NondetT n b
mapN f (N m)        = N (f m)


-- private:
instance Monad m => Functor (T m) where
  fmap _ Empty      = Empty
  fmap f (Cons a m) = Cons (f a) (fmap f m)


single              :: Monad m => a -> T m a
single x            = Cons x mzero

flatten             :: Monad m => T m a -> m [a]
flatten Empty       = return []
flatten (Cons a m)  = liftM (a :) (runNondets m)




--------------------------------------------------------------------------------
-- public:


-- other features.

instance MonadReader r m => MonadReader r (NondetT m) where
  ask               = ask'
  local             = local' mapN

instance MonadWriter w m => MonadWriter w (NondetT m) where
  tell              = tell'
  listen            = listen1' N unN (\w -> fmap (\a -> (a,w)))

instance MonadState s m => MonadState s (NondetT m) where
  get               = get'
  put               = put'

instance MonadError e m => MonadError e (NondetT m) where
  raise             = raise'
  handle            = handle1' N unN

instance Monad m => MonadPlus (NondetT m) where
  mzero             = N (return Empty)
  mplus m n         = N (do x <- unN m
                            case x of
                              Empty -> unN n
                              Cons a m' -> return (Cons a (mplus m' n)))

instance Monad m => MonadNondet (NondetT m) where
  findAll m         = lift (runNondets m)
  commit m          = N (do x <- unN m
                            case x of
                              Empty -> return Empty
                              Cons a _ -> return (single a))

instance MonadResume m => MonadResume (NondetT m) where
  delay                 = mapN delay
  step v d (N m)        = N (step v' d' m)
    where v' Empty      = return Empty
          v' (Cons a n) = unN (v a `mplus` step v d n)
          d' m          = unN (d (N m))


-- ergh, what does this do?
instance (MonadCont m) => MonadCont (NondetT m) where
  callCC            = callCC1' N unN single

   





