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
-- but has some difference, in particular we are not doing a 
-- CPS transform.
-- Also we have a choice for the representation of lists - normal vs join lists
-----------------------------------------------------------------------------


module Unstable.Control.Monad.NondetT (
  -- * Type and instances
  NondetT, Answer(..),

  -- * Functions
  -- ** Removing the transformer
  leftMost, depthFirst, breadthFirst, oneStep,

  MonadPlus(..),
  module T
  ) where

import Control.Monad(liftM,MonadPlus(..))
import Control.Monad.Fix
import Data.Maybe(listToMaybe)

import Unstable.Control.Monad.Trans as T
import Unstable.Control.Monad.Private.Utils

newtype NondetT m a = N { oneStep :: m (Answer m a) }
data Answer m a     = No | Yes a | Choice { leftChoice :: NondetT m a, rightChoice :: NondetT m a }


instance Monad m => Functor (Answer m) where
  fmap f No           = No
  fmap f (Yes a)      = Yes (f a)
  fmap f (Choice l r) = Choice (fmap f l) (fmap f r)



instance MonadTrans NondetT where
  lift m            = N (liftM Yes m)

instance Monad m => Functor (NondetT m) where
  fmap              = liftM

instance Monad m => Monad (NondetT m) where
  return x          = lift (return x)
  m >>= k           = N (do x <- oneStep m
                            case x of
                              No            -> return No
                              Choice m1 m2  -> return (Choice (m1 >>= k) (m2 >>= k))
                              Yes a         -> oneStep (k a)
                        )

instance HasBaseMonad m n => HasBaseMonad (NondetT m) n where
  inBase            = inBase'
  mapBase f         = mapBase' f

instance MapTrans NondetT where
  mapTrans f (N m)          = N (f (liftM mapMT_T m))
    where 
    mapMT_T No              = No
    mapMT_T (Yes a)         = Yes a
    mapMT_T (Choice m1 m2)  = Choice (mapTrans f m1) (mapTrans f m2)



instance MonadFix m => MonadFix (NondetT m) where
  mfix f  = N (mdo x@(~(Yes a)) <- oneStep (f a)
                   return $ case x of
                              No          -> No
                              Yes _       -> Yes a
                              Choice _ _  -> Choice (mfix (chooseLeft . f)) (mfix (chooseRight . f))
              )

chooseLeft   :: Monad m => NondetT m a -> NondetT m a
chooseLeft m  = N (do x <- oneStep m
                      case x of
                        Choice l _ -> oneStep l)
                
chooseRight   :: Monad m => NondetT m a -> NondetT m a
chooseRight m  = N (do x <- oneStep m
                       case x of
                         Choice _ r -> oneStep r)

--------------------------------------------------------------------------------

-- | Effects are performed in breadth first order.
-- | Answers are left to right.
breadthFirst m      = liftM reverse (bf ([m],[]) [])
  where 
  bf (m:ms,ns) acc  = do x <- oneStep m
                         case x of
                           Choice m1 m2 -> bf (ms,m2:m1:ns) acc
                           No           -> bf (ms,ns) acc
                           Yes a        -> bf (ms,ns) (a:acc) 
  bf ([],[]) acc    = return acc
  bf ([],ns) acc    = bf (reverse ns,[]) acc

-- | Effects are performed in depth first order.
-- | Answers are from left to right.
depthFirst         :: Monad m => NondetT m a -> m [a]
depthFirst m        = liftM reverse (df m [])
df m acc            = do x <- oneStep m
                         case x of
                           Choice m1 m2 -> df m2 =<< df m1 acc
                           No           -> return acc
                           Yes a        -> return (a:acc)

-- | Find the left most answer (if any).
leftMost           :: Monad m => NondetT m a -> m (Maybe a)
leftMost m          = do x <- oneStep m
                         case x of
                           No         -> return Nothing
                           Yes a      -> return (Just a)
                           Choice l r -> do x <- leftMost l
                                            case x of 
                                              Nothing -> leftMost r
                                              Just x  -> return (Just x)


mapN                :: Monad m => (m (Answer m a) -> n (Answer n b)) -> NondetT m a -> NondetT n b
mapN f (N m)        = N (f m)






--------------------------------------------------------------------------------
-- public:

-- other features.

instance MonadReader r m => MonadReader r (NondetT m) where
  ask               = ask'
  local             = local' mapN

instance MonadWriter w m => MonadWriter w (NondetT m) where
  tell              = tell'
  listen            = listen1' N oneStep (\w -> fmap (\a -> (a,w)))

instance MonadState s m => MonadState s (NondetT m) where
  get               = get'
  put               = put'

instance MonadError e m => MonadError e (NondetT m) where
  raise             = raise'
  handle            = handle1' N oneStep

instance Monad m => MonadPlus (NondetT m) where
  mzero             = N (return No)
  mplus m n         = N (return (Choice m n))

{-
instance Monad m => MonadNondet (NondetT m) where
  findAll m         = lift (runNondets m)
{-
  commit (N m)      = N (liftM hd' m) 
    where hd' Empty       = Empty
          hd' (Cons a _)  = single a
-}
  next (N m)        = N (liftM hd m)
    where hd Empty        = Empty
          hd (Cons a as)  = return (a,as)
-}

{-
instance MonadResume m => MonadResume (NondetT m) where
  delay (N m)             = N (delay m)
  step v d (N m)          = N (step v' d' m)
    where v' No           = return No
          v' (Yes a)      = oneStep (v a)
          v' (Choice l r) = step v d l `mplus` step v d r
          d' m            = oneStep (d (N m))

-- ergh, what does this do?
instance (MonadCont m) => MonadCont (NondetT m) where
  callCC            = callCC1' N oneStep return
-}
   





