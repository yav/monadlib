module Monad.BackT 
  ( Answer(..), BackT
  , breadthFirst, depthFirst, leftMost
  ) where

import Control.Monad.Fix
import Control.Monad(MonadPlus(..))

data Answer m a     = No | Yes a | Choice (BackT m a) (BackT m a)
newtype BackT m a   = B (m (Answer m a))

instance Functor m => Functor (Answer m) where
  fmap f No             = No
  fmap f (Yes a)        = Yes (f a)
  fmap f (Choice m1 m2) = Choice (fmap f m1) (fmap f m2)

instance Functor m => Functor (BackT m) where
  fmap f (B m)      = B (fmap (fmap f) m)

instance Monad m => Monad (BackT m) where
  return a          = B (return (Yes a))
  B m1 >>= k        = B (do x <- m1
                            case x of
                              No            -> return No
                              Yes a         -> let B m2 = k a in m2
                              Choice m2 m3  -> return (Choice (m2 >>= k) (m3 >>= k)))

instance MonadFix m => MonadFix (BackT m) where
  mfix f            = B (mfix (\x@(~(Yes a)) -> 
                           let B m = f a
                           in return (
                                case x of
                                  No         -> No
                                  Yes _      -> Yes a
                                  Choice _ _ -> Choice (mfix (chooseLeft . f)) (mfix (chooseRight . f)))))
    where 
    chooseLeft (B m)  = B (do x <- m
                              case x of
                                Choice (B l) _ -> l)

    chooseRight (B m) = B (do x <- m
                              case x of
                                Choice _ (B r) -> r)

instance Monad m => MonadPlus (BackT m) where
  mzero             = B (return No)
  mplus m1 m2       = B (return (Choice m1 m2))



-- | Effects are performed in breadth first order.
-- | Answers are left to right.
breadthFirst       :: Monad m => BackT m a -> m [a]
breadthFirst m      = do xs <- bf ([m],[]) []
                         return (reverse xs)
  where
  bf (B m:ms,ns) acc= do x <- m
                         case x of
                           Choice m1 m2 -> bf (ms,m2:m1:ns) acc
                           No           -> bf (ms,ns) acc
                           Yes a        -> bf (ms,ns) (a:acc)
  bf ([],[]) acc    = return acc
  bf ([],ns) acc    = bf (reverse ns,[]) acc

-- | Effects are performed in depth first order.
-- | Answers are from left to right.
depthFirst         :: Monad m => BackT m a -> m [a]
depthFirst m        = do xs <- df m []
                         return (reverse xs)
  where
  df (B m) acc      = do x <- m
                         case x of
                           Choice m1 m2 -> df m2 =<< df m1 acc
                           No           -> return acc
                           Yes a        -> return (a:acc)

-- | Find the left most answer (if any).
leftMost           :: Monad m => BackT m a -> m (Maybe a)
leftMost (B m)      = do x <- m
                         case x of
                           No         -> return Nothing
                           Yes a      -> return (Just a)
                           Choice l r -> do x <- leftMost l
                                            case x of
                                              Nothing -> leftMost r
                                              Just x  -> return (Just x)

