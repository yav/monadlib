-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) OGI at OHSU, 2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (multi-param classes, functional dependencies)
--
-- The implementation of the resumption transformer.
-- For details on the methods used to manipulate resumptions see the class 'MonadResume'.
--
-- This implementation of resumptions was inspired from the paper
-- "A syntactic Approach to Modulariy in Denotational Semantics"
-- by Pietro Canciarelli and Eugenio Moggi
-----------------------------------------------------------------------------



module Unstable.Control.Monad.ResumeT 
  (ResumeT, runResume, foldResume, foldResume', module T) where

import Control.Monad(liftM,MonadPlus(..))

import Control.Monad.Fix
import Unstable.Control.Monad.Trans as T
import Unstable.Control.Monad.Private.Utils


newtype ResumeT m a   = Re { unRe :: m (Res m a) }
data Res m a          = Value a | Delay (ResumeT m a)


instance MonadTrans ResumeT where
  lift m  = Re (liftM Value m) 

instance Monad m => Functor (ResumeT m) where
  fmap    = liftM

instance Monad m => Monad (ResumeT m) where
  return  = return'
  m >>= f = Re (do x <- unRe m
                   case x of
                     Value a -> unRe (f a)
                     Delay m' -> return (Delay (m' >>= f)))

instance HasBaseMonad m n => HasBaseMonad (ResumeT m) n where
  inBase    = inBase'
  mapBase f = mapBase' f 

instance MapTrans ResumeT where
  mapTrans f (Re m)        = Re (f (liftM mapMT_Res m))
    where
    mapMT_Res (Delay m)     = Delay (mapTrans f m)
    mapMT_Res (Value a)     = Value a


instance Monad m => Functor (Res m) where
  fmap f (Value a)      = Value (f a)
  fmap f (Delay m)      = Delay (liftM f m)

--------------------------------------------------------------------------------

-- | A general way to "execute" computations in the resumption monad.
foldResume  :: Monad m => (a -> m b) -> (m b -> m b) -> ResumeT m a -> m b
foldResume value delay m  
            = do x <- unRe m
                 case x of
                   Value a -> value a
                   Delay m -> delay (foldResume value delay m)

foldResume' done paused = step done (paused . foldResume' done paused)


-- | Executes all computations, until a value is reached.  
runResume   :: Monad m => ResumeT m a -> m a
runResume m = foldResume return id m


-- private
mapR        :: (m (Res m a) -> n (Res n b)) -> ResumeT m a -> ResumeT n b
mapR f      = Re . f . unRe 

--------------------------------------------------------------------------------


instance MonadReader r m => MonadReader r (ResumeT m) where
  ask       = ask'
  local     = local' mapR

instance MonadWriter w m => MonadWriter w (ResumeT m) where
  tell      = tell'
  listen    = listen1' Re unRe (\w -> fmap (\a -> (a,w)))

instance MonadState s m => MonadState s (ResumeT m) where
  get       = get'
  put       = put'

instance MonadError e m => MonadError e (ResumeT m) where
  raise     = raise'
  handle    = handle1' Re unRe

instance MonadPlus m => MonadPlus (ResumeT m) where
  mzero     = mzero'
  mplus     = mplus1' Re unRe

instance MonadNondet m => MonadNondet (ResumeT m) where
  findAll   = error "findAll ResumeT TODO"
  commit    = mapR commit

instance Monad m => MonadResume (ResumeT m) where
  delay m         = Re (return (Delay m))
  step v d (Re m) = Re (do x <- m
                           case x of
                             Value a -> unRe (v a)
                             Delay m' -> unRe (d m'))

instance MonadCont m => MonadCont (ResumeT m) where
  callCC    = callCC1' Re unRe Value



