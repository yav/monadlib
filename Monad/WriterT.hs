-- | Implements the writer (aka output) transformer.
-- This is useful when a computation needs to collect a number of items,
-- besides its main result.
-- 
-- This implementation is strict in the buffer, but not the values that are 
-- stored in it:
-- 
-- > put undefined === undefined
-- > put [undefined] =/= undefined
--
-- This was done to avoid leaking memory in computations that do not 
-- have any output.  

module Monad.WriterT 
  ( -- * Examples
    -- $HandlerM
 
  WriterT, runWriter, evalWriter, execWriter, module Monad.Prelude
  ) where

import Monad.Prelude
import Control.Monad.Fix
import Data.Monoid

-- | A computation that computes a result of type /a/, may place items 
-- in a buffer of type /w/, and can also side-effect as described by /m/.
newtype WriterT w m a = W { unW :: m (P a w) }
data P a w            = P { res :: a, out :: !w }

runWriter          :: Monad m => WriterT w m a -> m (a,w) 
runWriter (W m)     = do P a w <- m
                         return (a,w)

-- | Execute a computation ignoring the output.
evalWriter         :: Monad m => WriterT w m a -> m a
evalWriter (W m)    = res # m

-- | Execute a computation just for the output.
execWriter          :: Monad m => WriterT w m a -> m w
execWriter (W m)     = out # m

instance (Monoid w, Monad m) => Functor (WriterT w m) where
  fmap f m          = liftM f m

instance (Monoid w, Monad m) => Monad (WriterT w m) where
  return a          = lift (return a)
  W m >>= k         = W (do P a w1 <- m
                            P b w2 <- unW (k a)
                            return (P b (w1 `mappend` w2)))
  W m >> W n        = W (do P _ w1 <- m
                            P b w2 <- n
                            return (P b (w1 `mappend` w2)))
                        
instance Monoid w => Trans (WriterT w) where
  lift m            = W (do a <- m
                            return (P a mempty))

instance (Monoid w, BaseM m b) => BaseM (WriterT w m) b where
  inBase m          = lift (inBase m)

instance (Monoid w, MonadFix m) => MonadFix (WriterT w m) where
  mfix f            = W (mfix (\r -> unW (f (res r))))

                        
instance (Monoid w, ReaderM m r) => ReaderM (WriterT w m) r where
  getR               = lift getR

instance (Monoid w, ReadUpdM m r) => ReadUpdM (WriterT w m) r where
  updateR f (W m)    = W (updateR f m)
  setR x (W m)       = W (setR x m)

instance (Monoid w, Monad m) => WriterM (WriterT w m) w where
  put w             = W (return (P () w))

instance (Monoid w, Monad m) => CollectorM (WriterT w m) w where
  censor (W m) f    = W (do P a w1 <- m
                            P b w2 <- unW (f w1)
                            return (P (a,b) w2))
  collect (W m)     = W (do P a w <- m
                            return (P (a,w) mempty))

instance (Monoid w, StateM m s) => StateM (WriterT w m) s where
  get               = lift get
  set s             = lift (set s)
  update f          = lift (update f)

instance (Monoid w, ExceptM m e) => ExceptM (WriterT w m) e where
  raise e           = lift (raise e)


-- $HandlerM 
-- Exceptions undo the output. For example:
-- 
-- > test = runId $ runExcept $ runWriter 
-- >      $ withHandler (\_ -> return 42) 
-- >      $ do put "hello"
-- >           raise "Error"
-- 
-- produces @Right (42, \"\")@

instance (Monoid w, HandlerM m e) => HandlerM (WriterT w m) e where
  checkExcept (W m) = W (do r <- checkExcept m
                            case r of
                              Left err      -> return (P (Left err) mempty)
                              Right (P a w) -> return (P (Right a) w))
  handle (W m) h    = W (m `handle` (\e -> unW (h e)))

instance (Monoid w, MonadPlus m) => MonadPlus (WriterT w m) where
  mzero             = lift mzero
  mplus (W f) (W g) = W (mplus f g)

instance (Monoid w, SearchM m) => SearchM (WriterT w m) where
  checkSearch (W m) = W $ do x <- checkSearch m
                             case x of
                               Nothing -> return (P Nothing mempty)
                               Just (P x w,m) -> return (P (Just (x,W m)) w)

instance (Monoid w, ContM m) => ContM (WriterT w m) where
  callcc m          = W $ callcc $ \k -> 
                          unW $ m $ \a -> W (k (P a mempty))

