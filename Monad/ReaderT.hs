-- | An implementation of the reader (aka environment) monad transformer.
-- It is useful when computations need to access some immutable context.

module Monad.ReaderT 
  ( -- * Examples
    -- $HandlerM
    -- $SearchM

  ReaderT, runReader, module Monad.Prelude
  ) where

import Monad.Prelude
import Control.Monad.Fix

-- | A computation that computes a value of type /a/, 
-- may use a context of type /r/,
-- and my side-effect as described by /m/.
newtype ReaderT r m a = R (r -> m a) 

-- | Execute a computation in the given context.
runReader          :: r -> ReaderT r m a -> m a
runReader r (R m)   = m r

instance Monad m => Functor (ReaderT r m) where
  fmap f m          = liftM f m

instance Monad m => Monad (ReaderT r m) where
  return a          = lift (return a)
  R m >>= k         = R (\r -> (runReader r . k) =<< m r)
  R m >> R n        = R (\r -> m r >> n r)

instance BaseM m b => BaseM (ReaderT r m) b where
  inBase m          = lift (inBase m)

instance Trans (ReaderT r) where
  lift m            = R (\_ -> m)

instance MonadFix m => MonadFix (ReaderT r m) where
  mfix f            = R (\r -> mfix (runReader r . f))

instance Monad m => ReaderM (ReaderT r m) r where
  getR              = R return

instance Monad m => ReadUpdM (ReaderT r m) r where
  updateR f (R m)   = R (\r -> m (f r))
  setR r (R m)      = R (\_ -> m r)

instance WriterM m w => WriterM (ReaderT r m) w where
  put o             = lift (put o)

instance CollectorM m w => CollectorM (ReaderT r m) w where
  censor (R m) f    = R (\r -> censor (m r) (runReader r . f))
  collect (R m)     = R (\r -> collect (m r))

instance StateM m s => StateM (ReaderT r m) s where
  get               = lift get
  set s             = lift (set s)
  update f          = lift (update f)

instance ExceptM m e => ExceptM (ReaderT r m) e where
  raise e           = lift (raise e)

-- $HandlerM
-- When an exception is thrown the context in the handler is restored.
-- For example:
-- 
-- > test = runId $ runExcept $ runReader 42  
-- >      $ withHandler (\_ -> getR) 
-- >      $ setR 17 (raise "Error") 
-- 
-- produces @Right 42@

instance HandlerM m e => HandlerM (ReaderT r m) e where
  handle (R m) h    = R (\r -> withHandler (runReader r . h) (m r))
  checkExcept (R m) = R (\r -> checkExcept (m r))

instance MonadPlus m => MonadPlus (ReaderT r m) where
  mzero             = lift mzero
  mplus (R m) (R n) = R (\r -> mplus (m r) (n r))


-- $SearchM$
-- The context for all alternatives is fixed at the time when we 'checkSearch'.
-- For example:
--
-- > test = runId $ runSearchOne $ runReader 42
-- >      $ do x <- checkSearch (getR `mplus` getR)
-- >           case x of
-- >             Just (_,next) -> setR 17 next
-- >             Nothing       -> return 0
--
-- produces @Just 42@

instance SearchM m => SearchM (ReaderT r m) where
  checkSearch (R m) = R (\r -> (f # ) # checkSearch (m r))
    where f (a,x)   = (a, lift x)
                                

instance ContM m => ContM (ReaderT r m) where
  callcc m        = R (\r -> callcc $ \k -> 
                             runReader r $ m $ \a -> lift (k a))




