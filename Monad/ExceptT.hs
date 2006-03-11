-- | An implementation of the exception monad transformer.
-- Adds the ability to raise and handle exceptions.
-- This implementation uses rank-2 polymorphism.

module Monad.ExceptT 
  ( -- * Examples
    -- $WriterM
    -- $StateM

  ExceptT, runExceptWith, runExcept, unsafeRunExcept, module Monad.Prelude
  ) where

import Monad.Prelude
import Control.Monad.Fix

-- | A computation that computes a value of type /a/, and may raise 
-- exceptions of type /x/, and can also side-effect as described by /m/.
newtype ExceptT x m a = E (forall r. (a -> m r) -> (x -> m r) -> m r)

-- | Exceute a computation, with the given handler.
runExceptWith      :: Monad m => (x -> m a) -> ExceptT x m a -> m a
runExceptWith h (E f) = f return h

-- | Execute a computation: if it succeeds the result can
-- be found in the right component of the result;
-- if the computation failed, the exception is reported
-- in the left component of the result.
runExcept          :: Monad m => ExceptT x m a -> m (Either x a)
runExcept m         = runExceptWith (return . Left) (Right # m)

-- | Execute a computation that will not raise exceptions.
-- If an exception is risen, the program crashes.
unsafeRunExcept  :: Monad m => ExceptT x m a -> m a
unsafeRunExcept e = runExceptWith (\_ -> error "unsafeRunExcept: exception.") e


instance Monad m => Functor (ExceptT x m) where
  fmap f m        = liftM f m

instance Monad m => Monad (ExceptT x m) where
  return x          = lift (return x)
  E f >>= k         = E (\ok fail -> f (\a -> let E g = k a
                                              in g ok fail
                                       ) fail)

instance Trans (ExceptT x) where
  lift m            = E (\ok _ -> ok =<< m)

instance BaseM m b => BaseM (ExceptT x m) b where
  inBase m          = lift (inBase m)

instance MonadFix m => MonadFix (ExceptT x m) where
  mfix f            = E comp
    where 
    comp ok fail    = do (_,r) <- mfix (\ ~(a,_) -> let E g = f a 
                                                    in g ok' fail')
                         return r
      where 
      ok' a         = do r <- ok a
                         return (a,r)

      fail' x       = do r <- fail x
                         return (error "<<ExceptT: 'mfix' looped>>",r)


instance ReaderM m r => ReaderM (ExceptT x m) r where
  getR              = lift getR

instance ReadUpdM m r => ReadUpdM (ExceptT x m) r where
  setR x (E m)      = E (\ok fail -> do r <- getR
                                        let ok' a   = setR r (ok a)
                                            fail' x = setR r (fail x)
                                        setR x (m ok' fail'))
  updateR f (E m)   = E (\ok fail -> do r <- getR
                                        let ok' a   = setR r (ok a)
                                            fail' x = setR r (fail x)
                                        updateR f (m ok' fail'))

-- $WriterM
-- Raising an exception does not affect the output. For example:
--
-- > test = runId $ runWriter $ runExcept 
-- >      $ withHandler (\_ -> return 42) 
-- >      $ do put "hello"
-- >           raise "Error"
-- 
-- produces @(Right 42, \"hello\")@

instance WriterM m o => WriterM (ExceptT x m) o where
  put o             = lift (put o)


-- $StateM
-- Raising an exception does not affect the state. For example: 
-- 
-- > test = runId $ runState 42 $ runExcept 
-- >      $ withHandler (\_ -> get) 
-- >      $ do set 17
-- >           raise "Error"
-- 
-- produces @(Right 17, 17)@

instance StateM m s => StateM (ExceptT x m) s where
  get               = lift get
  set s             = lift (set s)
  update f          = lift (update f)

instance Monad m => ExceptM (ExceptT x m) x where
  raise x           = E (\_ fail -> fail x)

instance Monad m => HandlerM (ExceptT x m) x where
  checkExcept (E f) = E (\ok _ -> f (\a -> ok (Right a)) (\x -> ok (Left x)))
  handle (E f) h    = E (\ok fail -> f ok (\x -> let E g = h x
                                                 in g ok fail))

instance MonadPlus m => MonadPlus (ExceptT x m) where
  mzero             = lift mzero
  mplus (E f) (E g) = E (\ok fail -> mplus (f ok fail) (g ok fail))

instance ContM m => ContM (ExceptT x m) where
  callcc m          = E (\ok fail -> 
                        do x <- callcc $ \k -> 
                                runExcept $ m $ \a -> lift (k (Right a))
                           case x of
                             Left err -> fail err
                             Right a  -> ok a)


