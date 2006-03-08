-- | An implementation of the exception monad transformer.
-- Adds the ability to raise and handle exceptions.
-- This implementation uses rank-2 polymorphism.
--
--
-- * Commutes with: "Monad.ReaderT", "Monad.ExceptT"
--
-- * Does not commute with: "Monad.WriterT", "Monad.StateT"
module Monad.ExceptT 
  ( -- * Instance notes

    -- ** instance 'WriterM'
    -- $WriterM

    -- ** instance 'CollectorM'
    -- $CollectorM

    -- ** instance 'StateM'
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


instance Functor (ExceptT x m) where
  fmap f (E g)      = E (\ok fail -> g (ok . f) fail)

instance Monad (ExceptT x m) where
  return x          = E (\ok _ -> ok x)
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
                         return (error "ExceptT: mfix looped",r)

instance MonadPlus m => MonadPlus (ExceptT x m) where
  mzero             = lift mzero
  mplus (E f) (E g) = E (\ok fail -> mplus (f ok fail) (g ok fail))

instance ReaderM m r => ReaderM (ExceptT x m) r where
  getR              = lift getR

instance ReadUpdM m r => ReadUpdM (ExceptT x m) r where
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


-- $CollectorM
-- If an exception occurs while collecting the output we transfer control
-- to the exception handler.  The output that was produced is lost. 
-- One can avoid loosing the output by using 'checkExcept' to ensure
-- that the argument to 'collect' does not raise an exception.
--
-- > test = runId $ runWriter $ runExcept 
-- >      $ withHandler (\_ -> put "world" >> return ((),"")) 
-- >      $ collect $ do put "hello"
-- >                     raise "Error"
-- 
-- produces @(Right ((),\"\"), \"world\")@

instance CollectorM m w => CollectorM (ExceptT x m) w where
  collect (E m)     = E (\ok fail -> do let ok' a   = return (Right a)
                                            fail' x = return (Left x)
                                        (a,w) <- collect (m ok' fail')
                                        case a of
                                          Left x  -> fail x
                                          Right a -> ok (a,w))

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

instance ExceptM (ExceptT x m) x where
  raise x           = E (\_ fail -> fail x)

instance HandlerM (ExceptT x m) x where
  handle (E f) h    = E (\ok fail -> f ok (\x -> let E g = h x
                                                 in g ok fail))



