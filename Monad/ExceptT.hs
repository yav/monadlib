-- | An implementation of the exception monad transformer.
-- Adds the ability to raise and handle exceptions.
-- This implementation uses rank-2 polymorphism.

module Monad.ExceptT 
  ( ExceptT, runExceptWith, runExcept, unsafeRunExcept, module Monad.Prelude
    -- * Examples
    -- $Examples
  ) where

import Monad.Prelude
import Control.Monad.Fix

-- | A computation that may raise exceptions of type /x/, 
-- may side-effect as described by /m/, 
-- and computes a value of type /a/,
newtype ExceptT x m a = E (forall r. (a -> m r) -> (x -> m r) -> m r)

-- | Exceute a computation, with the given handler.
runExceptWith      :: Monad m => (x -> m a) -> ExceptT x m a -> m a
runExceptWith h (E f) = f return h

-- | Execute a computation: if it succeeds the answer can
-- be found in the right component of the result;
-- if the computation failed, the exception is reported
-- in the left component of the result.
runExcept          :: Monad m => ExceptT x m a -> m (Either x a)
runExcept m         = runExceptWith (return . Left) (Right # m)

-- | Execute a computation that will not raise exceptions.
-- The program crashes if an exception occurs.
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

instance WriterM m o => WriterM (ExceptT x m) o where
  put o             = lift (put o)

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

{- $Examples

Raising an exception does not affect the output.

> prop_ExceptT'WriterM = test == (Right 42, ["Hello"])
>   where test  = runId $ runWriter $ runExcept
>               $ do put ["Hello"]
>                    raise "Error"
>                  `handle_` return 42


Raising an exception does not affect the state. For example:

> prop_ExceptT'StateM = test == (Right 17, 17)
>   where test  = runId $ runState 42 $ runExcept
>               $ do set 17
>                    raise "Error"
>                  `handle_` get


Raising an exception does not prevent backtracking.

> prop_ExceptT'MonadPlus = test == [Left "Hello", Right 42]
>   where test  = runId $ runSearchAll $ runExcept
>               $ raise "Hello" `mplus` return 42


Raising an exception does not prevent us from jumping to a continutaion.

> prop_ExceptT'ContM = test == Right 42
>   where test  = runId $ runCont $ runExcept
>               $ do (stop,k) <- returnCC False
>                    do if stop then return 42
>                               else raise "Error"
>                     `handle_` cJump True k

-}


