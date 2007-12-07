{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances #-}
{-| This library provides a collection of monad transformers that
    can be combined to produce various monads.
-}
module MonadLib (
  -- * Types
  -- $Types
  Id, Lift, IdT, ReaderT, WriterT, StrictWriterT,
  StateT, ExceptionT, ChoiceT, ContT,

  -- * Lifting
  -- $Lifting
  MonadT(..), BaseM(..),

  -- * Effect Classes
  -- $Effects
  ReaderM(..), WriterM(..), StateM(..), ExceptionM(..), ContM(..),
  Label, labelCC, jump,

  -- * Execution

  -- ** Eliminating Effects
  -- $Execution
  runId, runLift,
  runIdT, runReaderT, runWriterT, runStrictWriterT,
  runStateT, runExceptionT, runContT,
  runChoiceT, findOne, findAll,

  -- ** Nested Execution
  -- $Nested_Exec
  RunReaderM(..), RunWriterM(..), RunExceptionM(..),

  -- * Deriving functions
  Iso(..), derive_fmap, derive_return, derive_bind, derive_fail, derive_mfix,
  derive_ask, derive_put, derive_get, derive_set, derive_raise, derive_callCC,
  derive_local, derive_collect, derive_try,
  derive_mzero, derive_mplus,
  derive_lift, derive_inBase,

  -- * Miscellaneous
  version,
  module Control.Monad
) where


import Control.Monad
import Control.Monad.Fix
import Data.Monoid
import Prelude hiding (Ordering(..))

-- | The current version of the library.
version :: (Int,Int,Int)
version = (3,4,1)


-- $Types
--
-- The following types define the representations of the
-- computation types supported by the library.
-- Each type adds support for a different effect.

-- | Computations with no effects.
newtype Id a              = I a

-- | Computation with no effects (strict).
data Lift a               = L a

-- | Add nothing.  Useful as a placeholder.
newtype IdT m a           = IT (m a)

-- | Add support for propagating a context.
newtype ReaderT i m a     = R (i -> m a)

-- | Add support for collecting values (generic version).
-- Based on an idea by Josef Svenningsson from the Haskell mailing list.
newtype GenWriterT p i m a  = W { unW :: m (p a i) }

-- | Add support for collecting values.
newtype WriterT i m a       = LW { unLW :: GenWriterT (,) i m a }

-- | Add support for collecting values.
-- The strict version forces the collected output at every
-- step to avoid memory leaks.
newtype StrictWriterT i m a = SW { unSW :: GenWriterT P2  i m a }
iso_LW = Iso LW unLW
iso_SW = Iso SW unSW

-- | Add support for threading state.
newtype StateT     i m a  = S (i -> m (a,i))

-- | Add support for exceptions.
newtype ExceptionT i m a  = X (m (Either i a))

-- | Add support for multiple answers.
data ChoiceT m a          = NoAnswer
                          | Answer a
                          | Choice (ChoiceT m a) (ChoiceT m a)
                          | ChoiceEff (m (ChoiceT m a))

-- | Add support for jumps.
newtype ContT i m a  = C ((a -> m i) -> m i)


data P2 a b  = P2 a !b

class P t where
  to_pair :: t a b -> (a,b)
  pair :: a -> b -> t a b

instance P (,) where to_pair = id; pair = (,)
instance P P2  where to_pair ~(P2 a b) = (a,b); pair = P2

-- $Execution
--
-- The following functions eliminate the outermost effect
-- of a computation by translating a computation into an
-- equivalent computation in the underlying monad.
-- (The exceptions are 'Id' and 'Lift' which are not transformers
-- but ordinary monas and so, their run operations simply
-- eliminate the monad.)


-- | Get the result of a pure computation.
runId         :: Id a -> a
runId (I a) = a

-- | Get the result of a pure strict computation.
runLift       :: Lift a -> a
runLift (L a) = a


-- | Remove an identity layer.
runIdT        :: IdT m a -> m a
runIdT (IT a)  = a

-- | Execute a reader computation in the given context.
runReaderT    :: i -> ReaderT i m a -> m a
runReaderT i (R m) = m i

runGenWriterT    :: (Monad m, P p) => GenWriterT p i m a -> m (a,i)
runGenWriterT (W m) = liftM to_pair m

-- | Execute a writer computation.
-- Returns the result and the collected output.
runWriterT :: (Monad m) => WriterT i m a -> m (a,i)
runWriterT m = runGenWriterT (unLW m)

-- | Execute a (strict) writer computation.
-- Returns the result and the collected output.
runStrictWriterT :: (Monad m) => StrictWriterT i m a -> m (a,i)
runStrictWriterT m = runGenWriterT (unSW m)

-- | Execute a stateful computation in the given initial state.
-- The second component of the result is the final state.
runStateT     :: i -> StateT i m a -> m (a,i)
runStateT i (S m) = m i

-- | Execute a computation with exceptions.
-- Successful results are tagged with 'Right',
-- exceptional results are tagged with 'Left'.
runExceptionT :: ExceptionT i m a -> m (Either i a)
runExceptionT (X m) = m

-- | Execute a computation that may return multiple answers.
-- The resulting computation computation returns 'Nothing'
-- if no answers were found, or @Just (answer,new_comp)@,
-- where @answer@ is an answer, and @new_comp@ is a computation
-- that may produce more answers.
-- The search is depth-first and left-biased with respect to the
-- 'mplus' operation.
runChoiceT :: (Monad m) => ChoiceT m a -> m (Maybe (a,ChoiceT m a))
runChoiceT (Answer a)     = return (Just (a,NoAnswer))
runChoiceT NoAnswer       = return Nothing
runChoiceT (Choice l r)   = do x <- runChoiceT l
                               case x of
                                 Nothing      -> runChoiceT r
                                 Just (a,l1)  -> return (Just (a,Choice l1 r))
runChoiceT (ChoiceEff m)  = runChoiceT =<< m

-- | Execute a computation that may return multiple answers,
-- returning at most one answer.
findOne :: (Monad m) => ChoiceT m a -> m (Maybe a)
findOne m = fmap fst `liftM` runChoiceT m

-- | Executie a computation that may return multiple answers,
-- collecting all possible answers.
findAll :: (Monad m) => ChoiceT m a -> m [a]
findAll m = all =<< runChoiceT m
  where all Nothing       = return []
        all (Just (a,as)) = (a:) `liftM` findAll as

-- | Execute a computation with the given continuation.
runContT      :: (a -> m i) -> ContT i m a -> m i
runContT i (C m) = m i



-- $Lifting
--
-- The following operations allow us to promote computations
-- in the underlying monad to computations that support an extra
-- effect.  Computations defined in this way do not make use of
-- the new effect but can be combined with other operations that
-- utilize the effect.

class MonadT t where
  -- | Promote a computation from the underlying monad.
  lift :: (Monad m) => m a -> t m a

-- It is interesting to note that these use something the resembles
-- the non-transformer 'return's.

instance MonadT IdT            where lift m = IT m
instance MonadT (ReaderT    i) where lift m = R (\_ -> m)
instance MonadT (StateT     i) where lift m = S (\s -> liftM (\a -> (a,s)) m)
instance (Monoid i, P p) => MonadT (GenWriterT p  i)
                               where lift m = W (liftM (\a -> pair a mempty) m)
instance (Monoid i) => MonadT (WriterT i)       where lift = derive_lift iso_LW
instance (Monoid i) => MonadT (StrictWriterT i) where lift = derive_lift iso_SW
instance MonadT (ExceptionT i) where lift m = X (liftM Right m)
instance MonadT ChoiceT        where lift m = ChoiceEff (liftM Answer m)
instance MonadT (ContT      i) where lift m = C (m >>=)


-- Definitions for some of the methods that are the same for all transformers

t_inBase   :: (MonadT t, BaseM m n) => n a -> t m a
t_inBase m  = lift (inBase m)

t_return   :: (MonadT t, Monad m) => a -> t m a
t_return x  = lift (return x)

t_fail     :: (MonadT t, Monad m) => String -> t m a
t_fail x    = lift (fail x)

t_mzero    :: (MonadT t, MonadPlus m) => t m a
t_mzero     = lift mzero

t_ask      :: (MonadT t, ReaderM m i) => t m i
t_ask       = lift ask

t_put      :: (MonadT t, WriterM m i) => i -> t m ()
t_put x     = lift (put x)

t_get      :: (MonadT t, StateM m i) => t m i
t_get       = lift get

t_set      :: (MonadT t, StateM m i) => i -> t m ()
t_set i     = lift (set i)

t_raise    :: (MonadT t, ExceptionM m i) => i -> t m a
t_raise i   = lift (raise i)
--------------------------------------------------------------------------------


class (Monad m, Monad n) => BaseM m n | m -> n where
  -- | Promote a computation from the base monad.
  inBase :: n a -> m a

instance BaseM IO IO        where inBase = id
instance BaseM Maybe Maybe  where inBase = id
instance BaseM [] []        where inBase = id
instance BaseM Id Id        where inBase = id
instance BaseM Lift Lift    where inBase = id


instance (BaseM m n) => BaseM (IdT          m) n where inBase = t_inBase
instance (BaseM m n) => BaseM (ReaderT    i m) n where inBase = t_inBase
instance (BaseM m n) => BaseM (StateT     i m) n where inBase = t_inBase
instance (BaseM m n,Monoid i, P p) => BaseM (GenWriterT p i m) n
                                                 where inBase = t_inBase
instance (BaseM m n,Monoid i) => BaseM (WriterT i m) n
                                                 where inBase = t_inBase
instance (BaseM m n,Monoid i) => BaseM (StrictWriterT i m) n
                                                 where inBase = t_inBase
instance (BaseM m n) => BaseM (ExceptionT i m) n where inBase = t_inBase
instance (BaseM m n) => BaseM (ChoiceT      m) n where inBase = t_inBase
instance (BaseM m n) => BaseM (ContT      i m) n where inBase = t_inBase


instance Monad Id where
  return x = I x
  fail x   = error x
  m >>= k  = k (runId m)

instance Monad Lift where
  return x  = L x
  fail x    = error x
  L x >>= k = k x     -- Note: the pattern is important here
                      -- because it makes things strict


-- Note: None of the transformers make essential use of the 'fail' method.
-- Instead, they delegate its behavior to the underlying monad.

instance (Monad m) => Monad (IdT m) where
  return  = t_return
  fail    = t_fail
  m >>= k = IT (runIdT m >>= (runIdT . k))

instance (Monad m) => Monad (ReaderT i m) where
  return  = t_return
  fail    = t_fail
  m >>= k = R (\r -> runReaderT r m >>= \a -> runReaderT r (k a))

instance (Monad m) => Monad (StateT i m) where
  return  = t_return
  fail    = t_fail
  m >>= k = S (\s -> runStateT s m >>= \ ~(a,s') -> runStateT s' (k a))

instance (Monad m,Monoid i,P p) => Monad (GenWriterT p i m) where
  return  = t_return
  fail    = t_fail
  m >>= k = W $ runGenWriterT m     >>= \ ~(a,w1) ->
                runGenWriterT (k a) >>= \ ~(b,w2) ->
                return (pair b (mappend w1 w2))

instance (Monad m,Monoid i) => Monad (WriterT i m) where
  return  = t_return
  fail    = t_fail
  (>>=)   = derive_bind iso_LW


instance (Monad m,Monoid i) => Monad (StrictWriterT i m) where
  return  = t_return
  fail    = t_fail
  (>>=)   = derive_bind iso_SW

instance (Monad m) => Monad (ExceptionT i m) where
  return  = t_return
  fail    = t_fail
  m >>= k = X $ runExceptionT m >>= \a ->
                case a of
                  Left x  -> return (Left x)
                  Right a -> runExceptionT (k a)

instance (Monad m) => Monad (ChoiceT m) where
  return x  = Answer x
  fail x    = lift (fail x)

  Answer a  >>= k     = k a
  NoAnswer >>= _      = NoAnswer
  Choice m1 m2 >>= k  = Choice (m1 >>= k) (m2 >>= k)
  ChoiceEff m >>= k   = ChoiceEff (liftM (>>= k) m)

instance (Monad m) => Monad (ContT i m) where
  return  = t_return
  fail    = t_fail
  m >>= k = C $ \c -> runContT (\a -> runContT c (k a)) m

instance                       Functor Id               where fmap = liftM
instance                       Functor Lift             where fmap = liftM
instance (Monad m)          => Functor (IdT          m) where fmap = liftM
instance (Monad m)          => Functor (ReaderT    i m) where fmap = liftM
instance (Monad m)          => Functor (StateT     i m) where fmap = liftM
instance (Monad m,Monoid i,P p) => Functor (GenWriterT p  i m)
                                                        where fmap = liftM
instance (Monad m,Monoid i) => Functor (WriterT    i m) where fmap = liftM
instance (Monad m,Monoid i) => Functor (StrictWriterT i m)
                                                        where fmap = liftM
instance (Monad m)          => Functor (ExceptionT i m) where fmap = liftM
instance (Monad m)          => Functor (ChoiceT      m) where fmap = liftM
instance (Monad m)          => Functor (ContT      i m) where fmap = liftM


-- $Monadic_Value_Recursion
--
-- Recursion that does not duplicate side-effects.
-- For details see Levent Erkok's dissertation.
--
-- Monadic types built with 'ContT' do not support
-- monadic value recursion.

instance MonadFix Id where
  mfix f  = let m = f (runId m) in m

instance MonadFix Lift where
  mfix f  = let m = f (runLift m) in m

instance (MonadFix m) => MonadFix (IdT m) where
  mfix f  = IT (mfix (runIdT . f))

instance (MonadFix m) => MonadFix (ReaderT i m) where
  mfix f  = R $ \r -> mfix (runReaderT r . f)

instance (MonadFix m) => MonadFix (StateT i m) where
  mfix f  = S $ \s -> mfix (runStateT s . f . fst)

instance (MonadFix m,Monoid i,P p) => MonadFix (GenWriterT p i m) where
  mfix f  = W $ mfix (unW . f . fst . to_pair)

instance (MonadFix m,Monoid i) => MonadFix (WriterT i m) where
  mfix    = derive_mfix iso_LW

instance (MonadFix m,Monoid i) => MonadFix (StrictWriterT i m) where
  mfix    = derive_mfix iso_SW

-- No instance for ChoiceT

instance (MonadFix m) => MonadFix (ExceptionT i m) where
  mfix f  = X $ mfix (runExceptionT . f . fromRight)
    where fromRight (Right a) = a
          fromRight _         = error "ExceptionT: mfix looped."

-- No instance for ContT

instance (MonadPlus m) => MonadPlus (IdT m) where
  mzero               = t_mzero
  mplus (IT m) (IT n) = IT (mplus m n)

instance (MonadPlus m) => MonadPlus (ReaderT i m) where
  mzero             = t_mzero
  mplus (R m) (R n) = R (\r -> mplus (m r) (n r))

instance (MonadPlus m) => MonadPlus (StateT i m) where
  mzero             = t_mzero
  mplus (S m) (S n) = S (\s -> mplus (m s) (n s))

instance (MonadPlus m,Monoid i,P p) => MonadPlus (GenWriterT p i m) where
  mzero             = t_mzero
  mplus (W m) (W n) = W (mplus m n)

instance (MonadPlus m,Monoid i) => MonadPlus (WriterT i m) where
  mzero             = t_mzero
  mplus             = derive_mplus iso_LW

instance (MonadPlus m,Monoid i) => MonadPlus (StrictWriterT i m) where
  mzero             = t_mzero
  mplus             = derive_mplus iso_SW

instance (MonadPlus m) => MonadPlus (ExceptionT i m) where
  mzero             = t_mzero
  mplus (X m) (X n) = X (mplus m n)

instance (Monad m) => MonadPlus (ChoiceT m) where
  mzero             = NoAnswer
  mplus m n         = Choice m n

-- Alternatives share the continuation.
instance (MonadPlus m) => MonadPlus (ContT i m) where
  mzero             = t_mzero
  mplus (C m) (C n) = C (\k -> m k `mplus` n k)


-- $Effects
--
-- The following classes define overloaded operations
-- that can be used to define effectful computations.


-- | Classifies monads that provide access to a context of type @i@.
class (Monad m) => ReaderM m i | m -> i where
  -- | Get the context.
  ask :: m i

instance (Monad m) => ReaderM (ReaderT i m) i where
  ask = R return

instance (ReaderM m j) => ReaderM (IdT m) j           where ask = t_ask
instance (ReaderM m j,Monoid i, P p) => ReaderM (GenWriterT p  i m) j
                                                      where ask = t_ask
instance (ReaderM m j,Monoid i) => ReaderM (WriterT i m) j
                                                      where ask = t_ask
instance (ReaderM m j,Monoid i) => ReaderM (StrictWriterT i m) j
                                                      where ask = t_ask
instance (ReaderM m j) => ReaderM (StateT i m) j      where ask = t_ask
instance (ReaderM m j) => ReaderM (ExceptionT i m) j  where ask = t_ask
instance (ReaderM m j) => ReaderM (ChoiceT m) j       where ask = t_ask
instance (ReaderM m j) => ReaderM (ContT i m) j       where ask = t_ask


-- | Classifies monads that can collect values of type @i@.
class (Monad m) => WriterM m i | m -> i where
  -- | Add a value to the collection.
  put  :: i -> m ()

instance (Monad m,Monoid i, P p) => WriterM (GenWriterT p i m) i where
  put x = W (return (pair () x))

instance (Monad m,Monoid i) => WriterM (WriterT i m) i where
  put   = derive_put iso_LW
instance (Monad m,Monoid i) => WriterM (StrictWriterT i m) i where
  put   = derive_put iso_SW

instance (WriterM m j) => WriterM (IdT          m) j where put = t_put
instance (WriterM m j) => WriterM (ReaderT    i m) j where put = t_put
instance (WriterM m j) => WriterM (StateT     i m) j where put = t_put
instance (WriterM m j) => WriterM (ExceptionT i m) j where put = t_put
instance (WriterM m j) => WriterM (ChoiceT      m) j where put = t_put
instance (WriterM m j) => WriterM (ContT      i m) j where put = t_put

-- | Classifies monads that propagate a state component of type @i@.
class (Monad m) => StateM m i | m -> i where
  -- | Get the state.
  get :: m i
  -- | Set the state.
  set :: i -> m ()

instance (Monad m) => StateM (StateT i m) i where
  get   = S (\s -> return (s,s))
  set s = S (\_ -> return ((),s))

instance (StateM m j) => StateM (IdT m) j where
  get = t_get; set = t_set
instance (StateM m j) => StateM (ReaderT i m) j where
  get = t_get; set = t_set
instance (StateM m j,Monoid i, P p) => StateM (GenWriterT p i m) j where
  get = t_get; set = t_set
instance (StateM m j,Monoid i) => StateM (WriterT i m) j where
  get = t_get; set = t_set
instance (StateM m j,Monoid i) => StateM (StrictWriterT i m) j where
  get = t_get; set = t_set
instance (StateM m j) => StateM (ExceptionT i m) j where
  get = t_get; set = t_set
instance (StateM m j) => StateM (ChoiceT m) j where
  get = t_get; set = t_set
instance (StateM m j) => StateM (ContT i m) j where
  get = t_get; set = t_set

-- | Classifies monads that support raising exceptions of type @i@.
class (Monad m) => ExceptionM m i | m -> i where
  -- | Raise an exception.
  raise :: i -> m a

instance (Monad m) => ExceptionM (ExceptionT i m) i where
  raise x = X (return (Left x))

instance (ExceptionM m j) => ExceptionM (IdT m) j where
  raise = t_raise
instance (ExceptionM m j) => ExceptionM (ReaderT i m) j where
  raise = t_raise
instance (ExceptionM m j,Monoid i, P p) => ExceptionM (GenWriterT p i m) j where
  raise = t_raise
instance (ExceptionM m j,Monoid i) => ExceptionM (WriterT i m) j where
  raise = t_raise
instance (ExceptionM m j,Monoid i) => ExceptionM (StrictWriterT i m) j where
  raise = t_raise
instance (ExceptionM m j) => ExceptionM (StateT  i m) j where
  raise = t_raise
instance (ExceptionM m j) => ExceptionM (ChoiceT   m) j where
  raise = t_raise
instance (ExceptionM m j) => ExceptionM (ContT   i m) j where
  raise = t_raise


-- The following instances differ from the others because the
-- liftings are not as uniform (although they certainly follow a pattern).

-- | Classifies monads that provide access to a computation's continuation.
class Monad m => ContM m where
  -- | Capture the current continuation.
  callCC :: ((a -> m b) -> m a) -> m a

instance (ContM m) => ContM (IdT m) where
  callCC f = IT $ callCC $ \k -> runIdT $ f $ \a -> lift $ k a

instance (ContM m) => ContM (ReaderT i m) where
  callCC f = R $ \r -> callCC $ \k -> runReaderT r $ f $ \a -> lift $ k a

instance (ContM m) => ContM (StateT i m) where
  callCC f = S $ \s -> callCC $ \k -> runStateT s $ f $ \a -> lift $ k (a,s)

instance (ContM m,Monoid i, P p) => ContM (GenWriterT p i m) where
  callCC f = W $ callCC $ \k -> unW $ f $ \a -> lift $ k (pair a mempty)

instance (ContM m,Monoid i) => ContM (WriterT i m) where
  callCC = derive_callCC iso_LW

instance (ContM m,Monoid i) => ContM (StrictWriterT i m) where
  callCC = derive_callCC iso_SW

instance (ContM m) => ContM (ExceptionT i m) where
  callCC f = X $ callCC $ \k -> runExceptionT $ f $ \a -> lift $ k $ Right a

instance (ContM m) => ContM (ChoiceT m) where
  callCC f = ChoiceEff $ callCC $ \k -> return $ f $ \a -> lift $ k $ Answer a
    -- ??? What does this do ???

instance (Monad m) => ContM (ContT i m) where
  callCC f = C $ \k -> runContT k $ f $ \a -> C $ \_ -> k a


-- $Nested_Exec
--
-- The following classes define operations that are overloaded
-- versions of the @run@ operations.   Unlike the @run@ operations,
-- these functions do not change the type of the computation (i.e, they
-- do not remove a layer).  Instead, they perform the effects in
-- a ``separate effect thread''.

-- | Classifies monads that support changing the context for a
-- sub-computation.
class (ReaderM m i) => RunReaderM m i | m -> i where
  -- | Change the context for the duration of a computation.
  local        :: i -> m a -> m a
  -- prop(?): local i (m1 >> m2) = local i m1 >> local i m2

instance (Monad m)        => RunReaderM (ReaderT    i m) i where
  local i m     = lift (runReaderT i m)

instance (RunReaderM m j) => RunReaderM (IdT m) j where
  local i (IT m) = IT (local i m)
instance (RunReaderM m j,Monoid i, P p) => RunReaderM (GenWriterT p i m) j where
  local i (W m) = W (local i m)
instance (RunReaderM m j,Monoid i) => RunReaderM (WriterT i m) j where
  local = derive_local iso_LW
instance (RunReaderM m j,Monoid i) => RunReaderM (StrictWriterT i m) j where
  local = derive_local iso_SW
instance (RunReaderM m j) => RunReaderM (StateT     i m) j where
  local i (S m) = S (local i . m)
instance (RunReaderM m j) => RunReaderM (ExceptionT i m) j where
  local i (X m) = X (local i m)

-- | Classifies monads that support collecting the output of
-- a sub-computation.
class WriterM m i => RunWriterM m i | m -> i where
  -- | Collect the output from a computation.
  collect :: m a -> m (a,i)

instance (Monad m,Monoid i, P p) => RunWriterM (GenWriterT p i m) i where
  collect m = lift (runGenWriterT m)
instance (Monad m,Monoid i) => RunWriterM (WriterT i m) i where
  collect = derive_collect iso_LW
instance (Monad m,Monoid i) => RunWriterM (StrictWriterT i m) i where
  collect = derive_collect iso_SW

instance (RunWriterM m j) => RunWriterM (IdT m) j where
  collect (IT m) = IT (collect m)
instance (RunWriterM m j) => RunWriterM (ReaderT i m) j where
  collect (R m) = R (collect . m)
instance (RunWriterM m j) => RunWriterM (StateT i m) j where
  collect (S m) = S (liftM swap . collect . m)
    where swap (~(a,s),w) = ((a,w),s)
instance (RunWriterM m j) => RunWriterM (ExceptionT i m) j where
  collect (X m) = X (liftM swap (collect m))
    where swap (Right a,w)  = Right (a,w)
          swap (Left x,_)   = Left x
  -- NOTE: if an exception is risen while we are collecting,
  -- then we ignore the output.  If the output is important,
  -- then use 'try' to ensure that no exception may occur.
  -- Example: do (r,w) <- collect (try m)
  --             case r of
  --               Left err -> ... do something ...
  --               Right a  -> ... do something ...

-- | Classifies monads that support handling of exceptions.
class ExceptionM m i => RunExceptionM m i | m -> i where
  -- | Exceptions are explicit in the result.
  try :: m a -> m (Either i a)

instance (Monad m) => RunExceptionM (ExceptionT i m) i where
  try m = lift (runExceptionT m)

instance (RunExceptionM m i) => RunExceptionM (IdT m) i where
  try (IT m) = IT (try m)
instance (RunExceptionM m i) => RunExceptionM (ReaderT j m) i where
  try (R m) = R (try . m)
instance (RunExceptionM m i,Monoid j, P p) =>
            RunExceptionM (GenWriterT p j m) i where
  try (W m) = W (liftM swap (try m))
    where swap (Right r) = let (a,w) = to_pair r in pair (Right a) w
          swap (Left e)       = pair (Left e) mempty
instance (RunExceptionM m i,Monoid j) => RunExceptionM (WriterT j m) i where
  try = derive_try iso_LW
instance (RunExceptionM m i,Monoid j) =>
            RunExceptionM (StrictWriterT j m) i where
  try = derive_try iso_SW
instance (RunExceptionM m i) => RunExceptionM (StateT j m) i where
  try (S m) = S (\s -> liftM (swap s) (try (m s)))
    where swap _ (Right ~(a,s)) = (Right a,s)
          swap s (Left e)       = (Left e, s)


--------------------------------------------------------------------------------
-- Some convenient functions for working with continuations.

-- | An explicit representation for continuations that store a value.
newtype Label m a    = Lab ((a, Label m a) -> m ())

-- | Capture the current continuation
-- This function is like 'return', except that it also captures
-- the current continuation.  Later we can use 'jump' to go back to
-- the continuation with a possibly different value.
labelCC            :: (ContM m) => a -> m (a, Label m a)
labelCC x           = callCC (\k -> return (x, Lab k))

-- | Change the value passed to a previously captured continuation.
jump               :: (ContM m) => a -> Label m a -> m b
jump x (Lab k)      = k (x, Lab k) >> return unreachable
  where unreachable = error "(bug) jump: unreachable"


--------------------------------------------------------------------------------
-- | A isomorphism between (usually) monads.
-- Typically the constructor and selector of a newtype delcaration.
data Iso m n = Iso { close :: forall a. m a -> n a,
                     open  :: forall a. n a -> m a }

-- | Derive the implementation of 'fmap' from 'Functor'.
derive_fmap :: (Functor m) => Iso m n -> (a -> b) -> n a -> n b
derive_fmap iso f m = close iso (fmap f (open iso m))

-- | Derive the implementation of 'return' from 'Monad'.
derive_return :: (Monad m) => Iso m n -> (a -> n a)
derive_return iso a = close iso (return a)

-- | Derive the implementation of '>>=' from 'Monad'.
derive_bind :: (Monad m) => Iso m n -> n a -> (a -> n b) -> n b
derive_bind iso m k = close iso ((open iso m) >>= \x -> open iso (k x))

derive_fail :: (Monad m) => Iso m n -> String -> n a
derive_fail iso a = close iso (fail a)

-- | Derive the implementation of 'mfix' from 'MonadFix'.
derive_mfix :: (MonadFix m) => Iso m n -> (a -> n a) -> n a
derive_mfix iso f = close iso (mfix (open iso . f))

-- | Derive the implementation of 'ask' from 'ReaderM'.
derive_ask :: (ReaderM m i) => Iso m n -> n i
derive_ask iso = close iso ask

-- | Derive the implementation of 'put' from 'WriterM'.
derive_put :: (WriterM m i) => Iso m n -> i -> n ()
derive_put iso x = close iso (put x)

-- | Derive the implementation of 'get' from 'StateM'.
derive_get :: (StateM m i) => Iso m n -> n i
derive_get iso = close iso get

-- | Derive the implementation of 'set' from 'StateM'.
derive_set :: (StateM m i) => Iso m n -> i -> n ()
derive_set iso x = close iso (set x)

-- | Derive the implementation of 'raise' from 'ExceptionM'.
derive_raise :: (ExceptionM m i) => Iso m n -> i -> n a
derive_raise iso x = close iso (raise x)

-- | Derive the implementation of 'callCC' from 'ContM'.
derive_callCC :: (ContM m) => Iso m n -> ((a -> n b) -> n a) -> n a
derive_callCC iso f = close iso (callCC (open iso . f . (close iso .)))

-- | Derive the implementation of 'local' from 'RunReaderM'.
derive_local :: (RunReaderM m i) => Iso m n -> i -> n a -> n a
derive_local iso i = close iso . local i . open iso

-- | Derive the implementation of 'collect' from 'RunWriterM'.
derive_collect :: (RunWriterM m i) => Iso m n -> n a -> n (a,i)
derive_collect iso = close iso . collect . open iso

-- | Derive the implementation of 'try' from 'RunExceptionM'.
derive_try :: (RunExceptionM m i) => Iso m n -> n a -> n (Either i a)
derive_try iso = close iso . try . open iso

derive_mzero :: (MonadPlus m) => Iso m n -> n a
derive_mzero iso = close iso mzero

derive_mplus :: (MonadPlus m) => Iso m n -> n a -> n a -> n a
derive_mplus iso n1 n2 = close iso (mplus (open iso n1) (open iso n2))

derive_lift :: (MonadT t, Monad m) => Iso (t m) n -> m a -> n a
derive_lift iso m = close iso (lift m)

derive_inBase :: (BaseM m x) => Iso m n -> x a -> n a
derive_inBase iso m = close iso (inBase m)
