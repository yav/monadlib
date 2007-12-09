{-# OPTIONS_GHC -XTypeFamilies -XRank2Types #-}
{-| This library provides a collection of monad transformers that
    can be combined to produce various monads.
-}
module MonadLib (
  -- * Types
  -- $Types
  Id, Lift, IdT, ReaderT, WriterT, StateT, ExceptionT, ChoiceT, ContT,

  -- * Lifting
  -- $Lifting
  MonadT(..), HasBase(..),

  -- * Effect Classes
  -- $Effects
  ReaderM(..), WriterM(..), StateM(..), ExceptionM(..), ContM(..),
  Label, labelCC, jump,

  -- * Execution

  -- ** Eliminating Effects
  -- $Execution
  runId, runLift,
  runIdT, runReaderT, runWriterT, runStateT, runExceptionT, runContT,
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
version = (3,4,0)


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
newtype ReaderT    i m a  = R (i -> m a)

-- | Add support for collecting values.
newtype WriterT    i m a  = W (m (a,i))

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
newtype ContT      i m a  = C ((a -> m i) -> m i)



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

-- | Execute a writer computation.
-- Returns the result and the collected output.
runWriterT    :: WriterT i m a -> m (a,i)
runWriterT (W m) = m

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
instance (Monoid i) =>
         MonadT (WriterT    i) where lift m = W (liftM (\a -> (a,mempty)) m)
instance MonadT (ExceptionT i) where lift m = X (liftM Right m)
instance MonadT ChoiceT        where lift m = ChoiceEff (liftM Answer m)
instance MonadT (ContT      i) where lift m = C (m >>=)


class Monad m => HasBase m where
  type BaseM m :: * -> *  -- How to specify: Monad (Base m)

  -- | Promote a computation from the base monad.
  inBase :: BaseM m a -> m a

instance HasBase IO     where type BaseM IO    = IO;     inBase = id
instance HasBase Maybe  where type BaseM Maybe = Maybe;  inBase = id
instance HasBase []     where type BaseM []    = [];     inBase = id
instance HasBase Id     where type BaseM Id    = Id;     inBase = id
instance HasBase Lift   where type BaseM Lift  = Lift;   inBase = id

instance HasBase m => HasBase (IdT m) where
  type BaseM (IdT m) = BaseM m
  inBase m = lift (inBase m)

instance (HasBase m) => HasBase (ReaderT i m) where
  type BaseM (ReaderT i m) = BaseM m
  inBase m = lift (inBase m)

instance (HasBase m) => HasBase (StateT i m) where
  type BaseM (StateT i m) = BaseM m
  inBase m = lift (inBase m)

instance (HasBase m,Monoid i) => HasBase (WriterT i m) where
  type BaseM (WriterT i m) = BaseM m
  inBase m = lift (inBase m)

instance (HasBase m) => HasBase (ExceptionT i m) where
  type BaseM (ExceptionT i m) = BaseM m
  inBase m = lift (inBase m)

instance (HasBase m) => HasBase (ChoiceT m) where
  type BaseM (ChoiceT m) = BaseM m
  inBase m = lift (inBase m)

instance (HasBase m) => HasBase (ContT i m) where
  type BaseM (ContT i m) = BaseM m
  inBase m = lift (inBase m)


instance Monad Id where
  return x = I x
  fail x   = error x
  m >>= k  = k (runId m)


instance Monad Lift where
  return x  = L x
  fail x    = error x
  L x >>= k = k x

-- For the monad transformers, the definition of 'return'
-- is completely determined by the 'lift' operations.

-- None of the transformers make essential use of the 'fail' method.
-- Instead they delegate its behavior to the underlying monad.

instance (Monad m) => Monad (IdT m) where
  return x    = lift (return x)
  fail x      = lift (fail x)
  m >>= k     = IT (runIdT m >>= (runIdT . k))

instance (Monad m) => Monad (ReaderT i m) where
  return x = lift (return x)
  fail x   = lift (fail x)
  m >>= k  = R (\r -> runReaderT r m >>= \a -> runReaderT r (k a))

instance (Monad m) => Monad (StateT i m) where
  return x = lift (return x)
  fail x   = lift (fail x)
  m >>= k  = S (\s -> runStateT s m >>= \ ~(a,s') -> runStateT s' (k a))

instance (Monad m,Monoid i) => Monad (WriterT i m) where
  return x = lift (return x)
  fail x   = lift (fail x)
  m >>= k  = W $ runWriterT m     >>= \ ~(a,w1) ->
                 runWriterT (k a) >>= \ ~(b,w2) ->
                 return (b,mappend w1 w2)

instance (Monad m) => Monad (ExceptionT i m) where
  return x = lift (return x)
  fail x   = lift (fail x)
  m >>= k  = X $ runExceptionT m >>= \a ->
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
  return x = lift (return x)
  fail x   = lift (fail x)
  m >>= k  = C $ \c -> runContT (\a -> runContT c (k a)) m

instance                       Functor Id               where fmap = liftM
instance                       Functor Lift             where fmap = liftM
instance (Monad m)          => Functor (IdT          m) where fmap = liftM
instance (Monad m)          => Functor (ReaderT    i m) where fmap = liftM
instance (Monad m)          => Functor (StateT     i m) where fmap = liftM
instance (Monad m,Monoid i) => Functor (WriterT    i m) where fmap = liftM
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

instance (MonadFix m,Monoid i) => MonadFix (WriterT i m) where
  mfix f  = W $ mfix (runWriterT . f . fst)

-- No instance for ChoiceT

instance (MonadFix m) => MonadFix (ExceptionT i m) where
  mfix f  = X $ mfix (runExceptionT . f . fromRight)
    where fromRight (Right a) = a
          fromRight _         = error "ExceptionT: mfix looped."

-- No instance for ContT


instance (MonadPlus m) => MonadPlus (IdT m) where
  mzero               = lift mzero
  mplus (IT m) (IT n) = IT (mplus m n)

instance (MonadPlus m) => MonadPlus (ReaderT i m) where
  mzero             = lift mzero
  mplus (R m) (R n) = R (\r -> mplus (m r) (n r))

instance (MonadPlus m) => MonadPlus (StateT i m) where
  mzero             = lift mzero
  mplus (S m) (S n) = S (\s -> mplus (m s) (n s))

instance (MonadPlus m,Monoid i) => MonadPlus (WriterT i m) where
  mzero             = lift mzero
  mplus (W m) (W n) = W (mplus m n)

instance (MonadPlus m) => MonadPlus (ExceptionT i m) where
  mzero             = lift mzero
  mplus (X m) (X n) = X (mplus m n)

instance (Monad m) => MonadPlus (ChoiceT m) where
  mzero             = NoAnswer
  mplus m n         = Choice m n

-- Alternatives share the continuation.
instance (MonadPlus m) => MonadPlus (ContT i m) where
  mzero             = lift mzero
  mplus (C m) (C n) = C (\k -> m k `mplus` n k)


-- $Effects
--
-- The following classes define overloaded operations
-- that can be used to define effectful computations.


-- | Classifies monads that provide access to a context of type @i@.
class (Monad m) => ReaderM m where
  type Reads m

  -- | Get the context.
  ask :: m (Reads m)

instance (Monad m) => ReaderM (ReaderT i m) where
  type Reads (ReaderT i m) = i
  ask = R return

instance (ReaderM m) => ReaderM (IdT m) where
  type Reads (IdT m) = Reads m
  ask = lift ask

instance (ReaderM m, Monoid i) => ReaderM (WriterT i m) where
  type Reads (WriterT i m) = Reads m
  ask = lift ask

instance (ReaderM m) => ReaderM (StateT i m) where
  type Reads (StateT i m) = Reads m
  ask = lift ask

instance (ReaderM m) => ReaderM (ExceptionT i m) where
  type Reads (ExceptionT i m) = Reads m
  ask = lift ask

instance (ReaderM m) => ReaderM (ChoiceT m) where
  type Reads (ChoiceT m) = Reads m
  ask = lift ask

instance (ReaderM m) => ReaderM (ContT i m) where
  type Reads (ContT i m) = Reads m
  ask = lift ask


-- t
-- | Classifies monads that can collect values.
class (Monad m) => WriterM m where

  -- | The types of the collection that we modify.
  type Writes m

  -- | Add a value to the collection.
  put  :: Writes m -> m ()

instance (Monad m,Monoid i) => WriterM (WriterT i m) where
  type Writes (WriterT i m) = i
  put x = W (return ((),x))

instance (WriterM m) => WriterM (IdT m) where
  type Writes (IdT m) = Writes m
  put x = lift (put x)

instance (WriterM m) => WriterM (ReaderT i m) where
  type Writes (ReaderT i m) = Writes m
  put x = lift (put x)

instance (WriterM m) => WriterM (StateT i m) where
  type Writes (StateT i m) = Writes m
  put x = lift (put x)

instance (WriterM m) => WriterM (ExceptionT i m) where
  type Writes (ExceptionT i m) = Writes m
  put x = lift (put x)

instance (WriterM m) => WriterM (ChoiceT m) where
  type Writes (ChoiceT m) = Writes m
  put x = lift (put x)

instance (WriterM m) => WriterM (ContT i m) where
  type Writes (ContT i m) = Writes m
  put x = lift (put x)


-- | Classifies monads that propagate a state component.
class (Monad m) => StateM m where

  -- | The type of the state.
  type State m

  -- | Get the state.
  get :: m (State m)

  -- | Set the state.
  set :: State m -> m ()

instance (Monad m) => StateM (StateT i m) where
  type State (StateT i m) = i
  get   = S (\s -> return (s,s))
  set s = S (\_ -> return ((),s))

instance (StateM m) => StateM (IdT m) where
  type State (IdT m) = State m
  get = lift get
  set s = lift (set s)

instance (StateM m) => StateM (ReaderT i m) where
  type State (ReaderT i m) = State m
  get = lift get
  set s = lift (set s)

instance (StateM m,Monoid i) => StateM (WriterT i m) where
  type State (WriterT i m) = State m
  get = lift get
  set s = lift (set s)

instance (StateM m) => StateM (ExceptionT i m) where
  type State (ExceptionT i m) = State m
  get = lift get
  set s = lift (set s)

instance (StateM m) => StateM (ChoiceT m) where
  type State (ChoiceT m) = State m
  get = lift get
  set s = lift (set s)

instance (StateM m) => StateM (ContT i m) where
  type State (ContT i m) = State m
  get = lift get
  set s = lift (set s)


-- | Classifies monads that support raising exceptions.
class (Monad m) => ExceptionM m where

  -- | The type of the exceptions.
  type Exception m

  -- | Raise an exception.
  raise :: Exception m -> m a

instance (Monad m) => ExceptionM (ExceptionT i m) where
  type Exception (ExceptionT i m) = i
  raise x = X (return (Left x))

instance (ExceptionM m) => ExceptionM (IdT m) where
  type Exception (IdT m) = Exception m
  raise x = lift (raise x)

instance (ExceptionM m) => ExceptionM (ReaderT i m) where
  type Exception (ReaderT i m) = Exception m
  raise x = lift (raise x)

instance (ExceptionM m,Monoid i) => ExceptionM (WriterT i m) where
  type Exception (WriterT i m) = Exception m
  raise x = lift (raise x)

instance (ExceptionM m) => ExceptionM (StateT  i m) where
  type Exception (StateT i m) = Exception m
  raise x = lift (raise x)

instance (ExceptionM m) => ExceptionM (ChoiceT m) where
  type Exception (ChoiceT m) = Exception m
  raise x = lift (raise x)

instance (ExceptionM m) => ExceptionM (ContT i m) where
  type Exception (ContT i m) = Exception m
  raise x = lift (raise x)


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

instance (ContM m,Monoid i) => ContM (WriterT i m) where
  callCC f = W $ callCC $ \k -> runWriterT $ f $ \a -> lift $ k (a,mempty)

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
class ReaderM m => RunReaderM m where
  -- | Change the context for the duration of a computation.
  local        :: Reads m -> m a -> m a
  -- prop(?): local i (m1 >> m2) = local i m1 >> local i m2

instance (Monad m) => RunReaderM (ReaderT i m) where
  local i m     = lift (runReaderT i m)

instance (RunReaderM m) => RunReaderM (IdT m) where
  local i (IT m) = IT (local i m)
instance (RunReaderM m,Monoid i) => RunReaderM (WriterT i m) where
  local i (W m) = W (local i m)
instance (RunReaderM m) => RunReaderM (StateT i m)where
  local i (S m) = S (local i . m)
instance (RunReaderM m) => RunReaderM (ExceptionT i m) where
  local i (X m) = X (local i m)

-- | Classifies monads that support collecting the output of
-- a sub-computation.
class WriterM m => RunWriterM m where
  -- | Collect the output from a computation.
  collect :: m a -> m (a,Writes m)

instance (Monad m, Monoid i) => RunWriterM (WriterT i m) where
  collect m = lift (runWriterT m)

instance (RunWriterM m) => RunWriterM (IdT m) where
  collect (IT m) = IT (collect m)
instance (RunWriterM m) => RunWriterM (ReaderT i m) where
  collect (R m) = R (collect . m)
instance (RunWriterM m) => RunWriterM (StateT i m) where
  collect (S m) = S (liftM swap . collect . m)
    where swap (~(a,s),w) = ((a,w),s)
instance (RunWriterM m) => RunWriterM (ExceptionT i m) where
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
class ExceptionM m => RunExceptionM m where
  -- | Exceptions are explicit in the result.
  try :: m a -> m (Either (Exception m) a)

instance (Monad m) => RunExceptionM (ExceptionT i m) where
  try m = lift (runExceptionT m)

instance (RunExceptionM m) => RunExceptionM (IdT m) where
  try (IT m) = IT (try m)

instance (RunExceptionM m) => RunExceptionM (ReaderT j m) where
  try (R m) = R (try . m)

instance (RunExceptionM m,Monoid j) => RunExceptionM (WriterT j m) where
  try (W m) = W (liftM swap (try m))
    where swap (Right ~(a,w)) = (Right a,w)
          swap (Left e)       = (Left e, mempty)

instance (RunExceptionM m) => RunExceptionM (StateT j m) where
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
derive_ask :: (ReaderM m) => Iso m n -> n (Reads m)
derive_ask iso = close iso ask

-- | Derive the implementation of 'put' from 'WriterM'.
derive_put :: (WriterM m) => Iso m n -> Writes m -> n ()
derive_put iso x = close iso (put x)

-- | Derive the implementation of 'get' from 'StateM'.
derive_get :: (StateM m) => Iso m n -> n (State m)
derive_get iso = close iso get

-- | Derive the implementation of 'set' from 'StateM'.
derive_set :: (StateM m) => Iso m n -> State m -> n ()
derive_set iso x = close iso (set x)

-- | Derive the implementation of 'raise' from 'ExceptionM'.
derive_raise :: (ExceptionM m) => Iso m n -> Exception m -> n a
derive_raise iso x = close iso (raise x)

-- | Derive the implementation of 'callCC' from 'ContM'.
derive_callCC :: (ContM m) => Iso m n -> ((a -> n b) -> n a) -> n a
derive_callCC iso f = close iso (callCC (open iso . f . (close iso .)))

-- | Derive the implementation of 'local' from 'RunReaderM'.
derive_local :: (RunReaderM m) => Iso m n -> Reads m -> n a -> n a
derive_local iso i = close iso . local i . open iso

-- | Derive the implementation of 'collect' from 'RunWriterM'.
derive_collect :: (RunWriterM m) => Iso m n -> n a -> n (a,Writes m)
derive_collect iso = close iso . collect . open iso

-- | Derive the implementation of 'try' from 'RunExceptionM'.
derive_try :: (RunExceptionM m) => Iso m n -> n a -> n (Either (Exception m) a)
derive_try iso = close iso . try . open iso

derive_mzero :: (MonadPlus m) => Iso m n -> n a
derive_mzero iso = close iso mzero

derive_mplus :: (MonadPlus m) => Iso m n -> n a -> n a -> n a
derive_mplus iso n1 n2 = close iso (mplus (open iso n1) (open iso n2))

derive_lift :: (MonadT t, Monad m) => Iso (t m) n -> m a -> n a
derive_lift iso m = close iso (lift m)

derive_inBase :: (HasBase m) => Iso m n -> BaseM m a -> n a
derive_inBase iso m = close iso (inBase m)

