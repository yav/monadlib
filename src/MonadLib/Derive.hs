{-| This module defines a number of functions that make it easy
to get the functionality of MonadLib for user-defined newtypes.
-}
module MonadLib.Derive (
  Iso(Iso), derive_fmap, derive_return, derive_bind, derive_fail, derive_mfix,
  derive_ask, derive_put, derive_get, derive_set, derive_raise, derive_callCC,
  derive_abort,
  derive_local, derive_collect, derive_try,
  derive_mzero, derive_mplus,
  derive_lift, derive_inBase,
) where


import MonadLib
import Control.Monad
import Control.Monad.Fix
import Prelude hiding (Ordering(..))

-- | An isomorphism between (usually) monads.
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

derive_abort :: (AbortM m i) => Iso m n -> i -> n a
derive_abort iso i = close iso (abort i)

-- | Derive the implementation of 'local' from 'RunReaderM'.
derive_local :: (RunReaderM m i) => Iso m n -> i -> n a -> n a
derive_local iso i = close iso . local i . open iso

-- | Derive the implementation of 'collect' from 'RunWriterM'.
derive_collect :: (RunWriterM m i) => Iso m n -> n a -> n (a,i)
derive_collect iso = close iso . collect . open iso

-- | Derive the implementation of 'try' from 'RunExceptionM'.
derive_try :: (RunExceptionM m i) => Iso m n -> n a -> n (Either i a)
derive_try iso = close iso . try . open iso

-- | Derive the implementation of 'mzero' from 'MonadPlus'.
derive_mzero :: (MonadPlus m) => Iso m n -> n a
derive_mzero iso = close iso mzero

-- | Derive the implementation of 'mplus' from 'MonadPlus'.
derive_mplus :: (MonadPlus m) => Iso m n -> n a -> n a -> n a
derive_mplus iso n1 n2 = close iso (mplus (open iso n1) (open iso n2))

-- | Derive the implementation of 'lift' from 'MonadT'.
derive_lift :: (MonadT t, Monad m) => Iso (t m) n -> m a -> n a
derive_lift iso m = close iso (lift m)

-- | Derive the implementation of 'inBase' from 'BaseM'.
derive_inBase :: (BaseM m x) => Iso m n -> x a -> n a
derive_inBase iso m = close iso (inBase m)
