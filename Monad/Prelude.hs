module Monad.Prelude 
  ( module Monad.Prelude
  , module Monad.Combinators
  , module Monad.ForEach
  ) where

import Monad.Combinators
import Monad.ForEach


-- Monad transformers ----------------------------------------------------------

-- | The class of monad transformers.
class Trans t where

  -- | Lift a computation in the underlying monad, to the constructed monad.
  lift             :: Monad m => m a -> t m a


-- | Provides means to execute a computation in the base of a tower of monads.
class (Monad m, Monad b) => BaseM m b | m -> b where
  inBase           :: b a -> m a

instance BaseM IO IO        where inBase x = x
instance BaseM [] []        where inBase x = x
instance BaseM Maybe Maybe  where inBase x = x



-- Monads with an environment --------------------------------------------------

-- | Monads that provide access to a context.
class Monad m => ReaderM m r | m -> r where
  
  -- | Get the value of the context.
  get              :: m r

  -- | Modify the context for the duration of a computation.
  local            :: (r -> r) -> m a -> m a


-- | Set the context to a particular value for the duration of a computation.
letLocal           :: ReaderM m r => r -> m a -> m a
letLocal r m        = local (\_ -> r) m         

-- | Promote a function to a computation with an environment.
-- The argument to the function is the environment.
inReader           :: ReaderM m r => (r -> a) -> m a
inReader f          = f # get



-- Monads with an output -------------------------------------------------------

-- | Monads that can perform output.
class Monad m => WriterM m w | m -> w where

  -- | Add a value to the output.
  put              :: w -> m ()

-- | Promote a pair to a computation with output.
-- The first component is the result of the computation,
-- while the second component is sent to the output.
inWriter           :: WriterM m w => (a,w) -> m a
inWriter ~(a,w)     = put w >> return a



-- | Monads that can perform output, and it is possible to gather the output of a subcomputation. 
class WriterM m w => TakeWriterM m w | m -> w where
  takeFrom         :: m a -> m (a,w)

-- | 'mapBuffer f m' is a computation that behaves as 'm', 
-- except that its output is modified by 'f'.  Note that 
-- any output produced by 'f' will appear /before/ any of the output of 'm'.
mapBuffer          :: TakeWriterM m w => (w -> m w) -> m a -> m a
mapBuffer f m       = do ~(a,w) <- takeFrom m
                         put =<< f w
                         return a 


-- Stateful computations -------------------------------------------------------

-- | Monads that can manipulate state.
class Monad m => StateM m s | m -> s where

  -- | Get the value of the state.
  peek             :: m s

  -- | Set the value of the state.  Returns the old state.
  poke             :: s -> m s

-- | Update the value of the state.  Returns the old state.
update             :: StateM m s => (s -> s) -> m s
update f            = do x <- peek
                         poke (f x)
                         return x

-- | Update the state, and dicard the old state.
update_            :: StateM m s => (s -> s) -> m ()
update_ f           = update f >> return ()

-- | Set the state, without using the old state.
poke_              :: StateM m s => s -> m ()
poke_ s             = poke s >> return ()

-- | Promote a state transformer function to a stateful computation.
inState            :: StateM m s => (s -> (a,s)) -> m a
inState f           = do s <- peek
                         let (a,s') = f s
                         poke s'
                         return a



-- Computations that may raise an exception ------------------------------------

-- | Monads that can throw and catch exceptions.
class Monad m => ExceptM m e | m -> e where

  -- | Raise an exception.
  raise            :: e -> m a

  -- | Execute a computation in the context of a handler.
  -- If an exception occurs the handler is invoked.
  handle           :: m a -> (e -> m a) -> m a


-- | Install an error handler that ignores the exception.
-- | The handler is the second argument.
handle_            :: ExceptM m e => m a -> m a -> m a
handle_ m h         = m `handle` \_ -> h

-- | Promote a value of type 'Either', to a computation that may fail.
-- Values tagged with 'Left' are errors,
-- while valies tagged with 'Right' succeed (because they are Right :-)
inExcept           :: ExceptM m e => Either e a -> m a
inExcept (Left err) = raise err
inExcept (Right a)  = return a



-- Back tracking computations --------------------------------------------------

-- The interface to those is the standrad Haskell class 'MPlus'.

-- | Promote a list value to a backtracking(non-deterministc) computation.
-- The computation may succeed with any of the elements in the list. 
inBack             :: MonadPlus m => [a] -> m a
inBack as           = foldr mplus mzero (map return as)



-- Computations that may refer to their continuation ---------------------------

-- | Monads that allow for explicit handling of continuations.
class Monad m => ContM m where

  -- | Capture the current continuation.
  callcc           :: ((a -> m b) -> m a) -> m a


-- doCont            :: ContM m => (forall o. (a -> o) -> o) -> m a
