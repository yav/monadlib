module Unstable.Control.Monad.Trans 
  ( -- * General transformer classes
    MonadTrans(..),
    HasBaseMonad(..),
    MapTrans(..),

    -- * Plumbing transformers
    -- $PlumbingDoc

    -- ** Reader
    MonadReader(..), 
    -- $MonadReaderDoc
    asks,
    localSet,

    -- ** Writer
    MonadWriter(..),
    -- $MonadWriterDoc
    listens,
    censor,
    pass,
    listenTell,

    -- ** State
    MonadState(..),   
    -- $MonadStateDoc
    gets,
    modify,

    -- * Control transformers
    -- $ControlDoc

    -- ** Exceptions
    MonadError(..),
    -- $MonadErrorDoc
    throwError,
    catchError,

    -- ** Non-determinism
    MonadPlus(..),
    MonadNondet(..),
    -- $MonadNondetDoc
    findAllW,
    findAllS,
    findAllE,

    -- ** Resumptions
    MonadResume(..),
    -- $MonadGenResumeDoc

    -- ** Continuations
    MonadCont(..),
    -- $MonadContDoc
  )
  where

import Control.Monad(MonadPlus(..),liftM)
import Data.Monoid(Monoid)



--------------------------------------------------------------------------------
-- | Provides a way of going across one transformer layer.

class MonadTrans t where
  lift  :: Monad m => m a -> t m a
  -- ^ Provides a way of going across one transformer layer.


--------------------------------------------------------------------------------
-- | The predicate @HasBaseMonad m n@ indicates that 'm' is a monad
-- built by applying a number of transformers to 'n'.

class (Monad m, Monad n) => HasBaseMonad m n | m -> n where
  inBase :: n a -> m a
  -- ^ Provides a way of going across multiple transformer layers,
  -- all the way to the innermost atomic monad.

  mapBase :: (forall a. n a -> n a) -> m a -> m a
  -- ^ Provides a way to apply a function in the base monad,
  -- it involves applying 'mapTrans' repeatedly.




-- | This class enables a programmer to change the underlying monad of a computation.
class MonadTrans t => MapTrans t where
  mapTrans :: (Monad m, Monad n) => (forall a. m a -> n a) -> t m b -> t n b


-- Move me somewhere else.
instance HasBaseMonad IO IO where inBase = id; mapBase f = f
instance HasBaseMonad [] [] where inBase = id; mapBase f = f
instance HasBaseMonad Maybe Maybe where inBase = id; mapBase f = f




{- $PlumbingDoc
  /Plumbing transformers/ take care of propagating information around in a computation.
They all commute with each other.  This means that it doesn't meter 
in what order they are added to a computation, the final effect is the same.
-}

-- | A reader monad has the ability to propagate around a read-only environment.
-- One can think of the environment as a special read only variable that can
-- be accessed via the methods of the class.
-- See also "Unstable.Control.Monad.ReaderT".

class (Monad m) => MonadReader r m | m -> r where
  ask         :: m r
  -- ^ Read the value of the variable.

  local       :: (r -> r) -> m a -> m a
  -- ^ The method @local f m@ uses @f@ to change the value of the variable 
  -- for the duration of a computation @m@. After @m@ completes its execution
  -- the original value of the variable is restored.

{- $MonadReaderDoc
  Read-only variables are useful when some information needs to be carried
around, but is not used all the time. Such a situation may occur when a deeply nested
function call needs the information, but most of the functions involved in
a computation will not use it and simply pass it around.  Read-only variables
are very closely related to /implicit parameters/ <...>.
See also `MonadWriter'. 
-}


-- | Gets specific component of the environment, using the projection function
-- supplied.
asks          :: (MonadReader r m) => (r -> a) -> m a
asks f        = liftM f ask


-- | Temporarily sets the value of the read-only variable. One can think of
-- @localSet x m@ as:
-- @
-- let /var/ = x in m
-- @ 
localSet      :: MonadReader r m => r -> m a -> m a
localSet      = local . const


-- | A writer monad has the ability to collect a number of outputs generated
-- during a computation.  It is like carrying around a buffer that can be
-- manipulated with the methods of the class.  The 'Monoid' class specifies
-- how to make an empty buffer, and how to join two buffers together.
-- See also "Unstable.Control.Monad.WriterT".

class (Monoid w, Monad m) => MonadWriter w m | m -> w where
  tell        :: w -> m ()
  -- ^ @tell w@ appends the new information @w@ to the buffer.

  listen      :: m a -> m (a, w)
  -- ^ @listen m@ moves the contents of the buffer of computation @m@ to its result.
  -- The resulting computation has an empty buffer.

{- $MonadWriterDoc
  Buffer variables are often useful when one needs to collect some
information, for example while traversing a data structure.  In a sense,
they are the dual of read-only variables, as they propagate outputs
of functions, rather then their inputs.
-}


-- | Gets specific component of the output, using the projection function supplied.
listens       :: (MonadWriter w m) => (w -> b) -> m a -> m (a, b)
listens f m   = liftM (\ ~(a,w) -> (a,f w)) (listen m)


-- | @censor f m@ behaves like @m@ except its output is modified by @f@. 
censor        :: MonadWriter w m => (w -> w) -> m a -> m a
censor f m    = do (a,w) <- listen m
                   tell (f w)   -- the media :-)
                   return a

-- | NOTE: SHOULD THIS BE IN THE LIBRARY?
-- Does what the type suggests.
pass          :: (MonadWriter w m) => m (a, w -> w) -> m a
pass m        = do ((a,f),w) <- listen m
                   tell (f w)
                   return a

-- | 'listenTell m' behaves just like 'listen m' except that it will produce
-- the same output as @m@.  This is how 'listen' used to behave in the
-- older versions of the monadic library.
listenTell    :: MonadWriter w m => m a -> m (a,w)
listenTell m  = do x@(_,w) <- listen m
                   tell w
                   return x



-- | A state monad carries around a piece of state.  It is just like
-- having a read-write variable in an imperative language.
-- See also "Unstable.Control.Monad.StateT".

class (Monad m) => MonadState s m | m -> s where
  get         :: m s
  -- ^ reads the value of the variable 

  put         :: s -> m ()
  -- ^ @put s@ permanently changes the value of the variable to @s@.



-- $MonadStateDoc
-- 

-- | Gets specific component of the state, using the projection function supplied.
gets          :: (MonadState s m) => (s -> a) -> m a
gets f        = liftM f get

-- | Update the state with a function.
modify        :: (MonadState s m) => (s -> s) -> m ()
modify f      = get >>= put . f


-- $ControlDoc
-- /Control transformers/ are used to manipulate the control flow in a program.
-- In general they do not commute between themselves and with other transformers.
-- This means that it is important in what order they are added on top of a monad.
-- Different orders yield monads with different behavior.  
-- TODO: Add examples.



-- | An error (or exception) monad is aware that computations may fail.
-- The type @e@ specifies what errors may occur in a computation.
-- See also "Unstable.Control.Monad.ErrorT".

class (Monad m) => MonadError e m | m -> e where
  raise :: e -> m a
  -- ^ The method @raise e@ raises exception @e@.
  -- It never returns a value.

  handle :: m a -> (e -> m a) -> m a
  -- ^ The method @handle m h@ uses the handler @h@ to handle exceptions
  -- raised in computation @m@.  If no exceptions are
  -- raised, the final computation behaves as @m@.  It is possible
  -- for the handler itself to throw an exception.

-- $ErrorDoc

-- | For backward compatibility
catchError  :: MonadError e m => m a -> (e -> m a) -> m a
catchError  = handle

-- | For backward compatibility
throwError  :: MonadError e m => e -> m a
throwError  = raise




-- | A nondeterminism (or backtracking) monad supports computations that 
-- may fail and backtrack, or produce multiple results.  
--
-- Currently some of the methods in this class are inherited from 
-- the class 'MonadPlus' defined in module "Control.Monad".
-- In this context, we use 'mzero' to indicate no results. 
-- We use 'mplus' to indicate alternatives.
-- See also "Unstable.Control.Monad.NondetT".

class (MonadPlus m) => MonadNondet m where

  findAll     :: m a -> m [a]
  -- ^ @findAll m@ is analogous to the construct found in logic languages
  -- (e.g. Prolog, Curry). It produces all possible results of @m@.

  commit      :: m a -> m a
  -- ^ @commit m@ behaves like @m@ except it will produce at most one result.
  -- Thus, it resembles the /cut/ operator of Prolog.
  -- @findAll (commit m)@ should never produce a list with more than one element.



-- | This method is useful for computations, where a writer is added after nondeterminism.
-- If a programmer needs the output of each alternative,
-- they can use 'findAllW', instead of 'findAll'.
findAllW      :: (MonadNondet m, MonadWriter w m) => m a -> m [(a,w)]
findAllW m    = findAll (listen m)

-- | This method is useful for computations, where state is added after nondeterminism.
-- If a programmer needs the final state after each alternative,
-- they can use 'findAllS', instead of 'findAll'.
findAllS      :: (MonadNondet m, MonadState s m) => m a -> m [(a,s)]
findAllS m    = findAll (do x <- m
                            s <- get
                            return (x,s))

-- | This method is useful for computations, where errors are added after nondeterminism.
-- If a programmer needs all computations, including the ones that failed,
-- they can use 'findAllS', instead of 'findAll'.  The answers are returned
-- like in 'run': Errors are injected in the left component of a sum,
-- while successes are injected on the right.
findAllE      :: (MonadNondet m, MonadError e m) => m a -> m [Either e a]
findAllE m    = findAll (handle (liftM Right m) (return . Left))



-- | See also "Unstable.Control.Monad.ResumeT".
class Monad m => MonadResume m where
  delay       :: m a -> m a
  step        :: (a -> m b) -> (m a -> m b) -> m a -> m b




-- | See also "Unstable.Control.Monad.ContT".
class (Monad m) => MonadCont m where
  callCC      :: ((a -> m b) -> m a) -> m a






