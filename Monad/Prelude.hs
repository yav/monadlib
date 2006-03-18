module Monad.Prelude 
  ( module Monad.Prelude
  , module Monad.Combinators
  , module Monad.ForEach
  -- * Examples
  -- $ContEx
  ) where

import Monad.Combinators
import Monad.ForEach

version             = (2,0,0)

-- Monad transformers ----------------------------------------------------------

-- | The class of monad transformers.
class Trans t where

  -- | Lift a computation from the underlying monad to the new monad.
  lift             :: Monad m => m a -> t m a


-- | Provides means to execute a computation in the base of a tower of monads.
class (Monad m, Monad b) => BaseM m b | m -> b where
  inBase           :: b a -> m a

instance BaseM IO IO        where inBase x = x
instance BaseM [] []        where inBase x = x
instance BaseM Maybe Maybe  where inBase x = x



-- Monads with an environment --------------------------------------------------


-- | Monads that provide access to a read only context.
class Monad m => ReaderM m r | m -> r where
  
  -- | Get the value of the context.
  getR             :: m r


-- | Promote a function to a computation with an environment.
-- The argument to the function is the environment.
inReader           :: ReaderM m r => (r -> a) -> m a
inReader f          = f # getR



-- | Monads that allow to temorarily modify the context.
class ReaderM m r => ReadUpdM m r | m -> r where 

  -- | Modify the context for the duration of a computation.
  updateR          :: (r -> r) -> m a -> m a
  updateR f m       = do r <- getR
                         setR (f r) m

  -- | Set the context to a particular value for the duration of a computation.
  setR             :: r -> m a -> m a
  setR r m          = updateR (\_ -> r) m         




-- Monads with an output -------------------------------------------------------

-- | Monads that can perform output.
class Monad m => WriterM m w | m -> w where

  -- | Add to the output.
  put              :: w -> m ()

-- | Promote a pair to a computation with output.
-- The first component is the result of the computation,
-- the second component is the output.
inWriter           :: WriterM m w => (a,w) -> m a
inWriter (a,w)      = put w >> return a


-- | Monads that can examine the output that they produce.
-- Definie at least one of 'censor' or 'collect'.

class WriterM m w => CollectorM m w | m -> w where

  -- | Modify the output of a computation.
  -- The ouput of the resulting computation is the output of
  -- the censoring function.
  censor           :: m a -> (w -> m b) -> m (a,b)
  censor m f        = do (a,w) <- collect m
                         b     <- f w
                         return (a,b)

  -- | Gather the output of a computation.
  -- The resulting computation does not produce any output.
  collect          :: m a -> m (a,w)
  collect m         = censor m return
  


-- Stateful computations -------------------------------------------------------

-- | Monads that can manipulate state.
-- Define at least one of: ('get' and 'set') or 'update'

class Monad m => StateM m s | m -> s where

  -- | Get the value of the state.
  get              :: m s
  get               = update id

  -- | Set the value of the state.  Returns the old state.
  set              :: s -> m s
  set x             = update (const x)

  -- | Update the value of the state. Returns the old state.
  update           :: (s -> s) -> m s
  update f          = do x <- get
                         set (f x)
                         return x

-- | Update the state, and discard the old state.
update_            :: StateM m s => (s -> s) -> m ()
update_ f           = update f >> return ()

-- | Set the state, and discard the old state.
set_               :: StateM m s => s -> m ()
set_ s              = set s >> return ()

-- | Promote a state transformer function to a stateful computation.
inState            :: StateM m s => (s -> (a,s)) -> m a
inState f           = do s <- get
                         let (a,s') = f s
                         set s'
                         return a



-- Computations that may raise an exception ------------------------------------

-- | Monads that can throw exceptions.
class Monad m => ExceptM m e | m -> e where

  -- | Raise an exception.
  raise            :: e -> m a

-- | Promote a value of type 'Either', to a computation that may fail.
-- Values tagged with 'Left' are errors, values tagged with 'Right' succeed.
inExcept           :: ExceptM m e => Either e a -> m a
inExcept (Left err) = raise err
inExcept (Right a)  = return a



-- | Monads that can examine the exceptions they throw.
-- Define at least one of 'handle' or 'checkExcept'.

class ExceptM m e => HandlerM m e | m -> e where

  -- | Execute a computation in the context of a handler.
  -- The handler is invoked if the computations raises an exception.
  -- The resulting computation throws an exception, 
  -- if the handler throws an exception.
  handle           :: m a -> (e -> m a) -> m a
  handle m h        = do r <- checkExcept m
                         case r of
                           Left err -> h err
                           Right a  -> return a 

  -- | Turn exceptions into values: 
  -- @Left@ values are exceptions;
  -- @Right@ values are successful results.
  -- The resulting computation does not throw any exceptions.
  checkExcept      :: m a ->  m (Either e a)
  checkExcept m     = (Right # m) `handle` \e -> return (Left e)

-- | The same as 'handle', but with the arguments swapped.
withHandler        :: HandlerM m e => (e -> m a) -> m a -> m a
withHandler h m     = m `handle` h 

-- | Install an error handler that ignores the exception.
-- | The handler is the second argument.
handle_            :: HandlerM m e => m a -> m a -> m a
handle_ m h         = m `handle` \_ -> h



-- Explicit continuations ------------------------------------------------------                   
class Monad m => ContM m where

  -- | Call with current continuation.
  callcc           :: ((a -> m b) -> m a) -> m a


-- | An explicit representation for continuations that store a value.
newtype Cont m a    = Cont ((a, Cont m a) -> m ())

-- | Capture the current continuation.  
-- This function is like 'return', except that it also captures
-- the current continuation.  Later we can use 'cJump' to go back to
-- the continuation with a possibly different value.
returnCC           :: ContM m => a -> m (a, Cont m a)
returnCC x          = callcc (\k -> return (x, Cont k))

cJump              :: ContM m => a -> Cont m a -> m b
cJump x (Cont k)    = k (x, Cont k) >> return unreachable
  where unreachable = error "(bug) cJump: unreachable"


-- $ContEx
-- An example of using the explicit continuations. 
-- We use "Monad.ContT" to add continuations to the 'IO' monad.
-- 
-- > test :: IO ()
-- > test = runCont
-- >      $ do (x,k) <- returnCC 0
-- >           when (x < 10) $ do inBase (print x)
-- >                              cJump (x+1) k 
--
-- This program will print the numbers 0 through 9 on the screen.





