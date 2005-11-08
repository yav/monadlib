-- | An implementation of the exception monad transformer.
-- Adds the ability to raise and handle exceptions.
--
-- * Commutes with: "Monad.ExceptT"
--
-- * Does not commute with: "Monad.ReaderT", "Monad.WriterT", "Monad.StateT", "Monad.BackT", "Monad.ContT"
module Monad.ExceptT 
  ( -- * Instance notes

    -- ** instance ReaderM
    -- $ReaderM

    -- ** instance WriterM
    -- $WriterM

    -- ** instance StateM
    -- $StateM

    -- ** instance SearchM
    -- $SearchM

  ExceptT, runExcept, unsafeRunExcept, module Monad.Prelude
  ) where

import Monad.Prelude
import Control.Monad.Fix

-- | A computation that computes a value of type /a/, and may raise 
-- exceptions of type /x/, and can also side-effect as described by /m/.
newtype ExceptT x m a = E (forall r. (a -> m r) -> (x -> m r) -> m r)

-- | Execute a computation: if it succeeds the result can
-- be found in the right component of the result (the _right_ answer :-).
-- If the computation failed, the exception is reported
-- in the left component of the result.
runExcept          :: Monad m => ExceptT x m a -> m (Either x a)
runExcept (E f)     = f (return . Right) (return . Left)


unsafeRunExcept    :: Monad m => ExceptT x m a -> m a
unsafeRunExcept (E m) = m return (\_ -> error "unsafeRunExcept: exception.")


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

  mfix f            = E (\ok fail -> do let ok' a   = do r <- ok a
                                                         return (a,r)
                                            fail' x = do r <- fail x
                                                         return (error "mfix looped",r)
                                        ~(_,r) <- mfix (\ ~(a,_) -> let E g = f a in g ok' fail')
                                        return r)

-- $ReaderM
-- Exceptions are handled in the context in which the exception was risen.
--
-- see: <Examples/Except/Reader.hs>
instance ReaderM m r => ReaderM (ExceptT x m) r where
  get               = lift get
  local f (E m)     = E (\ok fail -> local f (m ok fail))


-- $WriterM
-- Raising an exception does not affect the output.
--
-- see: <Examples/Except/Writer.hs>
instance WriterM m o => WriterM (ExceptT x m) o where
  put o             = lift (put o)



-- $StateM
-- Raising an exception does not affect the state.
--
-- see: <Examples/Except/State.hs>
instance StateM m s => StateM (ExceptT x m) s where
  peek              = lift peek
  poke s            = lift (poke s)

instance ExceptM (ExceptT x m) x where
  raise x           = E (\_ fail -> fail x)
  handle (E f) h    = E (\ok fail -> f ok (\x -> let E g = h x
                                                 in g ok fail))


-- $SearchM
-- Raising an exception in one alternative does not prevent other 
-- alternatives form executing.
--
-- see: <Examples/Except/Search>
instance MonadPlus m => MonadPlus (ExceptT x m) where
  mzero             = E (\_ _ -> mzero)
  mplus (E f) (E g) = E (\ok fail -> f ok fail `mplus` g ok fail)

instance SearchM m => SearchM (ExceptT x m) where
  force (E f)       = E (\ok fail -> force (f ok fail))


  findOne m         = E (\ok fail -> 
                          do mr <- findOne (runExcept m)
                             case mr of
                               Nothing -> ok Nothing
                               Just (r,m) ->
                                 case r of
                                   -- Left x -> fail x
                                   Left _ -> let E x = findOne (toCont m)
                                             in x ok fail
                                   Right a -> ok (Just (a, toCont m)))


toCont             :: Monad m => m (Either x a) -> ExceptT x m a
toCont m            = inExcept =<< lift m

instance ContM m => ContM (ExceptT x m) where
  callcc f          = toCont    $ callcc $ \k -> 
                      runExcept $ f      $ \a -> lift (k (Right a))
  


                    

