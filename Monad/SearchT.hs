-- | A search (aka backtracking) transformer.
-- We use the class 'MonadPlus' to specify alternatives.
-- In 'mplus' alternatives are processed left to right, so avoid left recursion.
-- This implementation uses rank-2 polymorphism.

module Monad.SearchT 
  ( SearchT, runSearchOne, runSearchAll, unsafeRunSearch, runSearch
  , ListM(..)
  , module Monad.Prelude
  -- * Examples
  -- $Examples
  ) where


import Monad.Prelude


-- | Computations that may backtrack,
-- side effect as described by /m/,
-- and return a result(s) of type /a/.
newtype SearchT m a  = C { unC :: forall r. (a -> m r -> m r) -> m r -> m r }

-- | The answer from a search.
data ListM m a      = Nil  
                      -- ^ No answer.

                    | Cons a (m (ListM m a))    
                      -- ^ An answer and a computation to produce more answers.

-- | Get the results of a search one at a time.
runSearch          :: Monad m => SearchT m a -> m (ListM m a)
runSearch (C m)     = m (\x xs -> return (Cons x xs)) (return Nil)

-- | Look for at most one answer.
runSearchOne       :: Monad m => SearchT m a -> m (Maybe a)
runSearchOne (C m)  = m (\x _ -> return (Just x)) (return Nothing)

-- | Look for all answers.
runSearchAll       :: Monad m => SearchT m a -> m [a]
runSearchAll (C m)  = m (\x xs -> (x:) # xs) (return [])

-- | Look for exactly one answer.  Crashes if there is no answer.
unsafeRunSearch    :: Monad m => SearchT m a -> m a
unsafeRunSearch (C m) = m (\x _ -> return x) err
  where err = error "unsafeRunSerach: no result."



instance Functor (SearchT m) where
  fmap f m          = liftM f m

instance Monad (SearchT m) where
  return x          = C (\c n -> c x n)
  C a >>= f         = C (\c n -> a (\x xs -> unC (f x) c xs) n)
  
instance Trans SearchT where
  lift m            = C (\c n -> do x <- m
                                    c x n)

instance BaseM m b => BaseM (SearchT m) b where
  inBase m          = lift (inBase m)

instance ReaderM m r => ReaderM (SearchT m) r where
  getR              = lift getR

instance WriterM m w => WriterM (SearchT m) w where
  put x             = lift (put x)

instance StateM m s => StateM (SearchT m) s where
  get               = lift get
  set x             = lift (set x)
  update f          = lift (update f)

instance ExceptM m x => ExceptM (SearchT m) x where
  raise x           = lift (raise x)

instance MonadPlus (SearchT m) where
  mzero             = C (\_ n -> n)
  mplus (C x) (C y) = C (\c n -> x c (y c n))

instance ContM m => ContM (SearchT m) where
  callcc m          = C (\c n -> 
    do (x,k) <- returnCC Nothing
       case x of
         Nothing -> unC (m $ \a -> lift $ cJump (Just a) k) c n
         Just a  -> c a n)


{- $Examples

Backtracking does not affect the output.

> prop_SearchT'WriterM = test == ["World","Hello"]
>   where test  = runId $ execWriter $ runSearchOne
>               $ do put ["Hello"]
>                    mzero
>                  `mplus` 
>                 put ["World"]


Backtracking does not affect the state.

> prop_SearchT'StateM = test == (Just 17, 17)
>   where test  = runId $ runState 42 $ runSearchOne
>               $ do set 17
>                    mzero
>                 `mplus`
>                   get


Backtracking is cancelled by an exception.

> prop_SearchT'ExceptM = test == Left "Error"
>   where test  = runId $ runExcept $ runSearchOne
>               $ raise "Error" `mplus` return 42



Jumping to a continuation cancells (local) backtracking.

> prop_SearchT'ContM = test == [42,10]
>   where test  = runId $ runCont $ runSearchAll
>               $ do (stop,k) <- returnCC False
>                    if stop then return 42  
>                            else cJump True k `mplus` return 17
>                  `mplus` return 10

-}



