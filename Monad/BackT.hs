-- | Implements the backtracking (aka non-determinism) monad transformer.
-- Adds the possibility to fail, backtrack, and collect multiple results.
--
-- * Commutes with: "Monad.BackT" 
-- 
-- * Does not commute with: "Monad.ReaderT", "Monad.WriterT", "Monad.StateT", "Monad.ExceptT", "Monad.ContT"
module Monad.BackT 
  (BackT, Traversal(..), breadthFirst, depthFirst, findAll, findOne, 
  -- * Instance notes
  
  -- ** instance ReaderM 
  -- $ReaderM
  
  -- ** instance WriterM
  -- $WriterM

  -- ** instance StateM
  -- $StateM

  -- ** instance ExceptM
  -- $ExceptM

  module Monad.Prelude) where
  
import Monad.Prelude
import Control.Monad.Fix
import Control.Monad(MonadPlus(..))


-- | A computation that can compute zero or more values of type /a/,
-- and in the process can side-effect as described by /m/.
newtype BackT m a                   = B (forall r. m r -> (a -> m r) -> (BackT m a -> BackT m a -> m r) -> m r)


newtype Traversal m x               = T { traverse :: (forall a. m a -> (x -> m a -> m a) -> BackT m x -> m a) }

-- | A strategy for traversing alternatives in a breadth first manner.
breadthFirst                       :: Traversal m x 
breadthFirst                        = T (\nil cons m -> bf' nil cons [m] [])
  where bf' nil cons (B m : q1) q2  = m (bf' nil cons q1 q2) 
                                        (\x -> cons x (bf' nil cons q1 q2)) 
                                        (\opt1 opt2 -> bf' nil cons q1 (opt2 : opt1 : q2))
        bf' nil cons [] []          = nil
        bf' nil cons [] q           = bf' nil cons (reverse q) []


-- | A strategy for traversing alternatives in a depth first manner.
depthFirst                         :: Traversal m x 
depthFirst                          = T (\nil cons m -> df' nil cons [m])
  where df' nil cons (B m : q)      = m (df' nil cons q) 
                                        (\x -> cons x (df' nil cons q)) 
                                        (\opt1 opt2 -> df' nil cons (opt1 : opt2 : q))
        df' nil cons []             = nil


-- | Execute all alternatives to get all possible results, using the specified strategy.
findAll                            :: Monad m => Traversal m a -> BackT m a -> m [a]
findAll method m                    = do xs <- traverse method nil cons m
                                         return (xs [])
  where nil                         = return id 
        cons x m                    = do xs <- m
                                         return ((x:) . xs)

-- | Execute until we find the first successful result, using the specified strategy.
findOne                            :: Monad m => Traversal m a -> BackT m a -> m (Maybe a)
findOne method m                    = traverse method nil cons m
  where nil                         = return Nothing
        cons a _                    = return (Just a)                                       
  







instance Functor (BackT m) where
  fmap f (B m)      = B (\no yes ch -> m no 
                                         (\a -> yes (f a))
                                         (\opt1 opt2 -> ch (fmap f opt1) (fmap f opt2)))

instance Monad (BackT m) where
  return x          = B (\no yes ch -> yes x)
  B m >>= k         = B (\no yes ch -> m no 
                                         (\a -> let B m' = k a in m' no yes ch)
                                         (\opt1 opt2 -> ch (opt1 >>= k) (opt2 >>= k)))

instance Trans BackT where
  lift m            = B (\no yes ch -> yes =<< m)

instance BaseM m b => BaseM (BackT m) b where
  inBase m          = lift (inBase m)

instance MonadFix m => MonadFix (BackT m) where
  mfix f            = B (\no yes ch -> mdo let B m = f a 
                                           ~(a,r) <- m (do r <- no
                                                           return (error "BackT: mfix looped -- no", r))
                                                       (\a -> do r <- yes a
                                                                 return (a,r))
                                                       (\_ _ -> do r <- ch (mfix (left . f)) (mfix (right . f))
                                                                   return (error "BackT: mfix looped -- choice", r))
                                           return r)

    where left (B m)  = B (\no yes ch -> m (error "BackT: mfix looped -- left -> no")
                                           (error "BackT: mfix looped -- left -> yes")
                                           (\(B opt1) _ -> opt1 no yes ch))
          right (B m) = B (\no yes ch -> m (error "BackT: mfix looped -- right -> no")
                                           (error "BackT mfix looped -- right -> yes")
                                           (\_ (B opt2) -> opt2 no yes ch))


-- $ReaderM
-- Backtracking does not affect the context, so if one alternative changes
-- the context this change will be seen in the next alternative.
--
-- see: runReaderIn in <Examples/Back.hs>
instance ReaderM m r => ReaderM (BackT m) r where
  get               = lift get
  local f (B m)     = B (\no yes ch -> local f (m no yes ch))


-- $WriterM
-- Backtracking does not affect the output, so the output of all alternatives
-- that were executed will be joined.  Another way to put this is that all alternatives
-- are sharing the same buffer.
--
-- see: runWriterIn in <Examples/Back.hs>
instance WriterM m o => WriterM (BackT m) o where
  put o             = lift (put o)


-- $StateM
-- Backtracking does not affect the state, so changes in one alternative are
-- visible in other alternatives.  Another way to put this is that all alternatives
-- are sharing the same heap.
--
-- see: runStateIn in <Examples/Back.hs>
instance StateM m s => StateM (BackT m) s where
  peek              = lift peek
  poke s            = lift (poke s)


-- $ExceptM
-- Raising an exception is global, i.e. it will cancel all other alternatives.
--
-- see: runBackExceptIn in <Examples/Control.hs>
instance ExceptM m e => ExceptM (BackT m) e where
  raise e           = lift (raise e)
  handle (B m) h    = B (\no yes ch -> m no yes ch `handle` (\x -> let B m' = h x in m' no yes ch))

instance Monad m => MonadPlus (BackT m) where
  mzero             = B (\no _ _ -> no)
  mplus m1 m2       = B (\_ _ ch -> ch m1 m2)






