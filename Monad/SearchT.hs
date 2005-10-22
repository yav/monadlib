-- | Implements a monad transformer for searching 
-- (aka backtracking, non-determinism).
-- Adds the possibility to fail, backtrack, and collect multiple results.
--
-- * Commutes with: "Monad.SerachT" 
-- 
-- * Does not commute with: "Monad.ReaderT", "Monad.WriterT", "Monad.StateT", 
--  "Monad.ExceptT", "Monad.ContT"
--
-- Simillar ideas:
-- "Deriving Backtracking Monad Transformers"
--   by Ralf Hinze 
--
-- "Backtracking, interleaving, and terminating monad transformers" 
--   by Oleg Kiselyov, Chung-chieh Shan, Daniel P. Friedman and Amr Sabry 


module Monad.SearchT 
  (SearchT, runSearch, unsafeRunSearch,
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


data Control r m a  = C { failK   :: m r
                        , succK   :: a -> m r
                        , forceK  :: SearchT m a -> m r
                        , choiceK :: SearchT m a -> SearchT m a -> m r
                        }

-- | A computation that can compute zero or more values of type /a/,
-- and in the process can side-effect as described by /m/.
newtype SearchT m a = B (forall r. Control r m a -> m r) 


-- | Make one step of a non-deterministic computation.
runSearch          :: Monad m => SearchT m a -> m (Maybe (a, SearchT m a))
runSearch m         = search [m] []
  where
  search [] []      = return Nothing
  search [] q       = search (reverse q) []
  search (B m:ms) q = m (C no yes fo ch)
    where
    no              = search ms q
    yes a           = return (Just (a, freeze))
    freeze          = foldr mplus mzero (ms ++ reverse q)
    ch x y          = search ms (y:x:q)
    fo m            = do x <- runSearch m
                         case x of
                           Nothing -> search ms q
                           Just (a,xs) -> return (Just (a,xs <+ freeze))
                              

unsafeRunSearch    :: Monad m => SearchT m a -> m a
unsafeRunSearch m   = do x <- runSearch m
                         case x of
                           Just (a,_) -> return a
                           _          -> error "unsafeRunSearch: no result."




instance Monad m => Functor (SearchT m) where
  fmap f m          = liftM f m

instance Monad m => Monad (SearchT m) where
  return x          = lift (return x)
  B m >>= k         = B (\c -> m (C (fail c) (succ c) (force c) (choice c)))
    where
    fail c          = failK c
    succ c a        = let B m = k a in m c
    force c m       = forceK c (m >>= k)
    choice c m1 m2  = choiceK c (m1 >>= k) (m2 >>= k)
                                

instance Trans SearchT where
  lift m            = B (\c -> succK c =<< m)

instance BaseM m b => BaseM (SearchT m) b where
  inBase m          = lift (inBase m)


instance MonadFix m => MonadFix (SearchT m) where
  mfix f            = B (\c -> 
                          do let fail       = do r <- failK c
                                                 return (loop, r)
                                 succ a     = do r <- succK c a
                                                 return (a,r)
                                 force _    = do r <- forceK c (mfix (fromF.f))
                                                 return (loop, r) 
                                 choice _ _ = do r <- choiceK c (mfix (fromL.f)) 
                                                                (mfix (fromR.f))
                                                 return (loop, r)

                             ~(_,r) <- mfix (\ ~(a,_) -> 
                                        let B m = f a 
                                        in m (C fail succ force choice))
                             return r)

    where 
    loop        = error "SearchT: mfix looped"
    fromF (B m) = B (\c -> m (C loop loop (\(B m) -> m c) loop))
    fromL (B m) = B (\c -> m (C loop loop loop (\ (B m) _ -> m c)))
    fromR (B m) = B (\c -> m (C loop loop loop (\_ (B m) -> m c)))


-- $ReaderM
-- Backtracking does not affect the context, so if one alternative changes
-- the context this change will be seen in the next alternative.
--
-- see: <Examples/Search/Reader.hs>
instance ReaderM m r => ReaderM (SearchT m) r where
  get               = lift get
  local f (B m)     = B (\c -> local f (m c))


-- $WriterM
-- Backtracking does not affect the output, so the output of all alternatives
-- that were executed will be joined.  Another way to put this is that all 
-- alternatives are sharing the same buffer.
--
-- see: <Examples/Search/Writer.hs>
instance WriterM m o => WriterM (SearchT m) o where
  put o             = lift (put o)


-- $StateM
-- Backtracking does not affect the state, so changes in one alternative are
-- visible in other alternatives.  Another way to put this is that all 
-- alternatives are sharing the same heap.
--
-- see: <Examples/Search/State.hs>
instance StateM m s => StateM (SearchT m) s where
  peek              = lift peek
  poke s            = lift (poke s)


-- $ExceptM
-- Raising an exception is global, i.e. it will cancel all other alternatives.
--
-- see: <Examples/Search/Except.hs>
instance ExceptM m e => ExceptM (SearchT m) e where
  raise e           = lift (raise e)
  handle (B m) h    = B (\c -> m c `handle` (\x -> let B m' = h x in m' c))


instance Monad m => MonadPlus (SearchT m) where
  mzero             = B (\c -> failK c)
  mplus m1 m2       = B (\c -> choiceK c m1 m2)

instance Monad m => SearchM (SearchT m) where
  force m           = B (\c -> forceK c m)
  findOne m         = lift (runSearch m)


-- XXX: Do continuations



