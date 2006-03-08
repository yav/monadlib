module Monad.Combinators (module Monad.Combinators, module Control.Monad) where

import Control.Monad


-- Monadic functions -----------------------------------------------------------

-- | A convenient name for 'map'.
( # )              :: Monad m => (a -> b) -> m a -> m b
f # mx              = do x <- mx
                         return (f x)

-- | Kleisli composition.  Composes two effectful functions.
(@@)               :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
(g @@ f) x          = g =<< f x

-- | Like 'map', but we need to compute to get the first function.
-- The function is evaluated before the argument.
(<#)               :: Monad m => m (a -> b) -> m a -> m b
mf <# mx            = do f <- mf
                         x <- mx
                         return (f x)

-- | Like '=<<', but we need to compute to get the first function.
-- The function is evaluated before the argument.
(<##)              :: Monad m => m (a -> m b) -> m a -> m b
mf <## mx           = do f <- mf
                         x <- mx
                         f x



-- Monadic booleans ------------------------------------------------------------

-- | If the 1st argument returns 'True' execute the 2nd argument,
-- otherwise execute the 3rd argument.
ifM                :: Monad m => m Bool -> m a -> m a -> m a
ifM m t e           = do x <- m
                         if x then t else e

-- | If the 1st argument returns 'True', execute the 2nd argument.
whenM              :: Monad m => m Bool -> m () -> m ()
whenM m t           = ifM m t (return ())

-- | If the 1st argument returns 'True', execute the second argument,
-- otherwise return 'False'.
andM               :: Monad m => m Bool -> m Bool -> m Bool
andM mx my          = ifM mx my (return False) 

-- | If the 1st argument returns 'False', execute the second argument,
-- otherwise return 'True'.
orM                :: Monad m => m Bool -> m Bool -> m Bool
orM mx my           = ifM mx (return True) my



-- Monadic lists ---------------------------------------------------------------

-- | A monadic version of 'concatMap'.
concatMapM         :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f        = liftM concat . mapM f

-- | A monadic version of 'partition'.
-- The test happens before checking the rest of the list.
partitionM         :: Monad m => (a -> m Bool) -> [a] -> m ([a],[a])
partitionM _ []     = return ([],[])
partitionM p (a:as) = do b <- p a
                         (xs,ys) <- partitionM p as
                         return (if b then (a:xs,ys) else (xs,a:ys))

-- | Traverse two lists in parallel, collecting the results.
-- The traversing stops if either of the lists runs out of elemnets.
forEach2           :: Monad m => [a] -> [b] -> (a -> b -> m c) -> m [c]
forEach2 xs ys f    = zipWithM f xs ys

-- | Traverse two lists in parallel, just for the side effects.
-- The traversing stops if either of the lists runs out of elemnets.
forEach2_          :: Monad m => [a] -> [b] -> (a -> b -> m ()) -> m ()
forEach2_ xs ys f   = zipWithM_ f xs ys

-- | Traverse three lists in parallel, collecting the results.
-- The traversing stops if either of the lists runs out of elemnets.
forEach3           :: Monad m 
                   => [a] -> [b] -> [c] -> (a -> b -> c -> m d) -> m [d]
forEach3 (x:xs) (y:ys) (z:zs) f   = (:) # f x y z <# forEach3 xs ys zs f
forEach3 _ _ _ _                  = return []

-- | Traverse three lists in parallel, just for the side effects.
-- The traversing stops if either of the lists runs out of elemnets.
forEach3_          :: Monad m 
                   => [a] -> [b] -> [c] -> (a -> b -> c -> m ()) -> m ()
forEach3_ (x:xs) (y:ys) (z:zs) f  = f x y z >> forEach3_ xs ys zs f
forEach3_ _ _ _ _                 = return ()



