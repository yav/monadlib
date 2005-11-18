module Monad.Combinators (module Monad.Combinators, module Control.Monad) where

import Control.Monad

-- | Like '(=<<'), but we need to compute to get the first function.
(<##)              :: Monad m => m (a -> m b) -> m a -> m b
mf <## mx           = do f <- mf
                         x <- mx
                         f x

-- | Like 'map', but we need to compute to get the first function.
(<#)               :: Monad m => m (a -> b) -> m a -> m b
mf <# mx            = do f <- mf
                         x <- mx
                         return (f x)

-- | A convenient name for 'map'.
( # )              :: Monad m => (a -> b) -> m a -> m b
f # mx              = do x <- mx
                         return (f x)

-- | Kleisli composition.  Composes two effectful functions.
(@@)               :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
(g @@ f) x          = g =<< f x



whenM              :: Monad m => m Bool -> m () -> m ()
whenM mb m          = (`when` m) =<< mb 

-- | A monadic version of 'concatMap'.
concatMapM         :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f        = liftM concat . mapM f

-- | A monadic version of 'partition'.
-- The test happens before checking the rest of the list.
partitionM         :: Monad m => (a -> m Bool) -> [a] -> m ([a],[a])
partitionM _ []     = return ([],[])
partitionM p (a:as) = do b <- p a 
                         (xs,ys) <- partitionM   p as
                         return (if b then (a:xs,ys) else (xs,a:ys))


forEach2           :: Monad m => [a] -> [b] -> (a -> b -> m c) -> m [c]
forEach2 xs ys f    = zipWithM f xs ys

forEach3           :: Monad m 
                   => [a] -> [b] -> [c] -> (a -> b -> c -> m d) -> m [d]
forEach3 xs ys zs f = zipWith3M f xs ys zs

-- | Combine 3 lists using a monadic funciton.
zipWith3M          :: Monad m => 
                      (a -> b -> c -> m d) -> [a] -> [b] -> [c] -> m [d]
zipWith3M _ [] _ _                = return []
zipWith3M _ _ [] _                = return []
zipWith3M _ _ _ []                = return []
zipWith3M f (x:xs) (y:ys) (z:zs)  = (:) # f x y z <# zipWith3M f xs ys zs

zipWith3M_         :: Monad m => 
                      (a -> b -> c -> m d) -> [a] -> [b] -> [c] -> m ()
zipWith3M_ _ [] _ _                 = return ()
zipWith3M_ _ _ [] _                 = return ()
zipWith3M_ _ _ _ []                 = return ()
zipWith3M_ f (x:xs) (y:ys) (z:zs)   = f x y z >> zipWith3M_ f xs ys zs


