-- | This is a private module and is not to be imported
-- directly by non-library modules.

module Unstable.Control.Monad.Private.Utils where

import Prelude(Monad(..),(.),String)
import Control.Monad(MonadPlus(..))

import Unstable.Control.Monad.Trans




-- has base
inBase'       :: (HasBaseMonad m n, MonadTrans t) => n a -> t m a
inBase' m     = lift (inBase m)

mapBase'      :: (HasBaseMonad m n, MapTrans t) => (forall a. n a -> n a) -> t m a -> t m a
mapBase' f    = mapTrans (mapBase f)

-- monad 
return'       :: (Monad m, MonadTrans t) => a -> t m a
return' x     = lift (return x)

fail'         :: (Monad m, MonadTrans t) => String -> t m a
fail' msg     = lift (fail msg)

-- reader
ask'          :: (MonadTrans t, MonadReader r m) => t m r
ask'          = lift ask
local' map f  = map (local f)

-- writer
tell'                   :: (MonadWriter w m, MonadTrans t) => w -> t m ()
tell' w                 = lift (tell w)

listen1' mk unmk add m  = mk (do (x,w) <- listen (unmk m)
                                 return (add w x))
listen2' mk unmk add m  = mk (\s -> do (x,w) <- listen (unmk m s)
                                       return (add w x))

-- state
get'      :: (MonadTrans t, MonadState s m) => t m s
get'      = lift get

put'      :: (MonadTrans t, MonadState s m) => s -> t m ()
put' s    = lift (put s)

-- error
raise' e              = lift (raise e)
handle1' mk unmk m h  = mk (handle (unmk m) (unmk . h))
handle2' mk unmk m h  = mk (\y -> handle (unmk m y) (\e -> unmk (h e) y))

-- mplus
mzero'    :: (MonadTrans t, MonadPlus m) => t m a
mzero'              = lift mzero
mplus1' mk unmk m n = mk (mplus (unmk m) (unmk n))
mplus2' mk unmk m n = mk (\y -> unmk m y `mplus` unmk n y)

-- cont
callCC1' mk unmk ret f  = mk (callCC (\br -> unmk (f (\a -> lift (br (ret a))))))
callCC2' mk unmk ret f  = mk (\s -> callCC (\br -> unmk (f (\a -> lift (br (ret a s)))) s))




