module Writer where

import Prop
import Unstable.Control.Monad.Trans
import Data.Monoid

listen_return x   = listen (return x) === return (x,mempty)
listen_bind m1 m2 = listen (m1 >>= m2) === (do (x,w1) <- listen m1
                                               (y,w2) <- listen (m2 x)
                                               return (y, w1 `mappend` w2)) 
listen_listen m   = listen (listen m) === fmap (\x -> (x,mempty)) (listen m)
listen_tell x     = listen (tell x) === return ((),x)
tell_tell x y     = (tell x >> tell y) === tell (x `mappend` y)
tell_return       :: MonadWriter w m => Prop (m ()) 
tell_return       = tell mempty === return ()


