module Resume where

import Prop
import Unstable.Control.Monad.Trans

{-
force_return x  = force (return x) === return x
-- force_bind1 m f = force (lift m >>= f) === (lift m >>= force . f)
force_bind2 m f = force (delay m >>= f) === (m >>= f)

lemma1 m        = [ force (delay m),
                    force (delay m >>= return),
                    m >>= return,
                    m ]
-}
 
