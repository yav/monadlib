module State where

import Prop
import Unstable.Control.Monad.Trans

get_ m            = (get >> m) === m
put_put x y       = (put x >> put y) === put y
put_get x         = (put x >> get) === (put x >> return x)





