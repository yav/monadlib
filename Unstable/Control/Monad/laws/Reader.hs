module Reader where

import Prop
import Unstable.Control.Monad.Trans


local_return f m    = local f (return m) === return m
local_bind f m1 m2  = (local f m1 >>= \x -> local f (m2 x)) === local f (m1 >>= m2)
local_local f g m   = local f (local g m) === local (g . f) m
local_get f         = local f get === fmap f get

get_bind m          = (get >> m) === m


